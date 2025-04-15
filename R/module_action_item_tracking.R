module_action_item_tracking_ui <- function(id) {
  ns <- NS(id)
  
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%", # Make card fill available space
      layout_sidebar(
        sidebar = sidebar(
          "Sidebar",
          open = TRUE,
          actionButton(inputId = ns("submit_actions"), label = "Submit Actions"),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
        ),
        # Main layout
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(8, 4),
            # First card
            card(
              height = "100%",
              card_header(h5("Assign Action Items")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                    selectizeInput(
                    ns("pids"),
                    "Select Properties (PIDs)",
                    choices = NULL,
                    multiple = TRUE, 
                    width = "80%"
                  ),
                  selectizeInput(
                    ns("checklist_fields"),
                    "Select Checklist Fields",
                    choices = NULL,
                    multiple = TRUE,
                    width = "80%"
                  ),
                  
                  # Dropdown without initial values
                  selectizeInput(
                    ns("action_value"),
                    "Select Action Value",
                    choices = NULL
                  ),
                  uiOutput(ns("status_message")),
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            # Second card
            card(
              height = "100%",
              card_header(h5("Other things")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            )
          )
        )
      )
    )
  )
}


module_action_item_tracking_server <- function(id, db_con, prd_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    # Create reactive values to store the submitted values
    submitted_values <- reactiveValues(
      pids = NULL,
      checklist_fields = NULL,
      action_value = NULL,
      has_submitted = FALSE
    )
    
    
    # Placeholder function to get PIDs from database
    pids <- dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
      pull()
    
    # Placeholder function to get checklist fields from database
    checklist_fields <- dbGetQuery(db_con, "SELECT column_name
                    FROM information_schema.columns
                    WHERE table_name = 'parcels';
           ") |>
      pull()
    
    checklist_fields <- checklist_fields[14:37]
    
    action_item_names <- read_xlsx("inputs/field and function mapping tables/df_views.xlsx") |> 
      filter(db_name %in% checklist_fields) |> 
      pull(df_name)
    
    checklist_fields <- setNames(checklist_fields, action_item_names)
    
    # Placeholder function to get action values from database
    action_lu <- dbGetQuery(db_con, "SELECT * FROM action_item_status;")
    action_values <- setNames(action_lu$id, action_lu$action_value)
    
    # Initialize the select inputs with data from DB
    observe({
      # Get PIDs
      updateSelectizeInput(session, "pids", choices = pids, server = TRUE)
      
      # Get checklist fields
      updateSelectizeInput(session, "checklist_fields", choices = checklist_fields, server = TRUE)
      
      # Get action values from the database
      # Make sure action_values is not empty before updating
      if (length(action_values) > 0) {
        updateSelectInput(session, "action_value",
                          choices = action_values,
                          selected = action_values[1]
        )
      }
    })
    
    # Clear inputs when clear button is clicked
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(session, "pids", selected = character(0))
      updateSelectizeInput(session, "checklist_fields", selected = character(0))
      
      # Reset action value dropdown to first value
      if (length(action_values) > 0) {
        updateSelectizeInput(session, "action_value", selected = action_values[1])
      }
      
      # Clear the status message
      output$status_message <- renderUI(NULL)
      
      # Reset submitted flag
      # submitted_values$has_submitted <- FALSE
    })
    
    # Update database when submit button is clicked
    observeEvent(input$submit_actions, {
      # Validate inputs
      if (length(input$pids) == 0) {
        output$status_message <- renderUI({
          div(class = "alert alert-warning", "Please select at least one property (PID).")
        })
        return()
      }
      
      if (length(input$checklist_fields) == 0) {
        output$status_message <- renderUI({
          div(class = "alert alert-warning", "Please select at least one checklist field.")
        })
        return()
      }
      
      if (is.null(input$action_value) || input$action_value == "") {
        output$status_message <- renderUI({
          div(class = "alert alert-warning", "Please select an action value.")
        })
        return()
      }
      
      new_actions <- reactive({
        tibble(
          pid = rep(input$pids, each = length(input$checklist_fields)),
          action_field = rep(input$checklist_fields, times = length(input$pids)),
          action_value = input$action_value
        ) |>
          pivot_wider(names_from = action_field, values_from = action_value)
      })
      
      
      print(new_actions())
      
      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = new_actions(),
        where_cols = c("pid")
      )
      
      # Store the values at the time of submission
      submitted_values$pids <- input$pids
      submitted_values$checklist_fields <- input$checklist_fields
      submitted_values$action_value <- input$action_value
      # submitted_values$has_submitted <- TRUE
      
      # Create a static success message using the saved values
      success_message <- sprintf(
        "Successfully updated %d checklist items across %d properties with action value: %s",
        length(submitted_values$checklist_fields),
        length(submitted_values$pids),
        submitted_values$action_value
      )
      
      # Show success message as a static text that won't change with input changes
      output$status_message <- renderUI({
        div(
          class = "alert alert-success mt-3",
          success_message
        )
      })
      
      # Log the action (in a real app, you might want to store this in a log table)
      message(sprintf(
        "Updated %d checklist items for %d properties to status '%s'",
        length(submitted_values$checklist_fields),
        length(submitted_values$pids),
        submitted_values$action_value
      ))
    })
  })
}
