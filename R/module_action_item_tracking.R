module_action_item_tracking_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%", # Make card fill available space
      layout_sidebar(
        sidebar = sidebar(
          "",
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


module_action_item_tracking_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    # Create reactive values to store the submitted values
    submitted_values <- reactiveValues(
      pids = NULL,
      checklist_fields = NULL,
      action_value = NULL,
      has_submitted = FALSE
    )


    # Define a reactive for PIDs that depends on db_updated, if provided
    pids_reactive <- reactive({
      # Only try to use db_updated if it is not NULL.
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }
      # Query database for PIDs
      dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
        pull() |>
        sort()
    })

    # Get checklist fields from database
    checklist_fields <- dbGetQuery(db_con, "SELECT column_name
                    FROM information_schema.columns
                    WHERE table_name = 'parcels';
           ") |> pull()

    # Restrict checklist_fields to a subset of columns
    ## *** Need some more robust selection here, possibly lookup table *** ##
    checklist_fields <- checklist_fields[14:37]
    checklist_fields <- base::setdiff(
      checklist_fields,
      c(
        "appraisal_date", "appraiser_name",
        "af_transaction", "llt_funding_id"
      )
    )

    action_item_names <- read_xlsx("inputs/field and function mapping tables/df_views.xlsx") |>
      filter(db_name %in% checklist_fields) |>
      pull(df_name)

    checklist_fields <- setNames(checklist_fields, action_item_names)

    # Get action values from the database
    action_lu <- dbGetQuery(db_con, "SELECT * FROM action_item_status;")
    action_values <- setNames(action_lu$id, action_lu$action_value)

    # Initialize the select inputs with data from the DB
    observe({
      # Update the PIDs selectize input – this will update whenever pids_reactive() changes
      updateSelectizeInput(session, "pids", choices = pids_reactive(), server = TRUE)

      # Update checklist fields selectize input
      updateSelectizeInput(session, "checklist_fields", choices = checklist_fields, server = TRUE)

      # Update action values select input if available
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

      # Reset action value dropdown to the first value
      if (length(action_values) > 0) {
        updateSelectizeInput(session, "action_value", selected = action_values[1])
      }

      # Clear the status message
      output$status_message <- renderUI(NULL)
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

      # Create a reactive expression to build the new actions tibble
      new_actions <- reactive({
        tibble(
          pid = rep(input$pids, each = length(input$checklist_fields)),
          action_field = rep(input$checklist_fields, times = length(input$pids)),
          action_value = input$action_value
        ) |>
          pivot_wider(names_from = action_field, values_from = action_value)
      })

      print(new_actions())

      # Update the database with the new actions
      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = new_actions(),
        where_cols = c("pid")
      )

      # Save the submitted values
      submitted_values$pids <- input$pids
      submitted_values$checklist_fields <- input$checklist_fields
      submitted_values$action_value <- input$action_value

      # Create and display a success message using the saved values
      success_message <- sprintf(
        "Successfully updated %d checklist items across %d properties with action value: %s",
        length(submitted_values$checklist_fields),
        length(submitted_values$pids),
        submitted_values$action_value
      )

      output$status_message <- renderUI({
        div(
          class = "alert alert-success mt-3",
          success_message
        )
      })

      # Log the action
      message(sprintf(
        "Updated %d checklist items for %d properties to status '%s'",
        length(submitted_values$checklist_fields),
        length(submitted_values$pids),
        submitted_values$action_value
      ))
    })
  })
}
