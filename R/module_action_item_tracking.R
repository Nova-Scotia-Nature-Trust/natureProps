# UI ----
module_action_item_tracking_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = FALSE,
      height = "100%",
      # Sidebar ----
      layout_sidebar(
        sidebar = sidebar(
          title = NULL,
          open = TRUE,
          width = 300,
          accordion(
            open = FALSE,
            multiple = FALSE,
            # Accordion Panel 01 ----
            accordion_panel(
              title = "Property & Action Type",
              value = "details_panel",
              selectizeInput(
                ns("property"),
                "Select Property",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              selectizeInput(
                ns("action_item_type"),
                "Select Action Item Type",
                choices = NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            # Accordion Panel 02 ----
            accordion_panel(
              title = "Status, Dates & Lead",
              dateInput(
                ns("action_due_date"),
                "Action Due Date",
                value = NA
              ),
              selectizeInput(
                ns("team_lead"),
                "Assign Team Lead",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              selectizeInput(
                ns("action_item_status"),
                "Select Action Status",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              dateInput(
                ns("action_completed_date"),
                "Action Completed Date",
                value = NA
              ),
            ),
            # Accordion Panel 03 ----
            accordion_panel(
              title = "Notes",
              textAreaInput(
                ns("action_item_notes"),
                label = "Action Item Notes",
                "",
                height = "200px",
                width = "100%"
              )
            )
          ),
          # Action Buttons ----
          actionButton(
            inputId = ns("submit_edits"),
            label = "Submit Edits",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          )
        ),
        # Module UI :: Data View ----
        module_data_viewer_ui(
          ns("action_item_viewer"),
          panel_id = "action_item_panel"
        )
      )
    )
  )
}

# Server ----
module_action_item_tracking_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("property", sv_required())
    iv$add_rule("action_item_type", sv_required())
    iv$enable()

    ## Reactive :: Property List ----
    property_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT sai.property_id AS id,
                pr.property_name
        FROM securement_action_items AS sai
        LEFT JOIN properties AS pr ON sai.property_id = pr.id
        ORDER BY pr.property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = isolate(input$property),
        server = TRUE
      )
    })

    ## Reactive :: Selected Property ----
    selected_property_name <- reactive({
      req(input$property)
      property_list() |>
        filter(id == input$property) |>
        pull(property_name)
    })

    ## Reactive :: Action Item Types ----
    action_item_type <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, type_value FROM action_item_type ORDER BY id;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "action_item_type",
        choices = setNames(
          action_item_type()$id,
          action_item_type()$type_value
        ),
        selected = isolate(input$action_item_type),
        server = TRUE
      )
    })

    ## Reactive :: Action Item Status ----
    action_item_status <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, status_value FROM action_item_status ORDER BY status_value;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "action_item_status",
        choices = setNames(
          action_item_status()$id,
          action_item_status()$status_value
        ),
        selected = isolate(input$action_item_status),
        server = TRUE
      )
    })

    ## Reactive :: Team Lead ----
    team_lead <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "team_lead",
        choices = setNames(
          team_lead()$id,
          team_lead()$team_value
        ),
        selected = isolate(input$team_lead),
        server = TRUE
      )
    })

    ## Observe Event :: Submit Action Edit ----
    observeEvent(input$submit_edits, {
      req(iv$is_valid())

      # At least one of these fields must be populated
      req(
        any(
          isTruthy(input$action_due_date),
          isTruthy(input$team_lead),
          isTruthy(input$action_item_status),
          isTruthy(input$action_item_notes),
          isTruthy(input$action_completed_date)
        )
      )

      # Minimum starting point
      updated_data <- tibble(
        property_id = input$property,
        action_item_type_id = input$action_item_type
      )

      # Add field to the dataframe if they're truthy
      if (isTruthy(input$action_due_date)) {
        updated_data <- updated_data |>
          mutate(action_due_date = input$action_due_date)
      }
      if (isTruthy(input$team_lead)) {
        updated_data <- updated_data |>
          mutate(team_lead_id = input$team_lead)
      }
      if (isTruthy(input$action_item_status)) {
        updated_data <- updated_data |>
          mutate(action_item_status_id = input$action_item_status)
      }
      if (isTruthy(input$action_item_notes)) {
        updated_data <- updated_data |>
          mutate(action_item_notes = input$action_item_notes)
      }
      if (isTruthy(input$action_completed_date)) {
        updated_data <- updated_data |>
          mutate(action_completed_date = input$action_completed_date)
      }

      # Write changes to database
      dbx::dbxUpdate(
        db_con,
        table = "securement_action_items",
        records = updated_data,
        where_cols = c("property_id", "action_item_type_id")
      )

      db_updated(db_updated() + 1)

      # Return message
      shinyalert(
        title = "Success",
        text = glue::glue(
          "{nrow(updated_data)} action items updated for {selected_property_name()}"
        ),
        type = "success",
        timer = 5000
      )
    })

    ## Observe Event :: Clear Inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(session, "property", selected = character(0))
      updateSelectizeInput(session, "action_item_type", selected = character(0))
      updateSelectizeInput(
        session,
        "action_item_status",
        selected = character(0)
      )
      updateSelectizeInput(session, "team_lead", selected = character(0))
      updateTextAreaInput(session, "action_item_notes", value = "")
      updateDateInput(session, "action_due_date", value = as.Date(NA))
      updateDateInput(session, "action_completed_date", value = as.Date(NA))
    })

    ## Module Server :: Data Viewer ----
    module_data_viewer_server(
      "action_item_viewer",
      db_con,
      db_updated,
      prop_filter = selected_property_name,
      panel_id = "action_item_panel"
    )
  })
}
