# UI ----
module_action_item_tracking_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = FALSE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          title = "Assign Securement Actions",
          open = TRUE,
          width = 300,
          accordion(
            open = FALSE,
            multiple = FALSE,
            # accordion_panel(
            #   title = "Setup Template",
            #   value = "setup_panel",
            #   selectizeInput(
            #     ns("property_setup"),
            #     "Select property",
            #     choices = NULL,
            #     multiple = FALSE,
            #     width = "100%"
            #   ),
            #   actionButton(
            #     inputId = ns("setup_template"),
            #     label = "Setup Securement Action Template",
            #     class = "btn-primary",
            #     width = "100%"
            #   )
            # ),
            accordion_panel(
              title = "Action Item Details",
              value = "details_panel",
              selectizeInput(
                ns("property"),
                "Select property",
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
              ),
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
              textAreaInput(
                ns("action_item_notes"),
                label = "Action Item Notes",
                "",
                height = "150px",
                width = "100%"
              ),
              actionButton(
                inputId = ns("submit_actions"),
                label = "Submit Action",
                class = "btn-primary"
              ),
              actionButton(
                inputId = ns("clear_inputs"),
                label = "Clear Inputs",
                class = "btn-secondary"
              )
            )
          )
        ),
        # Main layout - data viewer
        module_data_viewer_ui(
          ns("action_item_viewer"),
          panel_id = "panel_03"
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
    # iv$add_rule("action_item_status", sv_required())
    # iv$add_rule("team_lead", sv_required())
    iv$enable()

    ## Property list for action items (with securement data) ----
    props_action_reactive <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties 
         WHERE securement_probability_id IS NOT NULL 
               OR anticipated_closing_date IS NOT NULL
         ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = setNames(
          props_action_reactive()$id,
          props_action_reactive()$property_name
        ),
        selected = isolate(input$property),
        server = TRUE
      )
    })

    # ## Property list for setup (without securement data) ----
    # props_setup_reactive <- reactive({
    #   db_updated()
    #   dbGetQuery(
    #     db_con,
    #     "SELECT id, property_name FROM properties
    #      WHERE securement_probability_id IS NULL
    #            AND anticipated_closing_date IS NULL
    #      ORDER BY property_name;"
    #   )
    # })

    # observe({
    #   updateSelectizeInput(
    #     session,
    #     "property_setup",
    #     choices = setNames(
    #       props_setup_reactive()$id,
    #       props_setup_reactive()$property_name
    #     ),
    #     selected = isolate(input$property_setup),
    #     server = TRUE
    #   )
    # })

    selected_property_name <- reactive({
      req(input$property)

      props_action_reactive() |>
        filter(id == input$property) |>
        pull(property_name)
    })

    # ## Setup template action ----
    # observeEvent(input$setup_template, {
    #   req(input$property_setup)

    #   action_type_ids <- dbGetQuery(db_con, "SELECT id FROM action_item_type")

    #   action_structure <- expand.grid(
    #     action_item_type_id = action_type_ids$id,
    #     property_id = input$property_setup
    #   )

    #   append_db_data(
    #     "securement_action_items",
    #     data = action_structure,
    #     con = db_con,
    #     silent = TRUE
    #   )

    #   db_updated(db_updated() + 1)

    #   shinyalert(
    #     title = "Success",
    #     text = glue::glue(
    #       "Template created with {nrow(action_structure)} action items"
    #     ),
    #     type = "success",
    #     timer = 5000
    #   )
    # })

    ## Action item types ----
    action_item_types <- reactive({
      db_updated()
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
          action_item_types()$id,
          action_item_types()$type_value
        ),
        selected = isolate(input$action_item_type),
        server = TRUE
      )
    })

    ## Action item statuses ----
    action_item_statuses <- reactive({
      db_updated()
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
          action_item_statuses()$id,
          action_item_statuses()$status_value
        ),
        selected = isolate(input$action_item_status),
        server = TRUE
      )
    })

    ## Team leads ----
    team_leads <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "team_lead",
        choices = setNames(team_leads()$id, team_leads()$team_value),
        selected = isolate(input$team_lead),
        server = TRUE
      )
    })

    ## Upsert action item ----
    observeEvent(input$submit_actions, {
      req(iv$is_valid())
      req(input$action_item_type)

      # Start with all selected action types
      upsert_records <- tibble(
        property_id = input$property,
        action_item_type_id = input$action_item_type
      )

      # Dynamically add optional fields only if they are truthy
      if (isTruthy(input$action_due_date)) {
        upsert_records <- upsert_records |>
          mutate(action_due_date = input$action_due_date)
      }
      if (isTruthy(input$team_lead)) {
        upsert_records <- upsert_records |>
          mutate(team_lead_id = input$team_lead)
      }
      if (isTruthy(input$action_item_status)) {
        upsert_records <- upsert_records |>
          mutate(action_item_status_id = input$action_item_status)
      }
      if (isTruthy(input$action_item_notes)) {
        upsert_records <- upsert_records |>
          mutate(action_item_notes = input$action_item_notes)
      }
      if (isTruthy(input$action_completed_date)) {
        upsert_records <- upsert_records |>
          mutate(action_completed_date = input$action_completed_date)
      }

      # Upsert to DB
      dbx::dbxUpsert(
        db_con,
        table = "securement_action_items",
        records = upsert_records,
        where_cols = c("property_id", "action_item_type_id")
      )

      db_updated(db_updated() + 1)

      shinyalert(
        title = "Success",
        text = glue::glue(
          "{nrow(upsert_records)} action items updated for {selected_property_name()}"
        ),
        type = "success",
        timer = 5000
      )
    })

    ## Clear inputs ----
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
      updateDateInput(session, "action_due_date", value = NA)
      updateDateInput(session, "action_completed_date", value = NA)
    })

    ## Data viewer ----
    module_data_viewer_server(
      "action_item_viewer",
      db_con,
      db_updated,
      prop_filter = selected_property_name,
      panel_id = "panel_03"
    )
  })
}
