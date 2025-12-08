# UI ----
# UI ----
module_internal_communications_UI <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        ## Sidebar inputs ----
        sidebar = sidebar(
          "",
          open = TRUE,
          selectizeInput(
            ns("property"),
            "Select property",
            choices = NULL,
            multiple = FALSE,
            width = "80%"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          ),
        ),
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(7, 5),
            # Comms Card ----
            card(
              height = "100%",
              card_header(
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  h5("Log Internal Communications"),
                  popover(
                    div(
                      icon("question-circle"),
                      style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                    ),
                    "Some useful information",
                    title = "Property Help",
                    placement = "right"
                  )
                )
              ),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  layout_columns(
                    height = "100%",
                  ),
                  dateInput(
                    ns("comm_date"),
                    "Date",
                    value = Sys.Date(),
                    width = "100%"
                  ),
                  textAreaInput(
                    ns("communication_description"),
                    "Communication Description",
                    value = "",
                    width = "100%",
                    height = "200px",
                    resize = "vertical"
                  ),
                  actionButton(
                    inputId = ns("log_internal"),
                    label = "Log Communication",
                    class = "btn-success"
                  ),
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            # Action Item Card ----
            card(
              height = "100%",
              card_header(
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  h5("Log Action Item"),
                  popover(
                    div(
                      icon("question-circle"),
                      style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                    ),
                    "Useful information",
                    title = "Parcel Help",
                    placement = "right"
                  )
                )
              ),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    ns("team_lead"),
                    "Team Lead",
                    choices = NULL,
                    multiple = FALSE,
                    width = "100%"
                  ),
                  dateInput(
                    ns("due_date"),
                    "Due Date",
                    value = NULL,
                    width = "100%"
                  ),
                  textAreaInput(
                    ns("action_item_description"),
                    "Action Item Description",
                    value = "",
                    width = "100%",
                    height = "150px",
                    resize = "vertical"
                  ),
                  actionButton(
                    inputId = ns("log_action"),
                    label = "Log Action Item",
                    class = "btn-success"
                  ),
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

# Server ----
module_internal_communications_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ## Helper functions ----
    get_property_choices <- function(db_con) {
      dbGetQuery(
        db_con,
        "SELECT property_name FROM properties ORDER BY property_name"
      ) |>
        pull(property_name)
    }

    get_team_lead_choices <- function(db_con) {
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value"
      )
    }

    get_property_id <- function(db_con, property_name) {
      dbGetQuery(
        db_con,
        glue_sql(
          "SELECT id FROM properties WHERE property_name = {property_name};",
          .con = db_con
        )
      ) |>
        pull(id)
    }

    ## Reactives :: Input choices ----
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_property_choices(db_con)
    })

    team_lead_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_team_lead_choices(db_con)
    })

    ## Observer :: Update inputs ----
    observe({
      # Only update when db_updated changes
      if (!is.null(db_updated)) {
        db_updated()
      }

      # Preserve current selections
      current_selections <- list(
        property = isolate(input$property),
        team_lead = isolate(input$team_lead)
      )

      # Update inputs
      updateSelectizeInput(
        session,
        "property",
        choices = c("", property_choices()),
        selected = current_selections$property,
        server = TRUE
      )

      team_leads <- team_lead_choices()
      team_lead_list <- setNames(team_leads$id, team_leads$team_value)

      updateSelectizeInput(
        session,
        "team_lead",
        choices = c("", team_lead_list),
        selected = current_selections$team_lead,
        server = TRUE
      )
    })

    ## Event :: Log internal communication ----
    observeEvent(input$log_internal, {
      req(input$property, input$comm_date, input$communication_description)

      property_id <- get_property_id(db_con, input$property)

      df <- tibble(
        property_id = property_id,
        date = as.character(input$comm_date),
        communication_description = input$communication_description
      )

      append_db_data(
        db_table_name = "internal_communications",
        data = df,
        con = db_con,
        silent = TRUE
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Internal communication logged successfully for {input$property}"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Log action item ----
    observeEvent(input$log_action, {
      req(input$property, input$team_lead, input$action_item_description)

      property_id <- get_property_id(db_con, input$property)

      df <- tibble(
        property_id = property_id,
        team_lead_id = as.integer(input$team_lead),
        action_item_description = input$action_item_description,
        due_date = if_else(
          is.null(input$due_date),
          NA_character_,
          as.character(input$due_date)
        )
      )

      append_db_data(
        db_table_name = "team_lead_actions",
        data = df,
        con = db_con,
        silent = TRUE
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Action item logged successfully for {input$property}"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = property_choices(),
        selected = character(0),
        server = TRUE
      )

      team_leads <- team_lead_choices()
      team_lead_list <- setNames(team_leads$id, team_leads$team_value)

      updateSelectizeInput(
        session,
        inputId = "team_lead",
        choices = c("", team_lead_list),
        selected = character(0),
        server = TRUE
      )

      updateTextAreaInput(session, "communication_description", value = "")
      updateTextAreaInput(session, "action_item_description", value = "")
      updateDateInput(session, "comm_date", value = Sys.Date())
      updateDateInput(session, "due_date", value = NA)
    })
  })
}
