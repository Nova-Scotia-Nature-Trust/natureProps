# UI ----
module_action_item_tracking_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = FALSE,
      height = "100%", # Make card fill available space
      layout_sidebar(
        sidebar = sidebar(
          "",
          open = TRUE,
          actionButton(
            inputId = ns("submit_actions"),
            label = "Submit Actions"
          ),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
          # actionButton(
          #   inputId = ns("refresh_db"),
          #   label = "Refresh Data Viewer"
          # )
        ),
        # Main layout
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(3, 9),
            ## Card :: Assign action items ----
            card(
              height = "100%",
              card_header(h5("Assign Action Items")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    ns("property"),
                    "Select property",
                    choices = NULL,
                    multiple = FALSE,
                    width = "80%"
                  ),
                  selectizeInput(
                    ns("pids"),
                    "Select PIDs",
                    choices = NULL,
                    multiple = TRUE,
                    width = "80%"
                  ),
                  selectizeInput(
                    ns("action_item_fields"),
                    "Select action item fields",
                    choices = NULL,
                    multiple = TRUE,
                    width = "80%"
                  ),
                  selectizeInput(
                    ns("action_item_value"),
                    "Select action value",
                    choices = NULL
                  ),
                  div(
                    style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
                    tags$label(
                      "Securement action description",
                      `for` = ns("securement_notes")
                    ),
                    popover(
                      div(
                        icon("question-circle"),
                        style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
                      ),
                      includeMarkdown("help/securement_desc.md"),
                      title = "Securement Description Help",
                      placement = "top"
                    )
                  ),
                  textAreaInput(
                    ns("securement_notes"),
                    label = NULL,
                    "",
                    height = "150px",
                    width = "100%"
                  ),
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            ## Card :: Data viewer for action items ----
            module_data_viewer_ui(
              ns("action_item_viewer"),
              panel_id = "panel_02"
            )
          )
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
    iv$add_rule("pids", sv_required())
    iv$add_rule("action_item_fields", sv_required())
    iv$add_rule("action_item_value", sv_required())
    iv$enable()

    ## Property and PID reactives ----
    props_reactive <- reactive({
      ## Get property list
      dbGetQuery(db_con, "SELECT property_name FROM properties;") |>
        pull() |>
        sort()
    })

    ## Reactive for PIDS based on input$property
    pids_reactive <- reactive({
      req(input$property)

      prop_ref <- dbGetQuery(db_con, "SELECT pid, property_id FROM parcels;") |>
        as_tibble() |>
        left_join(
          dbGetQuery(
            db_con,
            "SELECT id, property_name FROM properties;"
          ) |>
            as_tibble(),
          join_by(property_id == id)
        )

      pid_list <- prop_ref |>
        filter(property_name == input$property) |>
        pull(pid)

      return(pid_list)
    })

    ## Reactive for securement notes based on input$property
    notes_reactive <- reactive({
      req(input$property)

      dbGetQuery(
        db_con,
        "SELECT securement_action_description, property_id FROM parcels;"
      ) |>
        as_tibble() |>
        left_join(
          dbGetQuery(
            db_con,
            "SELECT id, property_name FROM properties;"
          ) |>
            as_tibble(),
          join_by(property_id == id)
        ) |>
        filter(property_name == input$property) |>
        pull(securement_action_description) |>
        unique() |>
        paste(collapse = " ")
    })

    ## Action item fields and values ----
    df_view_meta <- read_xlsx(
      "inputs/field and function mapping tables/df_views.xlsx"
    )

    action_item_fields <- df_view_meta |>
      filter(action_item_fields) |>
      pull(db_name)

    action_item_names <- df_view_meta |>
      filter(action_item_fields) |>
      pull(df_name)

    action_item_fields <- setNames(action_item_fields, action_item_names)

    # Get action values from the database
    action_item_lu <- dbGetQuery(db_con, "SELECT * FROM action_item_status;")
    action_item_values <- setNames(
      action_item_lu$id,
      action_item_lu$action_value
    )

    ## Initialize inputs ----
    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = props_reactive(),
        selected = character(0),
        server = TRUE
      )
    })

    observe({
      pids <- pids_reactive()

      updateSelectizeInput(
        session,
        "pids",
        choices = pids,
        selected = pids,
        server = TRUE
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "action_item_fields",
        choices = action_item_fields,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "action_item_value",
        choices = action_item_values,
        selected = character(0),
        server = TRUE
      )

      observe({
        updateTextAreaInput(
          session,
          "securement_notes",
          value = notes_reactive()
        )
      })
    })

    ## Reactive :: build new action item table ----
    new_actions <- reactive({
      req(input$pids)
      req(input$action_item_fields)
      req(input$action_item_value)

      tibble(
        pid = rep(input$pids, each = length(input$action_item_fields)),
        action_field = rep(
          input$action_item_fields,
          times = length(input$pids)
        ),
        action_value = input$action_item_value,
        securement_action_description = if (isTruthy(input$securement_notes)) {
          input$securement_notes
        } else {
          as.character(0)
        }
      ) |>
        pivot_wider(names_from = action_field, values_from = action_value)
    })

    ## Event :: Update database action items ----
    observeEvent(input$submit_actions, {
      req(input$property)
      req(input$pids)
      req(input$action_item_fields)
      req(input$action_item_value)
      print(new_actions())

      ## Update the database ----
      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = new_actions(),
        where_cols = c("pid")
      )

      ## Signal that data has changed
      db_updated(db_updated() + 1)

      ## Alert message
      shinyalert(
        title = "Success",
        text = str_glue(
          "Action items for {input$property} have been successfully updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000 # Auto-close after 10 seconds
      )
    })

    ## Event :: Refresh DB ----
    # observeEvent(input$refresh_db, {
    #   db_updated(db_updated() + 1)
    # })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        "property",
        choices = props_reactive(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(session, "pids", selected = character(0))
      updateSelectizeInput(
        session,
        "action_item_fields",
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "action_item_value",
        selected = character(0)
      )
      updateTextAreaInput(session, "securement_notes", value = "")
    })

    ## Call data viewer module ----
    module_data_viewer_server(
      "action_item_viewer",
      db_con,
      db_updated,
      prop_filter = reactive(input$property)
    )
  })
}
