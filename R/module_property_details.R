# UI ----
module_property_details_ui <- function(id) {
  ns <- NS(id)

  card(
    full_screen = TRUE,
    height = "100%",
    card_header(h5("Property Details")),
    card_body(
      div(
        style = "display: flex; flex-direction: column; gap: 15px;",
        layout_columns(
          col_widths = c(7, 5),
          selectizeInput(
            inputId = ns("pid"),
            label = "Enter PID(s)",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              placeholder = "Type PID and press Enter"
            )
          ),
          dateInput(
            inputId = ns("date_added"),
            label = "Date Added",
            value = today()
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("property_name"),
            label = "Property Name",
            value = ""
          ),
          selectizeInput(
            inputId = ns("focus_area_internal"),
            label = "Focus Area (Internal)",
            choices = NULL,
            multiple = FALSE,
            options = list(
              create = TRUE,
              placeholder = "Select or add new focal area"
            )
          ),
          selectInput(
            inputId = ns("theme"),
            label = "Project Theme",
            choices = NULL,
            selected = character(0),
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("region"),
            label = "Project Region",
            choices = NULL,
            selected = character(0)
          ),
          selectInput(
            inputId = ns("source"),
            label = "Source",
            choices = NULL,
            selected = character(0)
          ),
          selectInput(
            inputId = ns("team_lead"),
            label = "Team Lead",
            choices = NULL,
            selected = character(0)
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectInput(
            inputId = ns("phase_id"),
            label = "Phase",
            choices = NULL,
            selected = character(0)
          ),
          selectInput(
            inputId = ns("acquisition_type"),
            label = "Acquisition Type",
            choices = NULL,
            selected = character(0)
          )
        ),
        div(
          style = "width: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
            tags$label(
              "Property & Opportunity Overview",
              `for` = ns("property_description")
            ),
            popover(
              div(
                icon("question-circle"),
                style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
              ),
              includeMarkdown("help/prop_opp_overview.md"),
              title = "Property Description Help",
              placement = "top"
            )
          ),
          textAreaInput(
            ns("property_description"),
            label = NULL,
            "",
            height = "100px",
            width = "100%"
          )
        ),
        div(
          style = "margin-top: 20px;",
          actionButton(
            inputId = ns("submit_property"),
            label = "Add Property",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          )
        ),
        div(style = "flex-grow: 1;")
      )
    )
  )
}

# Server ----
module_property_details_server <- function(id, db_con, prd_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    valid_pids <- dbGetQuery(prd_con, "SELECT DISTINCT(pid) FROM parcels;") |>
      pull(pid)

    iv <- InputValidator$new()
    iv$add_rule("date_added", sv_required())
    iv$add_rule("property_name", sv_required())
    iv$add_rule("phase_id", sv_required())
    iv$add_rule("source", sv_required())
    iv$add_rule("team_lead", sv_required())
    iv$add_rule("focus_area_internal", sv_required())
    iv$add_rule(
      "pid",
      ~ validate_pid_input(., valid_pids, enable_check = TRUE)
    )
    iv$enable()

    ## Populate UI inputs ----
    phase_choices <- reactive({
      dbReadTable(db_con, "phase") |>
        select(phase_value, id) |>
        deframe()
    })

    acquisition_choices <- reactive({
      dbReadTable(db_con, "acquisition_type") |>
        select(acquisition_value, id) |>
        deframe()
    })

    focus_area_choices <- reactive({
      dbReadTable(db_con, "focus_area_internal") |>
        arrange(internal_value) |>
        select(internal_value, id) |>
        deframe()
    })

    theme_choices <- reactive({
      dbReadTable(db_con, "project_theme") |>
        arrange(theme_value) |>
        select(theme_value, id) |>
        deframe()
    })

    region_choices <- reactive({
      dbReadTable(db_con, "project_region") |>
        arrange(region_value) |>
        select(region_value, id) |>
        deframe()
    })

    source_choices <- reactive({
      dbReadTable(db_con, "source") |>
        arrange(source_value) |>
        select(source_value, id) |>
        deframe()
    })

    team_choices <- reactive({
      dbReadTable(db_con, "team_lead") |>
        arrange(team_value) |>
        select(team_value, id) |>
        deframe()
    })

    observe({
      updateSelectizeInput(
        session,
        "focus_area_internal",
        choices = focus_area_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectInput(
        session,
        "phase_id",
        choices = phase_choices(),
        selected = character(0)
      )
      updateSelectInput(
        session,
        "acquisition_type",
        choices = acquisition_choices(),
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        inputId = "theme",
        choices = theme_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        inputId = "region",
        choices = region_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        inputId = "source",
        choices = source_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        inputId = "team_lead",
        choices = team_choices(),
        selected = character(0),
        server = TRUE
      )
    })

    ## Event :: Submit property ----
    observeEvent(input$submit_property, {
      req(input$pid)
      req(input$date_added)
      req(input$focus_area_internal)
      req(input$property_name)
      req(input$phase_id)
      req(input$source)
      req(input$team_lead)

      ### Focus area (internal) ----
      focus_area_check <- dbReadTable(db_con, "focus_area_internal") |>
        filter(id == input$focus_area_internal) |>
        pull(id)

      if (length(focus_area_check) == 0) {
        new_focus_area <- tibble(
          internal_value = input$focus_area_internal
        )

        append_db_data(
          "focus_area_internal",
          new_focus_area,
          db_con,
          silent = TRUE
        )

        focus_area_internal_id <- dbReadTable(db_con, "focus_area_internal") |>
          filter(internal_value == input$focus_area_internal) |>
          pull(id)

        message("FOCUS AREA ADDED TO DATABASE")
      } else {
        focus_area_internal_id <- input$focus_area_internal
        message("FOCUS AREA ALREADY IN DATABASE")
      }

      ## Property name & ID -----
      property_check <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name) |>
        pull(property_name)

      if (length(property_check) == 0) {
        new_property <- tibble(
          property_name = input$property_name,
          focus_area_internal_id,
          property_description = input$property_description,
          phase_id = input$phase_id,
          source_id = input$source,
          team_lead_id = input$team_lead,
          project_region_id = input$region
        )
        append_db_data("properties", new_property, db_con, silent = TRUE)
        message("NEW PROPERTY ADDED TO DATABASE")
      } else {
        message("PROPERTY ALREADY IN DATABASE")
      }

      property_id <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name) |>
        pull(id)

      ## Write property themes ----
      if (isTruthy(input$theme)) {
        new_property_themes <- tibble(
          property_id = property_id,
          project_theme_id = input$theme
        )

        append_db_data(
          "property_theme",
          new_property_themes,
          db_con,
          silent = TRUE
        )
        message("PROPERTY THEMES ADDED TO DATABASE")
      }

      ## Write new parcel(s) ----
      new_parcel <- tibble(
        pid = input$pid,
        date_added = input$date_added,
        property_id,
        acquisition_type_id = if (isTruthy(input$acquisition_type)) {
          input$acquisition_type
        } else {
          NA
        }
      )

      tryCatch(
        {
          append_db_data("parcels", new_parcel, db_con, silent = FALSE)
          db_updated(db_updated() + 1)
          populate_nsprd_tables(input$pid, prd_con, db_con)
        },
        error = function(e) {
          print(paste("Database error:", e$message))
        }
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(session, "pid", selected = character(0))
      updateDateInput(session, "date_added", value = Sys.Date())
      updateTextInput(session, "property_name", value = "")
      updateSelectInput(session, "source", selected = character(0))
      updateSelectInput(session, "theme", selected = character(0))
      updateSelectInput(session, "region", selected = character(0))
      updateSelectInput(session, "team_lead", selected = character(0))
      updateSelectInput(session, "phase_id", selected = character(0))
      updateTextInput(session, "focus_area_internal", value = "")
      updateSelectInput(session, "acquisition_type", selected = character(0))
      updateTextInput(session, "property_description", value = "")
    })
  })
}
