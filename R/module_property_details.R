# UI ----
module_property_details_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(8, 4),
    # Existing card: Add New Property ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(h5("Add New Property")),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          layout_columns(
            col_widths = c(6, 6),
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
            selectizeInput(
              inputId = ns("theme"),
              label = "Project Theme",
              choices = NULL,
              multiple = TRUE
            ),
            selectizeInput(
              inputId = ns("region"),
              label = "Project Region",
              choices = NULL
            ),
            selectizeInput(
              inputId = ns("source"),
              label = "Source",
              choices = NULL
            ),
            selectizeInput(
              inputId = ns("team_lead"),
              label = "Team Lead",
              choices = NULL
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectizeInput(
              inputId = ns("phase_id"),
              label = "Phase",
              choices = NULL
            ),
            selectizeInput(
              inputId = ns("acquisition_type"),
              label = "Acquisition Type",
              choices = NULL
            )
          ),
          div(
            style = "width: 100%;",
            div(
              style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
              tags$label(
                "Property & Opportunity Description",
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
            textAreaInput(
              ns("stewardship_concerns"),
              label = "Stewardship Concerns",
              value = "",
              # placeholder = "Enter any information around potential stewardship issues",
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
    ),
    # New card: Update Property ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(h5("Add PID to Existing Property")),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          layout_columns(
            col_widths = c(7, 5),
            selectizeInput(
              inputId = ns("update_pid"),
              label = "Enter PID(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = TRUE,
                placeholder = "Type PID and press Enter"
              )
            ),
            dateInput(
              inputId = ns("update_date_added"),
              label = "Date Added",
              value = today()
            )
          ),
          selectizeInput(
            inputId = ns("update_property"),
            label = "Select Property",
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Select existing property"
            )
          ),
          selectizeInput(
            inputId = ns("update_acquisition_type"),
            label = "Acquisition Type",
            choices = NULL
          ),
          div(
            style = "margin-top: 20px;",
            actionButton(
              inputId = ns("submit_update"),
              label = "Add PID to Property",
              class = "btn-primary"
            ),
            actionButton(
              inputId = ns("clear_update_inputs"),
              label = "Clear Inputs",
              class = "btn-secondary"
            )
          ),
          div(style = "flex-grow: 1;")
        )
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

    # Validation for new property form
    iv <- InputValidator$new()
    iv$add_rule("date_added", sv_required())
    iv$add_rule("property_name", sv_required())
    iv$add_rule("phase_id", sv_required())
    iv$add_rule("source", sv_required())
    iv$add_rule("team_lead", sv_required())
    # iv$add_rule("focus_area_internal", sv_required())
    iv$add_rule(
      "pid",
      ~ validate_pid_input(., valid_pids, enable_check = TRUE)
    )
    iv$enable()

    # Validation for update property form
    iv_update <- InputValidator$new()
    iv_update$add_rule("update_date_added", sv_required())
    iv_update$add_rule("update_property", sv_required())
    iv_update$add_rule(
      "update_pid",
      ~ validate_pid_input(., valid_pids, enable_check = TRUE)
    )
    iv_update$enable()

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

    property_choices <- reactive({
      dbReadTable(db_con, "properties") |>
        arrange(property_name) |>
        select(property_name, id) |>
        deframe()
    })

    ## Populate UI inputs ----
    observe({
      # New property form
      updateSelectizeInput(
        session,
        "focus_area_internal",
        choices = focus_area_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "phase_id",
        choices = phase_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "acquisition_type",
        choices = acquisition_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "theme",
        choices = theme_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "region",
        choices = region_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "source",
        choices = source_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "team_lead",
        choices = team_choices(),
        selected = character(0),
        server = TRUE
      )

      # Update property form
      updateSelectizeInput(
        session,
        "update_property",
        choices = property_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "update_acquisition_type",
        choices = acquisition_choices(),
        selected = character(0),
        server = TRUE
      )
    })

    ## Event :: Submit property ----
    observeEvent(input$submit_property, {
      req(input$pid)
      req(input$date_added)
      # req(input$focus_area_internal)
      req(input$property_name)
      req(input$phase_id)
      req(input$source)
      req(input$team_lead)

      # Check validation before proceeding
      if (!iv$is_valid()) {
        return()
      }

      # Check if any PIDs already exist in the database
      existing_pids <- dbReadTable(db_con, "parcels") |>
        filter(pid %in% input$pid) |>
        pull(pid)

      if (length(existing_pids) > 0) {
        shinyalert(
          title = "Database Error",
          text = glue(
            "The following PID(s) already exist in the database: {paste(existing_pids, collapse = ', ')}"
          ),
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return() # Stop execution here
      }

      ### Focus area (internal) ----

      if (isTruthy(input$focus_area_internal)) {
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

          focus_area_internal_id <- dbReadTable(
            db_con,
            "focus_area_internal"
          ) |>
            filter(internal_value == input$focus_area_internal) |>
            pull(id)

          message("FOCUS AREA ADDED TO DATABASE")
        } else {
          focus_area_internal_id <- input$focus_area_internal
          message("FOCUS AREA ALREADY IN DATABASE")
        }
      } else {
        focus_area_internal_id <- NA_integer_
        message("FOCUS AREA NOT ASSIGNED")
      }

      ## Property name & ID -----
      property_check <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name) |>
        pull(property_name)

      if (length(property_check) == 0) {
        new_property <- tibble(
          property_name = input$property_name,
          focus_area_internal_id,
          property_description = if_else(
            isTruthy(input$property_description),
            as.character(input$property_description),
            NA_character_
          ),
          stewardship_concerns = if_else(
            isTruthy(input$stewardship_concerns),
            as.character(input$stewardship_concerns),
            NA_character_
          ),
          phase_id = input$phase_id,
          source_id = input$source,
          team_lead_id = input$team_lead,
          project_region_id = if_else(
            isTruthy(input$region),
            as.integer(input$region),
            NA_integer_
          )
        )

        # Try to append property - stop if it fails
        property_success <- append_db_data(
          "properties",
          new_property,
          db_con,
          silent = TRUE
        )

        if (!property_success) {
          return() # Stop here without crashing
        }
        message("NEW PROPERTY ADDED TO DATABASE")
      } else {
        message("PROPERTY ALREADY IN DATABASE")
        shinyalert(
          title = "Database Error",
          text = "Property name already exists",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return()
      }

      ## Write new parcel(s) ----
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

      new_parcel <- tibble(
        pid = input$pid,
        date_added = input$date_added,
        property_id,
        acquisition_type_id = if_else(
          isTruthy(input$acquisition_type),
          as.integer(input$acquisition_type),
          NA_integer_
        )
      )
      # Try to append parcel - stop if it fails
      parcel_success <- append_db_data(
        "parcels",
        new_parcel,
        db_con,
        silent = FALSE
      )

      if (!parcel_success) {
        return() # Stop here without crashing
      }

      # Only proceed if successful
      db_updated(db_updated() + 1)
      # Extract data from NSPRD database
      populate_nsprd_tables(input$pid, prd_con, db_con)
    })

    ## Event :: Submit update (add PID to existing property) ----
    observeEvent(input$submit_update, {
      req(input$update_pid)
      req(input$update_date_added)
      req(input$update_property)

      # Check validation before proceeding
      if (!iv_update$is_valid()) {
        return()
      }

      # Check if any PIDs already exist in the database
      existing_pids <- dbReadTable(db_con, "parcels") |>
        filter(pid %in% input$update_pid) |>
        pull(pid)

      if (length(existing_pids) > 0) {
        shinyalert(
          title = "Database Error",
          text = glue(
            "The following PID(s) already exist in the database: {paste(existing_pids, collapse = ', ')}"
          ),
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return()
      }

      ## Write new parcel(s) to existing property ----
      new_parcel <- tibble(
        pid = input$update_pid,
        date_added = input$update_date_added,
        property_id = as.integer(input$update_property),
        acquisition_type_id = if_else(
          isTruthy(input$update_acquisition_type),
          as.integer(input$update_acquisition_type),
          NA_integer_
        )
      )

      # Try to append parcel - stop if it fails
      parcel_success <- append_db_data(
        "parcels",
        new_parcel,
        db_con,
        silent = FALSE
      )

      if (!parcel_success) {
        return()
      }

      # Only proceed if successful
      db_updated(db_updated() + 1)
      # Extract data from NSPRD database
      populate_nsprd_tables(input$update_pid, prd_con, db_con)

      message("PID(S) ADDED TO EXISTING PROPERTY")
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        "pid",
        label = "Enter PID(s)",
        choices = NULL,
        options = list(
          create = TRUE,
          placeholder = "Type PID and press Enter"
        ),
        server = TRUE
      )
      updateDateInput(session, "date_added", value = Sys.Date())
      updateTextInput(session, "property_name", value = "")
      updateSelectizeInput(
        session,
        "source",
        choices = source_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "theme",
        choices = theme_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "region",
        choices = region_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "team_lead",
        choices = team_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "phase_id",
        choices = phase_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "focus_area_internal",
        choices = focus_area_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "acquisition_type",
        choices = acquisition_choices(),
        selected = character(0),
        server = TRUE
      )
      updateTextInput(session, "property_description", value = "")
    })

    ## Event :: Clear update inputs ----
    observeEvent(input$clear_update_inputs, {
      updateSelectizeInput(
        session,
        "update_pid",
        label = "Enter PID(s)",
        choices = NULL,
        options = list(
          create = TRUE,
          placeholder = "Type PID and press Enter"
        ),
        server = TRUE
      )
      updateDateInput(session, "update_date_added", value = Sys.Date())
      updateSelectizeInput(
        session,
        "update_property",
        choices = property_choices(),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "update_acquisition_type",
        choices = acquisition_choices(),
        selected = character(0),
        server = TRUE
      )
    })
  })
}
