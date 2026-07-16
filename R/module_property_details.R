# UI ----
module_property_details_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(8, 4),
    ## Card :: Add New Property ----
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
              inputId = ns("phase"),
              label = "Phase",
              choices = NULL
            ),
            selectizeInput(
              inputId = ns("acquisition_type"),
              label = "Acquisition Type",
              choices = NULL
            ),
            numericInput(
              inputId = ns("price_asking"),
              label = "Property Asking Price",
              value = NA,
              step = 1000
            )
          ),
          div(
            style = "width: 100%;",
            div(
              style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
              "Property & Opportunity Description",
              popover(
                icon("question-circle"),
                includeMarkdown("popups/prop_opp_overview.md"),
                title = "Context",
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
            div(
              style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
              "Stewardship Concerns",
              popover(
                icon("question-circle"),
                includeMarkdown("popups/stewardship_concerns.md"),
                title = "Context",
                placement = "top"
              )
            ),
            textAreaInput(
              ns("stewardship_concerns"),
              label = NULL,
              value = "",
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
    # Card :: Update Property ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(h5("Add PID to Existing Property")),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
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

    ### New Property Form ----
    iv <- InputValidator$new()
    iv$add_rule("date_added", sv_required())
    iv$add_rule("property_name", sv_required())
    iv$add_rule("phase", sv_required())
    iv$add_rule("source", sv_required())
    iv$add_rule("team_lead", sv_required())
    iv$add_rule(
      "pid",
      ~ validate_pid_input(., valid_pids, enable_check = TRUE)
    )
    iv$enable()

    ### Update Property Form ----
    iv_update <- InputValidator$new()
    iv_update$add_rule("update_property", sv_required())
    iv_update$add_rule(
      "update_pid",
      ~ validate_pid_input(., valid_pids, enable_check = TRUE)
    )
    iv_update$enable()

    ## Database Lookup Values ----
    phase <- dbReadTable(db_con, "phase")
    acquisition <- dbReadTable(db_con, "acquisition_type")
    focus_area <- dbGetQuery(
      db_con,
      "SELECT * FROM focus_area_internal ORDER BY internal_value"
    )
    theme <- dbGetQuery(
      db_con,
      "SELECT * FROM project_theme ORDER BY theme_value"
    )
    region <- dbGetQuery(
      db_con,
      "SELECT * FROM project_region ORDER BY region_value"
    )
    source <- dbGetQuery(db_con, "SELECT * FROM source ORDER BY source_value")
    team_lead <- dbGetQuery(
      db_con,
      "SELECT * FROM team_lead ORDER BY team_value"
    )

    ## Reactive :: Property Lists ----
    property_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, property_name FROM properties 
         ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "update_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = isolate(input$update_property),
        server = TRUE
      )
    })

    ## Populate UI Select Inputs ----
    updateSelectizeInput(
      session,
      "acquisition_type",
      choices = setNames(
        acquisition$id,
        acquisition$acquisition_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "focus_area_internal",
      choices = setNames(
        focus_area$id,
        focus_area$internal_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "phase",
      choices = setNames(
        phase$id,
        phase$phase_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "region",
      choices = setNames(
        region$id,
        region$region_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "source",
      choices = setNames(
        source$id,
        source$source_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "team_lead",
      choices = setNames(
        team_lead$id,
        team_lead$team_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "theme",
      choices = setNames(
        theme$id,
        theme$theme_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "update_acquisition_type",
      choices = setNames(
        acquisition$id,
        acquisition$acquisition_value
      ),
      selected = character(0),
      server = TRUE
    )

    ## Event :: Submit property ----
    observeEvent(input$submit_property, {
      req(input$pid)
      req(iv$is_valid())

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

      ### Property name & ID -----
      property_check <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name) |>
        pull(property_name)

      if (length(property_check) == 0) {
        new_property <- tibble(
          property_name = input$property_name,
          date_added = input$date_added,
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
          phase_id = input$phase,
          phase_id_change = input$date_added,
          source_id = input$source,
          team_lead_id = input$team_lead,
          project_region_id = if_else(
            isTruthy(input$region),
            as.integer(input$region),
            NA_integer_
          ),
          price_asking = if_else(
            isTruthy(input$price_asking) && input$price_asking > 0,
            as.numeric(input$price_asking),
            NA_real_
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

      ### Write new parcel(s) ----
      property_id <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name) |>
        pull(id)

      ### Write property themes ----
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
      iv_update$is_valid()

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

    ## Event :: Clear New Property Inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "acquisition_type",
        choices = setNames(
          acquisition$id,
          acquisition$acquisition_value
        ),
        selected = character(0)
      )

      updateDateInput(
        session,
        inputId = "date_added",
        value = Sys.Date()
      )

      updateSelectizeInput(
        session,
        inputId = "focus_area_internal",
        choices = setNames(
          focus_area$id,
          focus_area$internal_value
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "phase",
        choices = setNames(
          phase$id,
          phase$phase_value
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "pid",
        label = "Enter PID(s)",
        choices = NULL,
        options = list(
          create = TRUE,
          placeholder = "Type PID and press Enter"
        )
      )

      updateTextInput(
        session,
        inputId = "property_description",
        value = ""
      )

      updateTextInput(
        session,
        inputId = "property_name",
        value = ""
      )

      updateSelectizeInput(
        session,
        inputId = "region",
        choices = setNames(
          region$id,
          region$region_value
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "source",
        choices = setNames(
          source$id,
          source$source_value
        ),
        selected = character(0)
      )

      updateTextInput(
        session,
        inputId = "stewardship_concerns",
        value = ""
      )

      updateSelectizeInput(
        session,
        inputId = "team_lead",
        choices = setNames(
          team_lead$id,
          team_lead$team_value
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "theme",
        choices = setNames(
          theme$id,
          theme$theme_value
        ),
        selected = character(0)
      )
    })

    ## Event :: Clear Update Property Inputs ----
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

      updateSelectizeInput(
        session,
        "update_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        "update_acquisition_type",
        choices = setNames(
          acquisition$id,
          acquisition$acquisition_value
        ),
        selected = character(0)
      )
    })
  })
}
