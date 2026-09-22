# UI ----
module_add_property_record_ui <- function(id) {
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
              label = "Date Added"
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
            autonumericInput(
              inputId = ns("price_asking"),
              label = "Property Asking Price",
              value = NULL,
              currencySymbol = "$",
              align = "left"
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
              class = "btn-success"
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
    # Accordion :: Update / Move Property ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_body(
        class = "p-0",
        accordion(
          id = ns("pid_accordion"),
          open = FALSE,
          multiple = FALSE,
          ## Panel :: Add PID to Existing Property ----
          accordion_panel(
            title = "Add New PID(s) to Existing Property",
            div(
              style = "display: flex; flex-direction: column; gap: 15px;",
              selectizeInput(
                inputId = ns("add_pid_pid"),
                label = "Enter PID(s)",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  create = TRUE,
                  placeholder = "Type PID and press Enter"
                )
              ),
              selectizeInput(
                inputId = ns("add_pid_property"),
                label = "Select Property",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  placeholder = "Select existing property"
                )
              ),
              selectizeInput(
                inputId = ns("add_pid_acquisition_type"),
                label = "Acquisition Type",
                choices = NULL
              ),
              div(
                style = "margin-top: 10px;",
                actionButton(
                  inputId = ns("add_pid_submit"),
                  label = "Add PID to Property",
                  class = "btn-success"
                ),
                actionButton(
                  inputId = ns("add_pid_clear"),
                  label = "Clear Inputs",
                  class = "btn-secondary"
                )
              )
            )
          ),
          ## Panel :: Move PID to New Property ----
          accordion_panel(
            title = "Move Existing PID(s) to New Property",
            div(
              style = "display: flex; flex-direction: column; gap: 15px;",
              selectizeInput(
                inputId = ns("move_to_new_pid"),
                label = "Select PID(s)",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select PID(s) to move"
                )
              ),
              textInput(
                inputId = ns("move_to_new_property_name"),
                label = "New Property Name",
                value = ""
              ),
              selectizeInput(
                inputId = ns("move_to_new_phase"),
                label = "Phase",
                choices = NULL
              ),
              textAreaInput(
                inputId = ns("move_to_new_phase_description"),
                label = "Phase Description",
                value = "",
                height = "100px",
                width = "100%"
              ),
              textAreaInput(
                inputId = ns("move_to_new_property_description"),
                label = "Property Description",
                value = "",
                height = "100px",
                width = "100%"
              ),
              div(
                style = "margin-top: 10px;",
                actionButton(
                  inputId = ns("move_to_new_submit"),
                  label = "Move PID(s) to New Property",
                  class = "btn-success"
                ),
                actionButton(
                  inputId = ns("move_to_new_clear"),
                  label = "Clear Inputs",
                  class = "btn-secondary"
                )
              )
            )
          ),
          ## Panel :: Move PID to Existing Property ----
          accordion_panel(
            title = "Move Existing PID(s) to Existing Property",
            div(
              style = "display: flex; flex-direction: column; gap: 15px;",
              selectizeInput(
                inputId = ns("move_to_existing_pid"),
                label = "Select PID(s)",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Select PID(s) to move"
                )
              ),
              selectizeInput(
                inputId = ns("move_to_existing_property"),
                label = "Select Destination Property",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  placeholder = "Select destination property"
                )
              ),
              div(
                style = "margin-top: 10px;",
                actionButton(
                  inputId = ns("move_to_existing_submit"),
                  label = "Move PID(s) to Property",
                  class = "btn-success"
                ),
                actionButton(
                  inputId = ns("move_to_existing_clear"),
                  label = "Clear Inputs",
                  class = "btn-secondary"
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
module_add_property_record_server <- function(id, db_con, prd_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    updateDateInput(session, "date_added", value = Sys.Date())

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
    iv_add_pid <- InputValidator$new()
    iv_add_pid$add_rule("add_pid_property", sv_required())
    iv_add_pid$add_rule(
      "add_pid_pid",
      ~ validate_pid_input(., valid_pids, enable_check = TRUE)
    )
    iv_add_pid$enable()

    ### Move PID Form ----
    iv_move_to_new <- InputValidator$new()
    iv_move_to_new$add_rule("move_to_new_pid", sv_required())
    iv_move_to_new$add_rule("move_to_new_property_name", sv_required())
    iv_move_to_new$add_rule("move_to_new_phase", sv_required())
    iv_move_to_new$add_rule("move_to_new_phase_description", sv_required())
    iv_move_to_new$add_rule("move_to_new_property_description", sv_required())
    iv_move_to_new$enable()

    ### Move PID to Existing Property Form ----
    iv_move_to_existing <- InputValidator$new()
    iv_move_to_existing$add_rule("move_to_existing_pid", sv_required())
    iv_move_to_existing$add_rule("move_to_existing_property", sv_required())
    iv_move_to_existing$enable()

    ## Database Lookup Values ----
    phase <- dbReadTable(db_con, "phase") |>
      arrange(phase_value)
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
      "SELECT * FROM team_lead WHERE team_value NOT IN ('Rich LaPaix', 'Keith Spafford') ORDER BY team_value"
    )

    ## Reactive :: Property Lists ----
    property_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, 
                CONCAT_WS(' || ', property_name, property_name_public) AS property_name 
        FROM properties 
        ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "add_pid_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "move_to_existing_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )
    })

    ## Reactive :: PID list ----
    pids <- reactive({
      db_updated()

      dbGetQuery(
        db_con,
        glue_sql(
          "SELECT pid FROM parcels ORDER BY pid;",
          .con = db_con
        )
      ) |>
        pull(pid)
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

    observe({
      updateSelectizeInput(
        session,
        inputId = "move_to_new_pid",
        choices = c("", pids()),
        selected = character(0),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "move_to_existing_pid",
        choices = c("", pids()),
        selected = character(0),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )
    })

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
      "move_to_new_phase",
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
      "add_pid_acquisition_type",
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
          title = "Error",
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
      property_check <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT EXISTS (SELECT 1 FROM properties WHERE property_name = {input$property_name})",
          .con = db_con
        )
      )[[1]]

      if (!property_check) {
        new_property <- tibble(
          property_name = input$property_name,
          date_added = input$date_added,
          focus_area_internal_id,
          property_description = if (isTruthy(input$property_description)) {
            as.character(input$property_description)
          } else {
            NA_character_
          },
          stewardship_concerns = if (isTruthy(input$stewardship_concerns)) {
            as.character(input$stewardship_concerns)
          } else {
            NA_character_
          },
          phase_id = input$phase,
          phase_id_change = input$date_added,
          source_id = input$source,
          team_lead_id = input$team_lead,
          project_region_id = if (isTruthy(input$region)) {
            as.integer(input$region)
          } else {
            NA_integer_
          },
          price_asking = if (
            isTruthy(input$price_asking) && input$price_asking > 0
          ) {
            as.numeric(input$price_asking)
          } else {
            NA_real_
          }
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
          text = "Property name already exists. Please use a unique name.",
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

    ## Event :: Add new PID to existing property ----
    observeEvent(input$add_pid_submit, {
      req(input$add_pid_pid)
      iv_add_pid$is_valid()

      # Check if any PIDs already exist in the database
      existing_pids <- dbReadTable(db_con, "parcels") |>
        filter(pid %in% input$add_pid_pid) |>
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
        pid = input$add_pid_pid,
        property_id = as.integer(input$add_pid_property),
        acquisition_type_id = if_else(
          isTruthy(input$add_pid_acquisition_type),
          as.integer(input$add_pid_acquisition_type),
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
      populate_nsprd_tables(input$add_pid_pid, prd_con, db_con)

      message("PID(S) ADDED TO EXISTING PROPERTY")

      updateSelectizeInput(
        session,
        "add_pid_pid",
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
        "add_pid_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "add_pid_acquisition_type",
        choices = setNames(
          acquisition$id,
          acquisition$acquisition_value
        ),
        selected = character(0)
      )
    })

    ## Event :: Move Exisiting PID to New Property ----
    observeEvent(input$move_to_new_submit, {
      iv_move_to_new$is_valid()

      ## Check property name
      property_check <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT EXISTS (SELECT 1 FROM properties WHERE property_name = {input$move_to_new_property_name})",
          .con = db_con
        )
      )[[1]]

      if (property_check) {
        shinyalert(
          title = "Database Error",
          text = "Property name already exists",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return()
      }

      ## Get the existing property
      pid_info <- DBI::dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT
            pid,
            property_id
          FROM parcels 
          WHERE pid IN ({input$move_to_new_pid*});
          ",
          .con = db_con
        )
      ) |>
        as_tibble()

      # All selected PIDs must belong to exactly one property
      old_property_id <- unique(pid_info$property_id)

      if (length(old_property_id) != 1 || is.na(old_property_id)) {
        shinyalert(
          title = "Database Error",
          text = "The selected PIDs do not all belong to the same existing property.",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )

        return()
      }

      ## Get attributes from the existing property
      old_prop_details <- dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT
            date_added,
            source_id,
            team_lead_id,
            focus_area_internal_id,
            project_region_id
          FROM properties
          WHERE id = {old_property_id};
          ",
          .con = db_con
        )
      ) |>
        as_tibble()

      ## Create the attributes for the new property
      new_property_info <- old_prop_details |>
        mutate(
          property_name = input$move_to_new_property_name,
          phase_id = input$move_to_new_phase,
          phase_id_description = input$move_to_new_phase_description,
          property_description = input$move_to_new_property_description,
          date_updated = Sys.Date(),
          phase_id_change = Sys.Date()
        )

      ## Create new property and move PIDs
      pool::poolWithTransaction(db_con, function(conn) {
        move_pid_query <- glue_sql(
          "
          WITH new_property AS (

            INSERT INTO properties (
              property_name,
              date_added,
              focus_area_internal_id,
              property_description,
              phase_id,
              phase_id_description,
              source_id,
              team_lead_id,
              project_region_id,
              date_updated,
              phase_id_change
            )

            VALUES (
              {new_property_info$property_name},
              {new_property_info$date_added},
              {new_property_info$focus_area_internal_id},
              {new_property_info$property_description},
              {new_property_info$phase_id},
              {new_property_info$phase_id_description},
              {new_property_info$source_id},
              {new_property_info$team_lead_id},
              {new_property_info$project_region_id},
              {new_property_info$date_updated},
              {new_property_info$phase_id_change}
            )

            RETURNING id
          )

          UPDATE parcels
          SET property_id = new_property.id
          FROM new_property
          WHERE parcels.pid IN ({input$move_to_new_pid*});
          ",
          .con = conn
        )

        DBI::dbExecute(conn, move_pid_query)
      })

      ## Message
      db_updated(db_updated() + 1)

      # Return message
      shinyalert(
        title = "Success",
        text = glue::glue(
          "PID(s) moved to new property"
        ),
        type = "success",
        timer = 5000
      )

      ## Reset inputs
      updateSelectizeInput(
        session,
        inputId = "move_to_new_pid",
        choices = c("", pids()),
        selected = character(0),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )

      updateTextInput(session, "move_to_new_property_name", value = "")

      updateSelectizeInput(
        session,
        "move_to_new_phase",
        choices = setNames(phase$id, phase$phase_value),
        selected = character(0)
      )

      updateTextInput(session, "move_to_new_phase_description", value = "")

      updateTextAreaInput(session, "move_to_new_property_description", value = "")
    })

    ## Event :: Move Exisiting PID to Existing Property ----
    observeEvent(input$move_to_existing_submit, {
      iv_move_to_existing$is_valid()

      target_property_id <- as.integer(input$move_to_existing_property)

      ## Get the current property for the selected PID(s)
      pid_info <- DBI::dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT
            pid,
            property_id
          FROM parcels
          WHERE pid IN ({input$move_to_existing_pid*});
          ",
          .con = db_con
        )
      ) |>
        as_tibble()

      if (any(pid_info$property_id == target_property_id, na.rm = TRUE)) {
        shinyalert(
          title = "Database Error",
          text = "One or more selected PIDs already belong to the destination property.",
          type = "error",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )

        return()
      }

      ## Move PIDs to the destination property
      pool::poolWithTransaction(db_con, function(conn) {
        move_pid_query <- glue_sql(
          "
          UPDATE parcels
          SET property_id = {target_property_id}
          WHERE pid IN ({input$move_to_existing_pid*});
          ",
          .con = conn
        )

        DBI::dbExecute(conn, move_pid_query)
      })

      ## Message
      db_updated(db_updated() + 1)

      # Return message
      shinyalert(
        title = "Success",
        text = glue::glue(
          "PID(s) moved to property"
        ),
        type = "success",
        timer = 5000
      )

      ## Reset inputs
      updateSelectizeInput(
        session,
        inputId = "move_to_existing_pid",
        choices = c("", pids()),
        selected = character(0),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "move_to_existing_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )
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

      updateAutonumericInput(session, "price_asking", value = "")
    })

    ## Event :: Clear Update Property Inputs ----
    observeEvent(input$add_pid_clear, {
      updateSelectizeInput(
        session,
        "add_pid_pid",
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
        "add_pid_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "add_pid_acquisition_type",
        choices = setNames(
          acquisition$id,
          acquisition$acquisition_value
        ),
        selected = character(0)
      )
    })

    ## Event :: Clear Move PID to New Property Inputs ----
    observeEvent(input$move_to_new_clear, {
      updateSelectizeInput(
        session,
        inputId = "move_to_new_pid",
        choices = c("", pids()),
        selected = character(0),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )

      updateTextInput(session, "move_to_new_property_name", value = "")

      updateSelectizeInput(
        session,
        "move_to_new_phase",
        choices = setNames(phase$id, phase$phase_value),
        selected = character(0)
      )

      updateTextInput(session, "move_to_new_phase_description", value = "")

      updateTextAreaInput(session, "move_to_new_property_description", value = "")
    })

    ## Event :: Clear Move PID to Existing Property Inputs ----
    observeEvent(input$move_to_existing_clear, {
      updateSelectizeInput(
        session,
        inputId = "move_to_existing_pid",
        choices = c("", pids()),
        selected = character(0),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "move_to_existing_property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )
    })
  })
}
