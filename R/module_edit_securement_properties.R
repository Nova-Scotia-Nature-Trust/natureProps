# UI ----
module_edit_securement_properties_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          "",
          open = TRUE,
          selectizeInput(
            inputId = ns("property_name"),
            label = "Select Property",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select property"
            )
          ),
          hr(),
          actionButton(
            inputId = ns("submit_edit"),
            label = "Submit Changes",
            class = "btn-success"
          ),
          actionButton(
            inputId = ns("clear_edit"),
            label = "Clear",
            class = "btn-secondary"
          )
        ),
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          card(
            height = "100%",
            card_header(
              h5("Edit Securement Details for Properties")
            ),
            card_body(
              div(
                style = "display: flex; flex-direction: column; gap: 15px; overflow-y: auto;",
                uiOutput(ns("edit_fields_ui")),
                div(style = "flex-grow: 1;")
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
module_edit_securement_properties_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -----------------------------
    # Reactive choices for dropdowns
    # -----------------------------
    property_choices <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties ORDER BY property_name;"
      )
    })

    phase_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, phase_value FROM phase ORDER BY phase_value;"
      )
    })

    focus_area_internal_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, internal_value FROM focus_area_internal ORDER BY internal_value;"
      )
    })

    team_lead_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value;"
      )
    })

    project_region_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, region_value FROM project_region ORDER BY region_value;"
      )
    })

    project_theme_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, theme_value FROM project_theme ORDER BY theme_value;"
      )
    })

    source_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, source_value FROM source ORDER BY source_value;"
      )
    })

    # -----------------------------
    # Update property dropdown
    # -----------------------------
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c(
          "",
          setNames(
            property_choices()$id,
            property_choices()$property_name
          )
        ),
        selected = isolate(input$property_name),
        server = TRUE
      )
    })

    # -----------------------------
    # Reactive value: selected record
    # -----------------------------
    selected_record <- reactiveVal(NULL)

    # -----------------------------
    # Load record when property selection changes
    # -----------------------------
    observeEvent(input$property_name, {
      property_id <- input$property_name

      if (!isTruthy(property_id)) {
        selected_record(NULL)
        return()
      }

      query <- glue_sql(
        "SELECT 
          id,
          property_name,
          property_name_public,
          focus_area_internal_id,
          property_description,
          phase_id,
          phase_id_description,
          phase_id_change,
          phase_id_followup,
          team_lead_id,
          project_region_id,
          source_id,
          stewardship_concerns
        FROM properties 
        WHERE id = {property_id}",
        .con = db_con
      )

      record <- dbGetQuery(db_con, query)

      if (nrow(record) == 1) {
        selected_record(record)
      } else {
        selected_record(NULL)
      }
    })

    # -----------------------------
    # Render UI fields
    # -----------------------------
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      # ---- Normalize record ----
      if (is.null(record)) {
        record <- tibble(
          id = NA_integer_,
          property_name = NULL,
          property_name_public = NULL,
          focus_area_internal_id = NULL,
          property_description = NULL,
          phase_id = NULL,
          phase_id_description = NULL,
          phase_id_change = as.Date(NA),
          phase_id_followup = as.Date(NA),
          team_lead_id = NULL,
          project_region_id = NULL,
          source_id = NULL,
          stewardship_concerns = NULL
        )
        selected_themes <- NULL
        header_text <- "No property selected"
      } else {
        selected_themes <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT project_theme_id
             FROM property_theme
             WHERE property_id = {record$id}",
            .con = db_con
          )
        )$project_theme_id

        header_text <- paste0("Editing: ", record$property_name)
      }

      # ---- UI ----
      tagList(
        h6(class = "text-muted", header_text),
        hr(),

        layout_columns(
          col_widths = c(6, 6),

          textInput(
            ns("edit_property_name"),
            "Property Name",
            value = record$property_name
          ),

          textInput(
            ns("edit_property_name_public"),
            "Property Name (Public)",
            value = record$property_name_public
          )
        ),

        layout_columns(
          col_widths = c(6, 6),

          selectizeInput(
            ns("edit_focus_area_internal_id"),
            "Focus Area Internal",
            choices = c(
              "",
              setNames(
                focus_area_internal_choices()$id,
                focus_area_internal_choices()$internal_value
              )
            ),
            selected = record$focus_area_internal_id,
            multiple = FALSE
          ),

          selectizeInput(
            ns("edit_team_lead_id"),
            "Team Lead",
            choices = c(
              "",
              setNames(
                team_lead_choices()$id,
                team_lead_choices()$team_value
              )
            ),
            selected = record$team_lead_id,
            multiple = FALSE
          ),

          selectizeInput(
            ns("edit_phase_id"),
            "Phase",
            choices = c(
              "",
              setNames(
                phase_choices()$id,
                phase_choices()$phase_value
              )
            ),
            selected = record$phase_id,
            multiple = FALSE
          ),

          textAreaInput(
            ns("edit_phase_id_description"),
            "Phase Description",
            value = record$phase_id_description,
            rows = 3
          ),

          dateInput(
            ns("edit_phase_id_change"),
            "Phase Change Date",
            value = record$phase_id_change
          ),

          dateInput(
            ns("edit_phase_id_followup"),
            "Phase Follow-up Date",
            value = record$phase_id_followup
          ),

          textAreaInput(
            ns("edit_property_description"),
            "Property & Opportunity Description",
            value = record$property_description,
            rows = 3
          ),

          selectizeInput(
            ns("edit_project_region_id"),
            "Project Region",
            choices = c(
              "",
              setNames(
                project_region_choices()$id,
                project_region_choices()$region_value
              )
            ),
            selected = record$project_region_id,
            multiple = FALSE
          ),

          selectizeInput(
            ns("edit_project_theme_id"),
            "Project Theme(s)",
            choices = setNames(
              project_theme_choices()$id,
              project_theme_choices()$theme_value
            ),
            selected = selected_themes,
            multiple = TRUE
          ),

          selectizeInput(
            ns("edit_source_id"),
            "Source",
            choices = c(
              "",
              setNames(
                source_choices()$id,
                source_choices()$source_value
              )
            ),
            selected = record$source_id,
            multiple = FALSE
          ),

          textAreaInput(
            ns("edit_stewardship_concerns"),
            "Stewardship Concerns",
            value = record$stewardship_concerns,
            rows = 3
          )
        )
      )
    })

    # -----------------------------
    # Submit edits
    # -----------------------------
    observeEvent(input$submit_edit, {
      # req(input$property_name)
      req(selected_record())
      db_id <- as.integer(input$property_name)
      original <- selected_record()

      # ---- Phase validation logic (same as the original) ----
      old_phase <- if (isTruthy(original$phase_id)) {
        as.integer(original$phase_id)
      } else {
        NA_integer_
      }
      new_phase <- if (isTruthy(input$edit_phase_id)) {
        as.integer(input$edit_phase_id)
      } else {
        NA_integer_
      }
      phase_changed <- !identical(old_phase, new_phase)

      old_desc <- if (isTruthy(original$phase_id_description)) {
        trimws(original$phase_id_description)
      } else {
        NA_character_
      }
      new_desc <- if (isTruthy(input$edit_phase_id_description)) {
        trimws(input$edit_phase_id_description)
      } else {
        NA_character_
      }
      desc_changed <- !identical(old_desc, new_desc)

      old_change_dt <- if (!isTruthy(original$phase_id_change)) {
        as.Date(NA)
      } else {
        as.Date(original$phase_id_change)
      }
      new_change_dt <- if (!isTruthy(input$edit_phase_id_change)) {
        as.Date(NA)
      } else {
        as.Date(input$edit_phase_id_change)
      }
      date_changed <- !identical(old_change_dt, new_change_dt)

      if (phase_changed) {
        if (!desc_changed || is.na(new_desc) || new_desc == "") {
          shinyalert(
            "Phase Description Required",
            "You changed the phase, but the Phase Description was not updated.",
            type = "warning"
          )
          return()
        }
        if (!date_changed || is.na(new_change_dt)) {
          shinyalert(
            "Phase Change Date Required",
            "You changed the phase, but the Phase Change Date was not updated.",
            type = "warning"
          )
          return()
        }
      }

      # ---- Build update tibble for properties ----
      update_tibble <- tibble(
        id = db_id,

        property_name = if (isTruthy(input$edit_property_name)) {
          input$edit_property_name
        } else {
          NA_character_
        },

        property_name_public = if (isTruthy(input$edit_property_name_public)) {
          input$edit_property_name_public
        } else {
          NA_character_
        },

        focus_area_internal_id = if (
          isTruthy(input$edit_focus_area_internal_id)
        ) {
          as.integer(input$edit_focus_area_internal_id)
        } else {
          NA_integer_
        },

        property_description = if (isTruthy(input$edit_property_description)) {
          input$edit_property_description
        } else {
          NA_character_
        },

        phase_id = if (isTruthy(input$edit_phase_id)) {
          as.integer(input$edit_phase_id)
        } else {
          NA_integer_
        },

        phase_id_description = if (isTruthy(input$edit_phase_id_description)) {
          input$edit_phase_id_description
        } else {
          NA_character_
        },

        phase_id_change = if (isTruthy(input$edit_phase_id_change)) {
          as.Date(input$edit_phase_id_change)
        } else {
          as.Date(NA)
        },

        phase_id_followup = if (isTruthy(input$edit_phase_id_followup)) {
          as.Date(input$edit_phase_id_followup)
        } else {
          as.Date(NA)
        },

        team_lead_id = if (isTruthy(input$edit_team_lead_id)) {
          as.integer(input$edit_team_lead_id)
        } else {
          NA_integer_
        },

        project_region_id = if (isTruthy(input$edit_project_region_id)) {
          as.integer(input$edit_project_region_id)
        } else {
          NA_integer_
        },

        source_id = if (isTruthy(input$edit_source_id)) {
          as.integer(input$edit_source_id)
        } else {
          NA_integer_
        },

        stewardship_concerns = if (isTruthy(input$edit_stewardship_concerns)) {
          input$edit_stewardship_concerns
        } else {
          NA_character_
        }
      )

      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = update_tibble,
        where_cols = "id"
      )

      update_property_timestamp(con = db_con, property_id = db_id)

      # ---- Update property_theme junction table ----
      new_theme_ids <- as.integer(input$edit_project_theme_id)
      current_theme_ids <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT project_theme_id
           FROM property_theme
           WHERE property_id = {db_id}",
          .con = db_con
        )
      )$project_theme_id

      to_add <- setdiff(new_theme_ids, current_theme_ids)
      to_remove <- setdiff(current_theme_ids, new_theme_ids)

      if (length(to_remove) > 0) {
        dbExecute(
          db_con,
          glue_sql(
            "DELETE FROM property_theme
             WHERE property_id = {db_id} 
             AND project_theme_id IN ({to_remove*})",
            .con = db_con
          )
        )
      }

      if (length(to_add) > 0) {
        insert_df <- tibble(
          property_id = db_id,
          project_theme_id = to_add
        )
        dbx::dbxInsert(db_con, table = "property_theme", records = insert_df)
      }

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Database fields for {original$property_name} have been successfully updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )

      # selected_record(NULL)
    })

    # -----------------------------
    # Clear inputs
    # -----------------------------
    observeEvent(input$clear_edit, {
      selected_record(NULL)
      updateSelectizeInput(
        session,
        inputId = "property_name",
        selected = character(0),
        choices = c(
          "",
          setNames(
            property_choices()$id,
            property_choices()$property_name
          )
        ),
        server = TRUE
      )
    })
  })
}
