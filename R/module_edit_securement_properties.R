# UI ----
module_edit_securement_properties_ui <- function(id) {
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
            inputId = ns("property_name"),
            label = "Property Name",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select property"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Property",
            class = "btn-success"
          ),
          hr(),
          actionButton(
            inputId = ns("submit_edit"),
            label = "Submit Changes",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_edit"),
            label = "Clear",
            class = "btn-secondary"
          )
        ),
        ## Main panel ----
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

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties ORDER BY property_name;"
      ) |>
        select(property_name, id) |>
        deframe()
    })

    ## Reactive :: Phase choices ----
    phase_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, phase_value FROM phase ORDER BY phase_value;"
      ) |>
        select(phase_value, id) |>
        deframe()
    })

    ## Reactive :: Focus area internal choices ----
    focus_area_internal_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, internal_value FROM focus_area_internal ORDER BY internal_value;"
      ) |>
        select(internal_value, id) |>
        deframe()
    })

    ## Reactive :: Team lead choices ----
    team_lead_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value;"
      ) |>
        select(team_value, id) |>
        deframe()
    })

    ## Reactive :: Project region choices ----
    project_region_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, region_value FROM project_region ORDER BY region_value;"
      ) |>
        select(region_value, id) |>
        deframe()
    })

    ## Reactive :: Source choices ----
    source_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, source_value FROM source ORDER BY source_value;"
      ) |>
        select(source_value, id) |>
        deframe()
    })

    ## Update property dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$property_name)

      property_id <- input$property_name

      query <- glue_sql(
        "SELECT 
          id,
          property_name,
          property_name_public,
          focus_area_internal_id,
          property_description,
          phase_id,
          phase_id_description,
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

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      tagList(
        if (!is.null(record)) {
          h6(
            class = "text-muted",
            paste0("Editing: ", record$property_name)
          )
        } else {
          h6(
            class = "text-muted",
            "No property selected"
          )
        },
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_property_name"),
            label = "Property Name",
            value = if (!is.null(record) && !is.na(record$property_name)) {
              record$property_name
            } else {
              ""
            }
          ),
          textInput(
            inputId = ns("edit_property_name_public"),
            label = "Property Name (Public)",
            value = if (
              !is.null(record) && !is.na(record$property_name_public)
            ) {
              record$property_name_public
            } else {
              ""
            }
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectizeInput(
            inputId = ns("edit_focus_area_internal_id"),
            label = "Focus Area Internal",
            choices = c("", focus_area_internal_choices()),
            selected = if (
              !is.null(record) && !is.na(record$focus_area_internal_id)
            ) {
              record$focus_area_internal_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select focus area"
            )
          ),
          selectizeInput(
            inputId = ns("edit_team_lead_id"),
            label = "Team Lead",
            choices = c("", team_lead_choices()),
            selected = if (!is.null(record) && !is.na(record$team_lead_id)) {
              record$team_lead_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select team lead"
            )
          ),
          selectizeInput(
            inputId = ns("edit_phase_id"),
            label = "Phase",
            choices = c("", phase_choices()),
            selected = if (!is.null(record) && !is.na(record$phase_id)) {
              record$phase_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select phase"
            )
          ),
          textAreaInput(
            inputId = ns("edit_phase_id_description"),
            label = "Phase Description",
            value = if (
              !is.null(record) && !is.na(record$phase_id_description)
            ) {
              record$phase_id_description
            } else {
              ""
            },
            rows = 3,
            resize = "vertical"
          ),
          dateInput(
            inputId = ns("edit_phase_id_followup"),
            label = "Phase Follow-up Date",
            value = if (!is.null(record) && !is.na(record$phase_id_followup)) {
              record$phase_id_followup
            } else {
              NULL
            }
          ),
          textAreaInput(
            inputId = ns("edit_property_description"),
            label = "Property & Opportunity Description",
            value = if (
              !is.null(record) && !is.na(record$property_description)
            ) {
              record$property_description
            } else {
              ""
            },
            rows = 3,
            resize = "vertical"
          ),
          selectizeInput(
            inputId = ns("edit_project_region_id"),
            label = "Project Region",
            choices = c("", project_region_choices()),
            selected = if (
              !is.null(record) && !is.na(record$project_region_id)
            ) {
              record$project_region_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select project region"
            )
          ),
          selectizeInput(
            inputId = ns("edit_source_id"),
            label = "Source",
            choices = c("", source_choices()),
            selected = if (!is.null(record) && !is.na(record$source_id)) {
              record$source_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select source"
            )
          ),
          textAreaInput(
            inputId = ns("edit_stewardship_concerns"),
            label = "Stewardship Concerns",
            value = if (
              !is.null(record) && !is.na(record$stewardship_concerns)
            ) {
              record$stewardship_concerns
            } else {
              ""
            },
            rows = 3,
            resize = "vertical"
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4)
        )
      )
    })
    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$property_name)

      db_id <- as.integer(input$property_name)

      # Build update tibble
      update_tibble <- tibble(
        id = db_id,
        property_name = if (
          is.null(input$edit_property_name) ||
            input$edit_property_name == ""
        ) {
          NA_character_
        } else {
          input$edit_property_name
        },
        property_name_public = if (
          is.null(input$edit_property_name_public) ||
            input$edit_property_name_public == ""
        ) {
          NA_character_
        } else {
          input$edit_property_name_public
        },
        focus_area_internal_id = if (
          is.null(input$edit_focus_area_internal_id) ||
            input$edit_focus_area_internal_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_focus_area_internal_id)
        },
        property_description = if (
          is.null(input$edit_property_description) ||
            input$edit_property_description == ""
        ) {
          NA_character_
        } else {
          input$edit_property_description
        },
        phase_id = if (
          is.null(input$edit_phase_id) ||
            input$edit_phase_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_phase_id)
        },
        phase_id_description = if (
          is.null(input$edit_phase_id_description) ||
            input$edit_phase_id_description == ""
        ) {
          NA_character_
        } else {
          input$edit_phase_id_description
        },
        phase_id_followup = if (is.null(input$edit_phase_id_followup)) {
          NA_character_
        } else {
          as.character(input$edit_phase_id_followup)
        },
        team_lead_id = if (
          is.null(input$edit_team_lead_id) ||
            input$edit_team_lead_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_team_lead_id)
        },
        project_region_id = if (
          is.null(input$edit_project_region_id) ||
            input$edit_project_region_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_project_region_id)
        },
        source_id = if (
          is.null(input$edit_source_id) ||
            input$edit_source_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_source_id)
        },
        stewardship_concerns = if (
          is.null(input$edit_stewardship_concerns) ||
            input$edit_stewardship_concerns == ""
        ) {
          NA_character_
        } else {
          input$edit_stewardship_concerns
        }
      )

      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = update_tibble,
        where_cols = "id"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Securement properties for {selected_record()$property_name} have been successfully updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      selected_record(NULL)

      # Clear the sidebar filter
      updateSelectizeInput(
        session,
        inputId = "property_name",
        selected = "",
        choices = c("", property_choices()),
        server = TRUE
      )
    })
  })
}
