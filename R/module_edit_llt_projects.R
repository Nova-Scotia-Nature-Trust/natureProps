# UI ----
module_edit_llt_projects_ui <- function(id) {
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
        ## Main panel ----
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          card(
            height = "100%",
            card_header(
              h5("Edit LLT Project")
            ),
            card_body(
              div(
                style = "display: flex; flex-direction: column; gap: 15px;",
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
module_edit_llt_projects_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Reactive :: Property choices ----
    #' Only show properties that have an existing llt_projects record
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        db_con,
        "SELECT p.id, p.property_name
         FROM properties p
         INNER JOIN llt_projects llt ON llt.property_id = p.id
         ORDER BY p.property_name;"
      )
    })

    ## Update property dropdown ----
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

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(tibble(
      id = NA_integer_,
      property_id = NA_integer_,
      property_name = "",
      legacy_property_name = NA_character_,
      date_funding_received = as.Date(NA),
      funding_value = NA_integer_,
      endowment_notes = NA_character_,
      stewardship_plan_complete = FALSE,
      stewardship_plan_notes = NA_character_
    ))

    ## Event :: Load record ----
    observeEvent(input$property_name, {
      req(input$property_name)

      property_id <- as.integer(input$property_name)

      query <- glue_sql(
        "SELECT
          llt.id,
          llt.property_id,
          p.property_name,
          llt.legacy_property_name,
          llt.date_funding_received,
          llt.funding_value,
          llt.endowment_notes,
          llt.stewardship_plan_complete,
          llt.stewardship_plan_notes
        FROM llt_projects llt
        INNER JOIN properties p ON llt.property_id = p.id
        WHERE llt.property_id = {property_id}",
        .con = db_con
      )

      record <- dbGetQuery(db_con, query)

      if (nrow(record) >= 1) {
        # Take the first record if multiple exist for the same property
        selected_record(record[1, ])
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      property_name_text <- if (isTruthy(record$property_name)) {
        paste0("Editing: ", record$property_name)
      } else {
        "No property selected"
      }

      tagList(
        h6(
          class = "text-muted",
          property_name_text
        ),

        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_legacy_property_name"),
            label = "LLT Property Name",
            value = if (!is.na(record$legacy_property_name)) {
              record$legacy_property_name
            } else {
              ""
            }
          ),
          dateInput(
            inputId = ns("edit_date_funding_received"),
            label = "Date Funding Received",
            value = if (!is.na(record$date_funding_received)) {
              record$date_funding_received
            } else {
              NA
            }
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          numericInput(
            inputId = ns("edit_funding_value"),
            label = "Endowment Funding Amount ($)",
            value = if (!is.na(record$funding_value)) {
              record$funding_value
            } else {
              NA
            },
            min = 0
          ),
          checkboxInput(
            inputId = ns("edit_stewardship_plan_complete"),
            label = "Stewardship Plan Complete",
            value = if (!is.na(record$stewardship_plan_complete)) {
              record$stewardship_plan_complete
            } else {
              FALSE
            }
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          textAreaInput(
            inputId = ns("edit_endowment_notes"),
            label = "Endowment Notes",
            value = if (!is.na(record$endowment_notes)) {
              record$endowment_notes
            } else {
              ""
            },
            rows = 4
          ),
          textAreaInput(
            inputId = ns("edit_stewardship_plan_notes"),
            label = "Stewardship Plan Notes",
            value = if (!is.na(record$stewardship_plan_notes)) {
              record$stewardship_plan_notes
            } else {
              ""
            },
            rows = 4
          )
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$property_name)

      record <- selected_record()
      req(!is.na(record$id))

      llt_update <- tibble(
        id = record$id,
        legacy_property_name = if (isTruthy(input$edit_legacy_property_name)) {
          input$edit_legacy_property_name
        } else {
          NA_character_
        },
        date_funding_received = if (
          isTruthy(input$edit_date_funding_received)
        ) {
          as.Date(input$edit_date_funding_received)
        } else {
          as.Date(NA)
        },
        funding_value = if (isTruthy(input$edit_funding_value)) {
          as.integer(input$edit_funding_value)
        } else {
          NA_integer_
        },
        endowment_notes = if (isTruthy(input$edit_endowment_notes)) {
          input$edit_endowment_notes
        } else {
          NA_character_
        },
        stewardship_plan_complete = as.logical(
          input$edit_stewardship_plan_complete
        ),
        stewardship_plan_notes = if (
          isTruthy(input$edit_stewardship_plan_notes)
        ) {
          input$edit_stewardship_plan_notes
        } else {
          NA_character_
        }
      )

      dbx::dbxUpdate(
        db_con,
        table = "llt_projects",
        records = llt_update,
        where_cols = "id"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "LLT project details for {record$property_name} have been successfully updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      # Reset to empty template
      selected_record(tibble(
        id = NA_integer_,
        property_id = NA_integer_,
        property_name = "",
        legacy_property_name = NA_character_,
        date_funding_received = as.Date(NA),
        funding_value = NA_integer_,
        endowment_notes = NA_character_,
        stewardship_plan_complete = FALSE,
        stewardship_plan_notes = NA_character_
      ))

      # Clear the sidebar filter
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
