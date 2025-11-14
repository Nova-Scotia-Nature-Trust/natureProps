# UI ----
module_edit_records_ui <- function(id) {
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
            inputId = ns("db_table"),
            label = "Database Table",
            selected = NULL,
            choices = c("", "Parcels", "Properties"),
            options = list(
              create = FALSE,
              placeholder = "Select table"
            )
          ),
          selectizeInput(
            inputId = ns("record_id"),
            label = "Database Record ID",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select ID"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Record",
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
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(12, -1),
            ## Card :: Edit ----
            card(
              height = "100%",
              card_header(div(
                style = "display: flex; align-items: center; gap: 8px;",
                h5("Edit Records"),
                popover(
                  div(
                    icon("question-circle"),
                    style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                  ),
                  includeMarkdown("help/edit_records_help.md"),
                  title = "Help on editing fields",
                  placement = "right"
                )
              )),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  uiOutput(ns("edit_fields_ui")),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ) #,
            ## Card :: Other content ----
            # card(
            #   height = "100%",
            #   card_header(h5("Other content")),
            #   div(
            #     style = "display: flex; flex-direction: column; gap: 15px;",
            #     "Content here.",
            #     # Add a spacer div to prevent pushing everything to bottom
            #     div(style = "flex-grow: 1;")
            #   )
            # )
          )
        )
      )
    )
  )
}

# Server ----
module_edit_records_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Assign fields from each database table ----
    table_fields <- list(
      Parcels = c(
        "property_id", # This should not be an editable field (just using for reference here)
        "acquisition_type_id",
        "date_added",
        "date_updated",
        "priority_securement_ranking_id",
        "priority_ecological_ranking_id",
        "size_confirmed_ha"
      ),
      Properties = c(
        "property_name",
        "property_name_public",
        "focus_area_internal_id",
        "property_description",
        "phase_id",
        "anticipated_closing_year",
        "phase_id_description",
        "phase_id_followup",
        "team_lead_id",
        "securement_probability_id"
      )
    )

    ## Set custom field data types ----
    ## Unassigned fields will be treated as text inputs
    date_fields <- c("date_added", "date_updated", "phase_id_followup")
    text_area_fields <- c("property_description", "phase_id_description")
    numeric_fields <- c(
      "size_acres_confirmed",
      "price_asking",
      "price_appraised"
    )
    lookup_fields <- c(
      "phase_id",
      "acquisition_type_id",
      "priority_securement_ranking_id",
      "priority_ecological_ranking_id",
      "focus_area_internal_id",
      "team_lead_id",
      "securement_probability_id"
    )

    lookup_fields_details <- list(
      phase_id = list(
        table_name = "phase",
        value_name = "phase_value"
      ),
      acquisition_type_id = list(
        table_name = "acquisition_type",
        value_name = "acquisition_value"
      ),
      priority_securement_ranking_id = list(
        table_name = "ranking",
        value_name = "ranking_value"
      ),
      priority_ecological_ranking_id = list(
        table_name = "ranking",
        value_name = "ranking_value"
      ),
      focus_area_internal_id = list(
        table_name = "focus_area_internal",
        value_name = "internal_value"
      ),
      team_lead_id = list(
        table_name = "team_lead",
        value_name = "team_value"
      ),
      securement_probability_id = list(
        table_name = "securement_probability",
        value_name = "probability_value"
      )
    )

    ## Reactive :: Record ID choices ----
    id_choices <- reactive({
      req(input$db_table)
      tbl <- input$db_table

      if (tbl == "Parcels") {
        dbGetQuery(db_con, glue("SELECT pid FROM {tbl};")) |>
          pull(pid) |>
          sort()
      } else if (tbl == "Properties") {
        dbGetQuery(db_con, glue("SELECT property_name FROM {tbl};")) |>
          pull(property_name) |>
          sort()
      }
    })

    ## Update select input with record IDs based on table
    observe({
      updateSelectizeInput(
        session,
        inputId = "record_id",
        choices = c("", id_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Clear selected record and input UI elements when table changes
    observeEvent(input$db_table, {
      selected_record(NULL)
    })

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$db_table)
      req(input$record_id)

      tbl <- input$db_table

      if (tbl == "Parcels") {
        pid <- input$record_id
        fields <- table_fields$Parcels
        query <- glue_sql(
          "SELECT {`fields`*} FROM parcels WHERE pid = {pid}",
          .con = db_con
        )
        record <- dbGetQuery(db_con, query)
      } else if (tbl == "Properties") {
        prop_name <- input$record_id
        fields <- table_fields$Properties
        query <- glue_sql(
          "SELECT {`fields`*} FROM properties WHERE property_name = {prop_name}",
          .con = db_con
        )
        record <- dbGetQuery(db_con, query)
      }

      if (nrow(record) == 1) {
        selected_record(record)
      } else {
        selected_record(NULL)
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      req(input$db_table)
      req(!is.null(selected_record()))

      fields <- table_fields[[input$db_table]]
      print(fields)
      record <- selected_record()
      print(record)
      glimpse(record)

      # Generate list of UI elements
      ui_elements <- purrr::map(
        fields,
        ~ {
          field_name <- .x
          field_label <- str_replace_all(field_name, "_", " ") |> str_to_title()

          if (field_name %in% date_fields) {
            dateInput(
              inputId = ns(paste0("edit_", field_name)),
              label = field_label,
              value = if (!is.na(record[[field_name]])) {
                record[[field_name]]
              } else {
                ""
              }
            )
          } else if (field_name %in% text_area_fields) {
            textAreaInput(
              inputId = ns(paste0("edit_", field_name)),
              label = field_label,
              value = if (!is.na(record[[field_name]])) {
                record[[field_name]]
              } else {
                ""
              },
              height = "200px",
              width = "100%"
            )
          } else if (field_name %in% numeric_fields) {
            numericInput(
              inputId = ns(paste0("edit_", field_name)),
              label = field_label,
              value = if (!is.na(record[[field_name]])) {
                record[[field_name]]
              } else {
                NA_real_
              }
            )
          } else if (field_name %in% lookup_fields) {
            # Access the relevant lookup table
            lu_details <- lookup_fields_details[[field_name]]
            lu_table <- dbReadTable(db_con, lu_details$table_name)

            # Create named vector for choices
            choice_vals <- lu_table |>
              select(!!lu_details$value_name, id) |>
              deframe()

            # Determine selected value
            select_val <- if (!is.na(record[[field_name]])) {
              lu_table |>
                filter(id == record[[field_name]]) |>
                pull(id)
            } else {
              ""
            }

            selectInput(
              inputId = ns(paste0("edit_", field_name)),
              label = field_label,
              choices = c("", choice_vals),
              selected = select_val
            )
          } else {
            textInput(
              inputId = ns(paste0("edit_", field_name)),
              label = field_label,
              value = if (!is.null(record)) record[[field_name]] else ""
            )
          }
        }
      )

      # Group into pairs and pass each as arguments to layout_columns()
      rows <- purrr::map(
        seq(1, length(ui_elements), by = 3),
        function(i) {
          elems <- ui_elements[i:min(i + 2, length(ui_elements))]
          do.call(
            bslib::layout_columns,
            c(elems, list(col_widths = c(4, 4, 4)))
          )
        }
      )

      tagList(rows)
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$db_table, input$record_id)

      tbl <- input$db_table
      print("tbl object")
      print(tbl)
      db_id <- input$record_id
      print("database record id object")
      print(db_id)
      fields <- table_fields[[tbl]]
      print("table fields")
      print(fields)

      # Get values, handling different input types appropriately
      values <- map(fields, function(field) {
        input_id <- paste0("edit_", field)
        if (field %in% date_fields) {
          # Adjust format as needed for database
          if (length(input[[input_id]]) > 0) {
            format(input[[input_id]], "%Y-%m-%d")
          } else {
            as.Date(NA)
          }
        } else {
          input[[input_id]]
        }
      })

      print("Values object")
      print(values)

      # Creates a tibble with named columns, one per field
      update_tibble <- tibble(!!!set_names(values, fields))

      # Convert lgl NAs from numeric columns into numeric NAs
      update_tibble <- update_tibble |>
        mutate(across(
          any_of(numeric_fields),
          ~ as.numeric(replace(., is.na(.), NA_real_))
        ))

      update_tibble <- update_tibble |>
        mutate(across(
          any_of(lookup_fields),
          ~ as.integer(replace(., . == "", NA_integer_))
        ))

      update_tibble <- update_tibble |>
        mutate(across(where(is.character), ~ na_if(., "")))

      glimpse(update_tibble)

      # Overwrite the record in the DB
      if (tbl == "Parcels") {
        dbx::dbxUpdate(
          db_con,
          table = "parcels",
          records = update_tibble |> mutate(pid = db_id),
          where_cols = "pid"
        )
      } else if (tbl == "Properties") {
        dbx::dbxUpdate(
          db_con,
          table = "properties",
          records = update_tibble |> mutate(property_name = db_id),
          where_cols = "property_name"
        )
      }

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Table record {db_id} has been successfully updated in {tbl} table"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )

      # showNotification("Record updated, lekker!", type = "message")
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      selected_record(NULL)
      fields <- table_fields[[input$db_table]]

      purrr::walk(
        fields,
        ~ {
          field_id <- paste0("edit_", .x)
          if (.x %in% date_fields) {
            updateDateInput(session, field_id, value = "")
          } else if (.x %in% text_area_fields) {
            updateTextAreaInput(session, field_id, value = "")
          } else if (.x %in% numeric_fields) {
            updateNumericInput(session, field_id, value = NA)
          } else {
            updateTextInput(session, field_id, value = "")
          }
        }
      )

      updateSelectizeInput(
        session,
        inputId = "record_id",
        choices = c("", id_choices()),
        selected = "",
        server = TRUE
      )

      # updateSelectInput(
      #   session,
      #   inputId = "db_table",
      #   selected = ""
      # )
    })
  })
}
