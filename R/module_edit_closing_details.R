# UI ----
module_edit_closing_details_ui <- function(id) {
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
              h5("Edit Closing Details")
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
module_edit_closing_details_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    iv <- InputValidator$new()
    iv$add_rule("edit_date_closed_fiscal", function(value) {
      if (is.null(value) || value == "") {
        return() # Allow empty values
      }

      # Check format: YYYY/YY
      if (!str_detect(value, "^[0-9]{4}/[0-9]{2}$")) {
        return("Must be in format YYYY/YY (e.g., 2025/26)")
      }

      # Extract year components
      start_year <- as.integer(str_sub(value, 1, 4))
      end_year <- as.integer(str_sub(value, 6, 7))

      # Check if end year is consecutive (start_year + 1) % 100
      if ((start_year + 1) %% 100 != end_year) {
        return("Years must be consecutive (e.g., 2025/26, not 2025/27)")
      }
    })

    iv$enable()

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties ORDER BY property_name;"
      ) |>
        select(property_name, id) |>
        deframe()
    })

    ## Reactive :: Acquisition securement type choices ----
    acquisition_securement_type_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, acquisition_value FROM acquisition_securement_type ORDER BY acquisition_value;"
      ) |>
        select(acquisition_value, id) |>
        deframe()
    })

    ## Reactive :: Ownership choices ----
    ownership_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, ownership_value FROM ownership ORDER BY ownership_value;"
      ) |>
        select(ownership_value, id) |>
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

    ## Reactive value :: Selected record (initialize with empty template) ----
    selected_record <- reactiveVal(tibble(
      id = NA_integer_,
      property_name = NA_character_,
      acquisition_securement_type_id = NA_integer_,
      ownership_id = NA_integer_,
      owner_name = NA_character_,
      donor_vendor = NA_character_,
      date_closed = NA_character_,
      date_closed_fiscal = NA_character_,
      ecogift_number = NA_character_,
      public_view = FALSE,
      notes_sensitivity = NA_character_,
      notes_other = NA_character_
    ))

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$property_name)

      property_id <- input$property_name

      query <- glue_sql(
        "SELECT 
          id,
          property_name,
          acquisition_securement_type_id,
          ownership_id,
          owner_name,
          donor_vendor,
          date_closed,
          date_closed_fiscal,
          ecogift_number,
          public_view,
          notes_sensitivity,
          notes_other
        FROM properties 
        WHERE id = {property_id}",
        .con = db_con
      )

      record <- dbGetQuery(db_con, query)

      if (nrow(record) == 1) {
        selected_record(record)
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      req(!is.null(selected_record()))

      record <- selected_record()

      # Extract values if record exists, otherwise NULL
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
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          selectizeInput(
            inputId = ns("edit_acquisition_securement_type_id"),
            label = "Acquisition Securement Type",
            choices = c("", acquisition_securement_type_choices()),
            selected = if (!is.na(record$acquisition_securement_type_id)) {
              record$acquisition_securement_type_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select type"
            )
          ),
          selectizeInput(
            inputId = ns("edit_ownership_id"),
            label = "Ownership",
            choices = c("", ownership_choices()),
            selected = if (!is.na(record$ownership_id)) {
              record$ownership_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select ownership"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_owner_name"),
            label = "Owner Name",
            value = if (!is.na(record$owner_name)) {
              record$owner_name
            } else {
              ""
            }
          ),
          textInput(
            inputId = ns("edit_donor_vendor"),
            label = "Donor/Vendor",
            value = if (!is.na(record$donor_vendor)) {
              record$donor_vendor
            } else {
              ""
            }
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          dateInput(
            inputId = ns("edit_date_closed"),
            label = "Date Closed",
            value = if (!is.na(record$date_closed)) {
              record$date_closed
            } else {
              NULL
            }
          ),
          textInput(
            inputId = ns("edit_date_closed_fiscal"),
            label = "Date Closed (Fiscal)",
            value = if (!is.na(record$date_closed_fiscal)) {
              record$date_closed_fiscal
            } else {
              ""
            },
            placeholder = "e.g., 2024/25"
          ),
          textInput(
            inputId = ns("edit_ecogift_number"),
            label = "Ecogift Number",
            value = if (!is.na(record$ecogift_number)) {
              record$ecogift_number
            } else {
              ""
            }
          )
        ),
        checkboxInput(
          inputId = ns("edit_public_view"),
          label = "Public View",
          value = if (!is.na(record$public_view)) {
            record$public_view
          } else {
            FALSE
          }
        ),
        textAreaInput(
          inputId = ns("edit_notes_sensitivity"),
          label = "Notes (Sensitivity)",
          value = if (!is.na(record$notes_sensitivity)) {
            record$notes_sensitivity
          } else {
            ""
          },
          rows = 3,
          resize = "vertical"
        ),
        textAreaInput(
          inputId = ns("edit_notes_other"),
          label = "Notes (Other)",
          value = if (!is.na(record$notes_other)) {
            record$notes_other
          } else {
            ""
          },
          rows = 3,
          resize = "vertical"
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$property_name)

      # Check validation before proceeding
      if (!iv$is_valid()) {
        return()
      }

      db_id <- as.integer(input$property_name)

      # Build update tibble
      update_tibble <- tibble(
        id = db_id,
        acquisition_securement_type_id = if (
          is.null(input$edit_acquisition_securement_type_id) ||
            input$edit_acquisition_securement_type_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_acquisition_securement_type_id)
        },
        ownership_id = if (
          is.null(input$edit_ownership_id) ||
            input$edit_ownership_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_ownership_id)
        },
        owner_name = if (
          is.null(input$edit_owner_name) ||
            input$edit_owner_name == ""
        ) {
          NA_character_
        } else {
          input$edit_owner_name
        },
        donor_vendor = if (
          is.null(input$edit_donor_vendor) ||
            input$edit_donor_vendor == ""
        ) {
          NA_character_
        } else {
          input$edit_donor_vendor
        },
        date_closed = if (is.null(input$edit_date_closed)) {
          NA_character_
        } else {
          as.character(input$edit_date_closed)
        },
        date_closed_fiscal = if (
          is.null(input$edit_date_closed_fiscal) ||
            input$edit_date_closed_fiscal == ""
        ) {
          NA_character_
        } else {
          input$edit_date_closed_fiscal
        },
        ecogift_number = if (
          is.null(input$edit_ecogift_number) ||
            input$edit_ecogift_number == ""
        ) {
          NA_character_
        } else {
          input$edit_ecogift_number
        },
        public_view = as.logical(input$edit_public_view),
        notes_sensitivity = if (
          is.null(input$edit_notes_sensitivity) ||
            input$edit_notes_sensitivity == ""
        ) {
          NA_character_
        } else {
          input$edit_notes_sensitivity
        },
        notes_other = if (
          is.null(input$edit_notes_other) ||
            input$edit_notes_other == ""
        ) {
          NA_character_
        } else {
          input$edit_notes_other
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
          "Closing details for {selected_record()$property_name} have been successfully updated"
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
        property_name = NA_character_,
        acquisition_securement_type_id = NA_integer_,
        ownership_id = NA_integer_,
        owner_name = NA_character_,
        donor_vendor = NA_character_,
        date_closed = NA_character_,
        date_closed_fiscal = NA_character_,
        ecogift_number = NA_character_,
        public_view = FALSE,
        notes_sensitivity = NA_character_,
        notes_other = NA_character_
      ))

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
