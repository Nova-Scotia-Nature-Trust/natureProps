# UI ----
module_edit_securement_parcels_ui <- function(id) {
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
          selectizeInput(
            inputId = ns("pid"),
            label = "Select PID",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "First select a property"
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
              h5("Edit Securement Details for Parcels")
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
module_edit_securement_parcels_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()

    iv$add_rule("edit_tax_exempt_year", function(value) {
      if (isTRUE(input$edit_tax_exempt) && !isTruthy(value)) {
        "Tax Exempt Year is required"
      }
    })

    iv$add_rule("edit_tax_exempt_year", function(value) {
      if (isTruthy(value) && (value < 1900 || value > 2200)) {
        "Tax Exempt Year must be between 1900 and 2200"
      }
    })

    iv$enable()

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT property_name FROM properties ORDER BY property_name;"
      ) |>
        pull(property_name) |>
        sort()
    })

    ## Reactive :: PIDs for selected property ----
    pids_reactive <- reactive({
      db_updated()
      req(input$property_name)

      query <- glue_sql(
        "SELECT p.pid 
        FROM parcels p
        JOIN properties prop ON p.property_id = prop.id
        WHERE prop.property_name = {input$property_name}
        ORDER BY p.pid;",
        .con = db_con
      )

      dbGetQuery(db_con, query) |>
        pull(pid) |>
        sort()
    })

    ## Reactive :: Acquisition type choices ----
    acquisition_type_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, acquisition_value FROM acquisition_securement_type ORDER BY acquisition_value;"
      )
    })

    ## Reactive :: Priority ranking choices ----
    priority_ranking_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, ranking_value FROM ranking;"
      )
    })

    ## Update property dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c("", property_choices()),
        selected = isolate(input$property_name),
        server = TRUE
      )
    })

    ## Update PID dropdown based on selected property ----
    observe({
      req(input$property_name)

      pids <- pids_reactive()

      updateSelectizeInput(
        session,
        inputId = "pid",
        choices = c("", pids),
        selected = isolate(input$pid),
        options = list(
          create = FALSE,
          placeholder = "Search or select PID"
        ),
        server = TRUE
      )
    }) |>
      bindEvent(input$property_name)

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record ----
    observeEvent(input$pid, {
      pid <- input$pid

      if (!isTruthy(pid)) {
        selected_record(NULL)
        return()
      }

      query <- glue_sql(
        "SELECT 
          pid,
          property_id,
          acquisition_type_id,
          priority_securement_ranking_id,
          priority_ecological_ranking_id,
          size_confirmed_ha,
          size_confirmed_acres,
          size_confirmed_notes,
          af_transaction,
          landowner_interest_ranking_id,
          tax_exempt,
          tax_exempt_year
        FROM parcels 
        WHERE pid = {pid}",
        .con = db_con
      )

      record <- dbGetQuery(db_con, query)

      if (nrow(record) == 1) {
        selected_record(record)
      } else {
        selected_record(NULL)
      }
    })

    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      tagList(
        h6(
          class = "text-muted",
          if (is.null(record)) {
            "No parcels selected"
          } else {
            paste0("Editing Parcel: ", record$pid)
          }
        ),
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          selectizeInput(
            inputId = ns("edit_acquisition_type_id"),
            label = "Acquisition Type",
            choices = c(
              "",
              setNames(
                acquisition_type_choices()$id,
                acquisition_type_choices()$acquisition_value
              )
            ),
            selected = if (
              !is.null(record) && !is.na(record$acquisition_type_id)
            ) {
              record$acquisition_type_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select acquisition type"
            )
          ),
          numericInput(
            inputId = ns("edit_size_confirmed_acres"),
            label = "Size Confirmed (acres)",
            value = if (
              !is.null(record) && !is.na(record$size_confirmed_acres)
            ) {
              record$size_confirmed_acres
            } else {
              NA_real_
            }
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          checkboxInput(
            inputId = ns("edit_af_transaction"),
            label = "AF Transaction",
            value = if (!is.null(record) && !is.na(record$af_transaction)) {
              record$af_transaction
            } else {
              FALSE
            }
          ),
          selectizeInput(
            inputId = ns("edit_landowner_interest_ranking_id"),
            label = "Landowner Interest Ranking",
            choices = c(
              "",
              setNames(
                priority_ranking_choices()$id,
                priority_ranking_choices()$ranking_value
              )
            ),
            selected = if (
              !is.null(record) && !is.na(record$landowner_interest_ranking_id)
            ) {
              record$landowner_interest_ranking_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select landowner interest ranking"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectizeInput(
            inputId = ns("edit_priority_securement_ranking_id"),
            label = "Priority Securement Ranking",
            choices = c(
              "",
              setNames(
                priority_ranking_choices()$id,
                priority_ranking_choices()$ranking_value
              )
            ),
            selected = if (
              !is.null(record) && !is.na(record$priority_securement_ranking_id)
            ) {
              record$priority_securement_ranking_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select securement ranking"
            )
          ),
          selectizeInput(
            inputId = ns("edit_priority_ecological_ranking_id"),
            label = "Priority Ecological Ranking",
            choices = c(
              "",
              setNames(
                priority_ranking_choices()$id,
                priority_ranking_choices()$ranking_value
              )
            ),
            selected = if (
              !is.null(record) && !is.na(record$priority_ecological_ranking_id)
            ) {
              record$priority_ecological_ranking_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select ecological ranking"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          checkboxInput(
            inputId = ns("edit_tax_exempt"),
            label = "Tax Exempt",
            value = if (!is.null(record) && !is.na(record$tax_exempt)) {
              record$tax_exempt
            } else {
              FALSE
            }
          ),
          numericInput(
            inputId = ns("edit_tax_exempt_year"),
            label = "Tax Exempt Year",
            value = if (!is.null(record) && !is.na(record$tax_exempt_year)) {
              record$tax_exempt_year
            } else {
              NA_real_
            },
            min = 1900,
            max = 2200,
            step = 1
          )
        ),
        layout_columns(
          col_widths = c(12),
          textAreaInput(
            inputId = ns("edit_size_confirmed_notes"),
            label = "Size Confirmed Notes",
            value = if (
              !is.null(record) && !is.na(record$size_confirmed_notes)
            ) {
              record$size_confirmed_notes
            } else {
              ""
            },
            rows = 3,
            placeholder = "Enter notes about size confirmation"
          )
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$pid, iv$is_valid())

      pid <- input$pid

      # Convert acres to hectares (1 acre = 0.404686 ha)
      size_confirmed_acres <- if (
        is.null(input$edit_size_confirmed_acres) ||
          is.na(input$edit_size_confirmed_acres)
      ) {
        NA_real_
      } else {
        as.numeric(input$edit_size_confirmed_acres)
      }

      size_confirmed_ha <- if (is.na(size_confirmed_acres)) {
        NA_real_
      } else {
        size_confirmed_acres * 0.404686
      }

      # Build update tibble
      update_tibble <- tibble(
        pid = pid,
        acquisition_type_id = if (isTruthy(input$edit_acquisition_type_id)) {
          as.integer(input$edit_acquisition_type_id)
        } else {
          NA_integer_
        },
        priority_securement_ranking_id = if (
          isTruthy(input$edit_priority_securement_ranking_id)
        ) {
          as.integer(input$edit_priority_securement_ranking_id)
        } else {
          NA_integer_
        },
        priority_ecological_ranking_id = if (
          isTruthy(input$edit_priority_ecological_ranking_id)
        ) {
          as.integer(input$edit_priority_ecological_ranking_id)
        } else {
          NA_integer_
        },
        size_confirmed_ha = size_confirmed_ha,
        size_confirmed_acres = size_confirmed_acres,
        size_confirmed_notes = if (isTruthy(input$edit_size_confirmed_notes)) {
          as.character(input$edit_size_confirmed_notes)
        } else {
          NA_character_
        },
        af_transaction = if (isTruthy(input$edit_af_transaction)) {
          as.logical(input$edit_af_transaction)
        } else {
          NA
        },
        landowner_interest_ranking_id = if (
          isTruthy(input$edit_landowner_interest_ranking_id)
        ) {
          as.integer(input$edit_landowner_interest_ranking_id)
        } else {
          NA_integer_
        },
        tax_exempt = if (isTruthy(input$edit_tax_exempt)) {
          as.logical(input$edit_tax_exempt)
        } else {
          NA
        },
        tax_exempt_year = if (isTruthy(input$edit_tax_exempt_year)) {
          as.integer(input$edit_tax_exempt_year)
        } else {
          NA_integer_
        }
      )
      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = update_tibble,
        where_cols = "pid"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Securement details for parcel {selected_record()$pid} have been successfully updated"
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

      # Clear the sidebar filters
      updateSelectizeInput(
        session,
        inputId = "property_name",
        selected = character(0),
        choices = c("", property_choices()),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "pid",
        selected = character(0),
        choices = character(0),
        options = list(
          create = FALSE,
          placeholder = "First select a property"
        ),
        server = TRUE
      )
    })
  })
}
