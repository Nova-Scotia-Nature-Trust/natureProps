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
            label = "Property Name",
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
            label = "PID",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "First select a property"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Parcel",
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

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT property_name FROM properties ORDER BY property_name;"
      ) |>
        pull(property_name) |>
        sort()
    })

    ## Reactive :: PIDs for selected property ----
    pids_reactive <- reactive({
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
      ) |>
        select(acquisition_value, id) |>
        deframe()
    })

    ## Reactive :: Priority securement ranking choices ----
    priority_securement_ranking_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, ranking_value FROM ranking ORDER BY ranking_value;"
      ) |>
        select(ranking_value, id) |>
        deframe()
    })

    ## Reactive :: Priority ecological ranking choices ----
    priority_ecological_ranking_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, ranking_value FROM ranking ORDER BY ranking_value;"
      ) |>
        select(ranking_value, id) |>
        deframe()
    })

    ## Reactive :: Landowner interest ranking choices ----
    landowner_interest_ranking_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, ranking_value FROM ranking ORDER BY ranking_value;"
      ) |>
        select(ranking_value, id) |>
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

    ## Update PID dropdown based on selected property ----
    observe({
      req(input$property_name)

      pids <- pids_reactive()

      updateSelectizeInput(
        session,
        inputId = "pid",
        choices = c("", pids),
        selected = "",
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
    observeEvent(input$load_record, {
      req(input$pid)

      pid <- input$pid

      query <- glue_sql(
        "SELECT 
          pid,
          property_id,
          acquisition_type_id,
          date_added,
          date_updated,
          priority_securement_ranking_id,
          priority_ecological_ranking_id,
          size_confirmed_ha,
          size_confirmed_acres,
          size_confirmed_notes,
          af_transaction,
          landowner_interest_ranking_id
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
            choices = c("", acquisition_type_choices()),
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
            choices = c("", landowner_interest_ranking_choices()),
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
            choices = c("", priority_securement_ranking_choices()),
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
            choices = c("", priority_ecological_ranking_choices()),
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
        ),
        layout_columns(
          col_widths = c(6, 6),
          dateInput(
            inputId = ns("edit_date_added"),
            label = "Date Added",
            value = if (!is.null(record) && !is.na(record$date_added)) {
              record$date_added
            } else {
              NULL
            }
          ),
          dateInput(
            inputId = ns("edit_date_updated"),
            label = "Date Updated",
            value = if (!is.null(record) && !is.na(record$date_updated)) {
              record$date_updated
            } else {
              NULL
            }
          )
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$pid)

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
        acquisition_type_id = if (
          is.null(input$edit_acquisition_type_id) ||
            input$edit_acquisition_type_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_acquisition_type_id)
        },
        date_added = if (is.null(input$edit_date_added)) {
          NA_character_
        } else {
          as.character(input$edit_date_added)
        },
        date_updated = if (is.null(input$edit_date_updated)) {
          NA_character_
        } else {
          as.character(input$edit_date_updated)
        },
        priority_securement_ranking_id = if (
          is.null(input$edit_priority_securement_ranking_id) ||
            input$edit_priority_securement_ranking_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_priority_securement_ranking_id)
        },
        priority_ecological_ranking_id = if (
          is.null(input$edit_priority_ecological_ranking_id) ||
            input$edit_priority_ecological_ranking_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_priority_ecological_ranking_id)
        },
        size_confirmed_ha = size_confirmed_ha,
        size_confirmed_acres = size_confirmed_acres,
        size_confirmed_notes = if (
          is.null(input$edit_size_confirmed_notes) ||
            input$edit_size_confirmed_notes == ""
        ) {
          NA_character_
        } else {
          as.character(input$edit_size_confirmed_notes)
        },
        af_transaction = if (is.null(input$edit_af_transaction)) {
          NA
        } else {
          as.logical(input$edit_af_transaction)
        },
        landowner_interest_ranking_id = if (
          is.null(input$edit_landowner_interest_ranking_id) ||
            input$edit_landowner_interest_ranking_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_landowner_interest_ranking_id)
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
        selected = "",
        choices = c("", property_choices()),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "pid",
        selected = "",
        choices = NULL,
        options = list(
          create = FALSE,
          placeholder = "First select a property"
        ),
        server = TRUE
      )
    })
  })
}
