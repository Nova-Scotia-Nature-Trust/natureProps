# UI ----
module_edit_pricing_ui <- function(id) {
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
              h5("Edit Property Pricing")
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
module_edit_pricing_server <- function(id, db_con, db_updated = NULL) {
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
          price_asking,
          price_appraised,
          price_purchase,
          donated_value,
          hst
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

      # Extract values if record exists, otherwise NULL
      property_name_text <- if (!is.null(record)) {
        paste0("Editing: ", record$property_name)
      } else {
        "No property selected"
      }

      price_asking_val <- if (!is.null(record) && !is.na(record$price_asking)) {
        record$price_asking
      } else {
        NULL
      }

      price_appraised_val <- if (
        !is.null(record) && !is.na(record$price_appraised)
      ) {
        record$price_appraised
      } else {
        NULL
      }

      price_purchase_val <- if (
        !is.null(record) && !is.na(record$price_purchase)
      ) {
        record$price_purchase
      } else {
        NULL
      }

      donated_value_val <- if (
        !is.null(record) && !is.na(record$donated_value)
      ) {
        record$donated_value
      } else {
        NULL
      }

      hst_val <- if (!is.null(record) && !is.na(record$hst)) {
        record$hst
      } else {
        NULL
      }

      tagList(
        h6(
          class = "text-muted",
          property_name_text
        ),
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          numericInput(
            inputId = ns("edit_price_asking"),
            label = "Asking Price",
            value = price_asking_val,
            min = 0,
            step = 1000
          ),
          numericInput(
            inputId = ns("edit_price_appraised"),
            label = "Appraised Price",
            value = price_appraised_val,
            min = 0,
            step = 1000
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          numericInput(
            inputId = ns("edit_price_purchase"),
            label = "Purchase Price",
            value = price_purchase_val,
            min = 0,
            step = 1000
          ),
          numericInput(
            inputId = ns("edit_donated_value"),
            label = "Donated Value",
            value = donated_value_val,
            min = 0,
            step = 1000
          )
        ),
        numericInput(
          inputId = ns("edit_hst"),
          label = "HST",
          value = hst_val,
          min = 0,
          step = 100
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
        price_asking = if (is.null(input$edit_price_asking)) {
          NA_real_
        } else {
          as.numeric(input$edit_price_asking)
        },
        price_appraised = if (is.null(input$edit_price_appraised)) {
          NA_real_
        } else {
          as.numeric(input$edit_price_appraised)
        },
        price_purchase = if (is.null(input$edit_price_purchase)) {
          NA_real_
        } else {
          as.numeric(input$edit_price_purchase)
        },
        donated_value = if (is.null(input$edit_donated_value)) {
          NA_real_
        } else {
          as.numeric(input$edit_donated_value)
        },
        hst = if (is.null(input$edit_hst)) {
          NA_real_
        } else {
          as.numeric(input$edit_hst)
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
          "Pricing for {selected_record()$property_name} has been successfully updated"
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

      updateNumericInput(session, "edit_price_asking", value = NULL)
      updateNumericInput(session, "edit_price_appraised", value = NULL)
      updateNumericInput(session, "edit_price_purchase", value = NULL)
      updateNumericInput(session, "edit_donated_value", value = NULL)
      updateNumericInput(session, "edit_hst", value = NULL)
    })
  })
}
