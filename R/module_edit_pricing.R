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
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, 
                CONCAT_WS(' || ', property_name, property_name_public) AS property_name 
        FROM properties 
        ORDER BY property_name;"
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
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record ----
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
          price_asking,
          price_offer,
          price_purchase,
          donated_value,
          hst,
          unpaid_land_value,
          price_offer_history
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
      property_name_text <- if (isTruthy(record$property_name)) {
        paste0("Editing: ", record$property_name)
      } else {
        "No property selected"
      }

      price_asking_val <- if (isTruthy(record$price_asking)) {
        record$price_asking
      } else {
        NULL
      }

      price_offer_val <- if (isTruthy(record$price_offer)) {
        record$price_offer
      } else {
        NULL
      }

      price_purchase_val <- if (isTruthy(record$price_purchase)) {
        record$price_purchase
      } else {
        NULL
      }

      donated_value_val <- if (isTruthy(record$donated_value)) {
        record$donated_value
      } else {
        NULL
      }

      hst_val <- if (isTruthy(record$hst)) {
        record$hst
      } else {
        FALSE
      }

      unpaid_land_value_val <- if (isTruthy(record$unpaid_land_value)) {
        record$unpaid_land_value
      } else {
        NULL
      }

      price_offer_history_val <- if (isTruthy(record$price_offer_history)) {
        record$price_offer_history
      } else {
        ""
      }

      # Per-acre calculations
      per_acre_ui <- if (isTruthy(record$id)) {
        acres_row <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT SUM(pi.area_ha * 2.471) AS total_acres
             FROM parcels pa
             JOIN parcel_info pi ON pa.id = pi.parcel_id
             WHERE pa.property_id = {record$id}",
            .con = db_con
          )
        )

        total_acres <- acres_row$total_acres

        if (!isTruthy(total_acres) || total_acres == 0) {
          NULL
        } else {
          per_acre <- function(val) {
            if (!isTruthy(val)) {
              return(NULL)
            }
            scales::dollar(round(val / total_acres, 2), big.mark = ",")
          }

          auth <- dbGetQuery(
            db_con,
            glue_sql(
              "SELECT a.fmv, a.fmv / NULLIF(SUM(pi.area_ha * 2.471), 0) AS fmv_per_acre
               FROM appraisals a
               JOIN parcels pa ON a.property_id = pa.property_id
               JOIN parcel_info pi ON pa.id = pi.parcel_id
               WHERE a.property_id = {record$id} AND a.authoritative = TRUE
               GROUP BY a.property_id, a.fmv
               LIMIT 1",
              .con = db_con
            )
          )

          fmv_line <- if (nrow(auth) > 0 && isTruthy(auth$fmv)) {
            fmv_fmt <- scales::dollar(auth$fmv, big.mark = ",")
            fmv_per_acre_fmt <- if (isTruthy(auth$fmv_per_acre)) {
              scales::dollar(round(auth$fmv_per_acre, 2), big.mark = ",")
            } else {
              "N/A"
            }
            paste0(
              "FMV (Authoritative): ",
              fmv_fmt,
              " | FMV/acre: ",
              fmv_per_acre_fmt
            )
          } else {
            NULL
          }

          lines <- list(
            fmv_line,
            if (!is.null(per_acre(price_asking_val))) {
              paste0("Asking Price/acre: ", per_acre(price_asking_val))
            },
            if (!is.null(per_acre(price_offer_val))) {
              paste0("Offer Price/acre: ", per_acre(price_offer_val))
            },
            if (!is.null(per_acre(price_purchase_val))) {
              paste0("Purchase Price/acre: ", per_acre(price_purchase_val))
            },
            if (!is.null(per_acre(donated_value_val))) {
              paste0("Donated Value/acre: ", per_acre(donated_value_val))
            },
            if (!is.null(per_acre(unpaid_land_value_val))) {
              paste0(
                "Unpaid Land Value/acre: ",
                per_acre(unpaid_land_value_val)
              )
            }
          )

          lines <- Filter(Negate(is.null), lines)

          if (length(lines) == 0) {
            NULL
          } else {
            div(
              class = "text-muted",
              style = "font-size: 0.85em;",
              tagList(lapply(lines, \(l) div(l)))
            )
          }
        }
      }

      tagList(
        h6(
          class = "text-muted",
          property_name_text
        ),
        per_acre_ui,
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          autonumericInput(
            inputId = ns("edit_price_asking"),
            label = "Asking Price",
            value = price_asking_val,
            currencySymbol = "$",
            align = "left"
          ),
          autonumericInput(
            inputId = ns("edit_price_offer"),
            label = "Offer Price",
            value = price_offer_val,
            currencySymbol = "$",
            align = "left"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              "Purchase Price",
              popover(
                icon("question-circle"),
                "If HST is applicable do not include that amount in the Purchase Price field",
                title = "Context",
                placement = "top"
              )
            ),
            autonumericInput(
              inputId = ns("edit_price_purchase"),
              label = NULL,
              value = price_purchase_val,
              currencySymbol = "$",
              align = "left"
            )
          ),
          autonumericInput(
            inputId = ns("edit_donated_value"),
            label = "Donated Value",
            value = donated_value_val,
            currencySymbol = "$",
            align = "left"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          checkboxInput(
            inputId = ns("edit_hst"),
            label = "HST",
            value = hst_val
          ),
          autonumericInput(
            inputId = ns("edit_unpaid_land_value"),
            label = "Unpaid Land Value",
            value = unpaid_land_value_val,
            currencySymbol = "$",
            align = "left"
          )
        ),
        textAreaInput(
          inputId = ns("edit_price_offer_history"),
          label = "Price Offer History",
          value = price_offer_history_val,
          rows = 4,
          resize = "vertical",
          width = "100%"
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
        price_asking = if (isTruthy(input$edit_price_asking)) {
          as.numeric(input$edit_price_asking)
        } else {
          NA_real_
        },
        price_offer = if (isTruthy(input$edit_price_offer)) {
          as.numeric(input$edit_price_offer)
        } else {
          NA_real_
        },
        price_purchase = if (isTruthy(input$edit_price_purchase)) {
          as.numeric(input$edit_price_purchase)
        } else {
          NA_real_
        },
        donated_value = if (isTruthy(input$edit_donated_value)) {
          as.numeric(input$edit_donated_value)
        } else {
          NA_real_
        },
        hst = isTruthy(input$edit_hst),
        unpaid_land_value = if (isTruthy(input$edit_unpaid_land_value)) {
          as.numeric(input$edit_unpaid_land_value)
        } else {
          NA_real_
        },
        price_offer_history = if (isTruthy(input$edit_price_offer_history)) {
          input$edit_price_offer_history
        } else {
          NA_character_
        }
      )
      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = update_tibble,
        where_cols = "id"
      )

      update_property_timestamp(con = db_con, property_id = db_id)

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
      updateAutonumericInput(session, "edit_price_asking", value = NULL)
      updateAutonumericInput(session, "edit_price_offer", value = NULL)
      updateAutonumericInput(session, "edit_price_purchase", value = NULL)
      updateAutonumericInput(session, "edit_donated_value", value = NULL)
      updateCheckboxInput(session, "edit_hst", value = FALSE)
      updateAutonumericInput(session, "edit_unpaid_land_value", value = NULL)
      updateTextAreaInput(session, "edit_price_offer_history", value = "")
    })
  })
}
