# UI ----
module_edit_property_contacts_ui <- function(id) {
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
            inputId = ns("contact_id"),
            label = "Select Contact Name",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select contact"
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
              h5("Edit Property Contact Details")
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
module_edit_property_contacts_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input Validation ----
    iv <- InputValidator$new()
    iv$add_rule("edit_postal_code", ~ if (isTruthy(.)) validate_postal_code(.))
    iv$enable()

    ## Reactive :: Contact choices ----
    contacts <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, name_first, name_last 
        FROM property_contact_details 
        ORDER BY name_last, name_first;"
      ) |>
        mutate(display_label = glue("{name_last}, {name_first} (ID:{id})"))
    })

    ## Update contact dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "contact_id",
        choices = c(
          "",
          setNames(
            contacts()$id,
            contacts()$display_label
          )
        ),
        selected = isolate(input$contact_id),
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record ----
    observeEvent(input$contact_id, {
      contact_id <- input$contact_id

      if (!isTruthy(contact_id)) {
        selected_record(NULL)
        return()
      }

      query <- glue_sql(
        "SELECT 
          id,
          name_last,
          name_first,
          email,
          phone_home,
          phone_cell,
          dnc,
          property_contact_description,
          re_constituent_id,
          address_line1,
          address_line2,
          city,
          state_province_code,
          postal_code,
          country_code
        FROM property_contact_details 
        WHERE id = {contact_id}",
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
            "No contact selected"
          } else {
            paste0(
              "Editing Contact: ",
              paste(
                record$name_last,
                record$name_first,
                sep = ", "
              ) |>
                str_trim() |>
                str_remove("^,\\s*|\\s*,$")
            )
          }
        ),
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_name_last"),
            label = "Last Name",
            value = if (!is.null(record) && !is.na(record$name_last)) {
              record$name_last
            } else {
              ""
            },
            placeholder = "Enter last name"
          ),
          textInput(
            inputId = ns("edit_name_first"),
            label = "First Name",
            value = if (!is.null(record) && !is.na(record$name_first)) {
              record$name_first
            } else {
              ""
            },
            placeholder = "Enter first name"
          )
        ),
        layout_columns(
          col_widths = c(12),
          textInput(
            inputId = ns("edit_email"),
            label = "Email",
            value = if (!is.null(record) && !is.na(record$email)) {
              record$email
            } else {
              ""
            },
            placeholder = "Enter email address"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_phone_home"),
            label = "Home Phone",
            value = if (!is.null(record) && !is.na(record$phone_home)) {
              record$phone_home
            } else {
              ""
            },
            placeholder = "Enter home phone"
          ),
          textInput(
            inputId = ns("edit_phone_cell"),
            label = "Cell Phone",
            value = if (!is.null(record) && !is.na(record$phone_cell)) {
              record$phone_cell
            } else {
              ""
            },
            placeholder = "Enter cell phone"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          checkboxInput(
            inputId = ns("edit_dnc"),
            label = "Do Not Contact (DNC)",
            value = if (!is.null(record) && !is.na(record$dnc)) {
              record$dnc
            } else {
              FALSE
            }
          ),
          textInput(
            inputId = ns("edit_re_constituent_id"),
            label = "RE Constituent ID",
            value = if (!is.null(record) && !is.na(record$re_constituent_id)) {
              record$re_constituent_id
            } else {
              ""
            },
            placeholder = "Enter RE constituent ID"
          )
        ),
        layout_columns(
          col_widths = c(12),
          textAreaInput(
            inputId = ns("edit_property_contact_description"),
            label = "Property Contact Description",
            value = if (
              !is.null(record) && !is.na(record$property_contact_description)
            ) {
              record$property_contact_description
            } else {
              ""
            },
            rows = 3,
            placeholder = "Enter notes or description about this contact"
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          h6("Mailing Address", class = "text-muted"),
          popover(
            icon("question-circle"),
            includeMarkdown("popups/mailing_address.md"),
            title = "Mailing Address Help",
            placement = "top"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_address_line1"),
            label = "Address Line 1",
            value = if (!is.null(record) && !is.na(record$address_line1)) {
              record$address_line1
            } else {
              ""
            },
            placeholder = "Enter street address"
          ),
          textInput(
            inputId = ns("edit_address_line2"),
            label = "Address Line 2",
            value = if (!is.null(record) && !is.na(record$address_line2)) {
              record$address_line2
            } else {
              ""
            },
            placeholder = "Enter apartment, unit, or suite"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_city"),
            label = "City",
            value = if (!is.null(record) && !is.na(record$city)) {
              record$city
            } else {
              ""
            },
            placeholder = "Enter city"
          ),
          textInput(
            inputId = ns("edit_state_province_code"),
            label = "Province/State",
            value = if (
              !is.null(record) && !is.na(record$state_province_code)
            ) {
              record$state_province_code
            } else {
              ""
            },
            placeholder = "Enter province or state"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_postal_code"),
            label = "Postal Code",
            value = if (!is.null(record) && !is.na(record$postal_code)) {
              record$postal_code
            } else {
              ""
            },
            placeholder = "Enter postal code"
          ),
          textInput(
            inputId = ns("edit_country_code"),
            label = "Country",
            value = if (!is.null(record) && !is.na(record$country_code)) {
              record$country_code
            } else {
              ""
            },
            placeholder = "Enter country code"
          )
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$contact_id)
      req(iv$is_valid())

      contact_id <- as.integer(input$contact_id)

      # Build update tibble
      update_tibble <- tibble(
        id = contact_id,
        name_last = if (isTruthy(input$edit_name_last)) {
          as.character(input$edit_name_last)
        } else {
          NA_character_
        },
        name_first = if (isTruthy(input$edit_name_first)) {
          as.character(input$edit_name_first)
        } else {
          NA_character_
        },
        email = if (isTruthy(input$edit_email)) {
          as.character(input$edit_email)
        } else {
          NA_character_
        },
        phone_home = if (isTruthy(input$edit_phone_home)) {
          as.character(input$edit_phone_home)
        } else {
          NA_character_
        },
        phone_cell = if (isTruthy(input$edit_phone_cell)) {
          as.character(input$edit_phone_cell)
        } else {
          NA_character_
        },
        dnc = if (isTruthy(input$edit_dnc)) {
          as.logical(input$edit_dnc)
        } else {
          FALSE
        },
        property_contact_description = if (
          isTruthy(input$edit_property_contact_description)
        ) {
          as.character(input$edit_property_contact_description)
        } else {
          NA_character_
        },
        re_constituent_id = if (isTruthy(input$edit_re_constituent_id)) {
          as.character(input$edit_re_constituent_id)
        } else {
          NA_character_
        },
        address_line1 = if (isTruthy(input$edit_address_line1)) {
          as.character(input$edit_address_line1)
        } else {
          NA_character_
        },
        address_line2 = if (isTruthy(input$edit_address_line2)) {
          as.character(input$edit_address_line2)
        } else {
          NA_character_
        },
        city = if (isTruthy(input$edit_city)) {
          as.character(input$edit_city)
        } else {
          NA_character_
        },
        state_province_code = if (isTruthy(input$edit_state_province_code)) {
          as.character(input$edit_state_province_code)
        } else {
          NA_character_
        },
        postal_code = if (isTruthy(input$edit_postal_code)) {
          as.character(input$edit_postal_code)
        } else {
          NA_character_
        },
        country_code = if (isTruthy(input$edit_country_code)) {
          as.character(input$edit_country_code)
        } else {
          NA_character_
        }
      )

      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "property_contact_details",
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
          "Contact details for {selected_record()$name_last}, {selected_record()$name_first} have been successfully updated"
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
        inputId = "contact_id",
        selected = character(0),
        choices = c(
          "",
          setNames(
            contacts()$id,
            contacts()$display_label
          )
        ),
        server = TRUE
      )
    })
  })
}
