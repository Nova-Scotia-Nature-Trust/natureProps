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
            label = "Contact Name",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select contact"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Contact",
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

    ## Reactive :: Contact choices ----
    contact_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, name_first, name_last 
        FROM property_contact_details 
        ORDER BY name_last, name_first;"
      ) |>
        mutate(
          display_name = paste(
            coalesce(name_last, ""),
            coalesce(name_first, ""),
            sep = ", "
          ) |>
            str_trim() |>
            str_remove("^,\\s*|\\s*,$")
        ) |>
        select(display_name, id) |>
        deframe()
    })

    ## Update contact dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "contact_id",
        choices = c("", contact_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$contact_id)

      contact_id <- input$contact_id

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
          re_constituent_id
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
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$contact_id)

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
        selected = "",
        choices = c("", contact_choices()),
        server = TRUE
      )
    })
  })
}
