# UI ----
module_property_contact_ui <- function(id) {
  ns <- NS(id)

  card(
    full_screen = TRUE,
    height = "100%",
    card_header(
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        h5("Property Contact Details"),
        popover(
          div(
            icon("question-circle"),
            style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
          ),
          "Explain why we're calling this 'Primary Property Contact'. Enter contact information for property owners. Select associated PIDs from the dropdown to link this contact to specific parcels.",
          title = "Property Contact Details Help",
          placement = "right"
        )
      )
    ),
    card_body(
      div(
        style = "display: flex; flex-direction: column; gap: 15px;",
        selectizeInput(
          inputId = ns("pid_input_property_contact"),
          label = "Select PID(s):",
          choices = NULL,
          multiple = TRUE,
          options = list(
            create = FALSE,
            plugins = list("remove_button"),
            placeholder = "Select PIDs associated with property contact"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("name_first_input"),
            label = "First Name",
            value = ""
          ),
          textInput(
            inputId = ns("name_last_input"),
            label = "Last Name",
            value = ""
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("email_input"),
            label = "Email",
            value = ""
          ),
          selectInput(
            inputId = ns("dnc_input"),
            label = "Do Not Contact",
            choices = list("TRUE" = TRUE, "FALSE" = FALSE),
            selected = "FALSE"
          )
        ),
        layout_columns(
          col_width = c(6, 6),
          textInput(
            inputId = ns("phone_home_input"),
            label = "Home Phone",
            value = ""
          ),
          textInput(
            inputId = ns("phone_cell_input"),
            label = "Cell Phone",
            value = ""
          )
        ),
        div(
          style = "width: 100%;",
          div(
            style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
            tags$label(
              "Property Contact Description",
              `for` = ns("property_contact_description")
            ),
            popover(
              div(
                icon("question-circle"),
                style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
              ),
              includeMarkdown("help/prop_contact_desc.md"),
              title = "Property Contact Help",
              placement = "top"
            )
          ),
          textAreaInput(
            ns("property_contact_description"),
            label = NULL,
            "",
            height = "100px",
            width = "100%"
          )
        ),
        div(
          style = "margin-top: 20px;",
          actionButton(
            inputId = ns("submit_property_contact"),
            label = "Add Property Contact",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          )
        ),
        div(style = "flex-grow: 1;")
      )
    )
  )
}

# Server ----
module_property_contact_server <- function(id, db_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("email_input", ~ if (isTruthy(.)) sv_email()(.))
    iv$add_rule("name_first_input", sv_required())
    iv$add_rule("name_last_input", sv_required())
    iv$add_rule("pid_input_property_contact", sv_required())
    iv$enable()

    ## Populate UI inputs ----
    pid_choices <- reactive({
      db_updated()
      dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
        pull() |>
        sort()
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "pid_input_property_contact",
        choices = pid_choices(),
        server = TRUE
      )
    })

    ## Event :: Submit property contact details ----
    observeEvent(input$submit_property_contact, {
      req(input$name_first_input)
      req(input$name_last_input)
      req(input$pid_input_property_contact)

      new_property_contact <- tibble(
        name_last = input$name_last_input,
        name_first = input$name_first_input,
        email = input$email_input,
        phone_home = input$phone_home_input,
        phone_cell = input$phone_cell_input,
        dnc = as.logical(input$dnc_input),
        property_contact_description = input$property_contact_description
      )

      append_db_data(
        "property_contact_details",
        new_property_contact,
        db_con,
        silent = FALSE
      )
      db_updated(db_updated() + 1)

      ## Get new contact ID
      property_contact_id <- new_property_contact |>
        left_join(dbReadTable(db_con, "property_contact_details")) |>
        pull(id)

      ## Assign the contact ID to relevant PIDs
      if (length(input$pid_input_property_contact) > 0) {
        dbx::dbxUpdate(
          db_con,
          table = "parcels",
          records = tibble(
            pid = input$pid_input_property_contact,
            property_contact_id
          ),
          where_cols = c("pid")
        )
      } else {
        message("NO PID ASSOCIATED WITH PROPERTY CONTACT")
      }
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        "pid_input_property_contact",
        choices = pid_choices(),
        selected = character(0),
        server = TRUE
      )
      updateTextInput(session, "name_last_input", value = "")
      updateTextInput(session, "name_first_input", value = "")
      updateTextInput(session, "email_input", value = "")
      updateTextInput(session, "phone_home_input", value = "")
      updateTextInput(session, "phone_cell_input", value = "")
      updateSelectInput(session, "dnc_input", selected = "FALSE")
      updateTextInput(session, "property_contact_description", value = "")
    })

    ## Return reactive to update PID choices when new properties are added
    return(
      list(
        update_pid_choices = function() {
          updateSelectizeInput(
            session,
            inputId = "pid_input_property_contact",
            choices = pid_choices(),
            server = TRUE
          )
        }
      )
    )
  })
}
