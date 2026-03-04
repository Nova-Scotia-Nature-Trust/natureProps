# UI ----
module_property_contact_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(8, 4),
    ## Card :: Add Property Contact ----
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
          layout_columns(
            col_widths = c(6, 6),
            selectizeInput(
              inputId = ns("pid"),
              label = "Select PID(s):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = FALSE,
                plugins = list("remove_button"),
                placeholder = "Select PIDs to be assigned to a contact"
              )
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            textInput(
              inputId = ns("name_first"),
              label = "First Name",
              value = ""
            ),
            textInput(
              inputId = ns("name_last"),
              label = "Last Name",
              value = ""
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            textInput(
              inputId = ns("email"),
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
              inputId = ns("phone_home"),
              label = "Home Phone",
              value = ""
            ),
            textInput(
              inputId = ns("phone_cell"),
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
                includeMarkdown("popups/prop_contact_desc.md"),
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
    ),
    # Card :: Update Property Contact ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          h5("Link Property Contact"),
          popover(
            div(
              icon("question-circle"),
              style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
            ),
            "Assign additional PIDs to an existing property contact. Select the PIDs you want to add, then choose the contact to link them to.",
            title = "Link Property Contact Help",
            placement = "right"
          )
        )
      ),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          selectizeInput(
            inputId = ns("pid_update"),
            label = "Select PID(s) to Add:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              plugins = list("remove_button"),
              placeholder = "Select PIDs to link to contact"
            )
          ),
          selectizeInput(
            inputId = ns("contact"),
            label = "Select Existing Property Contact:",
            choices = NULL,
            selected = character(0),
            multiple = FALSE,
            options = list(
              placeholder = "Choose a contact"
            )
          ),
          div(
            style = "margin-top: 20px;",
            actionButton(
              inputId = ns("update_property_contact"),
              label = "Update Contact",
              class = "btn-primary"
            ),
            actionButton(
              inputId = ns("clear_inputs_update"),
              label = "Clear Inputs",
              class = "btn-secondary"
            )
          ),
          div(style = "flex-grow: 1;")
        )
      )
    )
  )
}

# Server ----
module_property_contact_server <- function(id, db_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ## Input Validation :: Add New Contact ----
    iv_create <- InputValidator$new()
    iv_create$add_rule("email", ~ if (isTruthy(.)) sv_email()(.))
    iv_create$add_rule("name_first", sv_required())
    iv_create$add_rule("name_last", sv_required())
    iv_create$add_rule("pid", sv_required())
    iv_create$enable()

    ## Input Validation :: Update Existing Contact ----
    iv_update <- InputValidator$new()
    iv_update$add_rule("pid_update", sv_required())
    iv_update$add_rule("contact", sv_required())
    iv_update$enable()

    ## Reactive :: PID List ----
    pid_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, pid FROM parcels 
         ORDER BY pid;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "pid",
        choices = setNames(
          pid_list()$id,
          pid_list()$pid
        ),
        selected = isolate(input$pid),
        server = TRUE
      )
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "pid_update",
        choices = setNames(
          pid_list()$id,
          pid_list()$pid
        ),
        selected = isolate(input$pid_update),
        server = TRUE
      )
    })

    ## Reactive :: Property Contacts ----
    contacts <- reactive({
      db_updated()
      contacts <- dbReadTable(db_con, "property_contact_details") |>
        mutate(
          display_name = glue("{name_first} {name_last}"),
          display_label = if_else(
            !is.na(email) & email != "",
            glue("{display_name} ({email})"),
            display_name
          )
        ) |>
        arrange(name_last, name_first)
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "contact",
        choices = setNames(
          contacts()$id,
          contacts()$display_label
        ),
        selected = isolate(input$contact),
        server = TRUE
      )
    })

    ## Event :: Submit property contact details ----
    observeEvent(input$submit_property_contact, {
      req(iv_create$is_valid())

      new_property_contact <- tibble(
        name_last = input$name_last,
        name_first = input$name_first,
        email = if_else(isTruthy(input$email), input$email, NA_character_),
        phone_home = if_else(
          isTruthy(input$phone_home),
          input$phone_home,
          NA_character_
        ),
        phone_cell = if_else(
          isTruthy(input$phone_cell),
          input$phone_cell,
          NA_character_
        ),
        dnc = as.logical(input$dnc_input),
        property_contact_description = if_else(
          isTruthy(input$property_contact_description),
          input$property_contact_description,
          NA_character_
        )
      )

      append_db_data(
        "property_contact_details",
        new_property_contact,
        db_con,
        silent = FALSE
      )
      db_updated(db_updated() + 1)

      # Is this robust enough to deal with potential duplicate contact info?
      property_contact_id <- dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT id
          FROM property_contact_details
          WHERE
            name_first = {new_property_contact$name_first}
            AND name_last  = {new_property_contact$name_last}
            AND email IS NOT DISTINCT FROM {new_property_contact$email}
            AND phone_home IS NOT DISTINCT FROM {new_property_contact$phone_home}
            AND phone_cell IS NOT DISTINCT FROM {new_property_contact$phone_cell} 
            AND property_contact_description IS NOT DISTINCT FROM {new_property_contact$property_contact_description} 
          ORDER BY id DESC
          LIMIT 1
          ",
          .con = db_con
        )
      ) |>
        pull(id)

      if (length(input$pid) > 0) {
        dbx::dbxInsert(
          db_con,
          table = "parcel_property_contact",
          records = tibble(
            parcel_id = input$pid,
            property_contact_id = rep(
              x = property_contact_id,
              times = length(input$pid)
            )
          )
        )
      } else {
        message("NO PID ASSOCIATED WITH PROPERTY CONTACT")
      }
    })

    ## Event :: Update property contact with new PIDs ----
    observeEvent(input$update_property_contact, {
      req(iv_update$is_valid())

      dbx::dbxInsert(
        db_con,
        table = "parcel_property_contact",
        records = tibble(
          parcel_id = input$pid_update,
          property_contact_id = input$contact
        )
      )

      db_updated(db_updated() + 1)

      shinyalert(
        title = "Success",
        text = glue(
          "Successfully linked {length(input$pid_update)} PID(s) to the property contact."
        ),
        type = "success",
        closeOnClickOutside = FALSE,
        timer = 10000
      )

      ## Clear inputs after successful update
      updateSelectizeInput(
        session,
        "pid_update",
        choices = setNames(
          pid_list()$id,
          pid_list()$pid
        ),
        selected = character(0),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "contact",
        choices = setNames(
          contacts()$id,
          contacts()$display_label
        ),
        selected = character(0),
        server = TRUE
      )
    })

    ## Event :: Clear Input Add Contact ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        "pid",
        choices = setNames(
          pid_list()$id,
          pid_list()$pid
        ),
        selected = character(0)
      )
      updateTextInput(session, "name_last", value = "")
      updateTextInput(session, "name_first", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone_home", value = "")
      updateTextInput(session, "phone_cell", value = "")
      updateSelectInput(session, "dnc_input", selected = "FALSE")
      updateTextInput(session, "property_contact_description", value = "")
    })

    ## Event :: Clear inputs Update Contact ----
    observeEvent(input$clear_inputs_update, {
      updateSelectizeInput(
        session,
        "pid_update",
        choices = setNames(
          pid_list()$id,
          pid_list()$pid
        ),
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "contact",
        choices = setNames(
          contacts()$id,
          contacts()$display_label
        ),
        selected = character(0)
      )
    })
  })
}
