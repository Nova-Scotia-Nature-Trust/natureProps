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
            icon("question-circle"),
            includeMarkdown("popups/prop_contact_motivation.md"),
            title = "Context",
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
              inputId = ns("property_id"),
              label = "Select one or more properties:",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = FALSE,
                plugins = list("remove_button"),
                placeholder = "Select properties to be assigned to a contact"
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
              "Property Contact Description",
              popover(
                icon("question-circle"),
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
            icon("question-circle"),
            "Assign additional properties to an existing property contact. Select the properties you want to add, then choose the contact to link them to.",
            title = "Link Contacts",
            placement = "right"
          )
        )
      ),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          selectizeInput(
            inputId = ns("property_id_update"),
            label = "Select one or more properties to add:",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              plugins = list("remove_button"),
              placeholder = "Select properties to link to contact"
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
    iv_create$add_rule("property_id", sv_required())
    iv_create$add_rule(
      "phone_home",
      ~ if (isTruthy(.)) validate_phone_number(.)
    )
    iv_create$add_rule(
      "phone_cell",
      ~ if (isTruthy(.)) validate_phone_number(.)
    )
    iv_create$enable()

    ## Input Validation :: Update Existing Contact ----
    iv_update <- InputValidator$new()
    iv_update$add_rule("property_id_update", sv_required())
    iv_update$add_rule("contact", sv_required())
    iv_update$enable()

    ## Reactive :: Properties List ----
    properties_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties 
         ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property_id",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
        ),
        selected = isolate(input$property_id),
        server = TRUE
      )
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "property_id_update",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
        ),
        selected = isolate(input$property_id_update),
        server = TRUE
      )
    })

    ## Reactive :: Property Contacts ----
    contacts <- reactive({
      db_updated()
      contacts <- dbReadTable(db_con, "property_contact_details") |>
        mutate(
          display_label = str_glue("{name_first} {name_last}  (ID:{id})")
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

      # Require at least one contact method before proceeding
      if (
        !isTruthy(input$email) &&
          !isTruthy(input$phone_home) &&
          !isTruthy(input$phone_cell)
      ) {
        shinyalert(
          title = "Missing Contact Method",
          text = "Please provide at least one of Email, Home Phone, or Cell Phone.",
          type = "warning",
          closeOnClickOutside = FALSE,
          timer = 10000
        )
        return()
      }

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

      if (length(input$property_id) > 0) {
        # Guard against re-linking a property already tied to this contact
        # (violates the unique_properties_contact constraint)
        existing_property_ids <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT property_id FROM properties_contact WHERE property_contact_id = {property_contact_id}",
            .con = db_con
          )
        ) |>
          pull(property_id)

        new_property_ids <- setdiff(input$property_id, existing_property_ids)

        if (length(new_property_ids) > 0) {
          dbx::dbxInsert(
            db_con,
            table = "properties_contact",
            records = tibble(
              property_id = new_property_ids,
              property_contact_id = rep(
                x = property_contact_id,
                times = length(new_property_ids)
              )
            )
          )
        }

        if (length(new_property_ids) < length(input$property_id)) {
          shinyalert(
            title = "Some Properties Skipped",
            text = glue(
              "Skipped {length(input$property_id) - length(new_property_ids)} propert(y/ies) already linked to this contact."
            ),
            type = "info",
            closeOnClickOutside = FALSE,
            timer = 10000
          )
        }
      } else {
        message("NO PROPERTY ASSOCIATED WITH PROPERTY CONTACT")
      }
    })

    ## Event :: Update property contact with new properties ----
    observeEvent(input$update_property_contact, {
      req(iv_update$is_valid())

      # Guard against re-linking a property already tied to this contact
      # (violates the unique_properties_contact constraint)
      existing_property_ids <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT property_id FROM properties_contact WHERE property_contact_id = {input$contact}",
          .con = db_con
        )
      ) |>
        pull(property_id)

      new_property_ids <- setdiff(
        input$property_id_update,
        existing_property_ids
      )
      n_skipped <- length(input$property_id_update) - length(new_property_ids)

      if (length(new_property_ids) > 0) {
        dbx::dbxInsert(
          db_con,
          table = "properties_contact",
          records = tibble(
            property_id = new_property_ids,
            property_contact_id = input$contact
          )
        )

        db_updated(db_updated() + 1)
      }

      success_text <- glue(
        "Successfully linked {length(new_property_ids)} propert(y/ies) to the property contact."
      )
      if (n_skipped > 0) {
        success_text <- glue(
          "{success_text} Skipped {n_skipped} propert(y/ies) already linked to this contact."
        )
      }

      shinyalert(
        title = if (length(new_property_ids) > 0) "Success" else "No Changes",
        text = if (length(new_property_ids) > 0) {
          success_text
        } else {
          "All selected properties are already linked to this contact."
        },
        type = if (length(new_property_ids) > 0) "success" else "info",
        closeOnClickOutside = FALSE,
        timer = 10000
      )

      ## Clear inputs after successful update
      updateSelectizeInput(
        session,
        "property_id_update",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
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
        "property_id",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
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
        "property_id_update",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
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
