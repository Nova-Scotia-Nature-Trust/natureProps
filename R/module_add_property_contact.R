# UI ----
module_add_property_contact_ui <- function(id) {
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
              class = "btn-success"
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
              class = "btn-success"
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
module_add_property_contact_server <- function(id, db_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ## Reactive values :: Pending contact confirmation ----
    pending_contact <- reactiveVal(NULL)
    pending_property_ids <- reactiveVal(NULL)

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
        "SELECT
           id,
           CONCAT_WS(' || ', property_name, property_name_public) AS property_name
         FROM properties
         ORDER BY property_name;"
      )
    })

    ## Update :: Properties Select Input ----
    observe({
      updateSelectizeInput(
        session,
        "property_id",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )
    })

    observe({
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
    })

    ## Reactive :: Property Contacts ----
    contacts <- reactive({
      db_updated()

      dbReadTable(
        db_con,
        "property_contact_details"
      ) |>
        mutate(
          display_label = str_glue("{name_first} {name_last}  (ID:{id})")
        ) |>
        arrange(name_last, name_first)
    })

    ## Update :: Contact Select Input ----
    observe({
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

    ## Helper :: Clear Add Contact Inputs ----
    clear_add_contact_inputs <- function() {
      updateSelectizeInput(
        session,
        "property_id",
        choices = setNames(
          properties_list()$id,
          properties_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )

      updateTextInput(session, "name_last", value = "")
      updateTextInput(session, "name_first", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone_home", value = "")
      updateTextInput(session, "phone_cell", value = "")
      updateSelectInput(session, "dnc_input", selected = "FALSE")
      updateTextInput(session, "property_contact_description", value = "")
    }

    ## Helper :: Clear Update Contact Inputs ----
    clear_update_contact_inputs <- function() {
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
    }

    ## Helper :: Add Property Contact ----
    add_property_contact <- function(
      new_property_contact,
      property_ids
    ) {
      ## Add contact details ----
      append_db_data(
        "property_contact_details",
        new_property_contact,
        db_con,
        silent = FALSE
      )

      ## Find newly-created contact ID ----
      property_contact_id <- dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT id
          FROM property_contact_details
          WHERE
            name_first = {new_property_contact$name_first}
            AND name_last = {new_property_contact$name_last}
            AND email IS NOT DISTINCT FROM {new_property_contact$email}
            AND phone_home IS NOT DISTINCT FROM {new_property_contact$phone_home}
            AND phone_cell IS NOT DISTINCT FROM {new_property_contact$phone_cell}
            AND property_contact_description IS NOT DISTINCT FROM
                {new_property_contact$property_contact_description}
          ORDER BY id DESC
          LIMIT 1
          ",
          .con = db_con
        )
      ) |>
        pull(id)

      req(length(property_contact_id) == 1)

      ## Link contact to properties ----
      if (length(property_ids) > 0) {
        existing_property_ids <- dbGetQuery(
          db_con,
          glue_sql(
            "
            SELECT property_id
            FROM properties_contact
            WHERE property_contact_id = {property_contact_id}
            ",
            .con = db_con
          )
        ) |>
          pull(property_id)

        new_property_ids <- setdiff(
          property_ids,
          existing_property_ids
        )

        if (length(new_property_ids) > 0) {
          dbx::dbxInsert(
            db_con,
            table = "properties_contact",
            records = tibble(
              property_id = new_property_ids,
              property_contact_id = rep(
                property_contact_id,
                length(new_property_ids)
              )
            )
          )
        }

        ## Alert if any properties were skipped ----
        n_skipped <- length(property_ids) - length(new_property_ids)

        if (n_skipped > 0) {
          shinyalert(
            title = "Some Properties Skipped",
            text = glue(
              "Skipped {n_skipped} propert(y/ies) already linked to this contact."
            ),
            type = "info",
            closeOnClickOutside = FALSE,
            timer = 10000
          )
        }
      } else {
        message(
          "NO PROPERTY ASSOCIATED WITH PROPERTY CONTACT"
        )
      }

      ## Update database trigger ----
      db_updated(db_updated() + 1)
    }

    ## Event :: Submit property contact details ----
    observeEvent(
      input$submit_property_contact,
      {
        req(iv_create$is_valid())

        ## Require at least one contact method ----
        if (
          !isTruthy(input$email) &&
            !isTruthy(input$phone_home) &&
            !isTruthy(input$phone_cell)
        ) {
          shinyalert(
            title = "Missing Contact Method",
            text = paste(
              "Please provide at least one of Email, Home Phone,",
              "or Cell Phone."
            ),
            type = "warning",
            closeOnClickOutside = FALSE,
            timer = 10000
          )

          return()
        }

        ## Create new contact record ----
        new_property_contact <- tibble(
          name_last = input$name_last,
          name_first = input$name_first,

          email = if (isTruthy(input$email)) {
            input$email
          } else {
            NA_character_
          },

          phone_home = if (isTruthy(input$phone_home)) {
            input$phone_home
          } else {
            NA_character_
          },

          phone_cell = if (isTruthy(input$phone_cell)) {
            input$phone_cell
          } else {
            NA_character_
          },

          dnc = as.logical(input$dnc_input),

          property_contact_description = if (
            isTruthy(input$property_contact_description)
          ) {
            input$property_contact_description
          } else {
            NA_character_
          }
        )

        ## Find existing contacts with same name ----
        existing_contacts <- dbGetQuery(
          db_con,
          glue_sql(
            "
            SELECT
              id,
              name_first,
              name_last,
              email,
              phone_home,
              phone_cell
            FROM property_contact_details
            WHERE
              LOWER(TRIM(name_first)) = LOWER(TRIM({new_property_contact$name_first}))
              AND LOWER(TRIM(name_last)) = LOWER(TRIM({new_property_contact$name_last}))
            ORDER BY id
            ",
            .con = db_con
          )
        )

        ## Check for definite duplicate ----
        # Same first + last name AND at least one matching contact method.

        if (nrow(existing_contacts) > 0) {
          definite_duplicate <- vapply(
            seq_len(nrow(existing_contacts)),
            function(i) {
              existing <- existing_contacts[i, ]

              email_match <-
                isTruthy(new_property_contact$email) &&
                isTruthy(existing$email) &&
                str_to_lower(str_trim(new_property_contact$email)) ==
                  str_to_lower(str_trim(existing$email))

              home_match <-
                isTruthy(new_property_contact$phone_home) &&
                isTruthy(existing$phone_home) &&
                str_trim(new_property_contact$phone_home) ==
                  str_trim(existing$phone_home)

              cell_match <-
                isTruthy(new_property_contact$phone_cell) &&
                isTruthy(existing$phone_cell) &&
                str_trim(new_property_contact$phone_cell) ==
                  str_trim(existing$phone_cell)

              email_match ||
                home_match ||
                cell_match
            },
            logical(1)
          )

          ## Definite duplicate found ----
          if (any(definite_duplicate)) {
            duplicate_ids <- existing_contacts$id[
              definite_duplicate
            ]

            shinyalert(
              title = "Duplicate Contact",
              text = glue(
                "A contact with the same name and matching ",
                "contact information already exists ",
                "(ID: {paste(duplicate_ids, collapse = ', ')}). ",
                "The new contact was not added."
              ),
              type = "warning",
              closeOnClickOutside = FALSE,
              timer = 10000
            )

            return()
          }

          ## Same name but no matching contact information ----
          # This is a possible duplicate, so ask the user.
          existing_text <- paste(
            vapply(
              seq_len(nrow(existing_contacts)),
              function(i) {
                contact <- existing_contacts[i, ]

                methods <- c(
                  if (isTruthy(contact$email)) {
                    paste0(
                      "Email: ",
                      contact$email
                    )
                  },

                  if (isTruthy(contact$phone_home)) {
                    paste0(
                      "Home: ",
                      contact$phone_home
                    )
                  },

                  if (isTruthy(contact$phone_cell)) {
                    paste0(
                      "Cell: ",
                      contact$phone_cell
                    )
                  }
                )

                paste0(
                  "<strong>",
                  contact$name_first,
                  " ",
                  contact$name_last,
                  " (ID: ",
                  contact$id,
                  ")</strong><br>",

                  if (length(methods) > 0) {
                    paste(
                      methods,
                      collapse = "<br>"
                    )
                  } else {
                    "No contact information"
                  }
                )
              },
              character(1)
            ),
            collapse = "<br><br>"
          )

          ## Store pending contact ----
          pending_contact(
            new_property_contact
          )

          pending_property_ids(
            input$property_id
          )

          ## Show confirmation modal ----
          showModal(
            modalDialog(
              title = "Possible Duplicate Contact",

              HTML(
                paste0(
                  "<p>",
                  "A contact with the same first and last name ",
                  "already exists:",
                  "</p>",

                  existing_text,

                  "<br>",

                  "<p>",
                  "Do you want to add the current inputs as a new contact?",
                  "</p>"
                )
              ),

              footer = tagList(
                modalButton(
                  "Cancel"
                ),

                actionButton(
                  session$ns(
                    "confirm_add_property_contact"
                  ),
                  "Add Contact",
                  class = "btn-primary"
                )
              ),

              easyClose = FALSE
            )
          )

          return()
        }

        ## No existing contact with same name ----
        add_property_contact(
          new_property_contact = new_property_contact,
          property_ids = input$property_id
        )

        clear_add_contact_inputs()
      }
    )

    ## Event :: Confirm possible duplicate ----
    observeEvent(
      input$confirm_add_property_contact,
      {
        req(
          pending_contact(),
          pending_property_ids()
        )

        removeModal()

        add_property_contact(
          new_property_contact = pending_contact(),
          property_ids = pending_property_ids()
        )

        clear_add_contact_inputs()

        ## Clear pending values
        pending_contact(NULL)
        pending_property_ids(NULL)
      },
      ignoreInit = TRUE
    )

    ## Event :: Update property contact with new properties ----
    observeEvent(
      input$update_property_contact,
      {
        req(iv_update$is_valid())

        ## Existing property links ----
        existing_property_ids <- dbGetQuery(
          db_con,
          glue_sql(
            "
            SELECT property_id
            FROM properties_contact
            WHERE property_contact_id = {input$contact}
            ",
            .con = db_con
          )
        ) |>
          pull(property_id)

        ## Only add new links ----
        new_property_ids <- setdiff(
          input$property_id_update,
          existing_property_ids
        )

        n_skipped <- length(input$property_id_update) - length(new_property_ids)

        ## Insert new property links ----
        if (length(new_property_ids) > 0) {
          dbx::dbxInsert(
            db_con,
            table = "properties_contact",
            records = tibble(
              property_id = new_property_ids,
              property_contact_id = input$contact
            )
          )

          db_updated(
            db_updated() + 1
          )
        }

        ## Success message ----
        success_text <- glue(
          "Successfully linked ",
          "{length(new_property_ids)} ",
          "propert(y/ies) to the property contact."
        )

        if (n_skipped > 0) {
          success_text <- glue(
            "{success_text} Skipped ",
            "{n_skipped} propert(y/ies) already ",
            "linked to this contact."
          )
        }

        shinyalert(
          title = if (length(new_property_ids) > 0) {
            "Success"
          } else {
            "No Changes"
          },

          text = if (length(new_property_ids) > 0) {
            success_text
          } else {
            "All selected properties are already linked to this contact."
          },

          type = if (length(new_property_ids) > 0) {
            "success"
          } else {
            "info"
          },

          closeOnClickOutside = FALSE,
          timer = 10000
        )

        ## Clear inputs after successful update ----
        clear_update_contact_inputs()
      }
    )

    ## Event :: Clear Input Add Contact ----
    observeEvent(
      input$clear_inputs,
      {
        clear_add_contact_inputs()
      }
    )

    ## Event :: Clear inputs Update Contact ----
    observeEvent(
      input$clear_inputs_update,
      {
        clear_update_contact_inputs()
      }
    )
  })
}
