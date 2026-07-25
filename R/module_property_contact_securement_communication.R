# UI ----
module_property_contact_securement_communication_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    ## Card :: Property Contact Securement Communication ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(h5("Property Contact Securement Communication")),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          layout_columns(
            col_widths = c(6, 6),
            selectizeInput(
              ns("contact_property_id"),
              "Select Property",
              choices = NULL,
              multiple = FALSE,
              options = list(
                create = FALSE,
                placeholder = "Select a property"
              )
            ),
            selectizeInput(
              ns("contact"),
              "Select Property Contact",
              choices = NULL,
              multiple = FALSE,
              options = list(
                create = FALSE,
                placeholder = "Select a property first"
              )
            )
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            selectizeInput(
              ns("communication_method_id"),
              "Communication Method",
              choices = NULL
            ),
            dateInput(
              inputId = ns("date_contacted"),
              label = "Date Contacted",
              value = today()
            ),
            dateInput(
              inputId = ns("date_follow_up"),
              label = "Date Follow Up",
              value = as.Date(NA)
            )
          ),
          div(
            style = "width: 100%;",
            textAreaInput(
              ns("communication_description"),
              "Description",
              "",
              height = "200px",
              width = "100%"
            )
          ),
          div(
            style = "margin-top: 20px;",
            actionButton(
              inputId = ns("submit_communication"),
              label = "Submit Communication",
              class = "btn-primary"
            ),
            actionButton(
              inputId = ns("clear_inputs"),
              label = "Clear Inputs",
              class = "btn-secondary"
            )
          ),
          # Add a spacer div to prevent pushing everything to bottom
          div(style = "flex-grow: 1;")
        )
      )
    )
  )
}

# Server ----
module_property_contact_securement_communication_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("contact_property_id", sv_required())
    iv$add_rule("contact", sv_required())
    iv$add_rule("communication_method_id", sv_required())
    iv$add_rule("communication_description", sv_required())
    iv$add_rule("date_contacted", sv_required())
    iv$enable()

    ## Reactive :: Properties List (only properties with linked contacts) ----
    properties_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "
        SELECT p.id, p.property_name
        FROM properties p
        WHERE EXISTS (
          SELECT 1 FROM properties_contact pc WHERE pc.property_id = p.id
        )
        ORDER BY p.property_name;
        "
      )
    })

    ## Reactive :: Property Contacts (scoped to selected property) ----
    contacts <- reactive({
      db_updated()
      req(input$contact_property_id)
      dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT pcd.*
          FROM property_contact_details pcd
          INNER JOIN properties_contact pc ON pc.property_contact_id = pcd.id
          WHERE pc.property_id = {input$contact_property_id}
          ",
          .con = db_con
        )
      ) |>
        mutate(
          display_label = glue("{name_first} {name_last} (ID:{id})")
        ) |>
        arrange(name_last, name_first)
    })

    ## Lookup tables ----
    method <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_method ORDER BY method_value"
    )

    securement_purpose_id <- dbGetQuery(
      db_con,
      "SELECT id FROM communication_purpose WHERE purpose_value = 'Securement'"
    ) |>
      pull(id)

    ## Update Lookup Inputs ----
    updateSelectizeInput(
      session,
      "communication_method_id",
      choices = setNames(
        method$id,
        method$method_value
      ),
      server = TRUE,
      selected = character(0)
    )

    ## Observe :: Property Input ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "contact_property_id",
        choices = c(
          "",
          setNames(
            properties_list()$id,
            properties_list()$property_name
          )
        ),
        selected = isolate(input$contact_property_id),
        server = TRUE
      )
    })

    ## Observe :: Populate Property Contacts For Selected Property ----
    observeEvent(input$contact_property_id, {
      req(input$contact_property_id)
      updateSelectizeInput(
        session,
        inputId = "contact",
        choices = c(
          "",
          setNames(
            contacts()$id,
            contacts()$display_label
          )
        ),
        selected = isolate(input$contact),
        server = TRUE
      )
    })

    ## Event :: Submit communication ----
    observeEvent(input$submit_communication, {
      req(iv$is_valid())

      # Create the new communication record
      new_communication <- tibble(
        property_contact_id = input$contact,
        property_id = input$contact_property_id,
        communication_purpose_id = securement_purpose_id,
        communication_method_id = input$communication_method_id,
        date_contacted = input$date_contacted,
        communication_description = input$communication_description,
        date_follow_up = if (isTruthy(input$date_follow_up)) {
          input$date_follow_up
        } else {
          as.Date(NA)
        }
      )

      append_db_data(
        "property_contact_communication",
        new_communication,
        db_con,
        silent = FALSE
      )

      ## Signal that data has changed
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        "contact_property_id",
        choices = c(
          "",
          setNames(
            properties_list()$id,
            properties_list()$property_name
          )
        ),
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "contact",
        choices = character(0),
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "communication_method_id",
        choices = setNames(
          method$id,
          method$method_value
        ),
        selected = character(0)
      )
      updateDateInput(session, "date_contacted", value = Sys.Date())
      updateDateInput(session, "date_follow_up", value = as.Date(NA))
      updateTextAreaInput(
        session,
        "communication_description",
        value = character(0)
      )
    })
  })
}
