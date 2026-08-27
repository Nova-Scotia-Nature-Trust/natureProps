# UI ----
module_property_contact_communication_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    ## Card :: Outreach & Contact Communication ----
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(h5("Outreach & Property Contact Communication")),
      card_body(
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          selectInput(
            ns("communication_type"),
            "Select Type",
            choices = c(
              "",
              "Property Contact Communication",
              "Outreach"
            ),
            selected = character(0)
          ),
          uiOutput(ns("conditional_contact_ui")),
          layout_columns(
            col_widths = c(6, 6),
            selectizeInput(
              ns("communication_purpose_id"),
              "Communication Purpose",
              choices = NULL
            ),
            selectizeInput(
              ns("communication_method_id"),
              "Communication Method",
              choices = NULL
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            dateInput(
              inputId = ns("date_contacted"),
              label = "Date Contacted"
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
              class = "btn-success"
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
module_property_contact_communication_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("communication_method_id", sv_required())
    iv$add_rule("communication_purpose_id", sv_required())
    iv$add_rule("communication_description", sv_required())
    iv$add_rule("date_contacted", sv_required())
    iv$enable()

    iv_pid <- InputValidator$new()
    iv_pid$add_rule("pid", sv_required())
    iv_pid$enable()

    iv_contact <- InputValidator$new()
    iv_contact$add_rule("contact", sv_required())
    iv_contact$enable()

    iv_contact_property <- InputValidator$new()
    iv_contact_property$add_rule("contact_property_id", sv_required())
    iv_contact_property$enable()

    updateDateInput(session, "date_contacted", value = Sys.Date())

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

    ## Reactive :: PID List ----
    pid_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, pid FROM parcels 
         ORDER BY pid;"
      )
    })

    ## Lookup tables ----
    method <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_method ORDER BY method_value"
    )

    purpose <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_purpose ORDER BY purpose_value"
    )

    ## Conditional UI :: Property + Contact (or PID) Select ----
    output$conditional_contact_ui <- renderUI({
      ns <- session$ns

      req(input$communication_type)

      if (input$communication_type == "Property Contact Communication") {
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
        )
      } else if (input$communication_type == "Outreach") {
        selectizeInput(
          inputId = ns("pid"),
          label = "Select PID(s):",
          choices = NULL,
          multiple = TRUE,
          options = list(
            create = FALSE,
            placeholder = "Select PIDs for outreach"
          )
        )
      }
    })

    ## Observe :: Inputs For Conditional UI ----
    observe({
      req(input$communication_type)

      if (input$communication_type == "Property Contact Communication") {
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
          selected = character(0),
          server = TRUE
        )
      } else {
        updateSelectizeInput(
          session,
          inputId = "pid",
          choices = setNames(
            pid_list()$id,
            pid_list()$pid
          ),
          selected = character(0),
          server = TRUE
        )
      }
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
        selected = character(0),
        server = TRUE
      )
    })

    ## Update Lookup Inputs ----
    updateSelectizeInput(
      session,
      "communication_purpose_id",
      choices = setNames(
        purpose$id,
        purpose$purpose_value
      ),
      server = TRUE,
      selected = character(0)
    )

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

    ## Event :: Submit communication ----
    observeEvent(input$submit_communication, {
      req(iv$is_valid())

      if (input$communication_type == "Property Contact Communication") {
        req(input$contact_property_id, iv_contact_property$is_valid())
        req(input$contact, iv_contact$is_valid())
        # Create the new communication record
        new_communication <- tibble(
          property_contact_id = input$contact,
          property_id = input$contact_property_id,
          communication_purpose_id = input$communication_purpose_id,
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
      } else if (input$communication_type == "Outreach") {
        req(input$pid, iv_pid$is_valid())
        # Create the new outreach record(s)
        new_outreach <- tibble(
          parcel_id = input$pid,
          dnc = FALSE,
          communication_purpose_id = input$communication_purpose_id,
          communication_method_id = input$communication_method_id,
          date_contacted = input$date_contacted,
          outreach_description = input$communication_description,
          date_follow_up = if (isTruthy(input$date_follow_up)) {
            input$date_follow_up
          } else {
            as.Date(NA)
          }
        )
        append_db_data("outreach", new_outreach, db_con, silent = FALSE)
      }

      ## Signal that data has changed
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      clear_inputs()
    })

    ## Helper :: Reset all inputs ----
    clear_inputs <- function() {
      updateSelectInput(session, "communication_type", selected = character(0))
      updateSelectizeInput(
        session,
        "contact_property_id",
        selected = character(0)
      )
      updateSelectizeInput(session, "contact", selected = character(0))
      updateSelectizeInput(session, "pid", selected = character(0))

      updateSelectizeInput(
        session,
        "communication_purpose_id",
        choices = setNames(
          purpose$id,
          purpose$purpose_value
        ),
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
    }

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      clear_inputs()
    })
  })
}
