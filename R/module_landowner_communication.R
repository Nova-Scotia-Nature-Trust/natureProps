# UI ----
module_landowner_communication_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%", # Make card fill available space
      layout_sidebar(
        sidebar = sidebar(
          "Sidebar",
          open = TRUE,
          actionButton(inputId = ns("submit_communication"), label = "Submit Communication"),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
        ),
        # Main layout
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(8, 4),
            ## Card :: Landowner communication ----
            card(
              height = "100%",
              card_header(h5("Outreach & Landowner Communication")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  layout_columns(
                    col_widths = c(6, 6),
                    selectInput(
                      ns("communication_type"),
                      "Select Type",
                      choices = c("", "Landowner Communication", "Outreach"),
                      selected = ""
                    ),
                    uiOutput(ns("conditional_contact_ui"))
                  ),
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
                      "Description", "",
                      height = "200px", width = "100%"
                    )
                  ),
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            ## Card :: [other things] ----
            card(
              height = "100%",
              card_header(h5("Other things")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
module_landowner_communication_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("communication_method_id", sv_required())
    iv$add_rule("communication_purpose_id", sv_required())
    iv$add_rule("communication_description", sv_required())
    iv$add_rule("date_contacted", sv_required())
    iv$enable()

    ## Conditionally add validation rules based on communication type
    observe({
      if (input$communication_type == "Landowner Communication") {
        iv$add_rule("landowner_contact_id", sv_required())
      } else {
        iv$add_rule("pid_input", sv_required())
      }
    })

    ## Populate UI inputs ----

    # Define a reactive for landowners that depends on db_updated, if provided
    landowner_ids <- reactive({
      # Only try to use db_updated if it is not NULL.
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }
      # Query database for landowners
      dbGetQuery(db_con, "SELECT id, name_last, name_first FROM landowner_details;") |>
        mutate(name = str_glue("{name_first} {name_last} (ID:{id})")) |>
        arrange(name_last) %>%
        {
          setNames(.$id, .$name)
        }
    })

    # Define choices for PID input
    pid_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }

      dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
        pull() |>
        sort()
    })

    method_choices <- reactive({
      dbReadTable(db_con, "communication_method") %>%
        {
          setNames(.$id, .$method_value)
        } # Magic
    })

    purpose_choices <- reactive({
      dbReadTable(db_con, "communication_purpose") %>%
        {
          setNames(.$id, .$purpose_value)
        } # Magic
    })

    ## Conditional UI ----
    output$conditional_contact_ui <- renderUI({
      ns <- session$ns

      req(input$communication_type)

      if (input$communication_type == "Landowner Communication") {
        selectizeInput(
          ns("landowner_contact_id"),
          "Select Landowner ID",
          choices = NULL,
          multiple = FALSE,
          options = list(
            create = FALSE,
            placeholder = "Select a landowner"
          )
        )
      } else if (input$communication_type == "Outreach") {
        selectizeInput(
          inputId = ns("pid_input"),
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

    # Then in an observe block, update the input after it's created
    observe({
      req(input$communication_type)

      if (input$communication_type == "Landowner Communication") {
        # Only runs when landowner option is selected and the input exists
        updateSelectizeInput(
          session,
          inputId = "landowner_contact_id",
          choices = landowner_ids(),
          selected = character(0),
          server = TRUE
        )
      } else {
        # Only runs when outreach option is selected and the input exists
        updateSelectizeInput(
          session,
          inputId = "pid_input",
          choices = pid_choices(),
          selected = character(0),
          server = TRUE
        )
      }
    })

    # Initialize the select inputs with data from the DB
    observe({
      # Update the purpose and method inputs
      updateSelectizeInput(session, "communication_purpose_id",
        choices = purpose_choices(), server = TRUE, selected = character(0)
      )

      updateSelectizeInput(session, "communication_method_id",
        choices = method_choices(), server = TRUE, selected = character(0)
      )
    })

    ## Event :: Submit communication ----

    # Update database when submit button is clicked
    observeEvent(input$submit_communication, {
      req(input$communication_method_id)
      req(input$communication_purpose_id)
      req(input$date_contacted)
      req(input$communication_description)

      if (input$communication_type == "Landowner Communication") {
        req(input$landowner_contact_id)

        # Create the new communication record
        new_communication <- tibble(
          landowner_contact_id = input$landowner_contact_id,
          communication_purpose_id = input$communication_purpose_id,
          communication_method_id = input$communication_method_id,
          date_contacted = input$date_contacted,
          communication_description = input$communication_description,
          date_follow_up = if (isTruthy(input$date_follow_up)) input$date_follow_up else as.Date(NA)
        )

        print(glimpse(new_communication))
        append_db_data("landowner_communication", new_communication, db_con, silent = FALSE)
      } else if (input$communication_type == "Outreach") {
        req(input$pid_input)

        parcel_id <- dbGetQuery(
          db_con,
          glue_sql("SELECT id AS parcel_id FROM parcels
                   WHERE pid IN ({input$pid_input*});",
            .con = db_con
          )
        ) |> pull(parcel_id)

        # Create the new outreach record(s)
        new_outreach <- tibble(
          parcel_id = parcel_id,
          dnc = FALSE,
          communication_purpose_id = input$communication_purpose_id,
          communication_method_id = input$communication_method_id,
          date_contacted = input$date_contacted,
          outreach_description = input$communication_description,
          date_follow_up = if (isTruthy(input$date_follow_up)) input$date_follow_up else as.Date(NA)
        )

        print(glimpse(new_outreach))
        append_db_data("outreach", new_outreach, db_con, silent = FALSE)
      }

      ## Signal that data has changed
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      
      updateSelectInput(session, "communication_type", selected = "")

      updateSelectizeInput(session, "communication_purpose_id",
        choices = purpose_choices(), server = TRUE, selected = character(0)
      )
      updateSelectizeInput(session, "communication_method_id",
        choices = method_choices(), server = TRUE, selected = character(0)
      )
      updateDateInput(session, "date_contacted", value = Sys.Date())
      updateDateInput(session, "date_follow_up", value = as.Date(NA))
      updateTextAreaInput(session, "communication_description", value = "")
    })
  })
}
