module_property_intake_ui <- function(id) {
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
          actionButton(inputId = ns("submit_property"), label = "Add Property"),
          actionButton(inputId = ns("submit_landowner"), label = "Add Landowner Contact"),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
        ),
        # Main layout
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(6, 6),
            # First card
            card(
              height = "100%",
              card_header(h5("Property Details")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  layout_columns(
                    col_widths = c(7, 5),
                    selectizeInput(
                      inputId = ns("pid_input"),
                      label = "Enter PID(s):",
                      choices = NULL, multiple = TRUE,
                      options = list(create = TRUE, placeholder = "Type PID and press Enter")
                    ),
                    dateInput(
                      inputId = ns("date_added_input"),
                      label = "Date Added",
                      value = today()
                    )
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    textInput(
                      inputId = ns("property_name_input"),
                      label = "Property Name",
                      value = "North Cove (Henry) - V4"
                    ),
                    textInput(
                      inputId = ns("focus_area_internal_input"),
                      label = "Focus Area (Internal)",
                      value = "Henry Watershed"
                    )
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    selectInput(
                      inputId = ns("phase_id_input"),
                      label = "Phase",
                      choices = NULL,
                      selected = character(0)
                    ),
                    selectInput(
                      inputId = ns("acquisition_type_input"),
                      label = "Acquisition Type",
                      choices = NULL,
                      selected = character(0)
                    )
                  ),
                  # TextArea on its own row with controlled spacing (span entire row)
                  div(
                    style = "width: 100%;", # Ensure it spans the full width
                    textAreaInput(
                      ns("property_description_input"),
                      "Property Description", "",
                      height = "100px", width = "100%"
                    )
                  ),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            # Second card
            card(
              height = "100%",
              card_header(h5("Landowner Details")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    inputId = ns("pid_input_landowner"),
                    label = "Select PID(s):",
                    choices = NULL, multiple = TRUE,
                    options = list(
                      create = FALSE,
                      plugins = list("remove_button"),
                      placeholder = "Select PIDs associated with owner"
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


  #### OLD ----
  # nav_panel(
  #   title = "Initialise PID",
  #   card(
  #     full_screen = TRUE,
  #     layout_sidebar(
  #       sidebar = sidebar(
  #         "Sidebar",
  #         open = TRUE,
  #         actionButton(inputId = ns("submit_property"), label = "Add Property"),
  #         actionButton(inputId = ns("submit_landowner"), label = "Add Landowner Contact"),
  #         actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
  #       ),
  #       layout_columns(
  #         card(
  #           h2("Property Details"),
  #           selectizeInput(
  #             inputId = ns("pid_input"),
  #             label = "Enter PID(s):",
  #             choices = NULL, multiple = TRUE,
  #             options = list(create = TRUE, placeholder = "Type PID and press Enter")
  #           ),
  #           # verbatimTextOutput(ns("id_output")),
  #           dateInput(
  #             inputId = ns("date_added_input"),
  #             label = "Date Added",
  #             value = today()
  #           ),
  #           textInput(
  #             inputId = ns("property_name_input"),
  #             label = "Property Name",
  #             value = "North Cove (Henry) - V4"
  #           ),
  #           selectInput(
  #             inputId = ns("phase_id_input"),
  #             label = "Phase",
  #             choices = NULL,
  #             selected = character(0)
  #           ),
  #           textInput(
  #             inputId = ns("focus_area_internal_input"),
  #             label = "Focus Area (Internal)",
  #             value = "Henry Watershed"
  #           ),
  #           selectInput(
  #             inputId = ns("acquisition_type_input"),
  #             label = "Acquisition Type",
  #             choices = NULL,
  #             selected = character(0)
  #           ),
  #           textInput(
  #             inputId = ns("property_description_input"),
  #             label = "Property Description",
  #             value = "Looks like a good one!"
  #           )
  #         ),
  #         card(
  #           h2("Landowner Contact Details"),
  #           selectizeInput(
  #             inputId = ns("pid_input_landowner"),
  #             label = "Select PID(s):",
  #             choices = NULL, multiple = TRUE,
  #             options = list(
  #               create = FALSE,
  #               plugins = list("remove_button"),
  #               placeholder = "Select PIDs associated with owner"
  #             )
  #           ),
  #           textInput(
  #             inputId = ns("name_first_input"),
  #             label = "First Name",
  #             value = ""
  #           ),
  #           textInput(
  #             inputId = ns("name_last_input"),
  #             label = "Last Name",
  #             value = ""
  #           ),
  #           textInput(
  #             inputId = ns("email_input"),
  #             label = "Email",
  #             value = ""
  #           ),
  #           textInput(
  #             inputId = ns("phone_home_input"),
  #             label = "Home Phone",
  #             value = ""
  #           ),
  #           textInput(
  #             inputId = ns("phone_cell_input"),
  #             label = "Cell Phone",
  #             value = ""
  #           ),
  #           selectInput(
  #             inputId = ns("dnc_input"),
  #             label = "Do Not Contact",
  #             choices = list("TRUE" = TRUE, "FALSE" = FALSE),
  #             selected = "FALSE"
  #           )
  #         )
  #       )
  #     )
  #   )
  # )
}

module_property_intake_server <- function(id, db_con, prd_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    valid_pids <- dbGetQuery(prd_con, "SELECT DISTINCT(pid) FROM parcels;") |>
      pull(pid)

    iv <- InputValidator$new()
    iv$add_rule("date_added_input", sv_required())
    iv$add_rule("property_name_input", sv_required())
    iv$add_rule("phase_id_input", sv_required())
    iv$add_rule("acquisition_type_input", sv_required())
    iv$add_rule("focus_area_internal_input", sv_required())
    iv$add_rule("email_input", sv_email())

    enable_pid_check <- FALSE

    if (enable_pid_check) {
      iv$add_rule("pid_input", function(pid_input) {
        # If there is no input, exit without error
        if (is.null(pid_input) || length(pid_input) == 0 || all(pid_input == "")) {
          return(NULL)
        }
        # If pid_input is of length 1 but contains commas or spaces, split it.
        if (length(pid_input) == 1 && grepl("[,\\s]", pid_input)) {
          codes <- unlist(strsplit(pid_input, "[,\\s]+"))
        } else {
          # Otherwise, assume that pid_input is already a vector of codes
          codes <- pid_input
        }
        # Remove any accidental white space around each code
        codes <- trimws(codes)
        # Validate each code: Each must be exactly 8 characters and match 8 digits only
        if (any(nchar(codes) != 8)) {
          return("Each PID must be exactly 8 digits.")
        }
        if (any(!grepl("^[0-9]{8}$", codes))) {
          return("Each PID must contain only digits (0-9).")
        }
        # Validate that each code is among the valid Property Record Database PIDs.
        invalid_codes <- codes[!codes %in% valid_pids]
        if (length(invalid_codes) > 0) {
          return(sprintf(
            "The following PID(s) are invalid (missing from PRD): %s",
            paste(invalid_codes, collapse = ", ")
          ))
        }
        # If all codes are valid, return NULL (indicating no error)
        NULL
      })
    }

    iv$enable()

    phase_choices <- reactive({
      dbReadTable(db_con, "phase") |>
        pull(phase_value)
    })

    acquisition_choices <- reactive({
      dbReadTable(db_con, "acquisition_type") |>
        pull(acquisition_value)
    })

    observe({
      updateSelectInput(session, "phase_id_input", choices = phase_choices(), selected = character(0))
      updateSelectInput(session, "acquisition_type_input", choices = acquisition_choices(), selected = character(0))
    })

    pid_choices <- reactive({
      dbReadTable(db_con, "parcels") |>
        pull(pid) |>
        sort()
    })

    observe({
      updateSelectizeInput(session, inputId = "pid_input_landowner", choices = pid_choices(), server = TRUE)
    })

    observeEvent(input$submit_property, {
      ## Get focus area FK ----
      focus_area_check <- dbReadTable(db_con, "focus_area_internal") |>
        filter(internal_value == input$focus_area_internal_input) |>
        pull(id)

      if (length(focus_area_check) == 0) {
        new_focus_area <- tibble(internal_value = input$focus_area_internal_input)
        append_db_data("focus_area_internal", new_focus_area, db_con)
      } else {
        print("FOCUS AREA ALEADY IN DATABASE")
      }

      focus_area_internal_id <- dbReadTable(db_con, "focus_area_internal") |>
        filter(internal_value == input$focus_area_internal_input) |>
        pull(id)

      ## Add property name if it doesn't exist ----
      property_check <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name_input) |>
        pull(property_name)

      if (length(property_check) == 0) {
        new_property <- tibble(
          property_name = input$property_name_input,
          focus_area_internal_id,
          property_description = input$property_description_input
        )
        append_db_data("properties", new_property, db_con)
      } else {
        print("PROPERTY ALEADY IN DATABASE")
      }

      ## Get lookup table IDs ----
      property_id <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name_input) |>
        pull(id)

      phase_id <- dbReadTable(db_con, "phase") |>
        filter(phase_value == input$phase_id_input) |>
        pull(id)

      acquisition_type_id <- dbReadTable(db_con, "acquisition_type") |>
        filter(acquisition_value == input$acquisition_type_input) |>
        pull(id)

      ## Write new parcel(s) ----
      new_parcel <- tibble(
        pid = input$pid_input, date_added = input$date_added_input,
        phase_id, property_id, acquisition_type_id
      )

      print(new_parcel)

      append_db_data("parcels", new_parcel, db_con)
      # Signal that data has changed
      db_updated(db_updated() + 1)

      ## Populate NSPRD data (addresses, landowner info, etc) ----
      populate_nsprd_tables(input$pid_input, prd_con, db_con)

      ## Update PID list from DB ----
      pid_choices <- dbReadTable(db_con, "parcels") |>
        pull(pid) |>
        sort()

      updateSelectizeInput(session, inputId = "pid_input_landowner", choices = pid_choices, server = TRUE)

      shinyalert(
        title = "Success!",
        text = "Property added to database!",
        type = "success"
      )
    })

    observeEvent(input$submit_landowner, {
      new_landowner <- tibble(
        name_last = input$name_last_input,
        name_first = input$name_first_input,
        email = input$email_input,
        phone_home = input$phone_home_input,
        phone_cell = input$phone_cell_input,
        dnc = as.logical(input$dnc_input)
      )

      print(glimpse(new_landowner))

      append_db_data("landowner_details", new_landowner, db_con)

      # Signal that data has changed
      db_updated(db_updated() + 1)

      landowner_contact_id <- new_landowner |>
        left_join(dbReadTable(db_con, "landowner_details")) |>
        pull(id)

      if (length(input$pid_input_landowner) > 0) {
        dbx::dbxUpdate(
          db_con,
          table = "parcels",
          records = tibble(pid = input$pid_input_landowner, landowner_contact_id),
          where_cols = c("pid")
        )
      } else {
        print("NO PID ASSOCIATED WITH LANDOWNER CONTACT")
      }

      shinyalert(
        title = "Success!",
        text = "Landowner added to database!",
        type = "success"
      )
    })

    observeEvent(input$clear_inputs, {
      updateSelectizeInput(session, "pid_input", selected = character(0))
      updateDateInput(session, "date_added_input", value = Sys.Date())
      updateTextInput(session, "property_name_input", value = "")
      updateSelectInput(session, "phase_id_input", selected = character(0))
      updateTextInput(session, "focus_area_internal_input", value = "")
      updateSelectInput(session, "acquisition_type_input", selected = character(0))
      updateTextInput(session, "property_description_input", value = "")

      updateTextInput(session, "name_last_input", value = "")
      updateTextInput(session, "name_first_input", value = "")
      updateTextInput(session, "email_input", value = "")
      updateTextInput(session, "phone_home_input", value = "")
      updateTextInput(session, "phone_cell_input", value = "")
      updateSelectizeInput(session, "pid_input_landowner", selected = character(0))
    })

    output$id_output <- renderPrint({
      input$pid_input
    })
  })
}
