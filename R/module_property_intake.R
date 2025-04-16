# UI ----
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
            ## Card :: Property ----
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
                      options = list(
                        create = TRUE,
                        placeholder = "Type PID and press Enter"
                      )
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
                      value = "North Cove (Hendrix) - VX"
                    ),
                    selectizeInput(
                      inputId = ns("focus_area_internal_input"),
                      label = "Focus Area (Internal)",
                      choices = NULL, multiple = FALSE,
                      options = list(
                        create = TRUE,
                        placeholder = "Select or add new focal area"
                      )
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
            ## Card :: Landowner details ----
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
                  div(
                    style = "width: 100%;",
                    textAreaInput(
                      ns("TBD"),
                      "Notes", "",
                      height = "100px", width = "100%"
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
}

# Server ----
module_property_intake_server <- function(id, db_con, prd_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    valid_pids <- dbGetQuery(prd_con, "SELECT DISTINCT(pid) FROM parcels;") |>
      pull(pid)

    iv <- InputValidator$new()
    iv$add_rule("date_added_input", sv_required())
    iv$add_rule("property_name_input", sv_required())
    iv$add_rule("phase_id_input", sv_required())
    iv$add_rule("acquisition_type_input", sv_required())
    iv$add_rule("focus_area_internal_input", sv_required())
    iv$add_rule("email_input", ~ if (. != "") sv_email()(.))
    iv$add_rule("name_first_input", sv_required())
    iv$add_rule("name_last_input", sv_required())
    iv$add_rule("pid_input", ~ validate_pid_input(., valid_pids, enable_check = TRUE))
    iv$enable()

    ## Populate UI inputs ----
    phase_choices <- reactive({
      dbReadTable(db_con, "phase")  %>%
        {setNames(.$id, .$phase_value)} # Magic
    })

    acquisition_choices <- reactive({
      dbReadTable(db_con, "acquisition_type") %>%
        {setNames(.$id, .$acquisition_value)} # Magic
    })

    focus_area_choices <- reactive({
      dbReadTable(db_con, "focus_area_internal")  |> 
        arrange(internal_value) %>%
        {setNames(.$id, .$internal_value)} # Magic
    })

    pid_choices <- reactive({
      dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
        pull() |>
        sort()
    })

    observe({
      updateSelectizeInput(session, "focus_area_internal_input",
        choices = focus_area_choices(),
        selected = character(0), server = TRUE
      )
      updateSelectInput(session, "phase_id_input",
        choices = phase_choices(), selected = character(0)
      )
      updateSelectInput(session, "acquisition_type_input",
        choices = acquisition_choices(), selected = character(0)
      )
      updateSelectizeInput(session,
        inputId = "pid_input_landowner",
        choices = pid_choices(), server = TRUE
      )
    })

    ## Event :: Submit property ----
    observeEvent(input$submit_property, {
      req(input$pid_input)
      req(input$date_added_input)
      req(input$focus_area_internal_input)
      req(input$property_name_input)
      req(input$phase_id_input)
      req(input$acquisition_type_input)

      print(input$pid_input)
      print(input$date_added_input)
      print(input$focus_area_internal_input)
      print(input$property_name_input)
      print(input$phase_id_input)
      print(input$acquisition_type_input)
      
      ### Focus area (internal) ----
      focus_area_check <- dbReadTable(db_con, "focus_area_internal") |>
        filter(internal_value == input$focus_area_internal_input) |>
        pull(id)

      if (length(focus_area_check) == 0) {
        new_focus_area <- tibble(internal_value = input$focus_area_internal_input)
        append_db_data("focus_area_internal", new_focus_area, db_con, silent = TRUE)
        print("FOCUS AREA ADDED TO DATABASE")
      } else {
        print("FOCUS AREA ALEADY IN DATABASE")
      }

      focus_area_internal_id <- dbReadTable(db_con, "focus_area_internal") |>
        filter(internal_value == input$focus_area_internal_input) |>
        pull(id)

      ### Property name & ID -----
      property_check <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name_input) |>
        pull(property_name)

      if (length(property_check) == 0) {
        new_property <- tibble(
          property_name = input$property_name_input,
          focus_area_internal_id,
          property_description = input$property_description_input
        )
        append_db_data("properties", new_property, db_con, silent = TRUE)
        print("NEW PROPERTY ADDED TO DATABASE")
      } else {
        print("PROPERTY ALEADY IN DATABASE")
      }

      property_id <- dbReadTable(db_con, "properties") |>
        filter(property_name == input$property_name_input) |>
        pull(id)
      
      ### Write new parcel(s) ----
      new_parcel <- tibble(
        pid = input$pid_input,
        date_added = input$date_added_input,
        phase_id = input$phase_id_input,
        property_id,
        acquisition_type_id = input$acquisition_type_input
      )
      
      print(new_parcel)

      tryCatch(
        {
          ## Write to database
          append_db_data("parcels", new_parcel, db_con, silent = FALSE)
          ## Signal that data has changed
          db_updated(db_updated() + 1)
          ## Populate NSPRD data (addresses, landowner info, etc)
          populate_nsprd_tables(input$pid_input, prd_con, db_con)
          ## Update PID list for landowner from DB
          pid_choices <- dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
            pull() |>
            sort()

          updateSelectizeInput(
            session,
            inputId = "pid_input_landowner",
            choices = pid_choices, server = TRUE
          )
        },
        error = function(e) {
          print(paste("Database error:", e$message))
        }
      )
    })

    ## Event :: Submit landowner contact details ----
    observeEvent(input$submit_landowner, {
      req(input$name_first_input)
      req(input$name_last_input)

      new_landowner <- tibble(
        name_last = input$name_last_input,
        name_first = input$name_first_input,
        email = input$email_input,
        phone_home = input$phone_home_input,
        phone_cell = input$phone_cell_input,
        dnc = as.logical(input$dnc_input)
      )

      print(glimpse(new_landowner))

      append_db_data("landowner_details", new_landowner, db_con, silent = FALSE)
      db_updated(db_updated() + 1)

      ## Get new landowner ID
      landowner_contact_id <- new_landowner |>
        left_join(dbReadTable(db_con, "landowner_details")) |>
        pull(id)

      ## Assign the landowner ID to relevant PIDs
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
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(session, "pid_input", selected = character(0))
      updateDateInput(session, "date_added_input", value = Sys.Date())
      updateTextInput(session, "property_name_input", value = "")
      updateSelectInput(session, "phase_id_input", selected = character(0))
      updateTextInput(session, "focus_area_internal_input", value = "")
      updateSelectInput(session, "acquisition_type_input", selected = character(0))
      updateTextInput(session, "property_description_input", value = "")
      updateSelectizeInput(session, "pid_input_landowner", selected = character(0), server = TRUR)
      updateTextInput(session, "name_last_input", value = "")
      updateTextInput(session, "name_first_input", value = "")
      updateTextInput(session, "email_input", value = "")
      updateTextInput(session, "phone_home_input", value = "")
      updateTextInput(session, "phone_cell_input", value = "")
    })
  })
}
