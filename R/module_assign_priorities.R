# UI ----
module_assign_priorities_ui <- function(id) {
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
            ns("property"),
            "Select property",
            choices = NULL,
            multiple = FALSE,
            width = "80%"
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Record",
            class = "btn-success"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          ),
        ),
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(6, 6),
            ## Property Card ----
            card(
              height = "100%",
              card_header(
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  h5("Property"),
                  popover(
                    div(
                      icon("question-circle"),
                      style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                    ),
                    "Some useful information",
                    title = "Property Help",
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
                      inputId = ns("securement_prob"),
                      label = "Securement Probability",
                      choices = NULL,
                      multiple = FALSE
                    ),
                    textInput(
                      inputId = ns("closing_year"),
                      label = "Anticipated Closing Year",
                      value = "",
                      placeholder = "e.g., 2025/26"
                    )
                  ),
                  actionButton(
                    inputId = ns("submit_edit_properties"),
                    label = "Submit Changes",
                    class = "btn-primary"
                  ),
                  layout_columns(),
                  div(),
                  div(),
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            ## Parcel Card ----
            card(
              height = "100%",
              card_header(
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  h5("Parcels"),
                  popover(
                    div(
                      icon("question-circle"),
                      style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                    ),
                    "Useful information",
                    title = "Parcel Help",
                    placement = "right"
                  )
                )
              ),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    inputId = ns("pid"),
                    label = "PID",
                    choices = NULL,
                    multiple = FALSE
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    selectizeInput(
                      inputId = ns("ecological_priority"),
                      label = "Ecological Priority",
                      choices = NULL,
                      multiple = FALSE
                    ),
                    selectizeInput(
                      inputId = ns("securement_priority"),
                      label = "Securement Priority",
                      choices = NULL,
                      multiple = FALSE
                    )
                  ),
                  tableOutput(ns("parcels_table")),
                  actionButton(
                    inputId = ns("submit_edit_parcels"),
                    label = "Submit Changes",
                    class = "btn-primary"
                  ),
                  div(),
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
module_assign_priorities_server <- function(id, db_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("closing_year", function(value) {
      if (is.null(value) || value == "") {
        return() # Allow empty values
      }

      # Check format: YYYY/YY
      if (!str_detect(value, "^[0-9]{4}/[0-9]{2}$")) {
        return("Must be in format YYYY/YY (e.g., 2025/26)")
      }

      # Extract year components
      start_year <- as.integer(str_sub(value, 1, 4))
      end_year <- as.integer(str_sub(value, 6, 7))

      # Check if end year is consecutive (start_year + 1) % 100
      if ((start_year + 1) %% 100 != end_year) {
        return("Years must be consecutive (e.g., 2025/26, not 2025/27)")
      }
    })

    iv$enable()

    ## Helper functions ----
    get_property_choices <- function(db_con) {
      dbGetQuery(
        db_con,
        "SELECT property_name FROM properties ORDER BY property_name"
      ) |>
        pull(property_name)
    }

    get_securement_prob_choices <- function(db_con) {
      dbGetQuery(db_con, "SELECT * FROM securement_probability") |>
        select(probability_value, id) |>
        deframe()
    }

    get_ranking_choices <- function(db_con) {
      dbGetQuery(db_con, "SELECT * FROM ranking") |>
        select(ranking_value, id) |>
        deframe()
    }

    load_property_record <- function(db_con, prop_name) {
      query <- glue_sql(
        "SELECT 
          p.id, 
          pa.pid,
          p.property_name, 
          p.securement_probability_id,
          p.anticipated_closing_year,
          pa.priority_ecological_ranking_id,
          pa.priority_securement_ranking_id
        FROM properties p
        LEFT JOIN parcels pa ON p.id = pa.property_id
        WHERE p.property_name = {prop_name}",
        .con = db_con
      )
      dbGetQuery(db_con, query)
    }

    ## Reactives :: Input choices ----
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_property_choices(db_con)
    })

    securement_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_securement_prob_choices(db_con)
    })

    ranking_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_ranking_choices(db_con)
    })

    ## Observer :: Update inputs ----
    observe({
      # Only update when db_updated changes
      if (!is.null(db_updated)) {
        db_updated()
      }

      # Preserve current selections
      current_selections <- list(
        property = isolate(input$property),
        securement_prob = isolate(input$securement_prob),
        ecological_priority = isolate(input$ecological_priority),
        securement_priority = isolate(input$securement_priority),
        closing_year = isolate(input$closing_year)
      )

      # Update inputs
      updateSelectizeInput(
        session,
        "property",
        choices = c("", property_choices()),
        selected = current_selections$property,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "securement_prob",
        choices = c("", securement_choices()),
        selected = current_selections$securement_prob
      )

      updateTextInput(
        session,
        "closing_year",
        value = current_selections$closing_year
      )

      updateSelectizeInput(
        session,
        "ecological_priority",
        choices = c("", ranking_choices()),
        selected = current_selections$ecological_priority
      )

      updateSelectizeInput(
        session,
        "securement_priority",
        choices = c("", ranking_choices()),
        selected = current_selections$securement_priority
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event:: Load Record  ----
    observeEvent(input$load_record, {
      req(input$property)
      record <- load_property_record(db_con, input$property) |>
        arrange(pid)

      if (nrow(record) >= 1) {
        selected_record(record)

        updateSelectizeInput(
          session,
          inputId = "securement_prob",
          selected = unique(record$securement_probability_id)
        )

        updateTextInput(
          session,
          inputId = "closing_year",
          value = unique(record$anticipated_closing_year)
        )

        updateSelectizeInput(
          session,
          inputId = "pid",
          choices = c("", record$pid),
          selected = record$pid[1]
        )
      } else {
        selected_record(NULL)
      }

      print(record)
      print(paste("Selected value:", record$securement_probability_id))
      print("Available choices:")
      print(securement_choices())
    })

    ## Event:: PID selected ----
    observeEvent(input$pid, {
      req(input$pid, selected_record())

      # Find the ecological priority for the selected PID
      record <- selected_record()
      selected_parcel <- record |>
        filter(pid == input$pid)

      if (nrow(selected_parcel) == 1) {
        updateSelectizeInput(
          session,
          inputId = "ecological_priority",
          selected = selected_parcel$priority_ecological_ranking_id
        )

        updateSelectizeInput(
          session,
          inputId = "securement_priority",
          selected = selected_parcel$priority_securement_ranking_id
        )
      }
    })

    ## Output :: Parcels table ----
    output$parcels_table <- renderTable(
      {
        if (!is.null(db_updated)) {
          db_updated()
        }
        req(selected_record())

        # Convert ranking_choices() to a tibble for joining
        ranking_lookup <- tibble(
          id = as.integer(ranking_choices()),
          ranking_label = names(ranking_choices())
        )

        selected_record() |>
          select(
            pid,
            priority_ecological_ranking_id,
            priority_securement_ranking_id
          ) |>
          left_join(
            ranking_lookup,
            by = c("priority_ecological_ranking_id" = "id")
          ) |>
          rename(ecological_label = ranking_label) |>
          left_join(
            ranking_lookup,
            by = c("priority_securement_ranking_id" = "id")
          ) |>
          rename(securement_label = ranking_label) |>
          select(pid, ecological_label, securement_label) |>
          rename(
            PID = pid,
            `Ecological Priority` = ecological_label,
            `Securement Priority` = securement_label
          ) |>
          arrange(PID)
      },
      colnames = TRUE,
      spacing = "s"
    )

    ## Event :: Write changes (property) ----
    observeEvent(input$submit_edit_properties, {
      req(input$property, input$securement_prob)

      # Check validation before proceeding
      if (!iv$is_valid()) {
        return()
      }

      df <- tibble(
        property_name = input$property,
        securement_probability_id = input$securement_prob,
        anticipated_closing_year = if_else(
          input$closing_year == "" | is.null(input$closing_year),
          NA_character_,
          input$closing_year
        )
      )

      print(df)

      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = df,
        where_cols = "property_name"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh the selected record with updated data
      record <- load_property_record(db_con, input$property)
      selected_record(record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "Table record {input$property} has been successfully updated in Properties table"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })
    ## Event :: Write changes (parcels) ----
    observeEvent(input$submit_edit_parcels, {
      req(input$pid, input$ecological_priority)

      df <- tibble(
        pid = input$pid,
        priority_ecological_ranking_id = input$ecological_priority,
        priority_securement_ranking_id = input$securement_priority,
      )

      print(df)

      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = df,
        where_cols = "pid"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh the selected record with updated data
      record <- load_property_record(db_con, input$property)
      selected_record(record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "Table record {input$pid} has been successfully updated in Parcels table"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = property_choices(),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_prob",
        choices = c("", securement_choices()),
        selected = character(0),
        server = TRUE
      )

      updateTextInput(
        session,
        inputId = "closing_year",
        value = "",
        placeholder = "e.g., 2025/26"
      )

      updateSelectizeInput(
        session,
        inputId = "ecological_priority",
        choices = c("", ranking_choices()),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_priority",
        choices = c("", ranking_choices()),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "pid",
        choices = "",
        selected = character(0),
        server = TRUE
      )
    })

    ## Clear selected record and input UI elements when table changes
    observeEvent(input$clear_inputs, {
      selected_record(NULL)
    })
  })
}
