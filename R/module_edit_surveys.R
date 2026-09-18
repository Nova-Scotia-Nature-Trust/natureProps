# UI ----
# NAV PANEL :: SURVEYS
module_edit_surveys_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        ## Sidebar ----
        sidebar = sidebar(
          "",
          open = TRUE,
          accordion(
            id = ns("sidebar_accordion"),
            open = FALSE,
            multiple = FALSE,
            accordion_panel(
              title = "Add New Survey",
              value = "add_values",
              selectizeInput(
                inputId = ns("property_new"),
                label = "Select Property",
                choices = NULL,
                selected = NULL
              ),
              actionButton(
                inputId = ns("add_record"),
                label = "Submit Survey",
                class = "btn-success"
              )
            ),
            accordion_panel(
              title = "Edit Existing Survey",
              value = "edit_values",
              selectizeInput(
                inputId = ns("property_exists"),
                label = "Select Property",
                choices = NULL,
                selected = NULL
              ),
              selectizeInput(
                inputId = ns("survey"),
                label = "Select Survey",
                choices = NULL,
                selected = NULL
              ),
              hr(),
              actionButton(
                inputId = ns("submit_edit"),
                label = "Submit Edits",
                class = "btn-success"
              )
            )
          ),
          actionButton(
            inputId = ns("clear_edit"),
            label = "Clear Values",
            class = "btn-secondary"
          )
        ),
        ## Main Panel ----
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          card(
            height = "100%",
            card_header(
              h5("Edit Survey")
            ),
            card_body(
              div(
                style = "display: flex; flex-direction: column; gap: 15px;",
                uiOutput(ns("edit_fields_ui")),
                div(style = "flex-grow: 1;")
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
module_edit_surveys_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("edit_company", sv_required())
    iv$enable()

    ## Reactive :: Property List New ----
    property_list_new <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, 
                CONCAT_WS(' || ', property_name, property_name_public) AS property_name 
        FROM properties 
        ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property_new",
        choices = setNames(
          property_list_new()$id,
          property_list_new()$property_name
        ),
        selected = isolate(input$property_new),
        server = TRUE
      )
    })

    property_name_new <- reactiveVal(NULL)

    observe({
      req(input$property_new)
      name <- property_list_new() |>
        filter(id == input$property_new) |>
        pull(property_name)
      property_name_new(name)
    })

    ## Reactive :: Property List Existing ----
    property_list_exists <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT sv.property_id as id, 
                CONCAT_WS(' || ', pr.property_name, pr.property_name_public) AS property_name
        FROM surveys sv
        LEFT JOIN properties pr ON sv.property_id = pr.id
        ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property_exists",
        choices = setNames(
          property_list_exists()$id,
          property_list_exists()$property_name
        ),
        selected = isolate(input$property_exists),
        server = TRUE
      )
    })

    property_name_exists <- reactiveVal(NULL)

    observe({
      req(input$property_exists)
      name <- property_list_exists() |>
        filter(id == input$property_exists) |>
        pull(property_name)
      property_name_exists(name)
    })

    ## Reactive Value :: Property Survey ----
    property_survey <- reactiveVal(NULL)

    ## Observe :: Clear edit fields when switching to Add New ----
    observeEvent(
      input$property_new,
      {
        property_survey(NULL)

        updateSelectizeInput(
          session,
          inputId = "property_exists",
          choices = setNames(
            property_list_exists()$id,
            property_list_exists()$property_name
          ),
          selected = character(0),
          server = TRUE
        )

        updateSelectizeInput(
          session,
          inputId = "survey",
          choices = character(0),
          selected = character(0)
        )
      },
      ignoreInit = TRUE
    )

    ## Observe :: Clear new selection when switching to Edit Existing ----
    observeEvent(
      input$property_exists,
      {
        updateSelectizeInput(
          session,
          inputId = "property_new",
          choices = setNames(
            property_list_new()$id,
            property_list_new()$property_name
          ),
          selected = character(0),
          server = TRUE
        )
      },
      ignoreInit = TRUE
    )

    ## Reactive :: Existing Survey List ----
    survey_list <- reactive({
      db_updated()

      survey_ids <- dbGetQuery(
        db_con,
        "SELECT 
          id,
          property_id,
          company,
          paid_date
        FROM surveys
        ORDER BY paid_date DESC;"
      )

      req(input$property_exists)

      survey_data <- survey_ids |>
        filter(property_id == input$property_exists)

      setNames(
        survey_data$id,
        paste0(
          survey_data$company,
          " (",
          format(as.Date(survey_data$paid_date), "%Y-%m-%d"),
          ")"
        )
      )
    })

    ## Observe :: Update Survey selectize when property changes ----
    observeEvent(
      input$property_exists,
      {
        req(isTruthy(input$property_exists))

        updateSelectizeInput(
          session,
          inputId = "survey",
          choices = survey_list(),
          selected = isolate(input$survey),
          server = TRUE
        )
      },
      ignoreInit = FALSE
    )

    ## Event :: Load survey for selected record ----
    observeEvent(input$survey, {
      req(input$property_exists)
      req(input$survey)

      query <- glue_sql(
        "SELECT 
          sv.id,
          sv.property_id,
          sv.company,
          sv.timeline,
          sv.amount_quote,
          sv.amount_paid,
          sv.paid_date,
          sv.survey_notes,
          p.property_name
        FROM surveys sv
        JOIN properties p ON sv.property_id = p.id
        WHERE sv.property_id = {input$property_exists} AND sv.id = {input$survey}",
        .con = db_con
      )

      survey <- dbGetQuery(db_con, query)
      property_survey(survey)
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- property_survey()

      property_name_text <- if (isTruthy(record$property_name)) {
        paste0("Editing survey for: ", record$property_name)
      } else if (isTruthy(input$property_new)) {
        paste0("Adding new survey for: ", property_name_new())
      } else {
        "No survey selected"
      }

      company_val <- if (isTruthy(record$company)) record$company else ""
      timeline_val <- if (isTruthy(record$timeline)) record$timeline else ""
      amount_quote_val <- if (isTruthy(record$amount_quote)) {
        record$amount_quote
      } else {
        NULL
      }
      amount_paid_val <- if (isTruthy(record$amount_paid)) {
        record$amount_paid
      } else {
        NULL
      }
      paid_date_val <- if (isTruthy(record$paid_date)) {
        as.Date(record$paid_date)
      } else {
        NA
      }
      survey_notes_val <- if (isTruthy(record$survey_notes)) {
        record$survey_notes
      } else {
        ""
      }

      tagList(
        h6(
          class = "text-muted",
          property_name_text
        ),
        hr(),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_company"),
            label = "Company",
            value = company_val
          ),
          textInput(
            inputId = ns("edit_timeline"),
            label = "Timeline",
            value = timeline_val
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          autonumericInput(
            inputId = ns("edit_amount_quote"),
            label = "Amount Quoted",
            value = amount_quote_val,
            currencySymbol = "$",
            align = "left"
          ),
          autonumericInput(
            inputId = ns("edit_amount_paid"),
            label = "Amount Paid",
            value = amount_paid_val,
            currencySymbol = "$",
            align = "left"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          dateInput(
            inputId = ns("edit_paid_date"),
            label = "Paid Date",
            value = paid_date_val,
            format = "yyyy-mm-dd"
          ),
          textAreaInput(
            inputId = ns("edit_survey_notes"),
            label = "Survey Notes",
            value = survey_notes_val,
            rows = 4
          )
        )
      )
    })

    ## Event :: Submit Edits ----
    observeEvent(input$submit_edit, {
      req(!is.null(property_survey()))
      req(input$survey)

      valid_or_na <- function(x, na) {
        if (isTruthy(x)) x else na
      }

      update_df <- tibble(
        id = input$survey,
        company = valid_or_na(input$edit_company, NA_character_),
        timeline = valid_or_na(input$edit_timeline, NA_character_),
        amount_quote = valid_or_na(
          as.numeric(input$edit_amount_quote),
          NA_real_
        ),
        amount_paid = valid_or_na(
          as.integer(input$edit_amount_paid),
          NA_integer_
        ),
        paid_date = valid_or_na(as.Date(input$edit_paid_date), NA_Date_),
        survey_notes = valid_or_na(input$edit_survey_notes, NA_character_)
      )

      dbx::dbxUpdate(
        db_con,
        table = "surveys",
        records = update_df,
        where_cols = "id"
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Survey for {property_name_exists()} has been successfully updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Add Record ----
    observeEvent(input$add_record, {
      req(input$property_new)
      req(iv$is_valid())

      valid_or_na <- function(x, na) {
        if (isTruthy(x)) x else na
      }

      new_record <- tibble(
        property_id = input$property_new,
        company = valid_or_na(input$edit_company, NA_character_),
        timeline = valid_or_na(input$edit_timeline, NA_character_),
        amount_quote = valid_or_na(
          as.numeric(input$edit_amount_quote),
          NA_real_
        ),
        amount_paid = valid_or_na(
          as.integer(input$edit_amount_paid),
          NA_integer_
        ),
        paid_date = valid_or_na(as.Date(input$edit_paid_date), NA_Date_),
        survey_notes = valid_or_na(input$edit_survey_notes, NA_character_)
      )

      dbx::dbxInsert(
        db_con,
        table = "surveys",
        records = new_record
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      property_survey(new_record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "New survey for {property_name_new()} has been successfully created"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      property_survey(NULL)

      updateSelectizeInput(
        session,
        inputId = "survey",
        choices = character(0),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "property_exists",
        choices = setNames(
          property_list_exists()$id,
          property_list_exists()$property_name
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "property_new",
        choices = setNames(
          property_list_new()$id,
          property_list_new()$property_name
        ),
        selected = character(0)
      )

      updateTextInput(session, "edit_company", value = "")
      updateTextInput(session, "edit_timeline", value = "")
      updateAutonumericInput(session, "edit_amount_quote", value = NULL)
      updateAutonumericInput(session, "edit_amount_paid", value = NULL)
      updateDateInput(session, "edit_paid_date", value = as.Date(NA))
      updateTextAreaInput(session, "edit_survey_notes", value = "")
    })
  })
}
