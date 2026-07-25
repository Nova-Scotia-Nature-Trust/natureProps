# UI ----
# NAV PANEL :: APPRAISALS
module_edit_appraisals_ui <- function(id) {
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
              title = "Add New Appraisal",
              value = "add_values",
              selectizeInput(
                inputId = ns("property_new"),
                label = "Select Property",
                choices = NULL,
                selected = NULL
              ),
              actionButton(
                inputId = ns("add_record"),
                label = "Submit Appraisal",
                class = "btn-success"
              )
            ),
            accordion_panel(
              title = "Edit Exisiting Appraisal",
              value = "edit_values",
              selectizeInput(
                inputId = ns("property_exists"),
                label = "Select Property",
                choices = NULL,
                selected = NULL
              ),
              selectizeInput(
                inputId = ns("appraisal"),
                label = "Select Appraisal",
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
              h5("Edit Appraisal")
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
module_edit_appraisals_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("edit_appraisal_effective_date", sv_required())
    iv$add_rule("edit_fmv", sv_required())
    iv$add_rule("edit_appraiser_name", sv_required())
    iv$enable()

    ## Reactive :: Property List New ----
    property_list_new <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, property_name FROM properties 
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

    ## Reactive :: Property List Exisiting ----
    property_list_exists <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT ap.property_id as id, pr.property_name
        FROM appraisals ap
        LEFT JOIN properties pr ON ap.property_id = pr.id
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

    ## Reactive Value :: Property Appraisal ----
    property_appraisal <- reactiveVal(NULL)

    ## Observe :: Clear edit fields when switching to Add New ----
    observeEvent(
      input$property_new,
      {
        property_appraisal(NULL)

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
          inputId = "appraisal",
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

    ## Reactive :: Exisiting Appraisal ----
    appraisal_list <- reactive({
      db_updated()

      appraisal_ids <- dbGetQuery(
        db_con,
        "SELECT 
          a.id,
          a.property_id,
          a.appraisal_effective_date,
          a.appraiser_name
        FROM appraisals a
        ORDER BY a.appraisal_effective_date DESC;"
      )

      req(input$property_exists)

      appraisal_data <- appraisal_ids |>
        filter(property_id == input$property_exists)

      app_list <- setNames(
        appraisal_data$id,
        paste0(
          appraisal_data$appraiser_name,
          " (",
          format(as.Date(appraisal_data$appraisal_effective_date), "%Y-%m-%d"),
          ")"
        )
      )
      return(app_list)
    })

    ## Observe :: Current Apprasial ID ----
    observeEvent(
      input$property_exists,
      {
        req(isTruthy(input$property_exists))

        updateSelectizeInput(
          session,
          inputId = "appraisal",
          choices = appraisal_list(),
          selected = isolate(input$appraisal),
          server = TRUE
        )
      },
      ignoreInit = FALSE
    )

    ## Event :: Load appraisals for property ----
    observeEvent(input$appraisal, {
      req(input$property_exists)
      req(input$appraisal)

      query <- glue_sql(
        "SELECT 
          a.id,
          a.property_id,
          a.appraisal_effective_date,
          a.appraiser_name,
          a.fmv,
          a.appraisal_notes,
          p.property_name
        FROM appraisals a
        JOIN properties p ON a.property_id = p.id
        WHERE a.property_id = {input$property_exists} AND a.id = {input$appraisal}",
        .con = db_con
      )

      appraisal <- dbGetQuery(db_con, query)
      property_appraisal(appraisal)
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- property_appraisal()

      property_name_text <- if (!is.null(record)) {
        paste0("Editing appraisal for: ", record$property_name)
      } else if (isTruthy(input$property_new)) {
        paste0("Adding new appraisal for: ", property_name_new())
      } else {
        "No appraisal selected"
      }

      appraisal_effective_date_val <- if (
        !is.null(record) && !is.na(record$appraisal_effective_date)
      ) {
        as.Date(record$appraisal_effective_date)
      } else {
        NA
      }

      appraiser_name_val <- if (
        !is.null(record) && !is.na(record$appraiser_name)
      ) {
        record$appraiser_name
      } else {
        ""
      }

      fmv_val <- if (!is.null(record) && !is.na(record$fmv)) {
        record$fmv
      } else {
        NULL
      }

      appraisal_notes_val <- if (
        !is.null(record) && !is.na(record$appraisal_notes)
      ) {
        record$appraisal_notes
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
          dateInput(
            inputId = ns("edit_appraisal_effective_date"),
            label = "Appraisal Effective Date",
            value = appraisal_effective_date_val,
            format = "yyyy-mm-dd"
          ),
          numericInput(
            inputId = ns("edit_fmv"),
            label = "Fair Market Value",
            value = fmv_val,
            min = 0,
            step = 1000
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_appraiser_name"),
            label = "Appraiser Name",
            value = appraiser_name_val
          ),
          textAreaInput(
            inputId = ns("edit_appraisal_notes"),
            label = "Appraisal Notes",
            value = appraisal_notes_val,
            rows = 4
          )
        )
      )
    })

    ## Event :: Submit Edits ----
    observeEvent(input$submit_edit, {
      req(!is.null(property_appraisal()))
      req(input$appraisal)

      appraisal_id <- input$appraisal

      valid_or_na <- function(x, na) {
        if (isTruthy(x)) x else na
      }

      update_df <- tibble(
        id = input$appraisal,
        appraisal_effective_date = valid_or_na(
          as.Date(input$edit_appraisal_effective_date),
          NA_Date_
        ),
        appraiser_name = valid_or_na(
          input$edit_appraiser_name,
          NA_character_
        ),
        fmv = valid_or_na(
          as.numeric(input$edit_fmv),
          NA_real_
        ),
        appraisal_notes = valid_or_na(
          input$edit_appraisal_notes,
          NA_character_
        )
      )

      dbx::dbxUpdate(
        db_con,
        table = "appraisals",
        records = update_df,
        where_cols = "id"
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Appraisal for {property_name_exists()} has been successfully updated"
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
        appraisal_effective_date = valid_or_na(
          as.Date(input$edit_appraisal_effective_date),
          NA_Date_
        ),
        appraiser_name = valid_or_na(input$edit_appraiser_name, NA_character_),
        fmv = valid_or_na(
          as.numeric(input$edit_fmv),
          NA_real_
        ),
        appraisal_notes = valid_or_na(input$edit_appraisal_notes, NA_character_)
      )

      dbx::dbxInsert(
        db_con,
        table = "appraisals",
        records = new_record
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      property_appraisal(new_record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "New appraisal for {property_name_new()} has been successfully created"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      property_appraisal(NULL)

      updateSelectizeInput(
        session,
        inputId = "appraisal",
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

      updateDateInput(
        session,
        "edit_appraisal_effective_date",
        value = as.Date(NA)
      )
      updateTextInput(session, "edit_appraiser_name", value = "")
      updateNumericInput(session, "edit_fmv", value = NA)
      updateTextAreaInput(session, "edit_appraisal_notes", value = "")
    })
  })
}
