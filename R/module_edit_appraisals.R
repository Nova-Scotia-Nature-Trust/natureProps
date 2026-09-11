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
        selected = character(0),
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
        "SELECT DISTINCT ap.property_id as id, 
                CONCAT_WS(' || ', pr.property_name, pr.property_name_public) AS property_name
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
          SUM(pi.area_ha * 2.471) AS acres,
          a.fmv / NULLIF(SUM(pi.area_ha * 2.471), 0) AS fmv_per_acre,
          a.appraisal_notes,
          a.authoritative,
          a.paid_date,
          pr.property_name
        FROM appraisals a
        JOIN properties pr ON a.property_id = pr.id
        JOIN parcels pa ON pr.id = pa.property_id
        JOIN parcel_info pi ON pa.id = pi.parcel_id
        WHERE a.property_id = {input$property_exists}
          AND a.id = {input$appraisal}
        GROUP BY
          a.id,
          a.property_id,
          a.appraisal_effective_date,
          a.appraiser_name,
          a.fmv,
          a.appraisal_notes,
          a.authoritative,
          a.paid_date,
          pr.property_name",
        .con = db_con
      )

      appraisal <- dbGetQuery(db_con, query)
      property_appraisal(appraisal)
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- property_appraisal()

      property_name_text <- if (isTruthy(record$property_name)) {
        auth <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT a.fmv / NULLIF(SUM(pi.area_ha * 2.471), 0) AS fmv_per_acre
             FROM appraisals a
              JOIN properties pr ON a.property_id = pr.id
              JOIN parcels pa ON pr.id = pa.property_id
              JOIN parcel_info pi ON pa.id = pi.parcel_id
            WHERE a.property_id = {record$property_id} AND a.authoritative = TRUE
            GROUP BY          
            a.property_id,
            a.fmv
            LIMIT 1",
            .con = db_con
          )
        )

        base_text <- paste0("Editing appraisal for: ", record$property_name)

        if (nrow(auth) > 0 && !is.na(auth$fmv_per_acre)) {
          fmv_fmt <- scales::dollar(round(auth$fmv_per_acre, 2), big.mark = ",")
          paste0(base_text, ". Authoritative FMV/acre: ", fmv_fmt)
        } else {
          base_text
        }
      } else if (isTruthy(input$property_new)) {
        paste0("Adding new appraisal for: ", property_name_new())
      } else {
        "No appraisal selected"
      }

      appraisal_effective_date_val <- if (
        isTruthy(record$appraisal_effective_date)
      ) {
        as.Date(record$appraisal_effective_date)
      } else {
        NA
      }

      appraiser_name_val <- if (isTruthy(record$appraiser_name)) {
        record$appraiser_name
      } else {
        ""
      }

      fmv_val <- if (isTruthy(record$fmv)) {
        record$fmv
      } else {
        NULL
      }

      appraisal_notes_val <- if (isTruthy(record$appraisal_notes)) {
        record$appraisal_notes
      } else {
        ""
      }

      paid_date_val <- if (isTruthy(record$paid_date)) {
        as.Date(record$paid_date)
      } else {
        NA
      }

      # Default TRUE for new records; use stored value when editing
      authoritative_val <- if (!is.null(record$authoritative)) {
        isTRUE(record$authoritative)
      } else {
        TRUE
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
          autonumericInput(
            inputId = ns("edit_fmv"),
            label = "Fair Market Value",
            value = fmv_val,
            currencySymbol = "$",
            align = "left"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          textInput(
            inputId = ns("edit_appraiser_name"),
            label = "Appraiser Name",
            value = appraiser_name_val
          ),
          dateInput(
            inputId = ns("edit_paid_date"),
            label = "Invoice Paid Date",
            value = paid_date_val,
            format = "yyyy-mm-dd"
          )
        ),
        textAreaInput(
          inputId = ns("edit_appraisal_notes"),
          label = "Appraisal Notes",
          value = appraisal_notes_val,
          rows = 4,
          width = "50%"
        ),
        checkboxInput(
          inputId = ns("edit_authoritative"),
          label = "Authoritative Appraisal",
          value = authoritative_val
        )
      )
    })

    ## Event :: Submit Edits ----
    observeEvent(input$submit_edit, {
      req(!is.null(property_appraisal()))
      req(input$appraisal)

      appraisal_id <- input$appraisal

      # Conflict check: block if another appraisal for this property is already authoritative
      if (isTRUE(input$edit_authoritative)) {
        conflict <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT COUNT(*) AS n FROM appraisals
             WHERE property_id = {input$property_exists}
               AND authoritative = TRUE
               AND id != {input$appraisal}",
            .con = db_con
          )
        )
        if (conflict$n > 0) {
          shinyalert(
            title = "Error",
            text = "An authoritative appraisal already exists for this property. Please unmark it before adding a new authoritative appraisal.",
            type = "error",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE
          )
          return()
        }
      }

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
        ),
        paid_date = valid_or_na(
          as.Date(input$edit_paid_date),
          NA_Date_
        ),
        authoritative = isTRUE(input$edit_authoritative)
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

      # Conflict check: block if an authoritative appraisal already exists for this property
      if (isTRUE(input$edit_authoritative)) {
        conflict <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT COUNT(*) AS n FROM appraisals
             WHERE property_id = {input$property_new}
               AND authoritative = TRUE",
            .con = db_con
          )
        )
        if (conflict$n > 0) {
          shinyalert(
            title = "Error",
            text = "An authoritative appraisal already exists for this property. Please unmark it before adding a new authoritative appraisal.",
            type = "error",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE
          )
          return()
        }
      }

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
        appraisal_notes = valid_or_na(
          input$edit_appraisal_notes,
          NA_character_
        ),
        paid_date = valid_or_na(
          as.Date(input$edit_paid_date),
          NA_Date_
        ),
        authoritative = isTRUE(input$edit_authoritative)
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
      updateDateInput(
        session,
        "edit_paid_date",
        value = as.Date(NA)
      )
      updateTextInput(session, "edit_appraiser_name", value = "")
      updateAutonumericInput(session, "edit_fmv", value = NULL)
      updateTextAreaInput(session, "edit_appraisal_notes", value = "")
      updateCheckboxInput(session, "edit_authoritative", value = TRUE)
    })
  })
}
