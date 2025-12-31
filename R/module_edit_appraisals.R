# UI ----
module_edit_appraisals_ui <- function(id) {
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
            inputId = ns("property_name"),
            label = "Property Name",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select property"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Appraisals",
            class = "btn-success"
          ),
          uiOutput(ns("appraisal_select_ui")), # Change this to conditional UI
          hr(),
          actionButton(
            inputId = ns("submit_edit"),
            label = "Submit Changes",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_edit"),
            label = "Clear",
            class = "btn-secondary"
          )
        ),
        ## Main panel ----
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

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties ORDER BY property_name;"
      ) |>
        select(property_name, id) |>
        deframe()
    })

    ## Update property dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Appraisals for property ----
    property_appraisals <- reactiveVal(NULL)

    ## Reactive value :: Selected appraisal record ----
    selected_record <- reactiveVal(NULL)

    ## Conditional UI :: Appraisal select ----
    output$appraisal_select_ui <- renderUI({
      appraisals <- property_appraisals()

      if (!is.null(appraisals) && nrow(appraisals) > 0) {
        appraisal_choices <- setNames(
          appraisals$id,
          paste0(
            appraisals$appraiser_name,
            " (",
            format(as.Date(appraisals$appraisal_date), "%Y-%m-%d"),
            ")"
          )
        )

        selectInput(
          inputId = ns("appraisal_select"),
          label = "Select Appraisal",
          choices = c("Select an appraisal..." = "", appraisal_choices),
          selected = ""
        )
      }
    })

    ## Event :: Load appraisals for property ----
    observeEvent(input$load_record, {
      req(input$property_name)

      property_id <- input$property_name

      query <- glue_sql(
        "SELECT 
          a.id,
          a.property_id,
          a.appraisal_date,
          a.appraiser_name,
          a.appraisal_value,
          a.appraisal_notes,
          p.property_name
        FROM appraisals a
        JOIN properties p ON a.property_id = p.id
        WHERE a.property_id = {property_id}
        ORDER BY a.appraisal_date DESC",
        .con = db_con
      )

      appraisals <- dbGetQuery(db_con, query)

      if (nrow(appraisals) > 0) {
        property_appraisals(appraisals)
      } else {
        # No appraisals exist - set up for new entry
        property_appraisals(NULL)

        # Get property name for display
        property_name <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT property_name FROM properties WHERE id = {property_id}",
            .con = db_con
          )
        )$property_name

        # Create a placeholder record for the new appraisal
        new_record <- tibble(
          id = NA_integer_,
          property_id = as.integer(property_id),
          appraisal_date = NA_Date_,
          appraiser_name = NA_character_,
          appraisal_value = NA_real_,
          appraisal_notes = NA_character_,
          property_name = property_name
        )

        selected_record(new_record)

        shinyalert(
          title = "No Appraisals Found",
          text = "No existing appraisals for this property. You can add a new one in this form.",
          type = "info",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
      }
    })

    ## Event :: Select appraisal to edit ----
    observeEvent(input$appraisal_select, {
      req(input$appraisal_select)
      req(property_appraisals())

      appraisal_id <- as.integer(input$appraisal_select)

      record <- property_appraisals() |>
        filter(id == appraisal_id)

      if (nrow(record) == 1) {
        selected_record(record)
      } else {
        selected_record(NULL)
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      # Extract values if record exists, otherwise NULL
      property_name_text <- if (!is.null(record)) {
        if (is.na(record$id)) {
          paste0("Adding new appraisal for: ", record$property_name)
        } else {
          paste0("Editing appraisal for: ", record$property_name)
        }
      } else {
        "No appraisal selected"
      }

      appraisal_date_val <- if (
        !is.null(record) && !is.na(record$appraisal_date)
      ) {
        as.Date(record$appraisal_date)
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

      appraisal_value_val <- if (
        !is.null(record) && !is.na(record$appraisal_value)
      ) {
        record$appraisal_value
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
            inputId = ns("edit_appraisal_date"),
            label = "Appraisal Date",
            value = appraisal_date_val,
            format = "yyyy-mm-dd"
          ),
          numericInput(
            inputId = ns("edit_appraisal_value"),
            label = "Appraisal Value",
            value = appraisal_value_val,
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

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(selected_record())

      current_record <- selected_record()

      # Check if this is a new record (no id) or an update
      is_new_record <- is.na(current_record$id)

      if (is_new_record) {
        # Insert new appraisal
        insert_tibble <- tibble(
          property_id = current_record$property_id,
          appraisal_date = if (!is.null(input$edit_appraisal_date)) {
            as.Date(input$edit_appraisal_date)
          } else {
            NA_Date_
          },
          appraiser_name = if (
            !is.null(input$edit_appraiser_name) &&
              input$edit_appraiser_name != ""
          ) {
            input$edit_appraiser_name
          } else {
            NA_character_
          },
          appraisal_value = if (!is.null(input$edit_appraisal_value)) {
            as.numeric(input$edit_appraisal_value)
          } else {
            NA_real_
          },
          appraisal_notes = if (
            !is.null(input$edit_appraisal_notes) &&
              input$edit_appraisal_notes != ""
          ) {
            input$edit_appraisal_notes
          } else {
            NA_character_
          }
        )

        # Insert the record
        dbx::dbxInsert(
          db_con,
          table = "appraisals",
          records = insert_tibble
        )

        success_message <- str_glue(
          "New appraisal for {current_record$property_name} has been successfully created"
        )
      } else {
        # Update existing appraisal
        req(input$appraisal_select)
        appraisal_id <- as.integer(input$appraisal_select)

        update_tibble <- tibble(
          id = appraisal_id,
          appraisal_date = if (!is.null(input$edit_appraisal_date)) {
            as.Date(input$edit_appraisal_date)
          } else {
            NA_Date_
          },
          appraiser_name = if (
            !is.null(input$edit_appraiser_name) &&
              input$edit_appraiser_name != ""
          ) {
            input$edit_appraiser_name
          } else {
            NA_character_
          },
          appraisal_value = if (!is.null(input$edit_appraisal_value)) {
            as.numeric(input$edit_appraisal_value)
          } else {
            NA_real_
          },
          appraisal_notes = if (
            !is.null(input$edit_appraisal_notes) &&
              input$edit_appraisal_notes != ""
          ) {
            input$edit_appraisal_notes
          } else {
            NA_character_
          }
        )

        # Update the record
        dbx::dbxUpdate(
          db_con,
          table = "appraisals",
          records = update_tibble,
          where_cols = "id"
        )

        success_message <- str_glue(
          "Appraisal for {current_record$property_name} has been successfully updated"
        )
      }

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = success_message,
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      selected_record(NULL)
      property_appraisals(NULL)

      # Clear the sidebar filters
      updateSelectizeInput(
        session,
        inputId = "property_name",
        selected = "",
        choices = c("", property_choices()),
        server = TRUE
      )

      updateDateInput(session, "edit_appraisal_date", value = NA)
      updateTextInput(session, "edit_appraiser_name", value = "")
      updateNumericInput(session, "edit_appraisal_value", value = NA)
      updateTextAreaInput(session, "edit_appraisal_notes", value = "")
    })
  })
}
