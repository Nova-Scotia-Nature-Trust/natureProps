# UI ----
module_securement_actions_data_validation_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          "",
          open = TRUE,
          selectizeInput(
            ns("data_validation_choice"),
            "Select Data Validation Check",
            choices = c(
              "",
              "Updates to action items",
              "Missing status for action items",
              "Active securement properties not tracked"
            ),
            selected = "",
            multiple = FALSE
          ),
          uiOutput(ns("conditional_days_ui")),
          checkboxGroupButtons(
            inputId = ns("probability_filter"),
            label = "Closing Probability",
            choices = character(0),
            status = "outline-primary",
            size = "sm",
            direction = "vertical",
            width = "80%",
            checkIcon = list(yes = icon("check"))
          ),
          checkboxGroupButtons(
            inputId = ns("closing_year_filter"),
            label = "Anticipated Closing Year",
            choices = character(0),
            status = "outline-primary",
            size = "sm",
            direction = "vertical",
            width = "80%",
            checkIcon = list(yes = icon("check"))
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          )
        ),
        # Main layout - Data Validation result card only
        card(
          height = "100%",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            h5("Data Validation Result"),
            downloadButton(
              outputId = ns("download_query_data"),
              label = "Download",
              class = "btn-sm"
            )
          ),
          card_body(
            style = "padding: 0.5rem 1rem;",
            min_height = "300px",
            DTOutput(outputId = ns("view_df"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_securement_actions_data_validation_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    data_validation_choices <- c(
      "",
      "Updates to action items",
      "Missing status for action items",
      "Active securement properties not tracked"
    )

    ## Filter choices (refresh when db_updated changes) ----
    filter_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      # Fiscal year (April start) one year ago, e.g. "2025/26"
      prior_fiscal <- str_remove(
        quarter(Sys.Date() - 365, type = "year_start/end", fiscal_start = 4),
        " Q[0-9]"
      )

      list(
        probability = dbGetQuery(
          db_con,
          "SELECT probability_value FROM securement_probability ORDER BY id;"
        ) |>
          pull(probability_value) |>
          c("Unassigned"),
        closing_year = dbGetQuery(
          db_con,
          "SELECT DISTINCT COALESCE(anticipated_closing_year, 'Unassigned') AS closing_year
           FROM properties;"
        ) |>
          # Keep current fiscal year onward ("Unassigned" sorts after the years)
          filter(closing_year > prior_fiscal) |>
          pull(closing_year) |>
          sort()
      )
    })

    ## Initialize filters (all selected) ----
    observe({
      choices <- filter_choices()

      updateCheckboxGroupButtons(
        session,
        inputId = "probability_filter",
        choices = choices$probability,
        selected = isolate(input$probability_filter) %||% choices$probability,
        status = "outline-primary",
        checkIcon = list(yes = icon("check"))
      )

      updateCheckboxGroupButtons(
        session,
        inputId = "closing_year_filter",
        choices = choices$closing_year,
        selected = isolate(input$closing_year_filter) %||% choices$closing_year,
        status = "outline-primary",
        checkIcon = list(yes = icon("check"))
      )
    })

    ## Table data (re-runs when inputs or db_updated change) ----
    table_data <- reactive({
      req(input$data_validation_choice)

      if (!is.null(db_updated)) {
        db_updated()
      }

      data <- switch(
        input$data_validation_choice,
        "Updates to action items" = {
          req(input$days)
          get_updated_properties(db_con, days = input$days)
        },
        "Active securement properties not tracked" = get_untracked_properties(
          db_con
        ),
        "Missing status for action items" = get_missing_status_counts(db_con)
      )

      data |>
        filter(
          `Closing Probability` %in% input$probability_filter,
          `Anticipated Closing Year` %in% input$closing_year_filter
        )
    })

    ## Conditional UI (based on check select) ----
    output$conditional_days_ui <- renderUI({
      ns <- session$ns

      req(input$data_validation_choice == "Updates to action items")

      numericInput(
        ns("days"),
        "Days to look back",
        value = 7,
        min = 1,
        step = 1
      )
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "data_validation_choice",
        choices = data_validation_choices,
        selected = character(0),
        server = TRUE
      )

      # Reset filters to all options selected
      updateCheckboxGroupButtons(
        session,
        inputId = "probability_filter",
        selected = filter_choices()$probability
      )

      updateCheckboxGroupButtons(
        session,
        inputId = "closing_year_filter",
        selected = filter_choices()$closing_year
      )
    })

    ## Render data table ----
    output$view_df <- renderDT({
      req(table_data())

      # Convert character columns to factors to get select inputs
      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      dt <- datatable(
        data_for_display,
        # Omit "stripe" so the zebra-stripe box-shadow doesn't paint over formatStyle()
        class = "row-border hover",
        options = list(
          pageLength = 50,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c('10', '25', '50', '100', 'All')
          ),
          scrollX = TRUE,
          fixedHeader = TRUE,
          stateSave = FALSE,
          searching = TRUE
        ),
        filter = list(
          position = "top",
          clear = TRUE,
          plain = TRUE
        ),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons"),
        fillContainer = TRUE
      )

      # Highlight rows: red = no updates / missing status, green = updated
      if ("Number of Updates" %in% names(data_for_display)) {
        dt <- dt |>
          formatStyle(
            columns = names(data_for_display),
            valueColumns = "Number of Updates",
            backgroundColor = styleInterval(0, c("#F4A6A6", "#81D996")),
            color = "black"
          )
      } else if ("Missing Status Count" %in% names(data_for_display)) {
        dt <- dt |>
          formatStyle(
            columns = names(data_for_display),
            valueColumns = "Missing Status Count",
            backgroundColor = styleInterval(0, c("transparent", "#F4A6A6")),
            color = styleInterval(0, c("inherit", "black"))
          )
      }

      dt
    })

    ## Download handler ----
    output$download_query_data <- downloadHandler(
      filename = function() {
        query_name <- input$data_validation_choice
        if (query_name == "") {
          query_name <- "data_validation_results"
        }
        # Clean the check name for filename
        query_name <- str_replace_all(query_name, " ", "_") |>
          str_to_lower()
        glue("{query_name}_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        data_to_download <- table_data()

        if (!is.null(data_to_download) && nrow(data_to_download) > 0) {
          write_csv(data_to_download, file)
        } else {
          write_csv(data.frame(), file)
        }
      }
    )
  })
}
