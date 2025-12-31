# UI ----
module_outreach_queries_ui <- function(id) {
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
            ns("query_choice"),
            "Select query",
            choices = c("", "Focal area outreach priorities"),
            multiple = FALSE
          ),
          uiOutput(ns("conditional_contact_ui")),
          actionButton(
            inputId = ns("run_query"),
            label = "Run query",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          )
        ),
        # Main layout - Query Result card only
        card(
          height = "100%",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            h5("Query Result"),
            downloadButton(
              outputId = ns("download_query_data"),
              label = "Download",
              class = "btn-sm"
            )
          ),
          card_body(
            div(
              style = "display: flex; flex-direction: column; gap: 15px;",
              DTOutput(outputId = ns("view_df"), height = "100%"),
              div(style = "flex-grow: 1;")
            )
          )
        )
      )
    )
  )
}

# Server ----
module_outreach_queries_server <- function(
  id,
  db_con,
  db_updated = NULL,
  focal_pids_rv
) {
  moduleServer(id, function(input, output, session) {
    # Add this reactive value to track table clearing
    table_data <- reactiveVal(NULL)

    ## Conditional UI (based on query select) ----
    output$conditional_contact_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Focal area outreach priorities") {
        selectizeInput(
          ns("focal_area"),
          "Select Focal Area",
          choices = NULL,
          multiple = TRUE,
          options = list(
            create = FALSE,
            placeholder = "Select a focal area"
          )
        )
      } else if (input$query_choice == "XXX") {}
    })

    ## Focal area reactive ----
    # Define a reactive for Focal Areas that depends on db_updated, if provided
    focal_areas_reactive <- reactive({
      # Only try to use db_updated if it is not NULL.
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }
      # Query database for Focal Areas
      dbGetQuery(
        conn = db_con,
        "SELECT internal_value FROM focus_area_internal;"
      ) |>
        pull() |>
        sort()
    })

    ## Initialize inputs ----
    observe({
      req(input$query_choice)

      # Update the focal area selectize input (update whenever focal_areas_reactive() changes)
      updateSelectizeInput(
        session,
        inputId = "focal_area",
        choices = focal_areas_reactive(),
        server = TRUE
      )
    })

    # Event :: Run query ----
    observeEvent(input$run_query, {
      req(input$focal_area)

      col_name <- "Focus Area (Internal)"

      data <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT * FROM view_focal_areas_outreach WHERE {`col_name`} IN ({input$focal_area*})",
          .con = db_con
        )
      )

      focal_pids_rv(data$PID)

      # Update the reactive value with the new data
      table_data(data)
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "query_choice",
        choices = c("", "Focal area outreach priorities"),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "focal_area",
        choices = focal_areas_reactive(),
        selected = character(0),
        server = TRUE
      )

      # Add this line to clear the table data
      table_data(NULL)

      # Clear reactive pids
      focal_pids_rv(NULL)
    })

    ## Render data table ----

    output$view_df <- renderDT({
      # Use req to make sure we only render when we have data
      req(table_data())

      # Convert character columns to factors to get select inputs
      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      datatable(
        data_for_display,
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c('10', '25', '50', '100', 'All')
          ),
          scrollX = TRUE,
          scrollY = "400px",
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
        extensions = c("Buttons")
      )
    })

    ## Download handler ----
    output$download_query_data <- downloadHandler(
      filename = function() {
        query_name <- input$query_choice
        if (query_name == "") {
          query_name <- "query_results"
        }
        # Clean the query name for filename
        query_name <- str_replace_all(query_name, " ", "_") |>
          str_to_lower()
        glue("{query_name}_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        data_to_download <- table_data()

        if (!is.null(data_to_download) && nrow(data_to_download) > 0) {
          write_csv(data_to_download, file)
        } else {
          # Write empty file if no data
          write_csv(data.frame(), file)
        }
      }
    )
  })
}
