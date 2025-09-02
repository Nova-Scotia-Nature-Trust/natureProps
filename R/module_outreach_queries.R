# UI ----
module_outreach_queries_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%", # Make card fill available space
      layout_sidebar(
        sidebar = sidebar(
          "",
          open = TRUE,
          actionButton(inputId = ns("run_query"), label = "Run query"),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
        ),
        # Main layout
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(2, 10),
            ## Card :: Select query from list ----
            card(
              height = "100%",
              card_header(h5("Query List")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    ns("query_choice"),
                    "Select query",
                    choices = c("", "Focal area outreach priorities"),
                    multiple = FALSE,
                    width = "80%"
                  ),
                  uiOutput(ns("conditional_contact_ui")),
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            # Card :: Show query result ----
            card(
              height = "100%",
              card_header(h5("Query Result")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  DTOutput(outputId = ns("view_df"), height = "100%"),
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
module_outreach_queries_server <- function(
  id,
  db_con,
  db_updated = NULL,
  focal_pids_rv
) {
  moduleServer(id, function(input, output, session) {
    ## Read metadata file ----
    df_view_meta <- read_xlsx(
      "inputs/field and function mapping tables/df_views.xlsx"
    )

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
      } else if (input$communication_type == "XXX") {
      }
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
      selected_view <- "pid_view_03"
      focal_area <- input$focal_area

      view_data <- prep_view_query_focal_area(
        df_view_meta,
        selected_view,
        db_con,
        focal_area
      )

      data <- view_data$df
      focal_pids_rv(view_data$focal_pids)
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

    # Setup table layout
    dom_layout <- "
    <'row'<'col-sm-10'l><'col-sm-2 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "

    output$view_df <- renderDT({
      # Use req to make sure we only render when we have data
      req(table_data())

      datatable(
        table_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = dom_layout,
          buttons = list(
            "copy",
            "excel",
            "pdf",
            "print"
          ),
          # order = table_order(),
          stateSave = FALSE
        ),
        filter = list(position = "top", clear = FALSE),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons")
      )
    })
  })
}
