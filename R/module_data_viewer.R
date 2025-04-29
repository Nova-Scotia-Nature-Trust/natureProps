module_data_viewer_ui <- function(id, panel_id) {
  ns <- NS(id)

  # Define choices based on panel_type
  choices_list <- if (panel_id == "panel_01") {
    list(
      "Select a view from the list" = "",
      "PIDs" = "pid_view_01",
      "Landowner Details" = "landowner_details_view",
      "Communication History" = "communication_data_view",
      "Outreach" = "outreach_view"
    )
  } else if (panel_id == "panel_02") {
    list(
      "Select a view from the list" = "",
      "Action Items" = "pid_view_02"
    )
  }

  nav_panel(
    title = "Data Viewer",
    card(
      full_screen = TRUE,
      height = "100%", # Set card height to 100%
      card_header(
        selectInput(
          inputId = ns("data_view_input"),
          label = "Data Table View",
          choices = choices_list,
          selected = ""
        )
      ),
      card_body(
        style = "height: calc(100vh - 265px); padding: 0.5rem 1rem;", # Set explicit height and remove padding
        DTOutput(outputId = ns("view_df"), height = "100%") # Set output height to 100%
      )
    )
  )
}


module_data_viewer_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Load data view metadata table (parameters and attribute names)
    # Change name to df_views_meta
    df_view_meta <- read_xlsx(
      "inputs/field and function mapping tables/df_views.xlsx"
    )

    ## Reactive to capture the selected view
    view_scenario <- reactive({
      input$data_view_input
    })

    ## Create a data frame to render with DT
    output_view_data <- reactive({
      ## This ensures the reactive will re-execute when db_updated() value changes
      ## Only use if data_changed is provided
      if (!is.null(db_updated)) {
        db_updated()
      }

      ## Access the selected view
      selected_view <- view_scenario()

      ## Landowner details view ----
      if (selected_view == "landowner_details_view") {
        data <- prep_view_landowners(df_view_meta, db_con)

        ## PID view 01/02 ----
      } else if (selected_view %in% c("pid_view_01", "pid_view_02")) {
        data <- prep_view_pid(df_view_meta, selected_view, db_con)

        ## Communication data view ----
      } else if (selected_view == "communication_data_view") {
        data <- prep_view_communications(df_view_meta, selected_view, db_con)

        ## Outreach data view ----
      } else if (selected_view == "outreach_view") {
        data <- prep_view_outreach(df_view_meta, selected_view, db_con)
        ## Empty
      } else if (selected_view == "") {
        data <- NULL
      }

      return(data)
    })

    # Setup table layout
    dom_layout <- "
    <'row'<'col-sm-10'l><'col-sm-2 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "

    # Get ordering information
    table_order <- reactive({
      list(list(
        attr(output_view_data(), "order_column"),
        attr(output_view_data(), "order_direction")
      ))
    })

    # Render the datatable
    output$view_df <- renderDT({
      datatable(
        output_view_data(),
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
          order = table_order(),
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
