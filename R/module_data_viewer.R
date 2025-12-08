# UI ----
module_data_viewer_ui <- function(id, panel_id) {
  ns <- NS(id)

  ## Choices for data views ----
  choices_list <- if (panel_id == "panel_01") {
    list(
      "Select a view from the list" = "",
      "PIDs" = "pid_view",
      "Property Contact Details" = "property_contact_details_view",
      "Communication History" = "communication_data_view",
      "Outreach" = "outreach_view",
      "Land & Securement History" = "land_secure_comms",
      "Property Descriptions" = "property_descriptions",
      "Landowner & Address" = "landowner_address"
    )
  } else if (panel_id == "panel_02") {
    list(
      "Select a view from the list" = "",
      "Action Items" = "pid_view_02"
    )
  }
  ## Card :: Data viewer ----
  nav_panel(
    title = "Data Viewer",
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(
        tagList(
          selectInput(
            inputId = ns("data_view_input"),
            label = "Data Table View",
            choices = choices_list,
            selected = ifelse(panel_id == "panel_02", "pid_view_02", ""),
            width = "250px"
          ),
          if (panel_id == "panel_01") {
            div(
              style = "margin-top: 0.5rem;",
              input_switch(
                id = ns("filter_toggle"),
                label = "Filter by query results",
                value = FALSE
              )
            )
          }
        )
      ),
      card_body(
        style = "height: calc(100vh - 265px); padding: 0.5rem 1rem;",
        DTOutput(outputId = ns("view_df"), height = "100%")
      )
    )
  )
}

# Server ----
module_data_viewer_server <- function(
  id,
  db_con,
  db_updated = NULL,
  prop_filter = NULL,
  focal_pid_rv
) {
  moduleServer(id, function(input, output, session) {
    ## Load data view metadata table (parameters and attribute names)
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

      ## Access the selected view and filter toggle
      selected_view <- view_scenario()
      apply_filter <- input$filter_toggle

      ## Property contact details view ----
      if (selected_view == "property_contact_details_view") {
        data <- prep_view_property_contacts(df_view_meta, db_con)

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(
              `Property Contact PIDs`,
              str_c(focal_pid_rv(), collapse = "|")
            ))
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## PID view ----
      } else if (selected_view == "pid_view") {
        # Use pid_view_04 for unfiltered view in panel_01
        view_name <- if (id == "panel_01") "pid_view_04" else "pid_view_02"
        data <- prep_view_pid(df_view_meta, view_name, db_con)

        # if (!is.null(prop_filter)) {
        #   data <- data |>
        #     filter(`Property Name` == prop_filter())
        # }

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
      } else if (selected_view == "pid_view_02") {
        # Action items view (panel_02 only, no filtering)
        data <- prep_view_pid(df_view_meta, "pid_view_02", db_con)
        if (!is.null(prop_filter) && !is.null(prop_filter())) {
          data <- data |>
            filter(`Property Name` == prop_filter())
        }
        # Communication data view ----
      } else if (selected_view == "communication_data_view") {
        data <- prep_view_communications(
          df_view_meta,
          selected_view = "communication_data_view",
          db_con
        )

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(PIDs, str_c(focal_pid_rv(), collapse = "|")))
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }

        ## Outreach data view ----
      } else if (selected_view == "outreach_view") {
        data <- prep_view_outreach(df_view_meta, selected_view, db_con)

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }

        ## Historical communications data view ----
      } else if (selected_view == "land_secure_comms") {
        data <- prep_view_historical_comms(db_con)

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Property descriptions data view ----
      } else if (selected_view == "property_descriptions") {
        data <- prep_view_property_descriptions(db_con)

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(PIDs, str_c(focal_pid_rv(), collapse = "|")))
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
      } else if (selected_view == "landowner_address") {
        data <- prep_view_landowner_address(db_con)

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
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
      # req(output_view_data())

      if (is.null(output_view_data()) || nrow(output_view_data()) == 0) {
        return(datatable(data.frame()))
      }
      # Convert character columns to factors to get select inputs
      data_for_display <- output_view_data() |>
        mutate(across(where(is.character), as.factor))

      datatable(
        data_for_display,
        escape = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = dom_layout,
          buttons = list(
            "copy",
            "excel"
          ),
          order = table_order(),
          stateSave = FALSE
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
  })
}
