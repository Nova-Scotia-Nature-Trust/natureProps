# UI ----
module_data_viewer_ui <- function(id, panel_id) {
  ns <- NS(id)

  ## Choices for data views ----
  choices_list <- if (panel_id == "panel_01") {
    list(
      "Select a view from the list" = "",
      "PIDs (all)" = "pid_view_04",
      "PIDs (filtered)" = "pid_view_01",
      "Property Contact Details (all)" = "property_contact_details_view_all",
      "Property Contact Details (filtered)" = "property_contact_details_view_filtered",
      "Communication History" = "communication_data_view_all",
      "Communication History (filtered)" = "communication_data_view_filtered",
      "Outreach" = "outreach_view",
      "Land & Securement History (all)" = "land_secure_comms_all",
      "Land & Securement History (filtered)" = "land_secure_comms_filtered",
      "Property Descriptions (all)" = "property_descriptions_all",
      "Property Descriptions (filtered)" = "property_descriptions_filtered"
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
      height = "100%", # Set card height to 100%
      card_header(
        selectInput(
          inputId = ns("data_view_input"),
          label = "Data Table View",
          choices = choices_list,
          selected = ifelse(panel_id == "panel_02", "pid_view_02", ""),
          width = "350px"
        )
      ),
      card_body(
        style = "height: calc(100vh - 265px); padding: 0.5rem 1rem;", # Set explicit height and remove padding
        DTOutput(outputId = ns("view_df"), height = "100%") # Set output height to 100%
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
    # Change name to df_views_meta
    df_view_meta <- read_xlsx(
      "inputs/field and function mapping tables/df_views.xlsx"
    )

    ## WARNING THIS WILL NOT WORK IN DOCKER CONTAINER ##
    ## NEED TO COPY THIS FILE TO FILE INPUTS ###
    ## Load 'property database' spreadsheet data

    if (DOCKER_CON) {
      parcels_path <- "inputs/23-12-12 - Single Sheet Landowner Tracking Spreadsheet.xlsx"
    } else {
      parcels_path <- "C:/Users/dominic/OneDrive - Nova Scotia Nature Trust/Documents/Property database/inputs/reference files/23-12-12 - Single Sheet Landowner Tracking Spreadsheet.xlsx"
    }

    parcels_raw <- read_xlsx(
      parcels_path,
      sheet = "Properties"
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

      ## Property contact details view ----
      if (selected_view == "property_contact_details_view_all") {
        data <- prep_view_property_contacts(df_view_meta, db_con)
      } else if (selected_view == "property_contact_details_view_filtered") {
        data <- prep_view_property_contacts(df_view_meta, db_con)

        if (!is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(
              `Property Contact PIDs`,
              str_c(focal_pid_rv(), collapse = "|")
            ))
        } else {
          data <- data |>
            filter(FALSE)
        }
        ## PID view (multiple) ----
      } else if (
        selected_view %in% c("pid_view_01", "pid_view_02", "pid_view_04")
      ) {
        data <- prep_view_pid(df_view_meta, selected_view, db_con)

        if (!is.null(prop_filter)) {
          data <- data |>
            filter(`Property Name` == prop_filter())
        }

        if (selected_view == "pid_view_01") {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        }

        # Communication data view ----
      } else if (selected_view == "communication_data_view_all") {
        data <- prep_view_communications(
          df_view_meta,
          selected_view = "communication_data_view",
          db_con
        )
      } else if (selected_view == "communication_data_view_filtered") {
        data <- prep_view_communications(
          df_view_meta,
          selected_view = "communication_data_view",
          db_con
        )

        if (!is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(PIDs, str_c(focal_pid_rv(), collapse = "|")))
        } else {
          data <- data |>
            filter(FALSE)
        }

        ## Outreach data view ----
      } else if (selected_view == "outreach_view") {
        data <- prep_view_outreach(df_view_meta, selected_view, db_con)
        ## Historical communications data view ----
      } else if (selected_view == "land_secure_comms_all") {
        data <- prep_view_historical_comms(parcels_raw, db_con)
      } else if (selected_view == "land_secure_comms_filtered") {
        data <- prep_view_historical_comms(parcels_raw, db_con) |>
          filter(PID %in% focal_pid_rv())
        ## Property descriptions data view ----
      } else if (selected_view == "property_descriptions_all") {
        data <- prep_view_property_descriptions(db_con)
      } else if (selected_view == "property_descriptions_filtered") {
        data <- prep_view_property_descriptions(db_con)
        if (!is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(PIDs, str_c(focal_pid_rv(), collapse = "|")))
        } else {
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
