# UI ----
module_data_viewer_ui <- function(id, panel_id) {
  ns <- NS(id)

  ## Dropdown choices for each panel ----
  choices_list <- if (panel_id == "outreach_panel") {
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
  } else if (panel_id == "securement_panel") {
    list(
      "Select a view from the list" = "",
      "Secured Property Details" = "secured_props_view",
      "Action Items (long)" = "action_items_view",
      "Action Items (wide)" = "action_items_view_wide",
      "Appraisals" = "appraisals",
      "Property Sizes" = "property_sizes",
      "Insurance View" = "insurance",
      "LLT Projects" = "llt_projects",
      "Securement Communication" = "securement_communication"
    )
  } else if (panel_id == "action_item_panel") {
    list(
      "Action Items (long)" = "action_items_view",
      "Action Items (wide)" = "action_items_view_wide"
    )
  }
  ## Card :: Data viewer ----
  nav_panel(
    title = NULL,
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        div(
          selectInput(
            inputId = ns("data_view"),
            label = NULL,
            choices = choices_list,
            selected = ifelse(
              panel_id == "action_item_panel",
              "Action Items",
              ""
            ),
            width = "250px"
          ),
          if (panel_id == "outreach_panel") {
            div(
              style = "margin-top: 0.5rem;",
              input_switch(
                id = ns("filter_toggle"),
                label = "Filter by query results",
                value = FALSE
              )
            )
          }
        ),
        downloadButton(
          outputId = ns("download_data"),
          label = "Download",
          class = "btn-sm"
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
  focal_pid_rv,
  panel_id = NULL
) {
  moduleServer(id, function(input, output, session) {
    ## Reactive :: Data Views ----
    output_view_data <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      ## Access the selected view and filter toggle
      selected_view <- input$data_view
      apply_filter <- input$filter_toggle

      if (selected_view == "") {
        data <- NULL
        ## PID  ----
      } else if (selected_view == "pid_view") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_pid;")
        attr(data, "order_column") <- 1
        attr(data, "order_direction") <- "asc"

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Property Contact Details ----
      } else if (selected_view == "property_contact_details_view") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_property_contacts;")
        attr(data, "order_column") <- 2
        attr(data, "order_direction") <- "asc"

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
        ## Communication Data ----
      } else if (selected_view == "communication_data_view") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_communication_history;")
        attr(data, "order_column") <- 1
        attr(data, "order_direction") <- "asc"

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(PIDs, str_c(focal_pid_rv(), collapse = "|")))
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Outreach Data ----
      } else if (selected_view == "outreach_view") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_outreach;")
        attr(data, "order_column") <- 4
        attr(data, "order_direction") <- "desc"

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Historical Communications ----
      } else if (selected_view == "land_secure_comms") {
        data <- dbGetQuery(
          db_con,
          "SELECT * FROM view_historical_communications;"
        )
        attr(data, "order_column") <- 1
        attr(data, "order_direction") <- "asc"

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Property Descriptions ----
      } else if (selected_view == "property_descriptions") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_property_descriptions;")
        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(str_detect(PIDs, str_c(focal_pid_rv(), collapse = "|")))
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Landowner & Address Data ----
      } else if (selected_view == "landowner_address") {
        data <- prep_view_landowner_address(db_con)

        if (apply_filter && !is.null(focal_pid_rv())) {
          data <- data |>
            filter(PID %in% focal_pid_rv())
        } else if (apply_filter) {
          data <- data |>
            filter(FALSE)
        }
        ## Action Items Long ----
      } else if (selected_view == "action_items_view") {
        data <- dbGetQuery(
          db_con,
          "SELECT * FROM view_securement_action_items;"
        )
        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"

        if (!is.null(prop_filter) && !is.null(prop_filter())) {
          data <- data |>
            filter(`Property Name` == prop_filter())
        }
        ## Action Items Wide ----
      } else if (selected_view == "action_items_view_wide") {
        data <- dbGetQuery(
          db_con,
          "SELECT * FROM view_securement_action_items;"
        )

        data <- data |>
          select("Property Name", "Action Item", "Status") |>
          pivot_wider(
            id_cols = "Property Name",
            names_from = "Action Item",
            values_from = "Status"
          )

        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"

        if (!is.null(prop_filter) && !is.null(prop_filter())) {
          data <- data |>
            filter(`Property Name` == prop_filter())
        }
        ## Secured Property Details ----
      } else if (selected_view == "secured_props_view") {
        data <- prep_view_secured_properties(db_con, gis_con)
        ## Appraisals ----
      } else if (selected_view == "appraisals") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_appraisals;")
        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"
        ## Property Sizes ----
      } else if (selected_view == "property_sizes") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_property_sizes;")
        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"
        ## Insurance ----
      } else if (selected_view == "insurance") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_insurance;")
        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"
      } else if (selected_view == "llt_projects") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_llt_projects;")
        attr(data, "order_column") <- 3
        attr(data, "order_direction") <- "asc"
      } else if (selected_view == "securement_communication") {
        data <- dbGetQuery(
          db_con,
          "SELECT * FROM view_securement_communication_history;"
        )
        attr(data, "order_column") <- 5
        attr(data, "order_direction") <- "desc"
      }
      return(data)
    })

    ## Reactive :: Table Ordering ----
    table_order <- reactive({
      list(list(
        attr(output_view_data(), "order_column"),
        attr(output_view_data(), "order_direction")
      ))
    })

    ## Render datatable ----
    output$view_df <- renderDT({
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
          pageLength = 50,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c('10', '25', '50', '100', 'All')
          ),
          scrollX = TRUE,
          scrollY = "400px",
          fixedHeader = TRUE,
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

    ## Download Data View ----
    output$download_data <- downloadHandler(
      filename = function() {
        view_name <- input$data_view
        if (view_name == "") {
          view_name <- "data"
        }
        glue("{view_name}_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        data_to_download <- output_view_data()
        req(data_to_download, nrow(data_to_download) > 0)
        write_csv(data_to_download, file)
      }
    )
  })
}
