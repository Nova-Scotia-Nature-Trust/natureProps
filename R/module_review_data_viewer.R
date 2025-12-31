# UI ----
module_review_data_viewer_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          width = 300,
          selectizeInput(
            ns("table_choice"),
            "Select table",
            choices = c("", "Properties", "Parcels"),
            multiple = FALSE,
            width = "100%"
          ),
          actionButton(
            inputId = ns("load_table"),
            label = "Load Table",
            width = "100%",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            width = "100%"
          )
        ),
        # Main layout - results card
        card(
          height = "100%",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            h5("Table View"),
            downloadButton(
              outputId = ns("download_table"),
              label = "Download",
              class = "btn-sm"
            )
          ),
          card_body(
            DTOutput(outputId = ns("data_table"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_review_data_viewer_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store table data
    table_data <- reactiveVal(NULL)

    # Event :: Load table ----
    observeEvent(input$load_table, {
      req(input$table_choice)
      if (!is.null(db_updated)) {
        db_updated()
      }
      if (input$table_choice == "Properties") {
        # Query properties table with securement probability lookup
        data <- dbGetQuery(
          db_con,
          "SELECT 
            p.property_name,
            tl.team_value,
            sp.probability_value,
            p.anticipated_closing_year,
            ph.phase_value
          FROM properties p
          LEFT JOIN securement_probability sp ON p.securement_probability_id = sp.id
          LEFT JOIN phase ph ON p.phase_id = ph.id
          LEFT JOIN team_lead tl ON p.team_lead_id = tl.id
          WHERE sp.probability_value IS NOT NULL
          ORDER BY p.property_name"
        ) |>
          rename(
            `Property Name` = property_name,
            `Team Lead` = team_value,
            `Securement Probability` = probability_value,
            `Anticipated Closing Year` = anticipated_closing_year,
            Phase = phase_value
          )
      } else if (input$table_choice == "Parcels") {
        # Query parcels table with ecological priority lookup
        data <- dbGetQuery(
          db_con,
          "SELECT 
            p.property_name,
            pa.pid,
            r1.ranking_value AS ecological_priority,
            r2.ranking_value AS securement_priority
          FROM parcels pa
          LEFT JOIN ranking r1 ON pa.priority_ecological_ranking_id = r1.id
          LEFT JOIN ranking r2 ON pa.priority_securement_ranking_id = r2.id
          LEFT JOIN properties p ON pa.property_id = p.id
          ORDER BY p.property_name, pa.pid"
        ) |>
          rename(
            `Property Name` = property_name,
            PID = pid,
            `Ecological Priority` = ecological_priority,
            `Securement Priority` = securement_priority
          )
      }

      table_data(data)
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "table_choice",
        choices = c("", "Properties", "Parcels"),
        selected = character(0),
        server = TRUE
      )

      # Clear the table data
      table_data(NULL)
    })

    # Render data table ----

    output$data_table <- renderDT({
      req(table_data())

      # Convert character columns to factors for select inputs
      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
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

    ## Download handler ----
    output$download_table <- downloadHandler(
      filename = function() {
        table_name <- input$table_choice
        if (table_name == "") {
          table_name <- "table_data"
        }
        # Clean the table name for filename
        table_name <- str_replace_all(table_name, " ", "_") |>
          str_to_lower()
        glue("{table_name}_{format(Sys.Date(), '%Y%m%d')}.csv")
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
