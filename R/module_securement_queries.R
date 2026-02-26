# UI ----
module_securement_queries_ui <- function(id) {
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
            ns("query_choice"),
            "Select query",
            choices = c(
              "",
              "Focal area properties",
              "Securement action"
            ),
            multiple = FALSE,
            width = "100%"
          ),
          # Conditional UI for additional inputs
          uiOutput(ns("conditional_query_ui")),
          actionButton(
            inputId = ns("run_query"),
            label = "Run query",
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
            h5("Query Result"),
            downloadButton(
              outputId = ns("download_query_data"),
              label = "Download",
              class = "btn-sm"
            )
          ),
          card_body(
            DTOutput(outputId = ns("view_df"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_securement_queries_server <- function(
  id,
  db_con,
  db_updated = NULL,
  focal_pids_rv
) {
  moduleServer(id, function(input, output, session) {
    iv_create <- InputValidator$new()
    iv_create$add_rule("closing_year", sv_required())
    iv_create$add_rule("securement_probability", sv_required())
    iv_create$enable()

    # Add this reactive value to track table clearing
    table_data <- reactiveVal(NULL)

    ## Conditional UI (based on query select) ----
    output$conditional_query_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Focal area properties") {
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
      } else if (input$query_choice == "Securement action") {
        div(
          selectizeInput(
            ns("closing_year"),
            "Select Closing Year",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select a closing year"
            )
          ),
          selectizeInput(
            ns("securement_probability"),
            "Select Securement Probability",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select a probability"
            )
          )
          # input_switch(
          #   ns("prop_view"),
          #   "Properties only",
          #   value = FALSE
          # )
        )
      } else {
        return(NULL)
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
        statement = glue_sql(
          "SELECT DISTINCT fa.internal_value 
          FROM focus_area_internal fa
          INNER JOIN properties props ON fa.id = props.focus_area_internal_id
          WHERE props.ownership_id NOT IN (11, 12, 13, 14)
          ORDER BY fa.internal_value;",
          .con = db_con
        )
      ) |>
        pull()
    })

    closing_year_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      # Query database for Closing Years
      c(
        dbGetQuery(
          conn = db_con,
          statement = "SELECT DISTINCT anticipated_closing_year FROM properties;"
        ) |>
          pull(),
        "Unassigned"
      )
    })

    securement_probability_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      # Query database for Securement Probability values

      dbGetQuery(
        conn = db_con,
        statement = "SELECT DISTINCT probability_value 
                     FROM securement_probability 
                     ORDER BY probability_value;"
      ) |>
        pull()
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

      updateSelectizeInput(
        session,
        inputId = "closing_year",
        choices = closing_year_reactive(),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        choices = securement_probability_reactive(),
        server = TRUE
      )
    })

    # Event :: Run query ----
    observeEvent(input$run_query, {
      req(input$query_choice)

      if (input$query_choice == "Focal area properties") {
        req(input$focal_area)

        col_name <- "Focus Area (Internal)"

        data <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT * FROM view_focal_areas_securement WHERE {`col_name`} IN ({input$focal_area*})",
            .con = db_con
          )
        )
      } else if (input$query_choice == "Securement action") {
        req(input$closing_year)
        req(input$securement_probability)

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
        additional_data <- dbGetQuery(
          conn = db_con,
          statement = '
          SELECT pr.property_name AS "Property Name", 
                 COALESCE(pr.anticipated_closing_year, \'Unassigned\') AS "Closing Year",
                 pr.anticipated_closing_date AS "Closing Date",
                 pr.securement_action_description AS "Securement Status",
                 se.probability_value AS "Securement Probability",
                 ph.phase_value AS "Phase"
          FROM properties pr
          LEFT JOIN securement_probability se ON pr.securement_probability_id = se.id
          LEFT JOIN phase ph ON pr.phase_id = ph.id
          WHERE se.probability_value IS NOT NULL;'
        )

        data <- data |>
          left_join(additional_data, join_by("Property Name"))

        data <- data |>
          relocate(
            "Property Name",
            "Closing Year",
            "Closing Date",
            "Securement Probability",
            "Phase",
            "Securement Status"
          )

        no_current_action <- additional_data |>
          filter(!`Property Name` %in% data$`Property Name`)

        data <- data |>
          bind_rows(no_current_action) |>
          arrange(`Property Name`)

        data <- data |>
          filter(`Securement Probability` %in% input$securement_probability) |>
          filter(`Closing Year` %in% input$closing_year) |>
          arrange(`Property Name`)
      }

      table_data(data)
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "query_choice",
        choices = c(
          "",
          "Focal area properties",
          "Securement action"
        ),
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

      updateSelectizeInput(
        session,
        inputId = "closing_year",
        choices = closing_year_reactive(),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        choices = securement_probability_reactive(),
        selected = character(0),
        server = TRUE
      )

      # Add this line to clear the table data
      table_data(NULL)

      # Clear reactive pids
      focal_pids_rv(NULL)
    })

    # Render data table ----

    output$view_df <- renderDT({
      req(table_data())

      # Convert character columns to factors to get select inputs
      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
        data_for_display,
        options = list(
          pageLength = 50,
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
        extensions = c("Buttons", "FixedHeader")
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
