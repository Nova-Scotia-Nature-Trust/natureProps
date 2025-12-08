# UI ----
module_review_queries_ui <- function(id) {
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
              "Focal area chart"
            ),
            multiple = FALSE,
            width = "100%"
          ),
          # Conditional UI for additional inputs
          uiOutput(ns("conditional_filters_ui")),
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
          card_header(h5("Query Result")),
          card_body(
            plotOutput(outputId = ns("view_plot"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_review_queries_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Add reactive value to track plot data
    plot_data <- reactiveVal(NULL)
    # Add reactive value to track if query has been run
    query_run <- reactiveVal(FALSE)

    ## Conditional UI (based on query select) ----
    output$conditional_filters_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Focal area chart") {
        div(
          selectizeInput(
            ns("securement_probability"),
            "Securement probability",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select securement probability"
            )
          ),
          selectizeInput(
            ns("closing_year"),
            "Anticipated Closing Year",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select closing year"
            )
          )
        )
      } else {
        return(NULL)
      }
    })

    ## Securement probability reactive ----
    securement_probability_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      # Query database for distinct securement probability values
      dbGetQuery(db_con, "SELECT * FROM securement_probability") |>
        select(probability_value, id) |>
        deframe()
    })

    ## Closing year reactive ----
    closing_year_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      # Query database for distinct closing years
      dbGetQuery(
        conn = db_con,
        statement = "SELECT DISTINCT anticipated_closing_year 
                     FROM properties 
                     WHERE anticipated_closing_year IS NOT NULL
                     ORDER BY anticipated_closing_year;"
      ) |>
        pull()
    })

    ## Initialize inputs ----
    observe({
      req(input$query_choice)

      if (input$query_choice == "Focal area chart") {
        securement_choices <- securement_probability_reactive()
        closing_year_choices <- closing_year_reactive()

        updateSelectizeInput(
          session,
          inputId = "securement_probability",
          choices = securement_choices,
          selected = securement_choices,
          server = TRUE
        )

        updateSelectizeInput(
          session,
          inputId = "closing_year",
          choices = closing_year_choices,
          selected = closing_year_choices,
          server = TRUE
        )
      }
    })

    # Event :: Run query ----
    observeEvent(input$run_query, {
      req(input$query_choice)

      if (input$query_choice == "Focal area chart") {
        req(input$securement_probability, input$closing_year)

        # Mark that query has been run at least once
        query_run(TRUE)

        # Query properties with focal area, securement probability, and closing year
        query <- glue_sql(
          "SELECT 
            fa.internal_value AS focal_area,
            prop.securement_probability_id,
            prop.anticipated_closing_year
          FROM properties prop
          INNER JOIN focus_area_internal fa ON prop.focus_area_internal_id = fa.id
          WHERE prop.securement_probability_id IN ({securement_prob*})
            AND prop.anticipated_closing_year IN ({closing_yr*})
            AND prop.securement_probability_id IS NOT NULL
            AND prop.anticipated_closing_year IS NOT NULL;",
          securement_prob = input$securement_probability,
          closing_yr = input$closing_year,
          .con = db_con
        )

        data <- dbGetQuery(conn = db_con, statement = query) |>
          as_tibble()

        plot_data(data)
      }
    })

    # Auto-update plot when inputs change (after first run)
    observeEvent(
      list(input$securement_probability, input$closing_year),
      {
        req(query_run(), input$query_choice == "Focal area chart")
        req(input$securement_probability, input$closing_year)

        # Query properties with focal area, securement probability, and closing year
        query <- glue_sql(
          "SELECT 
            fa.internal_value AS focal_area,
            prop.securement_probability_id,
            prop.anticipated_closing_year
          FROM properties prop
          INNER JOIN focus_area_internal fa ON prop.focus_area_internal_id = fa.id
          WHERE prop.securement_probability_id IN ({securement_prob*})
            AND prop.anticipated_closing_year IN ({closing_yr*})
            AND prop.securement_probability_id IS NOT NULL
            AND prop.anticipated_closing_year IS NOT NULL;",
          securement_prob = input$securement_probability,
          closing_yr = input$closing_year,
          .con = db_con
        )

        data <- dbGetQuery(conn = db_con, statement = query) |>
          as_tibble()

        plot_data(data)
      },
      ignoreInit = TRUE
    )

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "query_choice",
        choices = c(
          "",
          "Focal area chart"
        ),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        choices = securement_probability_reactive(),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "closing_year",
        choices = closing_year_reactive(),
        selected = character(0),
        server = TRUE
      )

      # Reset query_run flag
      query_run(FALSE)

      # Clear plot data
      plot_data(NULL)
    })

    # Render plot ----
    output$view_plot <- renderPlot({
      req(plot_data())

      # Count properties by focal area
      chart_data <- plot_data() |>
        count(focal_area, name = "property_count") |>
        arrange(desc(property_count))

      # Create bar plot
      ggplot(
        chart_data,
        aes(x = reorder(focal_area, property_count), y = property_count)
      ) +
        geom_col(fill = "#2C3E50", alpha = 0.8) +
        coord_flip() +
        labs(
          title = "Properties by Focal Area",
          x = "Focal Area",
          y = "Number of Properties"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major.y = element_blank()
        )
    })
  })
}
