# UI ----
module_project_review_queries_ui <- function(id) {
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
              "Phase & Focus Area",
              "Focus Area Chart"
            ),
            multiple = FALSE,
            width = "100%"
          ),
          # Conditional UI for additional inputs
          uiOutput(ns("conditional_filters_ui")),
          # Conditional UI for query action buttons (Phase & Focus Area auto-refreshes)
          uiOutput(ns("query_actions_ui"))
        ),
        # Main layout - results card
        card(
          height = "100%",
          card_header(h5("Query Result")),
          card_body(
            uiOutput(ns("results_ui"))
          )
        )
      )
    )
  )
}

# Server ----
module_project_review_queries_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Add reactive value to track plot data
    plot_data <- reactiveVal(NULL)
    # Add reactive value to track table data
    table_data <- reactiveVal(NULL)
    # Add reactive value to track if query has been run
    query_run <- reactiveVal(FALSE)
    # Track the previous focus area selection, to resolve "All" vs. specific picks
    prev_focus_area <- reactiveVal("all")

    ## Results UI (plot vs. table, based on query select) ----
    output$results_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Phase & Focus Area") {
        DTOutput(outputId = ns("view_df"), height = "100%")
      } else {
        plotOutput(outputId = ns("view_plot"), height = "100%")
      }
    })

    ## Conditional UI (based on query select) ----
    output$conditional_filters_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Focus Area Chart") {
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
      } else if (input$query_choice == "Phase & Focus Area") {
        div(
          selectizeInput(
            inputId = ns("phase"),
            label = "Phase",
            choices = NULL,
            multiple = TRUE
          ),
          selectizeInput(
            inputId = ns("focus_area"),
            label = "Focus Area",
            choices = NULL,
            multiple = TRUE
          ),
          input_switch(
            ns("contacts_only"),
            "Only display properties with a contact",
            value = FALSE
          )
        )
      } else {
        return(NULL)
      }
    })

    ## Query action buttons (Run query only applies to Focus Area Chart) ----
    output$query_actions_ui <- renderUI({
      ns <- session$ns

      tagList(
        if (identical(input$query_choice, "Focus Area Chart")) {
          actionButton(
            inputId = ns("run_query"),
            label = "Run query",
            width = "100%",
            class = "btn-primary"
          )
        },
        actionButton(
          inputId = ns("clear_inputs"),
          label = "Clear Inputs",
          width = "100%"
        )
      )
    })

    ## Securement probability reactive ----
    securement_probability_reactive <- reactive({
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

    ## Phase value reactive ----
    phase_choices <- reactive({
      dbGetQuery(db_con, "SELECT * FROM phase")
    })

    ## Focus area value reactive ----
    focus_area_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT * FROM focus_area_internal ORDER BY internal_value"
      )
    })

    ## Initialize inputs ----
    observe({
      req(input$query_choice)

      if (input$query_choice == "Focus Area Chart") {
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
      } else if (input$query_choice == "Phase & Focus Area") {
        updateSelectizeInput(
          session,
          inputId = "phase",
          choices = setNames(
            phase_choices()$id,
            phase_choices()$phase_value
          ),
          selected = character(0),
          server = TRUE
        )

        # "All" represents every focus area; default selection on load
        updateSelectizeInput(
          session,
          inputId = "focus_area",
          choices = c(
            "All" = "all",
            setNames(
              focus_area_choices()$id,
              focus_area_choices()$internal_value
            )
          ),
          selected = "all",
          server = TRUE
        )
        prev_focus_area("all")
      }
    })

    # Event :: Focus area selection changed ("All" and specific picks are mutually exclusive) ----
    observeEvent(input$focus_area, {
      req(input$query_choice == "Phase & Focus Area")

      current <- input$focus_area
      previous <- prev_focus_area()

      if (length(current) == 0) {
        # Nothing selected - fall back to "All"
        updateSelectizeInput(session, "focus_area", selected = "all")
        prev_focus_area("all")
        return()
      }

      if (length(current) > 1 && "all" %in% current) {
        new_selection <- if ("all" %in% previous) {
          # User picked a specific area while "All" was selected - drop "All"
          setdiff(current, "all")
        } else {
          # User picked "All" while specific areas were selected - drop the rest
          "all"
        }

        updateSelectizeInput(session, "focus_area", selected = new_selection)
        prev_focus_area(new_selection)
        return()
      }

      prev_focus_area(current)
    })

    # Event :: Run query ----
    observeEvent(input$run_query, {
      req(input$query_choice)

      if (input$query_choice == "Focus Area Chart") {
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

    # Event :: Phase & Focus Area inputs changed (auto-refresh table, no run button) ----
    observeEvent(
      list(input$phase, input$focus_area, input$contacts_only),
      {
        req(input$query_choice == "Phase & Focus Area")

        if (length(input$phase) == 0 || length(input$focus_area) == 0) {
          table_data(NULL)
          return()
        }

        # Query property details, size, contacts, and last communication for selected phase(s)
        query <- glue_sql(
          "WITH property_sizes AS (
            SELECT
              pa.property_id,
              ROUND(SUM(COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471)), 2) AS acres,
              ROUND(SUM(COALESCE(pa.size_confirmed_ha, pi.area_ha)), 2) AS hectares
            FROM parcels pa
            LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
            GROUP BY pa.property_id
          ),
          property_contacts AS (
            SELECT
              ppc.property_id,
              STRING_AGG(
                DISTINCT con.name_first || ' ' || con.name_last, ', '
                ORDER BY con.name_first || ' ' || con.name_last
              ) AS contact_names
            FROM properties_contact ppc
            LEFT JOIN property_contact_details con ON ppc.property_contact_id = con.id
            GROUP BY ppc.property_id
          ),
          last_comm AS (
            SELECT property_id, MAX(date_contacted) AS last_contact_date
            FROM property_contact_communication
            GROUP BY property_id
          ),
          acquisition_types AS (
            SELECT
              pa.property_id,
              STRING_AGG(
                DISTINCT at.acquisition_value, ', '
                ORDER BY at.acquisition_value
              ) AS acquisition_value
            FROM parcels pa
            LEFT JOIN acquisition_type at ON pa.acquisition_type_id = at.id
            GROUP BY pa.property_id
          )
          SELECT
            p.property_name AS \"Property Name\",
            ph.phase_value AS \"Phase\",
            p.phase_id_description AS \"Phase Description\",
            fa.internal_value AS \"Focus Area\",
            ps.acres AS \"Size (acres)\",
            ps.hectares AS \"Size (ha)\",
            pc.contact_names AS \"Property Contacts\",
            lc.last_contact_date AS \"Last Contact Date\",
            ac.acquisition_value AS \"Acquisition Type\"
          FROM properties p
          LEFT JOIN phase ph ON p.phase_id = ph.id
          LEFT JOIN focus_area_internal fa ON p.focus_area_internal_id = fa.id
          LEFT JOIN property_sizes ps ON ps.property_id = p.id
          LEFT JOIN property_contacts pc ON pc.property_id = p.id
          LEFT JOIN last_comm lc ON lc.property_id = p.id
          LEFT JOIN acquisition_types ac ON ac.property_id = p.id
          WHERE p.phase_id IN ({phase_ids*})
          ORDER BY p.property_name;",
          phase_ids = input$phase,
          .con = db_con
        )

        data <- dbGetQuery(conn = db_con, statement = query) |>
          as_tibble()

        if (!("all" %in% input$focus_area)) {
          focus_area_labels <- focus_area_choices() |>
            filter(id %in% as.integer(input$focus_area)) |>
            pull(internal_value)

          data <- data |>
            filter(`Focus Area` %in% focus_area_labels)
        }

        if (isTRUE(input$contacts_only)) {
          data <- data |>
            filter(!is.na(`Property Contacts`))
        }

        table_data(data)
      },
      ignoreInit = TRUE
    )

    # Auto-update plot when inputs change (after first run)
    observeEvent(
      list(input$securement_probability, input$closing_year),
      {
        req(query_run(), input$query_choice == "Focus Area Chart")
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
          "Phase & Focus Area",
          "Focus Area Chart"
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

      updateSelectizeInput(
        session,
        inputId = "phase",
        choices = setNames(
          phase_choices()$id,
          phase_choices()$phase_value
        ),
        selected = character(0),
        server = TRUE
      )

      # Reset focus area back to the "All" default
      updateSelectizeInput(
        session,
        inputId = "focus_area",
        choices = c(
          "All" = "all",
          setNames(
            focus_area_choices()$id,
            focus_area_choices()$internal_value
          )
        ),
        selected = "all",
        server = TRUE
      )
      prev_focus_area("all")

      # Reset query_run flag
      query_run(FALSE)

      # Clear plot and table data
      plot_data(NULL)
      table_data(NULL)
    })

    # Render data table ----
    output$view_df <- renderDT({
      req(table_data())

      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
        data_for_display,
        options = list(
          pageLength = 50,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c("10", "25", "50", "100", "All")
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
        extensions = c("Buttons"),
        fillContainer = TRUE
      )
    })

    # Render plot ----
    output$view_plot <- renderCachedPlot(
      {
        req(plot_data())

        chart_data <- plot_data() |>
          count(focal_area, name = "property_count") |>
          arrange(desc(property_count))

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
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            panel.grid.major.y = element_blank()
          )
      },
      cacheKeyExpr = {
        list(plot_data())
      }
    )
  })
}
