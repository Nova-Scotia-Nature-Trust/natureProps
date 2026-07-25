# UI ----
module_eco_highlights_ui <- function(id) {
  ns <- NS(id)

  ## Custom CSS for value boxes ----
  tagList(
    tags$style(
      HTML(
        "
        .eco-feature-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
          gap: 18px;
        }

        .eco-card {
          display: flex;
          align-items: center;
          gap: 16px;
          padding: 18px 20px;
          border-radius: 16px;
          background: linear-gradient(135deg, #f7f9fa, #ffffff);
          box-shadow: 0 1px 4px rgba(0,0,0,0.08);
          transition: transform 0.15s ease, box-shadow 0.15s ease;
        }
        .eco-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 4px 16px rgba(0,0,0,0.15);
        }

        .eco-card .eco-icon {
          font-size: 42px;
          opacity: 0.8;
        }

        .eco-card .eco-value {
          font-size: 1.6em;
          font-weight: 600;
          margin: 0;
          line-height: 1.1;
        }

        .eco-card .eco-title {
          margin: 0;
          color: #607080;
          font-size: 0.9em;
          font-weight: 500;
          letter-spacing: 0.3px;
        }

        /* Custom styles for full-height data table */
        .sar-table-container {
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        
        .sar-table-container .dataTables_wrapper {
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        
        .sar-table-container .dataTables_scroll {
          flex: 1;
          display: flex;
          flex-direction: column;
        }
        
        .sar-table-container .dataTables_scrollBody {
          flex: 1;
          overflow-y: auto !important;
        }

        /* Dark mode adjustments */
        [data-bs-theme='dark'] .eco-card .eco-value {
          color: #495057;
        }
        [data-bs-theme='dark'] .eco-card .eco-title {
          color: #6c757d;
        }
      "
      )
    ),
    ## Sidebar layout ----
    div(
      style = "height: 100%; display: flex; flex-direction: column;",
      card(
        full_screen = TRUE,
        height = "100%",
        layout_sidebar(
          sidebar = sidebar(
            open = TRUE,
            width = 300,
            radioButtons(
              ns("property_filter"),
              "Property List",
              choices = c(
                "Nature Trust Lands" = "nt_lands",
                "All" = "all"
              ),
              selected = "nt_lands"
            ),
            selectizeInput(
              ns("property"),
              "Select Property",
              choices = NULL,
              multiple = FALSE,
              width = "100%"
            ),
            actionButton(
              inputId = ns("clear_selection"),
              label = "Clear Selection",
              width = "100%"
            )
          ),
          ## Card :: Eco Highlights ----
          card(
            height = "100%",
            card_header(
              h5(textOutput(ns("property_title")))
            ),
            card_body(
              style = "height: 100%; display: flex; flex-direction: column;",
              uiOutput(ns("eco_highlights_content"))
            )
          )
        )
      )
    )
  )
}

# Server ----
module_eco_highlights_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ## Reactive Values :: Eco Data & Prop Name ----
    eco_data <- reactiveVal(NULL)
    property_name <- reactiveVal(NULL)

    ## Reactive :: Property List ----
    property_list <- reactive({
      req(input$property_filter)
      db_updated()
      properties <- dbGetQuery(
        db_con,
        "SELECT DISTINCT id, internal_record_id, property_name, property_name_public FROM properties 
         ORDER BY property_name;"
      )

      if (input$property_filter == "nt_lands") {
        return(
          properties |>
            filter(str_detect(internal_record_id, "NT")) |>
            select(-internal_record_id, -property_name) |>
            rename(property_name = property_name_public) |>
            arrange(property_name)
        )
      } else {
        return(
          properties |>
            select(-internal_record_id, -property_name_public) |>
            arrange(property_name)
        )
      }
    })

    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0),
        server = TRUE
      )
    })

    ## Event :: Clear on Filter Change ----
    observeEvent(input$property_filter, {
      updateSelectizeInput(session, "property", selected = character(0))
      eco_data(NULL)
      property_name(NULL)
    })

    ## Reactive :: PID List ----
    pid_list <- reactive({
      req(input$property)
      dbGetQuery(
        db_con,
        query <- glue_sql(
          "
        SELECT pa.pid
        FROM properties pr
        JOIN parcels pa ON pr.id = pa.property_id
        WHERE pr.id = {input$property};
        ",
          .con = db_con
        )
      ) |>
        pull(pid)
    })
    ## Reactive :: SCC Point Data ----
    scc_data <- reactive({
      req(input$property)

      query <- glue_sql(
        'SELECT DISTINCT
          s.comname AS "Common Name",
          s.sciname AS "Scientific Name",
          s.srank AS "S-Rank"
        FROM
          parcels AS pa
          JOIN sar_rare AS s ON ST_Intersects (s.geom, pa.geom)
        WHERE
          pa.pid IN ({pid_list()*})
        ORDER BY
          s.comname;',
        .con = gis_con
      )

      dbGetQuery(gis_con, query)
    })

    ## Output :: SCC Table ----
    output$sar_table <- DT::renderDataTable({
      req(scc_data())

      DT::datatable(
        scc_data(),
        options = list(
          pageLength = -1, # Show all rows
          dom = 't', # Only show table (remove pagination)
          scrollY = TRUE, # Enable vertical scrolling
          scrollCollapse = FALSE,
          searching = FALSE,
          info = FALSE,
          paging = FALSE
        ),
        rownames = FALSE,
        class = 'compact stripe hover'
      )
    })

    ## Event :: Load Highlights on Property Selection ----
    observe({
      req(input$property)

      name <- property_list() |>
        filter(id == input$property) |>
        pull(property_name)

      property_name(name)

      total_shoreline_length <- query_shoreline(pid_list(), gis_con) |>
        pull(shoreline_length) |>
        sum()
      total_coastline_length <- query_coastline(pid_list(), gis_con) |>
        pull(coastline_length) |>
        sum()
      total_old_growth_area <- query_old_growth_forest(pid_list(), gis_con) |>
        pull(old_growth_forest_area) |>
        sum()
      total_karst_forest_area <- query_karst_forest(pid_list(), gis_con) |>
        pull(karst_forest_area) |>
        sum()
      waterbird_colony_count <- query_bird_colony(pid_list(), gis_con)
      waterbird_colony_count <- length(unique(
        waterbird_colony_count$waterbird_colony_id
      ))

      # Run queries
      eco_data(tibble(
        total_coastline_length = total_coastline_length,
        total_shoreline_length = total_shoreline_length,
        total_karst_forest_area = total_karst_forest_area,
        total_old_growth_area = total_old_growth_area,
        waterbird_colony_count = waterbird_colony_count
      ))
    })

    ## Output :: Card Title ----
    output$property_title <- renderText({
      if (is.null(property_name())) {
        "Ecological Highlights"
      } else {
        paste("Ecological Highlights:", property_name())
      }
    })

    ## Ecological Feature Cards Function ----
    feature_card <- function(
      title,
      value,
      icon,
      unit = NULL,
      show_if_zero = FALSE
    ) {
      # Hide card if NA or zero (unless explicitly allowed)
      if (is.na(value) || (!show_if_zero && value <= 0)) {
        return(NULL)
      }

      # Format numeric values and attach unit
      formatted_value <- if (is.numeric(value)) {
        paste0(
          format(round(value, 2), big.mark = ","),
          if (!is.null(unit)) paste0(" ", unit)
        )
      } else {
        value
      }

      div(
        class = "eco-card",
        div(class = "eco-icon", bs_icon(icon)),
        div(
          h3(class = "eco-value", formatted_value),
          p(class = "eco-title", title)
        )
      )
    }

    ## Output :: Feature Cards & SCC Table ----
    output$eco_highlights_content <- renderUI({
      data <- eco_data()

      if (is.null(data)) {
        return(
          div(
            style = "display:flex;height:100%;align-items:center;justify-content:center;",
            p(
              "Select a property to view ecological highlights.",
              style = "color:#6c757d;font-size:1.1em;"
            )
          )
        )
      }

      layout_columns(
        col_widths = c(4, 8),
        style = "height: 100%;",

        # Left column: Feature cards
        div(
          class = "eco-feature-grid",

          feature_card(
            "Coastline Length",
            data$total_coastline_length,
            "tsunami",
            unit = "m"
          ),

          feature_card(
            "Shoreline Length",
            data$total_shoreline_length,
            "water",
            unit = "m"
          ),

          feature_card(
            "Karst Forest Area",
            data$total_karst_forest_area,
            "tree-fill",
            unit = "ha"
          ),

          feature_card(
            "Old-Growth Forest Area",
            data$total_old_growth_area,
            "tree",
            unit = "ha"
          ),

          feature_card(
            "Waterbird Colonies",
            data$waterbird_colony_count,
            "feather",
            show_if_zero = FALSE
          )
        ),

        # Right column: SCC species table
        card(
          height = "100%",
          card_header("Species of Conservation Concern"),
          card_body(
            class = "sar-table-container",
            style = "height: 100%; padding: 0;",
            div(
              style = "height: 100%; padding: 1rem;",
              DT::dataTableOutput(session$ns("sar_table"), height = "100%")
            )
          )
        )
      )
    })

    ## Event :: Clear Selection ----
    observeEvent(input$clear_selection, {
      updateSelectizeInput(session, "property", selected = character(0))
      eco_data(NULL)
      property_name(NULL)
    })
  })
}
