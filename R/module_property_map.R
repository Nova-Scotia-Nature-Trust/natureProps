# R/module_property_map.R

# UI ----
module_property_map_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      selectizeInput(
        ns("property"),
        h5("Select property"),
        choices = NULL,
        multiple = FALSE
      ),
      actionButton(
        ns("load_record"),
        "Load Record",
        class = "btn-primary"
      ),
      actionButton(
        ns("reset_view"),
        "Reset Map View",
        class = "btn-secondary"
      ),
      hr(),
      h5("Ecological Priority Legend"),
      htmlOutput(ns("priority_legend"))
    ),
    card(
      full_screen = TRUE,
      card_header("Property Map"),
      card_body(
        leafletOutput(ns("map"), height = "600px")
      )
    )
  )
}

# Server ----
module_property_map_server <- function(id, db_con, gis_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }

      dbGetQuery(db_con, "SELECT property_name FROM properties;") |>
        pull(property_name) |>
        sort()
    })

    ## Update select input with property names
    observe({
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive :: Get all parcels with property info ----
    all_parcels_data <- reactive({
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }

      query <- glue_sql(
        "
        SELECT 
          prop.property_name,
          prop.property_description,
          tl.team_value as team_lead,
          ph.phase_value as phase,
          par.pid,
          ra.ranking_value AS ecological_priority,
          par.securement_action_description
        FROM parcels par
        LEFT JOIN properties prop ON par.property_id = prop.id
        LEFT JOIN team_lead tl ON prop.team_lead_id = tl.id
        LEFT JOIN phase ph ON prop.phase_id = ph.id
        LEFT JOIN ranking ra ON par.priority_ecological_ranking_id = ra.id;
        ",
        .con = db_con
      )

      dbGetQuery(db_con, query)
    })

    ## Reactive :: Get spatial data for all parcels ----
    parcels_sf <- reactive({
      req(all_parcels_data())

      # Get all PIDs from property manager database
      all_pids <- all_parcels_data() |>
        pull(pid) |>
        unique()

      # Query GIS database for spatial data matching these PIDs
      pid_list <- paste0("'", all_pids, "'", collapse = ", ")

      query <- glue(
        "
        SELECT pid, geom
        FROM parcels
        WHERE pid IN ({pid_list});
        "
      )

      # Join spatial data with parcel attributes
      st_read(gis_con, query = query) |>
        left_join(all_parcels_data(), by = "pid")
    })

    ## Color palette for ecological priority ----
    priority_pal <- reactive({
      req(parcels_sf())

      # Define the order of priority levels
      priority_levels <- c("Very High", "High", "Medium", "Low", "Very Low")

      # Create factor with explicit levels
      parcels_data <- parcels_sf() |>
        mutate(
          ecological_priority = factor(
            ecological_priority,
            levels = priority_levels
          )
        )

      colorFactor(
        palette = c(
          "Very High" = "#a50026",
          "High" = "#d73027",
          "Medium" = "#fee090",
          "Low" = "#91bfdb",
          "Very Low" = "#4575b4"
        ),
        domain = parcels_data$ecological_priority,
        na.color = "#808080" # Gray for missing values
      )
    })

    ## Render priority legend ----
    output$priority_legend <- renderUI({
      tags$div(
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$span(
            style = "background-color: #a50026; width: 20px; height: 20px; display: inline-block; margin-right: 8px; border: 1px solid #333;"
          ),
          tags$span("Very High")
        ),
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$span(
            style = "background-color: #d73027; width: 20px; height: 20px; display: inline-block; margin-right: 8px; border: 1px solid #333;"
          ),
          tags$span("High")
        ),
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$span(
            style = "background-color: #fee090; width: 20px; height: 20px; display: inline-block; margin-right: 8px; border: 1px solid #333;"
          ),
          tags$span("Medium")
        ),
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$span(
            style = "background-color: #91bfdb; width: 20px; height: 20px; display: inline-block; margin-right: 8px; border: 1px solid #333;"
          ),
          tags$span("Low")
        ),
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$span(
            style = "background-color: #4575b4; width: 20px; height: 20px; display: inline-block; margin-right: 8px; border: 1px solid #333;"
          ),
          tags$span("Very Low")
        ),
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$span(
            style = "background-color: #808080; width: 20px; height: 20px; display: inline-block; margin-right: 8px; border: 1px solid #333;"
          ),
          tags$span("Not Assigned")
        )
      )
    })

    ## Render base map ----
    output$map <- renderLeaflet({
      req(parcels_sf())

      # Define the order of priority levels
      priority_levels <- c("Very High", "High", "Medium", "Low", "Very Low")

      parcels <- parcels_sf() |>
        mutate(
          ecological_priority = factor(
            ecological_priority,
            levels = priority_levels
          )
        )

      leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
        addProviderTiles(
          providers$Esri.WorldGrayCanvas,
          group = "Gray Canvas"
        ) |>
        addLayersControl(
          baseGroups = c("Imagery", "Topo", "Gray Canvas"),
          options = layersControlOptions(collapsed = TRUE)
        ) |>
        addPolygons(
          data = parcels,
          fillColor = ~ priority_pal()(ecological_priority),
          fillOpacity = 0.7,
          color = "white",
          weight = 1.5,
          label = ~ glue(
            "{property_name} - PID: {pid} (Priority: {coalesce(as.character(ecological_priority), 'N/A')})"
          ),
          labelOptions = labelOptions(
            style = list("font-size" = "14px", "font-weight" = "bold"),
            direction = "auto"
          ),
          popup = ~ paste(
            "<div style='font-size: 14px;'>",
            "<b>Property Name:</b>",
            property_name,
            "<br>",
            "<b>PID:</b>",
            pid,
            "<br>",
            "<b>Ecological Priority:</b>",
            coalesce(as.character(ecological_priority), "Not assigned"),
            "<br>",
            "<b>Phase:</b>",
            coalesce(phase, "N/A"),
            "<br>",
            "<b>Team Lead:</b>",
            coalesce(team_lead, "N/A"),
            "<br>",
            "<b>Securement Status:</b>",
            coalesce(securement_action_description, "N/A"),
            "<br>",
            "<b>Description:</b>",
            coalesce(property_description, "N/A"),
            "<br>",
            "</div>"
          ),
          popupOptions = popupOptions(maxWidth = 500, minWidth = 300),
          highlight = highlightOptions(
            weight = 3,
            color = "yellow",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          layerId = ~pid
        ) |>
        setView(lng = -63.36, lat = 45.21, zoom = 8)
    })

    ## Zoom to selected property ----
    observeEvent(input$load_record, {
      req(input$property)
      req(input$property != "")
      req(parcels_sf())

      # Get all parcels for the selected property
      target_parcels <- parcels_sf() |>
        filter(property_name == input$property)

      if (nrow(target_parcels) > 0) {
        # Calculate bounding box and fit to bounds instead
        bbox <- st_bbox(target_parcels)

        leafletProxy(ns("map")) |>
          fitBounds(
            lng1 = bbox[["xmin"]],
            lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]],
            lat2 = bbox[["ymax"]]
          )
      }
    })

    ## Reset map view ----
    observeEvent(input$reset_view, {
      leafletProxy(ns("map")) |>
        setView(lng = -63.36, lat = 45.21, zoom = 8)

      updateSelectizeInput(session, "property", selected = "")
    })
  })
}
