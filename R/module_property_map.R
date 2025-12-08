# R/module_property_map.R

# UI ----
module_property_map_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Property & Parcel Navigation",
          icon = bsicons::bs_icon("geo-alt"),
          selectizeInput(
            ns("property"),
            label = "Property",
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Select property"
            )
          ),
          actionButton(
            ns("load_record"),
            "Load Record",
            class = "btn-primary"
          ),
          hr(),
          selectizeInput(
            ns("parcel"),
            label = "Parcel",
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Select parcel"
            )
          ),
          actionButton(
            ns("load_parcel"),
            "Load Parcel",
            class = "btn-primary"
          )
        ),
        accordion_panel(
          title = "NSPRD Data",
          icon = bsicons::bs_icon("database"),
          actionButton(
            ns("get_bounds"),
            "Load NSPRD parcels",
            class = "btn-primary"
          )
        ),
        accordion_panel(
          title = "Priority Ranking Legend",
          icon = bsicons::bs_icon("palette"),
          htmlOutput(ns("priority_legend"))
        )
      ),
      hr(),
      actionButton(
        ns("reset_view"),
        "Reset Map View",
        class = "btn-secondary"
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Property Map"),
      card_body(
        shinycssloaders::withSpinner(
          leafletOutput(ns("map"), height = "600px"),
          type = 4,
          color = "#0d51c5ff"
        )
      )
    )
  )
}

# Server ----
module_property_map_server <- function(id, db_con, gis_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message(
      "[",
      format(Sys.time(), "%H:%M:%OS3"),
      "] === MODULE INITIALIZED ==="
    )

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      start_time <- Sys.time()

      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }

      result <- dbGetQuery(db_con, "SELECT property_name FROM properties;") |>
        pull(property_name) |>
        sort()

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] property_choices completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )

      result
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

    ## Reactive :: Parcel choices ----
    parcel_choices <- reactive({
      start_time <- Sys.time()

      if (!is.null(db_updated)) {
        db_updated()
      }

      result <- dbGetQuery(db_con, "SELECT pid FROM parcels ORDER BY pid;") |>
        pull(pid)

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] parcel_choices completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )

      result
    })

    ## Update select input with PIDs
    observe({
      updateSelectizeInput(
        session,
        inputId = "parcel",
        choices = c("", parcel_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive :: Get all parcels with property info ----
    all_parcels_data <- reactive({
      start_time <- Sys.time()

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
          ra_eco.ranking_value AS ecological_priority,
          ra_sec.ranking_value AS securement_priority
        FROM parcels par
        LEFT JOIN properties prop ON par.property_id = prop.id
        LEFT JOIN team_lead tl ON prop.team_lead_id = tl.id
        LEFT JOIN phase ph ON prop.phase_id = ph.id
        LEFT JOIN ranking ra_eco ON par.priority_ecological_ranking_id = ra_eco.id
        LEFT JOIN ranking ra_sec ON par.priority_securement_ranking_id = ra_sec.id;
        ",
        .con = db_con
      )

      result <- dbGetQuery(db_con, query)

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] all_parcels_data completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )

      result
    })

    ## Reactive :: Get spatial data for all parcels ----
    parcels_sf <- reactive({
      start_time <- Sys.time()

      req(all_parcels_data())

      all_pids <- all_parcels_data() |>
        pull(pid) |>
        unique()

      query <- glue_sql(
        "
        SELECT pid, geom
        FROM parcels
        WHERE pid IN ({all_pids*});
        ",
        .con = gis_con
      )

      # Define priority levels once
      priority_levels <- c("Very High", "High", "Medium", "Low", "Very Low")

      # Join and transform in one step
      data <- st_read(gis_con, query = query) |>
        left_join(all_parcels_data(), join_by(pid)) |>
        mutate(
          ecological_priority = factor(
            ecological_priority,
            levels = priority_levels
          ),
          securement_priority = factor(
            securement_priority,
            levels = priority_levels
          )
        )

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] parcels_sf TOTAL completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )
      return(data)
    })

    ## Color palettes (now simpler) ----
    priority_pal <- colorFactor(
      palette = c("#a50026", "#d73027", "#fee090", "#91bfdb", "#4575b4"),
      levels = c("Very High", "High", "Medium", "Low", "Very Low"),
      ordered = TRUE,
      na.color = "#808080"
    )

    securement_pal <- colorFactor(
      palette = c("#a50026", "#d73027", "#fee090", "#91bfdb", "#4575b4"),
      levels = c("Very High", "High", "Medium", "Low", "Very Low"),
      ordered = TRUE,
      na.color = "#808080"
    )

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
      start_time <- Sys.time()
      message("[", format(Sys.time(), "%H:%M:%OS3"), "] Starting map render")

      req(parcels_sf())

      map <- leaflet() |>
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
          data = parcels_sf(),
          fillColor = ~ priority_pal(ecological_priority),
          fillOpacity = 0.7,
          color = ~ securement_pal(securement_priority),
          weight = 3,
          label = ~ glue(
            "{property_name} - PID: {pid}"
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
            "<b>Securement Priority:</b>",
            coalesce(as.character(securement_priority), "Not assigned"),
            "<br>",
            "<b>Phase:</b>",
            coalesce(as.character(phase), "Not assigned"),
            "<br>",
            "<b>Team Lead:</b>",
            coalesce(as.character(team_lead), "Not assigned"),
            "<br>",
            "<b>Property Description:</b>",
            coalesce(property_description, "N/A"),
            "<br>",
            "</div>"
          ),
          popupOptions = popupOptions(maxWidth = 500, minWidth = 300),
          highlight = highlightOptions(
            weight = 5,
            color = "yellow",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) |>
        setView(lng = -63.36, lat = 45.21, zoom = 8)

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] Map render TOTAL completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )

      map
    })

    ## Event :: Zoom to selected property ----
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

    ## Event :: Zoom to selected parcel ----
    observeEvent(input$load_parcel, {
      req(input$parcel)
      req(input$parcel != "")
      req(parcels_sf())

      target_parcel <- parcels_sf() |>
        filter(pid == input$parcel)

      if (nrow(target_parcel) > 0) {
        bbox <- st_bbox(target_parcel)

        leafletProxy(ns("map")) |>
          fitBounds(
            lng1 = bbox[["xmin"]],
            lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]],
            lat2 = bbox[["ymax"]]
          )
      }
    })
    ## Event :: Get bounding box ----
    observeEvent(input$get_bounds, {
      req(input$map_bounds)

      bounds <- input$map_bounds

      # Extract the bounding box coordinates
      bbox <- list(
        xmin = bounds$west,
        ymin = bounds$south,
        xmax = bounds$east,
        ymax = bounds$north
      )

      # Create PostGIS query to find intersecting parcels
      query <- glue_sql(
        "
        SELECT pid, geom
        FROM parcels
        WHERE ST_Intersects(
          geom,
          ST_MakeEnvelope({bbox$xmin}, {bbox$ymin}, {bbox$xmax}, {bbox$ymax}, 4326)
        );
        ",
        .con = gis_con
      )

      # Execute the query
      intersecting_parcels <- st_read(gis_con, query = query)

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] ",
        "Found ",
        nrow(intersecting_parcels),
        " parcels intersecting bounds: ",
        "West: ",
        bbox$xmin,
        ", ",
        "South: ",
        bbox$ymin,
        ", ",
        "East: ",
        bbox$xmax,
        ", ",
        "North: ",
        bbox$ymax
      )

      # Add intersecting parcels to map
      leafletProxy(ns("map")) |>
        clearGroup("bbox_parcels") |>
        addPolygons(
          data = intersecting_parcels,
          fillColor = "transparent",
          fillOpacity = 0,
          color = "black",
          weight = 2,
          group = "bbox_parcels",
          popup = ~ paste("<b>PID:</b>", pid),
          layerId = ~ paste0("bbox_", pid)
        )
    })

    ## Event :: Reset map view ----
    observeEvent(input$reset_view, {
      leafletProxy(ns("map")) |>
        clearGroup("bbox_parcels") |>
        setView(lng = -63.36, lat = 45.21, zoom = 8)

      updateSelectizeInput(
        session,
        inputId = "property",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
      updateSelectizeInput(
        session,
        inputId = "parcel",
        choices = c("", parcel_choices()),
        selected = "",
        server = TRUE
      )
    })
  })
}
