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

      parcel_query <- glue_sql(
        "SELECT 
        prop.property_name,
        prop.property_description,
        tl.team_value as team_lead,
        ph.phase_value as phase,
        par.pid,
        par.id,
        ra_eco.ranking_value AS ecological_priority,
        ra_sec.ranking_value AS securement_priority,
        info.area_ha
      FROM parcels par
      LEFT JOIN properties prop ON par.property_id = prop.id
      LEFT JOIN team_lead tl ON prop.team_lead_id = tl.id
      LEFT JOIN phase ph ON prop.phase_id = ph.id
      LEFT JOIN ranking ra_eco ON par.priority_ecological_ranking_id = ra_eco.id
      LEFT JOIN ranking ra_sec ON par.priority_securement_ranking_id = ra_sec.id
      LEFT JOIN parcel_info info ON par.id = info.parcel_id;
        ",
        .con = db_con
      )

      result <- dbGetQuery(db_con, parcel_query)

      # Get all landowners for these parcels
      db_owners <- dbGetQuery(
        db_con,
        statement = "SELECT * FROM landowners;"
      ) |>
        as_tibble()

      # Format each owner's name - vectorized approach
      db_formatted_owners <- db_owners |>
        mutate(
          # Build individual name from components - vectorized
          individual_name = paste(
            owner_name_first,
            owner_name_middle,
            owner_name_last
          ) |>
            str_remove_all("\\bNA\\b") |>
            str_trim() |>
            str_squish(),
          individual_name = na_if(individual_name, ""),

          # Use corp name if available, otherwise individual name
          owner_display = coalesce(owner_name_corp, individual_name)
        ) |>
        filter(!is.na(owner_display)) |>
        select(parcel_id, owner_display)

      # Group by parcel and combine multiple owners with commas
      db_owners_collapsed <- db_formatted_owners |>
        group_by(parcel_id) |>
        summarize(
          landowner_names = paste(owner_display, collapse = ", "),
          .groups = "drop"
        )

      result <- result |>
        left_join(db_owners_collapsed, join_by(id == parcel_id))

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] all_parcels_data completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )

      return(result)
    })

    ## Reactive :: Get spatial data for all parcels ----
    parcels_sf <- reactive({
      start_time <- Sys.time()

      req(all_parcels_data())

      all_pids <- all_parcels_data() |>
        pull(pid) |>
        unique()

      pid_geom_query <- glue_sql(
        "SELECT pid, geom
         FROM parcels
         WHERE pid IN ({all_pids*});
          ",
        .con = gis_con
      )

      # Join spatial data with parcel attributes
      result <- st_read(gis_con, query = pid_geom_query) |>
        left_join(all_parcels_data(), join_by(pid))

      # Define priority levels once
      priority_levels <- c("Very High", "High", "Medium", "Low", "Very Low")

      result <- result |>
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
      return(result)
    })

    ## Color palettes (now simpler) ----
    ecological_pal <- colorFactor(
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

    nsnt_cons_lands <- reactive({
      result <- st_read(
        gis_con,
        query = "SELECT property_name_public, geom FROM nsnt_conservation_lands;"
      )
    })

    # Define shared label
    shared_label_opts <- labelOptions(
      style = list("font-size" = "12px", "font-weight" = "bold"),
      direction = "auto"
    )

    ## Render base map ----
    output$map <- renderLeaflet({
      start_time <- Sys.time()
      message("[", format(Sys.time(), "%H:%M:%OS3"), "] Starting map render")

      req(parcels_sf(), nsnt_cons_lands())

      map <- leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
        addProviderTiles(
          providers$Esri.WorldGrayCanvas,
          group = "Gray Canvas"
        ) |>
        addLayersControl(
          baseGroups = c("Imagery", "Topo", "Gray Canvas"),
          overlayGroups = c(
            "DB Parcels",
            "Crown Land",
            "Protected Areas",
            "Nature Trust Conservation Lands"
          ),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        addMapPane("nsprd_pane", zIndex = 410) |>
        addMapPane("crown_pane", zIndex = 420) |>
        addMapPane("papa_pane", zIndex = 430) |>
        addMapPane("db_parcels_pane", zIndex = 440) |>
        addMapPane("nsnt_cons_lands_pane", zIndex = 450) |>
        addPolygons(
          data = parcels_sf(),
          group = "DB Parcels",
          fillColor = ~ ecological_pal(ecological_priority),
          fillOpacity = 0.7,
          color = ~ securement_pal(securement_priority),
          weight = 3,
          label = ~ str_glue("{property_name} - PID: {pid}"),
          labelOptions = shared_label_opts,
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
            "<b>Landowner:</b>",
            coalesce(landowner_names, "Unknown"),
            "<br>",
            "<b>Size (hectares):</b>",
            coalesce(as.character(area_ha), "Unknown"),
            "<br>",
            "</div>"
          ),
          popupOptions = popupOptions(maxWidth = 500, minWidth = 300),
          highlight = highlightOptions(
            weight = 5,
            color = "black",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          options = pathOptions(pane = "db_parcels_pane")
        ) |>
        addPolygons(
          data = nsnt_cons_lands(),
          group = "Nature Trust Conservation Lands",
          label = ~ str_glue("Public name: {property_name_public}"),
          labelOptions = shared_label_opts,
          fillColor = "#0544a9ff",
          fillOpacity = 0.8,
          color = "#0544a9ff",
          weight = 1,
          options = pathOptions(pane = "nsnt_cons_lands_pane")
        ) |>
        leafem::addFgb(
          file = "app_data/crown_land.fgb",
          group = "Crown Land",
          label = "",
          popup = FALSE,
          fill = TRUE,
          fillColor = "#bc5844ff",
          fillOpacity = 0.6,
          color = "black",
          weight = 1,
          options = pathOptions(pane = "crown_pane")
        ) |>
        leafem::addFgb(
          file = "app_data/papa.fgb",
          group = "Protected Areas",
          label = "prot_name",
          labelOptions = shared_label_opts,
          fill = TRUE,
          fillColor = "#05530bff",
          fillOpacity = 0.8,
          color = "black",
          weight = 1,
          options = pathOptions(pane = "papa_pane")
        ) |>
        groupOptions("Crown Land", zoomLevels = 13:20) |>
        groupOptions("Protected Areas", zoomLevels = 13:20) |>
        addLegend(
          position = "bottomright",
          colors = c("#0544a9", "#bc5844ff", "#05530b"),
          labels = c(
            "Nature Trust Conservation Lands",
            "Crown Land",
            "Protected Areas"
          ),
          title = "",
          opacity = 1
        ) |>
        addLegend(
          data = parcels_sf(),
          position = "bottomright",
          pal = ecological_pal,
          values = ~ecological_priority,
          title = "Priority Ranking",
          opacity = 1
        ) |>
        setView(lng = -63.36, lat = 45.21, zoom = 8)

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] Map render TOTAL completed in ",
        round(difftime(Sys.time(), start_time, units = "secs"), 2),
        " secs"
      )

      return(map)
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

      # Check zoom level before proceeding
      if (is.null(input$map_zoom) || input$map_zoom < 13) {
        shinyalert(
          title = "Zoom Required",
          text = "Zoom in further to show NSPRD data",
          type = "warning",
          closeOnClickOutside = TRUE
        )
        return()
      }

      bounds <- input$map_bounds

      # Extract the bounding box coordinates
      bbox <- list(
        xmin = bounds$west,
        ymin = bounds$south,
        xmax = bounds$east,
        ymax = bounds$north
      )

      # Create PostGIS query to find intersecting parcels
      nsprd_query <- glue_sql(
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

      nsprd_parcels <- st_read(gis_con, query = nsprd_query)

      pid_list <- nsprd_parcels$pid

      message(
        "[",
        format(Sys.time(), "%H:%M:%OS3"),
        "] ",
        "Found ",
        nrow(nsprd_parcels),
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

      landowner_query <- glue_sql(
        "SELECT pid, fname, mname, lname, corpname FROM pidnames WHERE pid IN ({pid_list*});",
        .con = prd_con
      )

      landowners <- dbGetQuery(prd_con, landowner_query)

      formatted_owners <- landowners |>
        mutate(
          # Build individual name from components - vectorized
          individual_name = paste(
            fname,
            mname,
            lname
          ) |>
            str_remove_all("\\bNA\\b") |>
            str_trim() |>
            str_squish(),
          individual_name = na_if(individual_name, ""),

          # Use corp name if available, otherwise individual name
          owner_display = coalesce(corpname, individual_name)
        ) |>
        filter(!is.na(owner_display)) |>
        select(pid, owner_display)

      # Group by parcel and combine multiple owners with commas
      owners_collapsed <- formatted_owners |>
        group_by(pid) |>
        summarize(
          landowner_names = paste(owner_display, collapse = ", "),
          .groups = "drop"
        )

      nsprd_size_query <- glue_sql(
        "SELECT pid, area_hect FROM pidmstrs WHERE pid IN ({pid_list*});",
        .con = prd_con
      )

      nsprd_size <- dbGetQuery(prd_con, nsprd_size_query)

      nsprd_parcels <- nsprd_parcels |>
        left_join(owners_collapsed, join_by(pid)) |>
        left_join(nsprd_size, join_by(pid))

      # Add NSPRD parcels to map
      leafletProxy(ns("map")) |>
        clearGroup("NSPRD Parcels") |>
        addPolygons(
          data = nsprd_parcels,
          group = "NSPRD Parcels",
          popup = ~ paste(
            "<b>PID:</b>",
            pid,
            "<br>",
            "<b>Landowner:</b>",
            landowner_names,
            "<br>",
            "<b>Size (ha):</b>",
            round(area_hect, 0)
          ),
          label = ~ str_glue("PID: {pid}"),
          labelOptions = shared_label_opts,
          fillColor = "transparent",
          fillOpacity = 0,
          color = "black",
          weight = 2,
          options = pathOptions(pane = "nsprd_pane"),
          highlight = highlightOptions(
            weight = 5,
            color = "#d94f05ff",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )
    })

    ## Event :: Reset map view ----
    observeEvent(input$reset_view, {
      leafletProxy(ns("map")) |>
        clearGroup("NSPRD Parcels") |>
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
