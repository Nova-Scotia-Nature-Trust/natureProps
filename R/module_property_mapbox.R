# R/module_property_mapbox.R

# UI ----
module_property_mapbox_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      accordion(
        open = FALSE,
        multiple = FALSE,
        accordion_panel(
          title = "Property & Parcel Navigation",
          icon = bs_icon("geo-alt"),
          selectizeInput(
            ns("property"),
            label = "Property",
            choices = NULL,
            multiple = FALSE,
            options = list(placeholder = "Select property")
          ),
          actionButton(
            ns("load_property"),
            "Zoom to Property",
            class = "btn-primary"
          ),
          hr(),
          selectizeInput(
            ns("parcel"),
            label = "Parcel (PID)",
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Type to search...",
              maxOptions = 50 # Limit displayed options for performance
            )
          ),
          actionButton(
            ns("load_parcel"),
            "Zoom to Parcel",
            class = "btn-primary"
          )
        ),
        accordion_panel(
          title = "Map Controls",
          icon = bs_icon("gear"),
          selectInput(
            ns("map_style"),
            label = "Base Map",
            choices = c(
              "Light" = "light",
              "Dark" = "dark",
              "Streets" = "streets",
              "Outdoors" = "outdoors",
              "Satellite" = "satellite",
              "Satellite Streets" = "satellite-streets"
            ),
            selected = "satellite-streets"
          ),
          hr(),
          actionButton(
            ns("hide_all_layers"),
            "Hide All Layers",
            class = "btn-warning"
          ),
          br(),
          actionButton(
            ns("show_all_layers"),
            "Show All Layers",
            class = "btn-success"
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
      card_header("Property Map (Mapbox GL)"),
      card_body(
        withSpinner(
          mapboxglOutput(ns("map"), height = "600px"),
          type = 4,
          color = "#0d51c5ff"
        )
      )
    )
  )
}

# Server ----
module_property_mapbox_server <- function(
  id,
  db_con,
  gis_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Property & Parcel Choices ----
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(db_con, "SELECT property_name FROM properties;") |>
        pull(property_name) |>
        sort()
    })

    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    parcel_choices <- dbGetQuery(
      gis_con,
      "SELECT pid 
        FROM parcels WHERE pid != '00000000' 
        ORDER BY pid;"
    ) |>
      pull(pid)

    updateSelectizeInput(
      session,
      "parcel",
      choices = parcel_choices,
      selected = character(0),
      server = TRUE
    )

    # ---- Parcel & Property Data ----
    all_parcels_data <- reactive({
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
          info.area_ha,
          info.area_ha * 2.471 AS area_acres
        FROM parcels par
        LEFT JOIN properties prop ON par.property_id = prop.id
        LEFT JOIN team_lead tl ON prop.team_lead_id = tl.id
        LEFT JOIN phase ph ON prop.phase_id = ph.id
        LEFT JOIN ranking ra_eco ON par.priority_ecological_ranking_id = ra_eco.id
        LEFT JOIN ranking ra_sec ON par.priority_securement_ranking_id = ra_sec.id
        LEFT JOIN parcel_info info ON par.id = info.parcel_id;",
        .con = db_con
      )

      result <- dbGetQuery(db_con, parcel_query)

      # Landowners
      db_owners <- dbGetQuery(db_con, "SELECT * FROM landowners;") |>
        as_tibble()

      db_formatted_owners <- db_owners |>
        mutate(
          individual_name = paste(
            owner_name_first,
            owner_name_middle,
            owner_name_last
          ) |>
            str_remove_all("\\bNA\\b") |>
            str_trim() |>
            str_squish(),
          individual_name = na_if(individual_name, ""),
          owner_display = coalesce(owner_name_corp, individual_name)
        ) |>
        filter(!is.na(owner_display)) |>
        select(parcel_id, owner_display)

      db_owners_collapsed <- db_formatted_owners |>
        group_by(parcel_id) |>
        summarize(
          landowner_names = paste(owner_display, collapse = ", "),
          .groups = "drop"
        )

      result <- left_join(
        result,
        db_owners_collapsed,
        by = c("id" = "parcel_id")
      )
      return(result)
    })

    parcels_sf <- reactive({
      req(all_parcels_data())
      all_pids <- all_parcels_data() |> pull(pid) |> unique()

      pid_geom_query <- glue_sql(
        "SELECT pid, geom FROM parcels WHERE pid IN ({all_pids*});",
        .con = gis_con
      )

      result <- st_read(gis_con, query = pid_geom_query) |>
        left_join(all_parcels_data(), by = "pid")

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
          ),
          parcel_popup = glue(
            "<div style='font-size: 14px;'>",
            "<b>Property Name:</b> {property_name} <br>",
            '<b>PID:</b> <a href="https://pol.novascotia.ca/POL/PropertyDetail/Index?pid={pid}" target="_blank">{pid}</a><br>',
            "<b>Ecological Priority:</b> {coalesce(as.character(ecological_priority), 'Not assigned')} <br>",
            "<b>Securement Priority:</b> {coalesce(as.character(securement_priority), 'Not assigned')} <br>",
            "<b>Phase:</b> {coalesce(as.character(phase), 'Not assigned')} <br>",
            "<b>Team Lead:</b> {coalesce(as.character(team_lead), 'Not assigned')} <br>",
            "<b>Property Description:</b> {coalesce(property_description, 'N/A')} <br>",
            "<b>Landowner:</b> {coalesce(landowner_names, 'Unknown')} <br>",
            "<b>Size (acres):</b> {coalesce(as.character(round(area_acres, 0)), 'Unknown')} <br>",
            "<b>Size (hectares):</b> {coalesce(as.character(round(area_ha, 0)), 'Unknown')} <br>",
            "</div>"
          ),
          parcel_tooltip = glue("{property_name} - PID: {pid}")
        )

      result
    })

    ns_bounds <- c(-66.3, 43.4, -59.7, 47.0)
    map_layer_ids <- c(
      "securement_priority",
      "ecological_priority",
      "nsnt_conservation_lands_layer",
      "papa_layer",
      "papa_pending_layer",
      "nsprd_layer",
      "crown_land_layer"
    )

    # ---- Render Map with All Layers ----
    output$map <- renderMapboxgl({
      priority_pal <- RColorBrewer::brewer.pal(5, "RdYlBu")

      mapboxgl(
        mapbox_style("satellite-streets"),
        bounds = ns_bounds
      ) |>
        # NSPRD
        add_vector_source(
          id = "nsprd",
          tiles = "http://192.168.1.51:7800/public.parcels/{z}/{x}/{y}.pbf"
        ) |>
        add_fill_layer(
          id = "nsprd_layer",
          source = "nsprd",
          source_layer = "public.parcels",
          fill_color = "rgba(0,0,0,0)",
          fill_outline_color = "white",
          fill_opacity = 1,
          min_zoom = 10,
          visibility = "none",
          popup = "pol_url_html"
        ) |>
        # Crown Land
        add_vector_source(
          id = "crown_land",
          tiles = "http://192.168.1.51:7800/public.crown_land/{z}/{x}/{y}.pbf"
        ) |>
        add_fill_layer(
          id = "crown_land_layer",
          source = "crown_land",
          source_layer = "public.crown_land",
          fill_color = "orange",
          fill_opacity = 0.5,
          visibility = "none"
        ) |>
        # Pending Protected Areas
        add_vector_source(
          id = "papa_pending",
          tiles = "http://192.168.1.51:7800/public.papa_pending/{z}/{x}/{y}.pbf"
        ) |>
        add_fill_layer(
          id = "papa_pending_layer",
          source = "papa_pending",
          source_layer = "public.papa_pending",
          fill_color = "lightgrey",
          fill_opacity = 0.8,
          tooltip = "int_name"
        ) |>
        # Protected Areas
        add_vector_source(
          id = "papa",
          tiles = "http://192.168.1.51:7800/public.papa/{z}/{x}/{y}.pbf"
        ) |>
        add_fill_layer(
          id = "papa_layer",
          source = "papa",
          source_layer = "public.papa",
          fill_color = "#048e0f",
          fill_opacity = 0.9,
          tooltip = "prot_name"
        ) |>

        # Ecological Priority
        add_fill_layer(
          id = "ecological_priority",
          source = parcels_sf(),
          fill_color = match_expr(
            column = "ecological_priority",
            values = c("Very High", "High", "Medium", "Low", "Very Low"),
            stops = priority_pal,
            default = "white"
          ),
          fill_opacity = 0.85,
          popup = "parcel_popup",
          tooltip = "parcel_tooltip",
          hover_options = list(fill_color = "#b15ef0", fill_opacity = 0.75)
        ) |>
        # Securement Priority
        add_line_layer(
          id = "securement_priority",
          source = parcels_sf(),
          line_color = match_expr(
            column = "securement_priority",
            values = c("Very High", "High", "Medium", "Low", "Very Low"),
            stops = priority_pal,
            default = "white"
          ),
          line_width = 3
        ) |>
        # NSNT Conservation Lands
        add_vector_source(
          id = "nsnt_conservation_lands",
          tiles = "http://192.168.1.51:7800/public.nsnt_conservation_lands/{z}/{x}/{y}.pbf"
        ) |>
        add_fill_layer(
          id = "nsnt_conservation_lands_layer",
          source = "nsnt_conservation_lands",
          source_layer = "public.nsnt_conservation_lands",
          fill_color = "blue",
          fill_opacity = 0.9,
          tooltip = "property_name_public"
        ) |>
        # Layers control
        add_layers_control(
          layers = list(
            "Securement Priority" = "securement_priority",
            "Ecological Priority" = "ecological_priority",
            "Nature Trust Conservation Lands" = "nsnt_conservation_lands_layer",
            "Protected Areas" = "papa_layer",
            "Pending Protected Areas" = "papa_pending_layer",
            "NSPRD Parcels" = "nsprd_layer",
            "Crown Land" = "crown_land_layer"
          ),
          position = "top-right",
          collapsible = TRUE
        ) |>
        add_navigation_control(position = "bottom-right") |>
        add_reset_control(position = "top-left", animate = TRUE) |>
        add_categorical_legend(
          legend_title = "Ecological Priority",
          values = c("Very High", "High", "Medium", "Low", "Very Low"),
          colors = priority_pal,
          patch_shape = "square",
          position = "bottom-left",
          width = "170px",
          layer_id = "ecological_priority",
          interactive = TRUE,
          style = list(
            background_opacity = 0.95,
            border_width = 1,
            border_color = "gray",
            title_color = "black",
            element_border_color = "black",
            element_border_width = 1
          )
        ) |>
        add_screenshot_control(
          position = "top-left",
          filename = "map-screenshot",
          include_legend = TRUE,
          hide_controls = TRUE,
          include_scale_bar = TRUE,
          image_scale = 1,
          button_title = "Capture screenshot"
        ) |>
        add_scale_control(
          position = "bottom-right",
          unit = "metric",
          max_width = 250
        )
    })

    # ---- Map Style ----
    observeEvent(input$map_style, {
      mapboxgl_proxy("map") |>
        set_style(mapbox_style(input$map_style), diff = TRUE)
    })

    # ---- Zoom to Property ----
    observeEvent(input$load_property, {
      req(input$property != "")
      target <- parcels_sf() |> filter(property_name == input$property)
      mapboxgl_proxy("map") |> fit_bounds(target, animate = TRUE)
    })

    # ---- Zoom to Parcel ----
    observeEvent(input$load_parcel, {
      req(input$parcel != "")

      # target <- parcels_sf() |> filter(pid == input$parcel)

      target <- st_read(
        dsn = gis_con,
        query = glue_sql(
          "SELECT geom FROM parcels WHERE pid = {input$parcel}; ",
          .con = gis_con
        )
      )

      mapboxgl_proxy("map") |> fit_bounds(target, animate = TRUE)
    })

    # ---- Toggle Layers ----
    observeEvent(input$hide_all_layers, {
      proxy <- mapboxgl_proxy("map")
      for (layer_id in map_layer_ids) {
        proxy <- proxy |>
          set_layout_property(layer_id, "visibility", "none")
      }
    })

    observeEvent(input$show_all_layers, {
      proxy <- mapboxgl_proxy("map")
      for (layer_id in map_layer_ids) {
        proxy <- proxy |>
          set_layout_property(layer_id, "visibility", "visible")
      }
    })

    # ---- Reset Map View ----
    observeEvent(input$reset_view, {
      mapboxgl_proxy("map") |>
        fit_bounds(ns_bounds, animate = TRUE)
      updateSelectizeInput(session, "property", selected = "")
      updateSelectizeInput(session, "parcel", selected = "")
    })
  })
}
