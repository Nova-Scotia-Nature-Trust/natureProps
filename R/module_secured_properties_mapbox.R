# R/module_secured_properties_mapbox.R

# UI ----
module_secured_properties_mapbox_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      accordion(
        id = ns("sidebar_accordion"),
        open = "Filters",
        multiple = FALSE,

        accordion_panel(
          title = "Filters",
          icon = bs_icon("funnel"),

          selectizeInput(
            ns("fiscal_year"),
            label = "Fiscal Closing Year",
            choices = NULL,
            selected = "__all_years__",
            multiple = FALSE,
            options = list(
              placeholder = "All years"
            )
          ),

          selectizeInput(
            ns("ownership_type"),
            label = "Ownership Type",
            choices = NULL,
            selected = "__all_ownership__",
            multiple = FALSE,
            options = list(
              placeholder = "All ownership types"
            )
          ),

          actionButton(
            ns("clear_filters"),
            "Clear Filters",
            class = "btn-outline-secondary btn-sm w-100",
            width = "100%"
          )
        ),

        accordion_panel(
          title = "Map Controls",
          icon = bs_icon("gear"),

          selectInput(
            ns("map_style"),
            label = "Base Map",
            choices = c(
              "Satellite Streets" = "satellite-streets",
              "Light" = "light",
              "Dark" = "dark",
              "Streets" = "streets",
              "Outdoors" = "outdoors",
              "Satellite" = "satellite"
            ),
            selected = "light"
          )
        )
      ),

      hr(),

      actionButton(
        ns("reset_view"),
        "Reset Map View",
        class = "btn-secondary w-100"
      )
    ),

    card(
      full_screen = TRUE,

      card_body(
        withSpinner(
          mapboxglOutput(
            ns("map"),
            height = "600px"
          ),
          type = 4,
          color = "#0d51c5ff",
          fill = TRUE
        )
      )
    )
  )
}


# Server ----
module_secured_properties_mapbox_server <- function(
  id,
  db_con,
  gis_con
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Load & Prepare Data
    # -------------------------------------------------------------------------

    # Spatial: one row per parcel polygon from GIS database
    cons_lands_sf <- st_read(
      gis_con,
      "nsnt_conservation_lands",
      quiet = TRUE
    )

    # Tabular: property attributes from properties database
    cons_lands_tab <- dbGetQuery(
      db_con,
      "SELECT * FROM view_conservation_lands"
    ) |>
      as_tibble() |>
      janitor::clean_names() |>
      rename(
        property_name_public = public_property_name,
        property_name = securement_property_name,
        ownership_value = ownership,
        date_closed_fiscal = fiscal_year_closed,
        size_ha = size_hectares,
        size_confirmed_acres = size_acres
      )

    # -------------------------------------------------------------------------
    # Aggregate to one row per property
    # -------------------------------------------------------------------------

    cons_lands_props <- cons_lands_tab |>
      summarise(
        property_name_public = first(
          na.omit(property_name_public)
        ),

        internal_record_id = first(
          na.omit(internal_record_id)
        ),

        acquisition_securement_type = first(
          na.omit(acquisition_securement_type)
        ),

        ownership_value = first(
          na.omit(ownership_value)
        ),

        date_closed_fiscal = as.character(
          first(
            na.omit(date_closed_fiscal)
          )
        ),

        size_ha = sum(
          size_ha,
          na.rm = TRUE
        ),

        size_confirmed_acres = sum(
          size_confirmed_acres,
          na.rm = TRUE
        ),

        .by = property_name
      ) |>
      mutate(
        acquisition_securement_type = coalesce(
          acquisition_securement_type,
          "Unknown"
        ),

        ownership_value = coalesce(
          ownership_value,
          "Unknown"
        ),

        date_closed_fiscal = coalesce(
          date_closed_fiscal,
          "Unknown"
        )
      ) |>
      mutate(
        ownership_value = case_when(
          ownership_value %in%
            c(
              "Easement",
              "Easement - Assigned AF Easement",
              "Transfer to Crown - NSNT Easement"
            ) ~ "Easement",

          ownership_value %in%
            c(
              "Interest in Property",
              "NSNT Owned",
              "NSNT Owned & NSNT Easement",
              "NSNT Owned & NSNT Easement - Assigned AF Easement",
              "NSNT Owned - Transferred AF Donation"
            ) ~ "NSNT Owned",

          ownership_value == "AF Owned" ~ "AF Owned",

          ownership_value == "Easement - Held by AF" ~
            "AF Held Easement",

          .default = ownership_value
        )
      )

    # -------------------------------------------------------------------------
    # Union parcel geometries by property
    # -------------------------------------------------------------------------

    cons_lands_union <- cons_lands_sf |>
      group_by(property_name) |>
      summarise(
        .groups = "drop"
      ) |>
      left_join(
        cons_lands_props,
        by = "property_name"
      ) |>
      mutate(
        popup_html = glue(
          "<div style='font-size: 14px;'>",
          "<b>{coalesce(property_name_public, property_name)}</b><br>",
          "<b>Securement Name:</b> {property_name}<br>",
          "<b>Internal Record ID:</b> ",
          "{coalesce(internal_record_id, 'N/A')}<br>",
          "<b>Ownership:</b> {ownership_value}<br>",
          "<b>Acquisition Type:</b> ",
          "{acquisition_securement_type}<br>",
          "<b>Fiscal Year Closed:</b> ",
          "{date_closed_fiscal}<br>",
          "<b>Size:</b> ",
          "{round(size_ha, 1)} ha / ",
          "{round(size_confirmed_acres, 1)} acres<br>",
          "</div>"
        ),

        tooltip_text = coalesce(
          property_name_public,
          property_name
        )
      )

    # -------------------------------------------------------------------------
    # Centroids
    # -------------------------------------------------------------------------

    cons_lands_centroid <- cons_lands_union |>
      st_centroid()

    # -------------------------------------------------------------------------
    # Filter choices
    # -------------------------------------------------------------------------

    fiscal_year_values <- cons_lands_props |>
      pull(date_closed_fiscal) |>
      unique() |>
      sort()

    ownership_values <- cons_lands_props |>
      pull(ownership_value) |>
      unique() |>
      sort()

    # Explicit "all" choices.
    #
    # The values are internal sentinel values so they cannot conflict
    # with actual database values.
    fiscal_year_choices <- c(
      "All years" = "__all_years__",
      setNames(
        fiscal_year_values,
        fiscal_year_values
      )
    )

    ownership_choices <- c(
      "All ownership types" = "__all_ownership__",
      setNames(
        ownership_values,
        ownership_values
      )
    )

    # -------------------------------------------------------------------------
    # Populate filter inputs
    # -------------------------------------------------------------------------

    updateSelectizeInput(
      session,
      "fiscal_year",
      choices = fiscal_year_choices,
      selected = "__all_years__"
    )

    updateSelectizeInput(
      session,
      "ownership_type",
      choices = ownership_choices,
      selected = "__all_ownership__"
    )

    # -------------------------------------------------------------------------
    # NS Bounds
    # -------------------------------------------------------------------------

    ns_bounds <- c(
      -66.4,
      43.5,
      -59.8,
      46.9
    )

    # -------------------------------------------------------------------------
    # Filtered Properties
    # -------------------------------------------------------------------------

    filtered_properties <- reactive({
      result <- cons_lands_union

      # ---- Fiscal year -------------------------------------------------------

      if (
        !is.null(input$fiscal_year) &&
          input$fiscal_year != "__all_years__"
      ) {
        result <- result |>
          filter(
            date_closed_fiscal == input$fiscal_year
          )
      }

      # ---- Ownership ---------------------------------------------------------

      if (
        !is.null(input$ownership_type) &&
          input$ownership_type != "__all_ownership__"
      ) {
        result <- result |>
          filter(
            ownership_value == input$ownership_type
          )
      }

      result
    })

    # -------------------------------------------------------------------------
    # Filtered Centroids
    # -------------------------------------------------------------------------

    filtered_centroids <- reactive({
      filtered_properties() |>
        st_centroid()
    })

    # -------------------------------------------------------------------------
    # Render Map
    # -------------------------------------------------------------------------

    output$map <- renderMapboxgl({
      pal_nsnt <- "#1b3858"
      pal_hover <- "#2d5f9a"

      mapboxgl(
        mapbox_style("light")
      ) |>

        fit_bounds(
          ns_bounds,
          animate = FALSE
        ) |>

        add_geocoder_control(
          position = "top-left",
          placeholder = "Search for a place..."
        ) |>

        # ---------------------------------------------------------------------
        # Conservation Land Polygons
        # ---------------------------------------------------------------------

        add_fill_layer(
          id = "cons_lands_fill",
          source = cons_lands_union,
          fill_color = pal_nsnt,
          fill_opacity = 0.75,
          popup = "popup_html",
          tooltip = "tooltip_text",
          hover_options = list(
            fill_color = pal_hover,
            fill_opacity = 0.9
          )
        ) |>

        add_line_layer(
          id = "cons_lands_outline",
          source = cons_lands_union,
          line_color = "white",
          line_width = 1
        ) |>

        # ---------------------------------------------------------------------
        # Centroid / Cluster Layer
        # ---------------------------------------------------------------------

        add_circle_layer(
          id = "cons_lands_clusters",
          source = cons_lands_centroid,

          cluster_options = cluster_options(
            max_zoom = 10,
            cluster_radius = 50,

            color_stops = c(
              "#7fbfff",
              "#3d9c68",
              "#1b6e3d"
            ),

            radius_stops = c(
              18,
              26,
              34
            ),

            count_stops = c(
              0,
              20,
              50
            ),

            circle_stroke_color = "white",
            circle_stroke_width = 2,
            text_color = "white"
          ),

          max_zoom = 11,

          tooltip = "tooltip_text"
        ) |>

        # ---------------------------------------------------------------------
        # Controls
        # ---------------------------------------------------------------------

        add_reset_control(
          position = "top-left",
          animate = TRUE
        ) |>

        add_scale_control(
          position = "top-left",
          unit = "metric",
          max_width = 200
        ) |>

        add_screenshot_control(
          position = "top-left",
          filename = "nsnt-secured-properties",
          include_legend = TRUE,
          hide_controls = TRUE,
          include_scale_bar = TRUE,
          image_scale = 3,
          button_title = "Capture Screenshot"
        ) |>

        # ---------------------------------------------------------------------
        # Legend
        # ---------------------------------------------------------------------

        add_categorical_legend(
          unique_id = "cons_lands_legend",
          legend_title = "Secured Properties",
          values = "NSNT Conservation Land",
          colors = pal_nsnt,
          patch_shape = "square",
          position = "bottom-left",
          width = "210px",

          style = list(
            background_opacity = 0.95,
            border_width = 1,
            border_color = "gray",
            title_color = "black",
            element_border_color = "black",
            element_border_width = 1
          )
        )
    })

    # -------------------------------------------------------------------------
    # Map Style
    # -------------------------------------------------------------------------

    observeEvent(
      input$map_style,
      {
        mapboxgl_proxy("map") |>
          set_style(
            mapbox_style(input$map_style),
            diff = TRUE,
            preserve_layers = TRUE
          )
      }
    )

    # -------------------------------------------------------------------------
    # Apply Filters
    # -------------------------------------------------------------------------

    observeEvent(
      list(
        input$fiscal_year,
        input$ownership_type
      ),
      {
        # Get the already-filtered data.
        filtered <- filtered_properties()

        filtered_centroids_data <- filtered_centroids()

        mapboxgl_proxy("map") |>

          # Replace polygon source
          set_source(
            layer_id = "cons_lands_fill",
            source = filtered
          ) |>

          # Replace polygon outline source
          set_source(
            layer_id = "cons_lands_outline",
            source = filtered
          ) |>

          # Replace centroid source
          set_source(
            layer_id = "cons_lands_clusters",
            source = filtered_centroids_data
          )
      },
      ignoreInit = FALSE
    )

    # -------------------------------------------------------------------------
    # Clear Filters
    # -------------------------------------------------------------------------

    observeEvent(
      input$clear_filters,
      {
        updateSelectizeInput(
          session,
          "fiscal_year",
          selected = "__all_years__"
        )

        updateSelectizeInput(
          session,
          "ownership_type",
          selected = "__all_ownership__"
        )
      }
    )

    # -------------------------------------------------------------------------
    # Reset Map View
    # -------------------------------------------------------------------------

    observeEvent(
      input$reset_view,
      {
        mapboxgl_proxy("map") |>
          fit_bounds(
            ns_bounds,
            animate = TRUE
          )
      }
    )
  })
}
