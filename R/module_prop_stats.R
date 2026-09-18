# UI ----
module_prop_stats_UI <- function(id) {
  ns <- NS(id)

  tagList(
    # Attach custom styles matching eco highlights
    tags$style(
      HTML(
        "
        .indicator-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(260px, 1fr));
          gap: 18px;
        }

        .indicator-card {
          display: flex;
          align-items: center;
          gap: 16px;
          padding: 18px 20px;
          border-radius: 16px;
          background: linear-gradient(135deg, #f7f9fa, #ffffff);
          box-shadow: 0 1px 4px rgba(0,0,0,0.08);
          transition: transform 0.15s ease, box-shadow 0.15s ease;
        }
        .indicator-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 4px 16px rgba(0,0,0,0.15);
        }

        .indicator-card .indicator-icon {
          font-size: 42px;
          opacity: 0.8;
        }

        .indicator-card .indicator-value {
          font-size: 1.6em;
          font-weight: 600;
          margin: 0;
          line-height: 1.1;
        }

        .indicator-card .indicator-title {
          margin: 0;
          color: #607080;
          font-size: 0.9em;
          font-weight: 500;
          letter-spacing: 0.3px;
        }

        /* Theme colors */
        .indicator-card.success .indicator-icon { color: #198754; }
        .indicator-card.primary .indicator-icon { color: #0d6efd; }
        .indicator-card.warning .indicator-icon { color: #ffc107; }
        .indicator-card.danger .indicator-icon { color: #dc3545; }

        /* Dark mode adjustments */
        [data-bs-theme='dark'] .indicator-card .indicator-value {
          color: #495057;
        }
        [data-bs-theme='dark'] .indicator-card .indicator-title {
          color: #6c757d;
        }
        "
      )
    ),

    layout_columns(
      col_widths = c(4, 8),

      # Left card: All indicator boxes
      card(
        height = "auto",
        # card_header(
        #   h5("Property Statistics")
        # ),
        card_body(
          div(
            class = "indicator-grid",
            uiOutput(ns("tcl")),
            uiOutput(ns("tclh")),
            uiOutput(ns("telh")),
            uiOutput(ns("tclo")),
            uiOutput(ns("afo")),
            uiOutput(ns("ttc")),
            uiOutput(ns("ecogifts"))
          )
        )
      ),
      card(
        height = "auto",
        full_screen = TRUE,
        card_body(
          mapboxglOutput(ns("closing_year_map"), height = "400px")
        )
      )
    )
  )
}

# Server ----
module_prop_stats_server <- function(id, db_con, gis_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    acerage_vals <- reactiveValues()
    nprop_vals <- reactiveValues()

    calc_acreage <- function(x) {
      acreage <- dbGetQuery(
        db_con,
        statement = glue_sql(
          "SELECT
              pa.property_id,
              pr.property_name,
              pr.property_name_public,
              pr.internal_record_id,
              pa.size_confirmed_acres,
              pi.area_ha * 2.471 AS pol_acres,
              COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471) AS acres
            FROM
              properties pr
              JOIN parcels pa ON pr.id = pa.property_id
              LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
              LEFT JOIN ownership ow ON pr.ownership_id = ow.id
            WHERE
              ow.ownership_value IN ({x*});",
          .con = db_con
        )
      ) |>
        as_tibble()

      return(acreage)
    }

    observe({
      if (!is.null(db_updated)) {
        db_updated()
      }

      ## Number of ecogifts
      nprop_vals$ecogifts <- dbGetQuery(
        db_con,
        "SELECT COUNT(*) FROM properties WHERE ecogift_number IS NOT NULL;"
      ) |>
        pull(count)

      ## Total Conservation Land Secured
      tcl <- calc_acreage(
        c(
          "AF Owned",
          "Easement",
          "Easement - Assigned AF Easement",
          "Easement - Held by AF",
          "NSNT Owned",
          "NSNT Owned - Transferred AF Donation",
          "NSNT Owned & NSNT Easement",
          "NSNT Owned & NSNT Easement - Assigned AF Easement",
          "Transfer to Crown - No Easement",
          "Transfer to Crown - NSNT Easement",
          "Transfer to Crown - NSNT Stewardship"
        )
      ) |>
        summarise(
          n_props = n_distinct(property_id),
          total_acres = sum(acres),
          .groups = "property_id"
        )

      acerage_vals$tcl <- tcl$total_acres
      nprop_vals$tcl <- tcl$n_props

      ## Total Conservation Land Held
      tclh <- calc_acreage(
        c(
          "AF Owned",
          "Easement",
          "Easement - Assigned AF Easement",
          "Easement - Held by AF",
          "NSNT Owned",
          "NSNT Owned - Transferred AF Donation",
          "NSNT Owned & NSNT Easement",
          "NSNT Owned & NSNT Easement - Assigned AF Easement",
          "Transfer to Crown - NSNT Easement"
        )
      ) |>
        summarise(
          n_props = n_distinct(property_id),
          total_acres = sum(acres),
          .groups = "property_id"
        )

      acerage_vals$tclh <- tclh$total_acres
      nprop_vals$tclh <- tclh$n_props

      ## Total Easement Land Held
      telh <- calc_acreage(c(
        "Easement",
        "Easement - Assigned AF Easement",
        "Easement - Held by AF",
        "Transfer to Crown - NSNT Easement"
      )) |>
        summarise(
          n_props = n_distinct(property_id),
          total_acres = sum(acres),
          .groups = "property_id"
        )

      acerage_vals$telh <- telh$total_acres
      nprop_vals$telh <- telh$n_props

      ## Total Conservation Land Owned
      tclo <- calc_acreage(c(
        "NSNT Owned",
        "NSNT Owned - Transferred AF Donation",
        "NSNT Owned & NSNT Easement",
        "NSNT Owned & NSNT Easement - Assigned AF Easement"
      )) |>
        summarise(
          n_props = n_distinct(property_id),
          total_acres = sum(acres),
          .groups = "property_id"
        )

      acerage_vals$tclo <- tclo$total_acres
      nprop_vals$tclo <- tclo$n_props

      ## AFCC owned
      afo <- calc_acreage("AF Owned") |>
        summarise(
          n_props = n_distinct(property_id),
          total_acres = sum(acres),
          .groups = "property_id"
        )

      acerage_vals$afo <- afo$total_acres
      nprop_vals$afo <- afo$n_props

      ## Transferred to Crown
      ttc <- calc_acreage(c(
        "Transfer to Crown - No Easement",
        "Transfer to Crown - NSNT Stewardship"
      )) |>
        summarise(
          n_props = n_distinct(property_id),
          total_acres = sum(acres),
          .groups = "property_id"
        )
      acerage_vals$ttc <- ttc$total_acres
      nprop_vals$ttc <- ttc$n_props

      # End
    })

    # Helper function to create indicator cards
    indicator_card <- function(
      title,
      acreage_value,
      n_prop,
      icon,
      theme = "primary",
      unit = NULL,
      custom_color = NULL
    ) {
      if (is.null(acreage_value)) {
        formatted_value <- n_prop
        n_prop_text <- NULL
      } else {
        formatted_value <- if (is.numeric(acreage_value)) {
          paste0(
            format(round(acreage_value, 1), big.mark = ","),
            if (!is.null(unit)) paste0(" ", unit)
          )
        } else {
          format(acreage_value, big.mark = ",")
        }

        n_prop_text <- paste0(n_prop, " properties")
      }

      card_content <- div(
        h3(class = "indicator-value", formatted_value),
        if (!is.null(n_prop_text)) {
          h5(
            class = "indicator-n-prop",
            style = "color: black;",
            n_prop_text
          )
        },
        p(class = "indicator-title", title)
      )

      if (!is.null(custom_color)) {
        div(
          class = "indicator-card",
          div(
            class = "indicator-icon",
            style = paste0("color: ", custom_color, ";"),
            bs_icon(icon)
          ),
          card_content
        )
      } else {
        div(
          class = paste("indicator-card", theme),
          div(class = "indicator-icon", bs_icon(icon)),
          card_content
        )
      }
    }

    output$tcl <- renderUI({
      indicator_card(
        title = "Conservation Land Secured",
        acreage_value = round(acerage_vals$tcl, 0),
        n_prop = nprop_vals$tcl,
        icon = "map",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$tclh <- renderUI({
      indicator_card(
        title = "Conservation Land Held",
        acreage_value = round(acerage_vals$tclh, 0),
        n_prop = nprop_vals$tclh,
        icon = "tree",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$telh <- renderUI({
      indicator_card(
        title = "Easements",
        acreage_value = round(acerage_vals$telh, 0),
        n_prop = nprop_vals$telh,
        icon = "file-text",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$tclo <- renderUI({
      indicator_card(
        title = "Nature Trust Owned",
        acreage_value = round(acerage_vals$tclo, 0),
        n_prop = nprop_vals$tclo,
        icon = "shield-check",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$afo <- renderUI({
      indicator_card(
        title = "American Friends Owned",
        acreage_value = round(acerage_vals$afo, 0),
        n_prop = nprop_vals$afo,
        icon = "globe-americas",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$ttc <- renderUI({
      indicator_card(
        title = "Transferred to Crown",
        acreage_value = round(acerage_vals$ttc, 0),
        n_prop = nprop_vals$ttc,
        icon = "arrow-right-square",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$ecogifts <- renderUI({
      indicator_card(
        title = "Number of Ecogifts",
        acreage_value = NULL,
        n_prop = nprop_vals$ecogifts,
        icon = "gift",
        theme = "success",
        unit = NULL,
        custom_color = NULL
      )
    })

    prior_fiscal <- str_remove(
      quarter(Sys.Date() - 365, type = "year_start/end", fiscal_start = 4),
      " Q[0-9]"
    )

    plot_data <- reactive({
      if (!is.null(db_updated)) {
        db_updated()

        plot_data <- dbGetQuery(
          db_con,
          'SELECT
          pr.id,
          sp.probability_value,
          COALESCE(pr.anticipated_closing_year, \'Unassigned\') as anticipated_closing_year,
          ph.phase_value
        FROM
          properties pr
          JOIN securement_probability sp ON pr.securement_probability_id = sp.id
          JOIN phase ph ON pr.phase_id = ph.id
        WHERE
          securement_probability_id IS NOT NULL;'
        ) |>
          as_tibble() |>
          filter(anticipated_closing_year > prior_fiscal)
      }
    })

    prob_map_sf <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      prop_data <- dbGetQuery(
        db_con,
        "SELECT
          pa.pid,
          pr.property_name,
          COALESCE(pr.anticipated_closing_year, 'Unassigned') AS anticipated_closing_year,
          sp.probability_value
        FROM properties pr
        JOIN securement_probability sp ON pr.securement_probability_id = sp.id
        JOIN parcels pa ON pa.property_id = pr.id
        WHERE pr.securement_probability_id IS NOT NULL;"
      ) |>
        filter(anticipated_closing_year > prior_fiscal)

      req(nrow(prop_data) > 0)

      pids <- prop_data |> pull(pid)

      parcel_geoms <- st_read(
        gis_con,
        query = glue_sql(
          "SELECT pid, geom FROM parcels WHERE pid IN ({pids*});",
          .con = gis_con
        ),
        quiet = TRUE
      ) |>
        left_join(prop_data, by = "pid")

      parcel_geoms |>
        group_by(property_name, probability_value, anticipated_closing_year) |>
        summarise(geom = st_union(geom), .groups = "drop") |>
        st_centroid() |>
        mutate(
          popup_html = glue(
            "<div style='font-size: 14px;'>",
            "<b>Property:</b> {property_name}<br>",
            "<b>Securement Probability:</b> {probability_value}<br>",
            "<b>Anticipated Closing Year:</b> {anticipated_closing_year}",
            "</div>"
          )
        )
    })

    output$closing_year_map <- renderMapboxgl({
      mapboxgl(mapbox_style("light")) |>
        fit_bounds(c(-66.4, 43.5, -59.8, 46.9), animate = FALSE) |>
        add_circle_layer(
          id = "prob_points",
          source = prob_map_sf(),
          circle_radius = 10,
          circle_color = match_expr(
            column = "probability_value",
            values = c("Confirmed", "Expected", "Potential"),
            stops = c("#2E7D32", "#1976D2", "#d36912ff")
          ),
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          circle_opacity = 0.9,
          popup = "popup_html",
          tooltip = "property_name"
        ) |>
        add_categorical_legend(
          legend_title = "Securement Probability",
          values = c("Confirmed", "Expected", "Potential"),
          colors = c("#2E7D32", "#1976D2", "#d36912ff"),
          patch_shape = "circle",
          position = "bottom-right",
          width = "210px"
        )
    })
  })
}
