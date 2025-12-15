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
      col_widths = c(6, 6),

      # Left card: All indicator boxes
      card(
        height = "auto",
        # card_header(
        #   h5("Property Statistics")
        # ),
        card_body(
          div(
            class = "indicator-grid",
            uiOutput(ns("acres_total")),
            uiOutput(ns("acres_nsnt")),
            uiOutput(ns("acres_easement")),
            uiOutput(ns("n_easements")),
            uiOutput(ns("n_ecogifts")),
            uiOutput(ns("ecological_vh_card")),
            uiOutput(ns("securement_vh_card")),
            uiOutput(ns("props_2025_card"))
          )
        )
      ),

      # Right card: NSNT ownership information
      card(
        height = "auto",
        card_header(
          h5("Other Info")
        ),
        card_body()
      )
    )
  )
}

# Server ----
module_prop_stats_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    valboxes <- reactiveValues()

    observe({
      if (!is.null(db_updated)) {
        db_updated()
      }

      valboxes$eco_high <- dbGetQuery(
        db_con,
        "SELECT COUNT(*) FROM parcels
               WHERE priority_ecological_ranking_id = 1;"
      ) |>
        pull(count)

      valboxes$sec_high <- dbGetQuery(
        db_con,
        "SELECT COUNT(*) FROM parcels
               WHERE priority_securement_ranking_id = 1;"
      ) |>
        pull(count)

      valboxes$prop_2025 <- dbGetQuery(
        db_con,
        "SELECT COUNT(*) FROM parcels
                WHERE date_added > '2024-12-31';"
      ) |>
        pull(count)

      # # Get total acres for NSNT-owned properties
      # valboxes$nsnt_acres <- dbGetQuery(
      #   db_con,
      #   "SELECT
      #     COALESCE(SUM(p.size_confirmed_acres), 0) +
      #     COALESCE(SUM(CASE WHEN p.size_confirmed_acres IS NULL THEN pi.area_ha * 2.471053 ELSE 0 END), 0) AS total_acres
      #   FROM parcels p
      #   LEFT JOIN parcel_info pi ON p.id = pi.parcel_id
      #   INNER JOIN properties pr ON p.property_id = pr.id
      #   WHERE pr.ownership_id NOT IN (11, 12, 13);"
      # ) |>
      #   pull(total_acres)

      # Number of ecogifts
      valboxes$n_ecogifts <- dbGetQuery(
        db_con,
        "SELECT COUNT(*) FROM properties WHERE ecogift_number IS NOT NULL;"
      ) |>
        pull(count)

      # Number of easements
      valboxes$n_easements <- dbGetQuery(
        db_con,
        "SELECT COUNT(*) FROM properties WHERE ownership_id IN (2,3,4,12);"
      ) |>
        pull(count)

      # Acres under easement
      valboxes$acres_easement <- dbGetQuery(
        db_con,
        "SELECT
          pa.property_id,
          pr.property_name,
          pa.size_confirmed_acres,
          pi.area_ha * 2.471 AS pol_acres,
          COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471) AS acres
        FROM
          properties pr
          JOIN parcels pa ON pr.id = pa.property_id
          LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
        WHERE
          pr.ownership_id IN (2, 3, 4, 12);"
      ) |>
        pull(acres) |>
        sum()

      # Total acres protected
      valboxes$acres_total <- dbGetQuery(
        db_con,
        "SELECT
          pa.property_id,
          pr.property_name,
          pa.size_confirmed_acres,
          pi.area_ha * 2.471 AS pol_acres,
          COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471) AS acres
        FROM
          properties pr
          JOIN parcels pa ON pr.id = pa.property_id
          LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
        WHERE
          pr.ownership_id IS NOT NULL AND pr.ownership_id != 7"
      ) |>
        pull(acres) |>
        sum()

      # Total acres held by NSNT
      valboxes$acres_nsnt <- dbGetQuery(
        db_con,
        "SELECT
          pa.property_id,
          pr.property_name,
          pa.size_confirmed_acres,
          pi.area_ha * 2.471 AS pol_acres,
          COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471) AS acres
        FROM
          properties pr
          JOIN parcels pa ON pr.id = pa.property_id
          LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
        WHERE
          pr.ownership_id NOT IN (7, 11, 12, 13);"
      ) |>
        pull(acres) |>
        sum()
    })

    # Helper function to create indicator cards
    indicator_card <- function(
      title,
      value,
      icon,
      theme = "primary",
      unit = NULL,
      custom_color = NULL
    ) {
      formatted_value <- if (is.numeric(value)) {
        paste0(
          format(round(value, 1), big.mark = ","),
          if (!is.null(unit)) paste0(" ", unit)
        )
      } else {
        format(value, big.mark = ",")
      }

      # Use custom color if provided, otherwise use theme class
      if (!is.null(custom_color)) {
        div(
          class = "indicator-card",
          div(
            class = "indicator-icon",
            style = paste0("color: ", custom_color, ";"),
            bs_icon(icon)
          ),
          div(
            h3(class = "indicator-value", formatted_value),
            p(class = "indicator-title", title)
          )
        )
      } else {
        div(
          class = paste("indicator-card", theme),
          div(class = "indicator-icon", bs_icon(icon)),
          div(
            h3(class = "indicator-value", formatted_value),
            p(class = "indicator-title", title)
          )
        )
      }
    }

    output$ecological_vh_card <- renderUI({
      indicator_card(
        title = "Very High Priority (Ecological)",
        value = valboxes$eco_high,
        icon = "tree",
        theme = "success",
        unit = "parcels",
        custom_color = NULL
      )
    })

    output$securement_vh_card <- renderUI({
      indicator_card(
        title = "Very High Priority (Securement)",
        value = valboxes$sec_high,
        icon = "houses",
        theme = "primary",
        unit = "parcels",
        custom_color = NULL
      )
    })

    output$props_2025_card <- renderUI({
      indicator_card(
        title = "Added to database in 2025",
        value = valboxes$prop_2025,
        icon = "calendar-event",
        theme = "warning",
        unit = "properties",
        custom_color = NULL
      )
    })

    output$acres_nsnt <- renderUI({
      indicator_card(
        title = "Nature Trust Held Conservation Land",
        value = round(valboxes$acres_nsnt, 0),
        icon = "map",
        theme = NULL,
        unit = "acres",
        custom_color = "#1717c9ff"
      )
    })

    output$acres_total <- renderUI({
      indicator_card(
        title = "Total Nature Trust Conservation Land",
        value = round(valboxes$acres_total, 0),
        icon = "map",
        theme = "success",
        unit = "acres",
        custom_color = NULL
      )
    })

    output$acres_easement <- renderUI({
      indicator_card(
        title = "Conservation Land Under Easement ",
        value = round(valboxes$acres_easement, 0),
        icon = "map",
        theme = NULL,
        unit = "acres",
        custom_color = "#ce3f0bff"
      )
    })

    output$n_ecogifts <- renderUI({
      indicator_card(
        title = "Number of Ecogifts",
        value = valboxes$n_ecogifts,
        icon = "houses",
        theme = "warning",
        unit = NULL,
        custom_color = NULL
      )
    })

    output$n_easements <- renderUI({
      indicator_card(
        title = "Number of Easements",
        value = valboxes$n_easements,
        icon = "houses",
        theme = "warning",
        unit = NULL,
        custom_color = NULL
      )
    })
  })
}
