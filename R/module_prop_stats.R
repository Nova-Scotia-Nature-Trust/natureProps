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
            uiOutput(ns("nsnt_acres_card")),
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

      # Get total acres for NSNT-owned properties
      valboxes$nsnt_acres <- dbGetQuery(
        db_con,
        "SELECT 
          COALESCE(SUM(p.size_confirmed_acres), 0) + 
          COALESCE(SUM(CASE WHEN p.size_confirmed_acres IS NULL THEN pi.area_ha * 2.471053 ELSE 0 END), 0) AS total_acres
        FROM parcels p
        LEFT JOIN parcel_info pi ON p.id = pi.parcel_id
        INNER JOIN properties pr ON p.property_id = pr.id
        WHERE pr.ownership_id NOT IN (11, 12, 13);"
      ) |>
        pull(total_acres)
    })

    # Helper function to create indicator cards
    indicator_card <- function(
      title,
      value,
      icon,
      theme = "primary",
      unit = NULL
    ) {
      formatted_value <- if (is.numeric(value)) {
        paste0(
          format(round(value, 1), big.mark = ","),
          if (!is.null(unit)) paste0(" ", unit)
        )
      } else {
        format(value, big.mark = ",")
      }

      div(
        class = paste("indicator-card", theme),
        div(class = "indicator-icon", bs_icon(icon)),
        div(
          h3(class = "indicator-value", formatted_value),
          p(class = "indicator-title", title)
        )
      )
    }

    output$ecological_vh_card <- renderUI({
      indicator_card(
        "Very High Priority (Ecological)",
        valboxes$eco_high,
        "tree",
        "success",
        unit = "parcels"
      )
    })

    output$securement_vh_card <- renderUI({
      indicator_card(
        "Very High Priority (Securement)",
        valboxes$sec_high,
        "houses",
        "primary",
        unit = "parcels"
      )
    })

    output$props_2025_card <- renderUI({
      indicator_card(
        "Added to database in 2025",
        valboxes$prop_2025,
        "calendar-event",
        "warning",
        unit = "properties"
      )
    })

    output$board_approval_card <- renderUI({
      indicator_card(
        "Properties requiring Board approval",
        valboxes$board_approval,
        "exclamation-square",
        "danger"
      )
    })

    output$nsnt_acres_card <- renderUI({
      indicator_card(
        "Nature Trust Conservation Land",
        round(valboxes$nsnt_acres, 1),
        "map",
        "success",
        unit = "acres"
      )
    })
  })
}
