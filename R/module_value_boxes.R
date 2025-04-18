module_value_boxes_UI <- function(id) {
  ns <- NS(id)

  card(
    height = "auto", # Make card height automatic
    margin = margin(b = 3), # Add some bottom margin
    h2("Indicators"),
    layout_columns(
      col_widths = c(3, 3),
      gap = "1rem", # Add some gap between value boxes
      fill = FALSE, # Prevent filling available space
      value_box(
        title = "Very High Priority (Ecological)",
        value = textOutput(ns("ecological_vh")),
        showcase = bsicons::bs_icon("tree"),
        theme = "success"
      ),
      value_box(
        title = "Very High Priority (Securement)",
        value = textOutput(ns("securement_vh")),
        showcase = bsicons::bs_icon("houses"),
        theme = "primary"
      ),
      value_box(
        title = "Properties added to database in 2025",
        value = textOutput(ns("props_2025")),
        showcase = bsicons::bs_icon("calendar-event"),
        theme = "warning"
      )
    ),
    h2("Action Items"),
    layout_columns(
      col_widths = c(3,3),
      fill = FALSE,
      value_box(
        title = "Properties requiring Board approval",
        value = textOutput(ns("board_approval")),
        showcase = bsicons::bs_icon("exclamation-square"),
        theme = "danger"
      ),
      # Add a spacer div to prevent pushing everything to bottom
      div(style = "flex-grow: 1;")
    )
  )
}

module_property_stats_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    
    valboxes <- reactiveValues()

    observe({
      
      if (!is.null(db_updated)) {
        db_updated()
      }
      
      valboxes$eco_high <- dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
               WHERE priority_ecological_ranking_id = 1;") |>
        pull(count)
      
      valboxes$sec_high <- dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
               WHERE priority_securement_ranking_id = 1;") |>
        pull(count)
      
      valboxes$prop_2025 <- dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
                WHERE date_added > '2024-12-31';") |>
        pull(count)
      
      valboxes$board_approval <- dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
               WHERE approval_board_id = 2;") |>
        pull(count)
      
    })

    
    output$board_approval <- renderText({
      format(valboxes$board_approval, big.mark = ",")
    })

    output$ecological_vh <- renderText({
      format(valboxes$eco_high, big.mark = ",")
    })

    output$securement_vh <- renderText({
      format(valboxes$sec_high, big.mark = ",")
    })

    output$props_2025 <- renderText({
      format(valboxes$prop_2025, big.mark = ",")
    })
  })
}
