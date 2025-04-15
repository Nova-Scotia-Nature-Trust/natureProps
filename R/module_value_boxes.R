module_value_boxes_UI <- function(id) {
  ns <- NS(id)

  card(
    height = "auto", # Make card height automatic
    margin = margin(b = 3), # Add some bottom margin
    layout_columns(
      col_widths = c(3, 3),
      gap = "1rem", # Add some gap between value boxes
      fill = FALSE, # Prevent filling available space
      value_box(
        title = "Very High Priority (Ecological)",
        value = textOutput(ns("ecological_vh")),
        showcase = bsicons::bs_icon("tree"),
        theme = "success",
        height = "120px", # Specify px for height
        full_screen = FALSE # Prevent full screen expansion
      ),
      value_box(
        title = "Very High Priority (Securement)",
        value = textOutput(ns("securement_vh")),
        showcase = bsicons::bs_icon("houses"),
        theme = "primary",
        height = "120px", # Specify px for height
        full_screen = FALSE # Prevent full screen expansion
      ),
      value_box(
        title = "Properties Added to database in 2025",
        value = textOutput(ns("props_2025")),
        showcase = bsicons::bs_icon("calendar-event"),
        theme = "warning",
        height = "120px", # Specify px for height
        full_screen = FALSE # Prevent full screen expansion
      )
    )
  )
}

module_property_stats_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    eco_high <- reactive({
      
      if (!is.null(db_updated)) {
        db_updated()
      }
      
      dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
               WHERE priority_ecological_ranking_id = 1;") |>
        pull(count)
    })

    sec_high <- reactive({
      
      if (!is.null(db_updated)) {
        db_updated()
      }
      
      dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
               WHERE priority_securement_ranking_id = 1;") |>
        pull(count)
    })

    prop_2025 <- reactive({
      
      if (!is.null(db_updated)) {
        db_updated()
      }
      
      dbGetQuery(db_con, "SELECT COUNT(*) FROM parcels
                WHERE date_added > '2024-12-31';") |>
        pull(count)
    })

    output$ecological_vh <- renderText({
      format(eco_high(), big.mark = ",")
    })

    output$securement_vh <- renderText({
      format(sec_high(), big.mark = ",")
    })

    output$props_2025 <- renderText({
      format(prop_2025(), big.mark = ",")
    })
  })
}
