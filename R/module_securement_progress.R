# UI ----
module_securement_focus_areas_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      open = TRUE,
      width = 260,
      selectizeInput(
        ns("focus_area"),
        "Select Focus Area",
        choices = NULL,
        multiple = FALSE,
        options = list(
          create = FALSE,
          placeholder = "Select a focus area"
        ),
        width = "100%"
      )
    ),

    div(
      style = "display: flex; gap: 16px; align-items: stretch;",

      # Left column: summary cards
      div(
        style = "flex: 0 0 350px; display: flex; flex-direction: column; gap: 16px;",

        uiOutput(ns("acres_card")),

        uiOutput(ns("properties_card"))
      ),

      # Right column: property list
      card(
        style = "flex: 1;",
        card_header(h5("Properties List")),
        card_body(
          style = "overflow-y: auto; max-height: 500px;",
          uiOutput(ns("properties_list"))
        )
      )
    )
  )
}

# Server ----
module_securement_focus_areas_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Focus areas dropdown ----
    focus_areas_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        conn = db_con,
        statement = "
      SELECT DISTINCT
        COALESCE(NULLIF(fae.external_value, ''), 'Not Assigned') AS external_value
      FROM properties pr
      LEFT JOIN focus_area_internal fai
        ON pr.focus_area_internal_id = fai.id
      LEFT JOIN focus_area_external fae
        ON fai.focus_area_external_id = fae.id
      WHERE pr.ownership_id NOT IN (7, 11, 12, 13, 14)
      ORDER BY external_value;"
      ) |>
        pull(external_value)
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "focus_area",
        choices = focus_areas_reactive(),
        server = TRUE
      )
    })

    # Securement data for selected focus area ----
    securement_data_reactive <- reactive({
      req(input$focus_area)

      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        conn = db_con,
        statement = glue_sql(
          "
        SELECT
          pr.id AS property_id,
          pr.property_name_public,
          pr.date_closed,
          pr.date_closed_fiscal,
          COALESCE(
            SUM(pa.size_confirmed_acres),
            SUM(pi.area_ha * 2.47105)
          ) AS acres
        FROM properties pr
        LEFT JOIN focus_area_internal fai
          ON pr.focus_area_internal_id = fai.id
        LEFT JOIN focus_area_external fae
          ON fai.focus_area_external_id = fae.id
        INNER JOIN parcels pa
          ON pa.property_id = pr.id
        LEFT JOIN parcel_info pi
          ON pi.parcel_id = pa.id
        WHERE
          COALESCE(
            NULLIF(fae.external_value, ''),
            'Not Assigned'
          ) = {input$focus_area}
          AND pr.ownership_id NOT IN (7, 11, 12, 13, 14)
        GROUP BY
          pr.id,
          pr.property_name_public,
          pr.date_closed,
          pr.date_closed_fiscal
        ORDER BY
          pr.date_closed;
        ",
          .con = db_con
        )
      ) |>
        as_tibble()
    })

    # Helper: indicator card ----
    indicator_card <- function(
      title,
      value,
      icon,
      theme = "primary",
      unit = NULL
    ) {
      formatted_value <- paste0(
        format(round(value, 0), big.mark = ","),
        if (!is.null(unit)) paste0(" ", unit)
      )

      div(
        class = paste("indicator-card", theme),
        div(class = "indicator-icon", bs_icon(icon)),
        div(
          h3(class = "indicator-value", formatted_value),
          p(class = "indicator-title", title)
        )
      )
    }

    # Acres value box ----
    output$acres_card <- renderUI({
      req(securement_data_reactive())

      total <- securement_data_reactive() |>
        pull(acres) |>
        sum(na.rm = TRUE)

      indicator_card(
        title = paste("Acres Secured —", input$focus_area),
        value = total,
        icon = "map",
        theme = "success",
        unit = "acres"
      )
    })

    # Properties value box ----
    output$properties_card <- renderUI({
      req(securement_data_reactive())

      n <- securement_data_reactive() |>
        pull(property_id) |>
        n_distinct()

      indicator_card(
        title = paste("Properties Secured —", input$focus_area),
        value = n,
        icon = "houses",
        theme = "primary",
        unit = "properties"
      )
    })

    # Properties list ----
    output$properties_list <- renderUI({
      req(securement_data_reactive())

      props_data <- securement_data_reactive() |>
        filter(!is.na(date_closed)) |>
        arrange(date_closed) |>
        select(property_name_public, date_closed, acres)

      if (nrow(props_data) == 0) {
        return(
          p(
            class = "text-muted",
            "No properties with close dates."
          )
        )
      }

      list_items <- lapply(seq_len(nrow(props_data)), function(i) {
        row <- props_data[i, ]

        div(
          class = "property-list-item",
          style = "
            padding: 10px 0;
            border-bottom: 1px solid #e0e0e0;
          ",

          p(
            style = "
              margin: 0;
              font-weight: 600;
            ",
            row$property_name_public
          ),

          p(
            style = "
              margin: 2px 0 0 0;
              color: #666;
              font-size: 0.9rem;
            ",
            format(row$date_closed, "%Y-%m-%d"),
            " • ",
            format(round(row$acres, 1), nsmall = 1),
            " acres"
          )
        )
      })

      div(
        style = "padding: 4px 8px;",
        list_items
      )
    })
  })
}
