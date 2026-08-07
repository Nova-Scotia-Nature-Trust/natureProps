# UI ----
module_species_properties_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          width = 300,
          radioButtons(
            ns("prop_filter"),
            "Property Filter",
            choices = c(
              "Nature Trust Lands" = "nt_lands",
              "All Properties" = "all"
            ),
            selected = "nt_lands",
            inline = FALSE
          ),
          selectizeInput(
            ns("species_choice"),
            "Select Species",
            choices = NULL,
            multiple = FALSE,
            width = "100%"
          ),
          actionButton(
            inputId = ns("clear_selection"),
            label = "Clear Selection",
            width = "100%"
          )
        ),
        card(
          height = "100%",
          card_header(
            h5(textOutput(ns("species_title")))
          ),
          card_body(
            DTOutput(outputId = ns("species_table"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_species_properties_server <- function(
  id,
  db_con,
  gis_con,
  db_updated = NULL,
  prop_spp_rv
) {
  moduleServer(id, function(input, output, session) {
    species_data <- reactiveVal(NULL)
    species_name <- reactiveVal(NULL)

    ## Focal species list ----
    scientific_names <- c(
      "Antrostomus vociferus",
      "Riparia riparia",
      "Catharus bicknelli",
      "Cardellina canadensis",
      "Euphagus carolinus",
      "Coccothraustes vespertinus",
      "Myotis lucifugus",
      "Myotis septentrionalis",
      "Perimyotis subflavus",
      "Glyptemys insculpta",
      "Thamnophis saurita pop. 3",
      "Emydoidea blandingii pop. 1",
      "Anzia colpodes",
      "Erioderma mollissimum",
      "Erioderma pedicellatum (Atlantic pop.)",
      "Pannaria lurida",
      "Pectenia plumbea",
      "Peltigera hydrothyria",
      "Contopus cooperi",
      "Coreopsis rosea",
      "Clethra alnifolia",
      "Drosera filiformis",
      "Sabatia kennedyana",
      "Eleocharis tuberculosa",
      "Rhynchospora macrostachya",
      "Lachnanthes caroliniana",
      "Lophiola aurea"
    )

    ## Reactive :: Species List ----
    species_list <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        gis_con,
        glue_sql(
          "SELECT DISTINCT comname AS common_name, sciname AS scientific_name
           FROM sar_rare WHERE sciname IN ({scientific_names*})
           ORDER BY comname;",
          .con = gis_con
        )
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "species_choice",
        choices = setNames(
          species_list()$scientific_name,
          species_list()$common_name
        ),
        selected = character(0),
        server = TRUE
      )
    })

    ## Reactive :: Species Intersect Data ----
    all_species_data <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      db_pids <- dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
        pull()

      # Intersect for all focal species
      spp_query <- glue_sql(
        "
        WITH
          selected_spp AS (
            SELECT
              comname,
              sciname,
              obdate,
              prec,
              locuncm,
              idnum,
              geom
            FROM
              sar_rare
            WHERE
              sciname IN ({scientific_names*})
          )
        SELECT DISTINCT
          pa.pid,
          spp.comname,
          spp.sciname,
          spp.obdate,
          spp.prec,
          spp.locuncm,
          spp.idnum
        FROM
          parcels AS pa
          JOIN selected_spp AS spp ON ST_Intersects(pa.geom, spp.geom)
          WHERE pa.pid IN ({db_pids*})
        ORDER BY
          pa.pid;
        ",
        .con = gis_con
      )

      result <- dbGetQuery(gis_con, spp_query) |>
        mutate(
          obyear = if_else(
            str_detect(obdate, "^\\d{2}XX"), # Extract year if it doesn't end with XX
            NA_integer_,
            as.integer(str_extract(obdate, "^\\d{4}")) # Extract first 4 digits as year
          )
        )

      # Summarise by PID and species
      spp_summary <- result |>
        group_by(pid, sciname) |>
        summarise(
          n_obs = length(comname),
          year_latest = max(obyear, na.rm = TRUE),
          prec_highest = min(prec, na.rm = TRUE),
          .groups = "drop"
        )

      spp_pids <- unique(spp_summary$pid)

      # Get property information for all matched PIDs
      prop_query <- glue_sql(
        "
        SELECT DISTINCT
          pa.pid,
          pr.property_name,
          pr.property_name_public,
          pr.internal_record_id,
          fa.internal_value AS focus_area
        FROM parcels AS pa
        LEFT JOIN properties AS pr ON pa.property_id = pr.id
        LEFT JOIN focus_area_internal AS fa ON fa.id = pr.focus_area_internal_id
        WHERE pa.pid IN ({spp_pids*});
        ",
        .con = db_con
      )

      props <- dbGetQuery(db_con, prop_query) |>
        as_tibble() |>
        arrange(focus_area, property_name)

      # Join property info with species summary
      props |>
        left_join(spp_summary, join_by(pid)) |>
        relocate(property_name)
    })

    ## Observe :: Filter by selected species ----
    observe({
      req(input$species_choice)
      prop_type <- input$prop_filter

      spp_common <- species_list() |>
        filter(scientific_name == input$species_choice) |>
        pull(common_name)

      species_name(spp_common)

      data <- all_species_data() |>
        filter(sciname == input$species_choice)

      # Filter to Nature Trust lands (internal_record_id starts with "NT")
      if (identical(prop_type, "nt_lands")) {
        data <- data |>
          filter(str_detect(internal_record_id, "^NT")) |>
          select(-sciname, -internal_record_id, -property_name) |>
          rename(property_name = property_name_public) |>
          relocate(property_name)
      } else {
        data <- data |>
          select(-sciname, -internal_record_id, -property_name_public)
      }

      data <- data |>
        rename(
          "Property Name" = property_name,
          PID = pid,
          "Internal Focus Area" = focus_area,
          "Observations" = n_obs,
          "Latest Year" = year_latest,
          "Precision" = prec_highest
        ) |>
        arrange(`Property Name`)

      prop_spp_rv(unique(data$`Property Name`))
      species_data(data)
    })

    ## Output :: Species data table ----
    output$species_table <- DT::renderDataTable({
      req(species_data())

      DT::datatable(
        species_data(),
        options = list(
          pageLength = 25,
          lengthMenu = list(
            c(10, 25, 50, -1),
            c('10', '25', '50', 'All')
          ),
          scrollX = TRUE,
          fixedHeader = TRUE,
          stateSave = FALSE
        ),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons"),
        fillContainer = TRUE
      )
    })

    ## Clear selection ----
    observeEvent(input$clear_selection, {
      updateSelectizeInput(session, "species_choice", selected = "")
      species_data(NULL)
      species_name(NULL)
    })

    ## Page title ----
    output$species_title <- renderText({
      if (is.null(species_name())) {
        "Species Locations"
      } else {
        paste("Species Locations:", species_name())
      }
    })
  })
}
