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
          selectizeInput(
            ns("species_choice"),
            "Select species",
            choices = NULL,
            multiple = FALSE,
            width = "100%",
            options = list(
              placeholder = "Choose a species"
            )
          ),
          actionButton(
            inputId = ns("load_species_data"),
            label = "Load Species Locations",
            width = "100%",
            class = "btn-primary"
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

    species_list <- c(
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

    ## Load species options ----
    species_choices_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        db_con,
        glue_sql(
          "SELECT common_name, scientific_name FROM sar WHERE scientific_name IN ({species_list*});",
          .con = db_con
        )
      ) |>
        arrange(common_name) |>
        deframe()
    })

    observe({
      choices <- species_choices_reactive()

      updateSelectizeInput(
        session,
        "species_choice",
        choices = choices,
        selected = character(0),
        server = TRUE
      )
    })

    ## Load species location data ----
    observeEvent(input$load_species_data, {
      req(input$species_choice)

      # Get the common name from the choices
      choices <- species_choices_reactive()
      spp_common <- names(choices)[match(input$species_choice, choices)]

      # Handle case where species not found
      if (is.na(spp_common)) {
        showNotification("Species not found", type = "error")
        return()
      }

      species_name(spp_common)

      # Get PIDs from database
      db_pids <- dbGetQuery(db_con, "SELECT pid FROM parcels;") |>
        pull()

      # Store the scientific name to use in the query
      sci_name <- input$species_choice

      # Query GIS database for species observations
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
              sciname = {sci_name}
          )
        SELECT DISTINCT
          par.pid,
          spp.comname,
          spp.sciname,
          spp.obdate,
          spp.prec,
          spp.locuncm,
          spp.idnum
        FROM
          parcels AS par
          JOIN selected_spp AS spp ON ST_Intersects(par.geom, spp.geom)
          WHERE par.pid IN ({db_pids*})
        ORDER BY
          par.pid;
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

      # Summarise by PID
      spp_summary <- result |>
        group_by(pid) |>
        summarise(
          n_obs = length(comname),
          year_latest = max(obyear, na.rm = TRUE),
          prec_highest = min(prec, na.rm = TRUE),
          .groups = "drop"
        )

      spp_pids <- unique(spp_summary$pid)

      # Get property information
      prop_query <- glue_sql(
        "   
        SELECT DISTINCT
          par.pid,
          pro.property_name, 
          fo.internal_value AS focus_area
        FROM parcels AS par
        LEFT JOIN properties AS pro ON par.property_id = pro.id
        LEFT JOIN focus_area_internal AS fo ON fo.id = pro.focus_area_internal_id
        WHERE par.pid IN ({spp_pids*});
        ",
        .con = db_con
      )

      props <- dbGetQuery(db_con, prop_query) |>
        as_tibble() |>
        arrange(focus_area, property_name)

      # Join and prepare final table
      data <- props |>
        left_join(spp_summary, join_by(pid)) |>
        relocate(property_name) |>
        rename(
          "Property Name" = property_name,
          PID = pid,
          "Internal Focus Area" = focus_area,
          "Observations" = n_obs,
          "Latest Year" = year_latest,
          "Precision" = prec_highest
        )

      prop_spp_rv(unique(data$`Property Name`))
      species_data(data)
    })

    ## Species data table ----
    output$species_table <- DT::renderDataTable({
      req(species_data())

      DT::datatable(
        species_data(),
        options = list(
          pageLength = 25,
          scrollY = "500px",
          scrollCollapse = FALSE,
          searching = TRUE,
          info = TRUE,
          paging = TRUE
        ),
        rownames = FALSE
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
