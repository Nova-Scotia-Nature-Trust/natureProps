# UI ----
module_admin_ui <- function(id) {
  ns <- NS(id)

  div(
    div(
      style = "padding: 12px; background-color: #f8f9fa; border-radius: 4px;",

      tags$h6("Landscape Shapefiles"),

      selectizeInput(
        ns("property"),
        "Select Property",
        choices = NULL,
        multiple = FALSE
      ),

      selectizeInput(
        ns("pid"),
        "Select PID(s)",
        choices = NULL,
        multiple = TRUE
      ),

      downloadButton(
        ns("download_landscape_pids"),
        "Download Landscape Shapefiles",
        class = "btn-primary"
      )
    ),

    hr(style = "margin: 20px 0;"),

    actionButton(
      inputId = ns("refresh_mv"),
      label = "Recreate Conservation Lands MV",
      icon = icon("arrows-rotate"),
      class = "btn-primary"
    ),

    hr(style = "margin: 20px 0;"),
    actionButton(
      inputId = ns("create_cons_lands"),
      label = "Update Conservation Lands GIS Table",
      icon = icon("arrows-rotate"),
      class = "btn-primary"
    ),
    div(style = "margin-top: 8px;"),

    hr(style = "margin: 20px 0;"),

    downloadButton(
      outputId = ns("download_cons_lands"),
      label = "Download Conservation Lands Shapefile",
      class = "btn-primary"
    )
  )
}

# Server ----
module_admin_server <- function(
  id,
  db_con,
  db_updated = NULL,
  cons_lands_data = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Event :: Write Cons Lands table ----
    observeEvent(input$create_cons_lands, {
      showPageSpinner()

      gis_con_16 <- create_db_pool("nsnt_gis", port = 5432)
      gis_con_18 <- create_db_pool("nsnt-gis", port = 5433)

      cons_lands_attribs <- prep_view_cons_lands(cons_lands_data(), "spatial")

      custom_easements <- st_read(gis_con_16, "nsnt_easement_boundaries")

      cons_lands_spatial <- st_read(
        gis_con_16,
        query = glue_sql(
          "SELECT * FROM parcels WHERE pid IN ({cons_lands_attribs$pid*})",
          .con = gis_con_16
        )
      ) |>
        group_by(pid) |>
        summarise() |>
        filter(!pid %in% custom_easements$pid) |>
        bind_rows(custom_easements |> select(pid)) |>
        left_join(cons_lands_attribs, join_by(pid)) |>
        relocate(pid, .after = property_name_public) |>
        relocate(geom, .after = everything())

      update_spatial_table(
        x = cons_lands_spatial,
        geom_type = NULL,
        table_name = "nsnt_conservation_lands",
        db_con = gis_con_16
      )

      update_spatial_table(
        x = cons_lands_spatial,
        geom_type = NULL,
        table_name = "nsnt_conservation_lands",
        db_con = gis_con_18
      )

      hidePageSpinner()

      shinyalert(
        title = "Success",
        text = str_glue(
          "NSNT Conservation Lands table updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )

      poolClose(gis_con_16)
      poolClose(gis_con_18)
    })

    ## Event :: Download Shapefile ----
    output$download_cons_lands <- downloadHandler(
      filename = function() {
        "nsnt_cons_lands.zip"
      },
      content = function(fname) {
        map_shp_names <- c(
          prop_pub = "property_name_public",
          pid = "pid",
          prop_id = "property_id",
          int_rec_id = "internal_record_id",
          prop_sec = "property_name",
          proj_reg = "project_region",
          focus_area = "focus_area",
          acq_type = "acquisition_securement_type",
          ownership = "ownership_value",
          ecogift = "ecogift_number",
          date_close = "date_closed_fiscal",
          pub_view = "public_view",
          notes_sens = "notes_sensitivity",
          size_cn_ha = "size_confirmed_ha",
          size_cn_ac = "size_confirmed_acres",
          cst_island = "coastal_island",
          island_id = "coastal_island_id",
          coast_len = "coastline_length",
          fw_island = "freshwater_island",
          shore_len = "shoreline_length",
          ogf_ha = "old_growth_forest_area",
          karst_ha = "karst_forest_area",
          colony_id = "waterbird_colony_id",
          geom = "geom"
        )

        # Use a fresh temp directory to avoid stale files
        tmpdir <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
        dir.create(tmpdir)
        filepath <- file.path(
          tmpdir,
          str_glue("nsnt_conservation_lands_{Sys.Date()}.shp")
        )

        st_read(gis_con, "nsnt_conservation_lands") |>
          select(all_of(map_shp_names)) |>
          st_write(
            filepath,
            delete_dsn = TRUE,
            overwrite = TRUE
          )

        shp_files <- list.files(
          path = tmpdir,
          pattern = "conservation_lands",
          full.names = TRUE
        )

        zip::zipr(
          zipfile = fname,
          files = shp_files
        )
      }
    )

    ## Reactive :: Property choices ----
    property_list <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      dbGetQuery(
        db_con,
        "SELECT property_name FROM properties ORDER BY property_name;"
      ) |>
        pull(property_name)
    })

    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = property_list(),
        selected = isolate(input$property),
        server = TRUE
      )
    })

    ## Reactive :: PIDs for selected property ----
    pids <- reactive({
      req(input$property)
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        db_con,
        glue_sql(
          "SELECT pa.pid 
        FROM parcels pa
        JOIN properties pr ON pa.property_id = pr.id
        WHERE pr.property_name = {input$property}
        ORDER BY pa.pid;",
          .con = db_con
        )
      ) |>
        pull(pid)
    })

    ## Update PID dropdown based on selected property ----
    observe({
      req(input$property)

      updateSelectizeInput(
        session,
        inputId = "pid",
        choices = c("", pids()),
        selected = pids(),
        server = TRUE
      )
    }) |>
      bindEvent(input$property)

    ## Download Landscape Shapefiles ----
    output$download_landscape_pids <- downloadHandler(
      filename = function() {
        str_glue("{input$property} Landscape PIDs - {Sys.Date()}.zip")
      },

      content = function(fname) {
        req(input$pid)

        workdir <- tempfile()
        dir.create(workdir)

        pid_zips <- map_chr(
          input$pid,
          function(pid) {
            shp_name <- str_glue("{input$property} PID{pid}")

            # Directory for this PID's shapefile components
            shp_dir <- file.path(workdir, shp_name)
            dir.create(shp_dir)

            st_read(
              dsn = gis_con,
              query = glue_sql(
                "SELECT * FROM parcels WHERE pid = {pid}",
                .con = db_con
              ),
              quiet = TRUE
            ) |>
              st_write(
                file.path(shp_dir, str_glue("{shp_name}.shp")),
                quiet = TRUE
              )

            # Zip this PID's shapefile set
            pid_zip <- file.path(workdir, str_glue("{shp_name}.zip"))

            zip::zipr(
              zipfile = pid_zip,
              files = list.files(
                shp_dir,
                full.names = TRUE
              )
            )

            pid_zip
          }
        )

        # Zip all PID zip files into final download
        zip::zipr(
          zipfile = fname,
          files = pid_zips
        )
      }
    )

    # Event :: Refresh MV ----
    observeEvent(input$refresh_mv, {
      shinyalert(
        title = "Refreshing",
        text = "Refreshing materialized view. Please wait...",
        type = "info",
        showConfirmButton = FALSE,
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE
      )

      DBI::dbExecute(
        gis_con,
        "REFRESH MATERIALIZED VIEW CONCURRENTLY mv_conservation_land_metrics;"
      )

      shinyalert::closeAlert()
      shinyalert(
        title = "Success",
        text = "Materialized view refresh complete",
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 5000
      )
    })
  })
}
