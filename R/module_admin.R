# UI ----
module_admin_ui <- function(id) {
  ns <- NS(id)

  div(
    actionButton(
      inputId = ns("create_cons_lands"),
      label = "Update Conservation Lands Table",
      class = "btn-primary"
    ),
    div(style = "margin-top: 8px;"),

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

      upload_spatial_table(
        x = cons_lands_spatial,
        geom_type = NULL,
        table_name = "nsnt_conservation_lands",
        db_con = gis_con_16
      )

      upload_spatial_table(
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
  })
}
