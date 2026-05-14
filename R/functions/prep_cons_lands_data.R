prep_cons_lands_data <- function(gis_con, db_con) {
  cons_lands <- dbGetQuery(
    db_con,
    "SELECT * FROM view_conservation_lands"
  ) |>
    as_tibble()

  start <- Sys.time()

  # Slow
  coastline <- query_coastline(cons_lands$PID, gis_con)
  waterbird_cols <- query_bird_colony(cons_lands$PID, gis_con)

  # Fast
  shoreline <- query_shoreline(cons_lands$PID, gis_con)
  old_growth <- query_old_growth_forest(cons_lands$PID, gis_con)
  karst_forest <- query_karst_forest(cons_lands$PID, gis_con)
  fw_island <- query_freshwater_island(cons_lands$PID, gis_con)
  coastal_islands <- query_coastal_island(cons_lands$PID, gis_con)
  end <- Sys.time()
  end - start

  cons_lands <- cons_lands |>
    left_join(shoreline, join_by(PID == pid)) |>
    left_join(coastline, join_by(PID == pid)) |>
    left_join(old_growth, join_by(PID == pid)) |>
    left_join(karst_forest, join_by(PID == pid)) |>
    left_join(waterbird_cols, join_by(PID == pid)) |>
    left_join(fw_island, join_by(PID == pid)) |>
    left_join(coastal_islands, join_by(PID == pid))

  cons_lands <- cons_lands |>
    rename(
      "Shoreline Length" = shoreline_length,
      "Coastline Length" = coastline_length,
      "Old Growth Forest Area" = old_growth_forest_area,
      "Karst Forest Area" = karst_forest_area,
      "Waterbird Colony ID" = waterbird_colony_id,
      "Freshwater Island" = freshwater_island,
      "Coastal Island" = coastal_island,
      "Coastal Island ID" = coastal_island_id
    )

  return(cons_lands)
}
