prep_cons_lands_data <- function(gis_con, db_con) {
  cons_lands <- dbGetQuery(
    db_con,
    "SELECT * FROM view_conservation_lands"
  ) |>
    as_tibble()

  metrics <- dbGetQuery(
    gis_con,
    "SELECT * FROM mv_conservation_land_metrics"
  ) |>
    as_tibble()

  cons_lands <- cons_lands |>
    left_join(metrics, join_by(PID == pid)) |>
    rename(
      "Shoreline Length" = shoreline_length,
      "Coastline Length" = coastline_length,
      "Old Growth Forest Area" = old_growth_forest_area,
      "Karst Forest Area" = karst_forest_area,
      "Waterbird Colony ID" = waterbird_colony_id,
      "Freshwater Island" = freshwater_island,
      "Coastal Island" = coastal_island,
      "Coastal Island ID" = coastal_island_id
    ) |>
    mutate(across(where(is.numeric), ~ round(., 1)))

  return(cons_lands)
}
