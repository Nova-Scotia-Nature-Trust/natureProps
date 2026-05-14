query_coastal_island <- function(x, gis_con) {
  # x = c('15772999', '15501505', '15827520')
  # x = '15827520'

  islands <- dbGetQuery(
    gis_con,
    glue_sql(
      "
    SELECT DISTINCT p.pid,
      i.island_num,
      i.island_nam
    FROM parcels p
    JOIN nova_scotia_islands i
      ON ST_Intersects(p.geom, i.geom)
    WHERE p.pid IN ({x*});
    ",
      .con = gis_con
    )
  ) |>
    as_tibble()

  island_table <- tibble(pid = x) |>
    left_join(
      islands |>
        group_by(pid) |>
        summarise(coastal_island_id = paste0(island_num, collapse = "; ")) |>
        mutate(coastal_island = TRUE),
      join_by(pid)
    ) |>
    replace_na(list(coastal_island = FALSE))

  return(island_table)
}
