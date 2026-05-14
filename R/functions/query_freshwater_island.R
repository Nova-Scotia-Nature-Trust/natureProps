query_freshwater_island <- function(x, gis_con) {
  # x = c('15772999', '15501505', '15827520')
  # x = '15827520'
  fw_islands <- dbGetQuery(
    gis_con,
    statement = glue_sql(
      " SELECT DISTINCT
        p.pid,
        i.feat_code
      FROM
        parcels p
        JOIN ns_hydro_network_lines i ON 
        ST_Intersects (p.geom, i.geom)
      WHERE
        p.pid IN ({x*})
        AND i.feat_code IN ('WARVLKIS10', 'WALKIS10', 'WARVIS10'); 
        ",
      .con = gis_con
    )
  ) |>
    as_tibble() |>
    mutate(freshwater_island = TRUE)

  fw_island_table <- tibble(pid = x) |>
    left_join(
      fw_islands |> select(-feat_code),
      join_by(pid)
    ) |>
    replace_na(list(freshwater_island = FALSE))

  return(fw_island_table)
}
