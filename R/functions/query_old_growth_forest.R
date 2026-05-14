query_old_growth_forest <- function(x, gis_con) {
  # x = c('50018647', '15501505', '15827520')
  old_growth <- dbGetQuery(
    gis_con,
    statement = glue_sql(
      " WITH
      selected_parcels AS (
        SELECT
          *
        FROM
          parcels
        WHERE
          pid IN ({x*})
      ),
      intersected AS (
        SELECT
          p.pid,
          ST_Intersection (p.geom, f.geom) AS geom
        FROM
          selected_parcels p
          JOIN old_forest_potential_index f ON ST_Intersects (p.geom, f.geom)
      )
    SELECT
      pid,
      SUM(ST_Area(ST_Transform(geom, 2961))) / 10000.0 AS old_growth_forest_area
    FROM
      intersected
    GROUP BY
      pid;",
      .con = gis_con
    )
  ) |>
    as_tibble()

  old_growth_table <- tibble(pid = x) |>
    left_join(
      old_growth,
      join_by(pid)
    ) |>
    replace_na(list(old_growth_forest_area = 0))

  return(old_growth_table)
}
