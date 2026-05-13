query_karst_forest <- function(x, gis_con) {
  karst <- dbGetQuery(
    gis_con,
    statement = glue_sql(
      "WITH
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
            ST_Intersection (p.geom, pem.geom) AS geom
          FROM
            selected_parcels p
            JOIN pem ON ST_Intersects (p.geom, pem.geom)
            WHERE level5 = 'Acadian Karst Forest'
        )
      SELECT
        pid,
        SUM(ST_Area(ST_Transform(geom, 2961))) / 10000.0 AS karst_forest_area
      FROM
        intersected
      GROUP BY
        pid;",
      .con = gis_con
    )
  ) |>
    as_tibble()

  total_karst <- tibble(pid = x) |>
    left_join(
      karst,
      join_by(pid)
    ) |>
    replace_na(list(karst_forest_area = 0)) |>
    pull(karst_forest_area) |>
    sum()

  return(total_karst)
}
