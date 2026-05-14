query_coastline <- function(x, gis_con) {
  # x = c('15772999', '15501505', '15827520')
  # x = '15827520'
  coastline <- dbGetQuery(
    gis_con,
    statement = glue_sql(
      "
    WITH parcel_boundaries AS (
    SELECT
        pid,
        ST_Boundary(geom) AS geom
    FROM parcels
    WHERE pid IN ({x*})
    ),

    parcel_boundary_buffers AS (
        SELECT
            pid,
            ST_Buffer(
                geom::geography,
                20
            )::geometry AS geom
        FROM parcel_boundaries
    ),

    coastline_clipped AS (
        SELECT
            p.pid,
            c.segment_id,
            ST_Intersection(c.geom, p.geom) AS geom
        FROM parcel_boundary_buffers p
        JOIN nova_scotia_coastline_segments c
          ON c.geom && p.geom
        AND ST_Intersects(c.geom, p.geom)
    )

      SELECT
          pid,
          SUM(
              ST_Length(
                  ST_Transform(geom, 2961)
              )
          ) AS coastline_length
      FROM coastline_clipped
      GROUP BY pid;
    ",
      .con = gis_con
    )
  ) |>
    as_tibble()

  coastline_table <- tibble(pid = x) |>
    left_join(
      coastline,
      join_by(pid)
    ) |>
    replace_na(list(coastline_length = 0))

  return(coastline_table)
}
