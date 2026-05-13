query_shoreline <- function(x, gis_con) {
  # x = c('50347525', '15501505', '15827520')

  shoreline <- dbGetQuery(
    gis_con,
    statement = glue_sql(
      "WITH parcel_geom AS 
      (
        SELECT 
          pid,
          ST_Transform(geom, 2961) AS geom
        FROM parcels
        WHERE pid IN ({x*})
      )
      SELECT
        p.pid,
        ST_Length(
          ST_Intersection(
            ST_Transform(h.geom, 2961),
            ST_Buffer(p.geom, 10)
          )
        ) AS shoreline_length_m
      FROM parcel_geom p
      JOIN ns_hydro_network_lines h
      ON ST_DWithin(
        p.geom,
        ST_Transform(h.geom, 2961),
        25
      )
      WHERE h.feat_code IN 
      (
        'WALK20', 'WALK25', 'WALKIS10',
        'WARV10', 'WARV20', 'WARVIS10',
        'WARVLK20', 'WARVLKIS10'
      );",
      .con = gis_con
    )
  ) |>
    group_by(pid) |>
    summarise(shoreline_length_m = sum(shoreline_length_m))

  total_shoreline <- tibble(pid = x) |>
    left_join(
      shoreline,
      join_by(pid)
    ) |>
    replace_na(list(shoreline_length_m = 0)) |>
    pull(shoreline_length_m) |>
    sum()

  return(total_shoreline)
}
