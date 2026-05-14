query_bird_colony <- function(x, gis_con) {
  # x = c("50146075", "50174242")
  # x = c('50347525', '15827520')

  # bird_colony_query <- glue_sql(
  #   "
  #   WITH buffered_colonies AS (
  #     SELECT colony_id, ST_Buffer(ST_Transform(geom, 2961), 120) AS geom_buffer
  #     FROM nova_scotia_bird_colonies
  #   )
  #   SELECT DISTINCT
  #     p.pid,
  #     b.colony_id
  #   FROM parcels p
  #   JOIN buffered_colonies b ON ST_Intersects(ST_Transform(p.geom, 2961), b.geom_buffer)
  #   WHERE p.pid IN ({x*});
  #     ",
  #   .con = gis_con
  # )

  bird_colony_query <- glue_sql(
    "
    SELECT
        p.pid,
        b.colony_id
    FROM parcels p
    JOIN nova_scotia_bird_colonies b
        ON ST_DWithin(
            ST_Transform(p.geom, 2961),
            ST_Transform(b.geom, 2961),
            120
        )
    WHERE p.pid IN ({x*}); 
      ",
    .con = gis_con
  )

  bird_table <- dbGetQuery(gis_con, bird_colony_query) |>
    as_tibble() |>
    distinct() |>
    group_by(pid) |>
    summarise(colony_id = paste(colony_id, collapse = "; "))

  bird_table <- tibble(pid = x) |>
    left_join(
      bird_table,
      join_by(pid)
    ) |>
    rename(waterbird_colony_id = colony_id) |>
    filter(!is.na(waterbird_colony_id))

  return(bird_table)
}
