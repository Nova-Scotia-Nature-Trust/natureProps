property_name_from_pid <- function(pid_vec, db_con) {
  parcels <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT * FROM parcels WHERE pid IN ({pid_vec*})",
      .con = db_con
    )
  )

  properties <- dbReadTable(db_con, "properties")

  prop_vec <- enframe(
    pid_vec,
    name = NULL,
    value = "pid"
  ) |>
    left_join(
      parcels |>
        select(property_id, pid),
      join_by(pid)
    ) |>
    select(property_id) |>
    left_join(properties, join_by(property_id == id)) |>
    pull(property_name)

  return(prop_vec)
}
