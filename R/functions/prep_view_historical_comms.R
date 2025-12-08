prep_view_historical_comms <- function(db_con) {
  data <- dbGetQuery(
    db_con,
    "SELECT pid, historical_landowner_notes, historical_securement_notes
     FROM parcels;
    "
  ) |>
    select(
      pid,
      landowner_history = historical_landowner_notes,
      securement_history = historical_securement_notes
    ) |>
    mutate(property_name = property_name_from_pid(pid, db_con)) |>
    relocate(property_name, .after = pid) |>
    rename(
      PID = pid,
      `Property Name` = property_name,
      `Landowner History` = landowner_history,
      `Securement History` = securement_history
    )

  attr(data, "order_column") <- 1
  attr(data, "order_direction") <- "asc"

  return(data)
}
