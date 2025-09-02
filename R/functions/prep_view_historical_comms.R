prep_view_historical_comms <- function(parcels_raw, db_con) {
  parcels_raw <- parcels_raw |>
    clean_names() |>
    mutate(
      phase = case_when(
        phase == "Active (Phase 1-3)" ~ phase_db,
        .default = phase
      )
    ) |>
    select(-phase_db) |>
    mutate(pid = str_trim(pid, "both"))

  data <- parcels_raw |>
    select(
      pid,
      landowner_history = landowner_details_history_communications,
      securement_history = securement_details_history_communications
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
