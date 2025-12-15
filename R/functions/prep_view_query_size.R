prep_view_query_size <- function(
  db_con
) {
  data <- dbGetQuery(
    db_con,
    "SELECT
          pr.property_name,
          pa.pid, 
          pa.size_confirmed_acres,
          pa.size_confirmed_ha,
          pi.area_ha * 2.471 AS pol_acres,
          pi.area_ha AS pol_ha,
          pa.size_confirmed_notes,
          COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471) AS reporting_size_acres,
          COALESCE(pa.size_confirmed_ha, pi.area_ha) AS reporting_size_ha
        FROM
          properties pr
          JOIN parcels pa ON pr.id = pa.property_id
          LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
        WHERE
          pr.ownership_id IS NOT NULL AND pr.ownership_id != 7"
  ) |>
    as_tibble() |>
    mutate(
      size_status = if_else(
        is.na(size_confirmed_acres),
        "Unconfirmed",
        "Confirmed"
      )
    ) |>
    relocate(size_status, .after = pid) |>
    arrange(property_name, pid) |>
    rename(
      `Property` = property_name,
      PID = pid,
      `Size status` = size_status,
      `Confirmed Size (acres)` = size_confirmed_acres,
      `Confirmed Size (ha)` = size_confirmed_ha,
      `POL Size (acres)` = pol_acres,
      `POL Size (ha)` = pol_ha,
      Notes = size_confirmed_notes,
      `Reporting Size (acres)` = reporting_size_acres,
      `Reporting Size (ha)` = reporting_size_ha
    ) |>
    mutate(across(.cols = where(is.numeric), .fns = ~ round(., 2)))

  return(data)
}
