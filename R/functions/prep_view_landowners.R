prep_view_landowners <- function(df_view_meta, db_con) {
  ## Get landowner details DB data
  land_own_details <- dbReadTable(db_con, "landowner_details")

  ## Get PIDs associated with each landowner
  lan_own_pids <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT pid, landowner_contact_id FROM parcels WHERE landowner_contact_id IN ({land_own_details$id*});",
      .con = db_con
    )
  ) |>
    group_by(landowner_contact_id) |>
    summarise(landowner_pids = paste(pid, collapse = ", "))

  ## Update the contact details with associated PIDs
  land_own_details <- land_own_details |>
    left_join(lan_own_pids, join_by(id == landowner_contact_id)) |>
    relocate(landowner_pids, .before = dnc)

  ## Transform to pretty column names
  pretty_col_names <- df_view_meta |>
    filter(group == "landowner_contact_details") |>
    filter(db_name %in% names(land_own_details)) |>
    select(df_name, db_name) |>
    deframe()

  ## Assign result to 'data' object
  data <- land_own_details |>
    select(all_of(pretty_col_names))

  ## Add ordering attribute for DT table
  attr(data, "order_column") <- 1
  attr(data, "order_direction") <- "asc"

  return(data)
}
