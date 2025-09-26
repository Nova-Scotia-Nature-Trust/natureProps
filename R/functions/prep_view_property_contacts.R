prep_view_property_contacts <- function(df_view_meta, db_con) {
  ## Get property contact details DB data
  property_contact_details <- dbReadTable(db_con, "property_contact_details")

  ## Get PIDs associated with each property contact
  prop_con_pids <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT pid, property_contact_id FROM parcels WHERE property_contact_id IN ({property_contact_details$id*});",
      .con = db_con
    )
  ) |>
    group_by(property_contact_id) |>
    summarise(property_contact_pids = paste(pid, collapse = ", "))

  ## Update the contact details with associated PIDs
  property_contact_details <- property_contact_details |>
    left_join(prop_con_pids, join_by(id == property_contact_id)) |>
    relocate(property_contact_pids, .before = dnc)

  prop_ref <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT property_contact_id, property_id FROM parcels WHERE property_contact_id IN ({property_contact_details$id*});",
      .con = db_con
    )
  ) |>
    left_join(
      dbReadTable(db_con, "properties") |>
        select(id, property_name),
      join_by(property_id == id)
    ) |>
    select(-property_id) |>
    rename(id = property_contact_id) |>
    group_by(id) |>
    summarise(property_name = paste(property_name, collapse = ", "))

  ## Transform to pretty column names
  pretty_col_names <- df_view_meta |>
    filter(group == "property_contact_details") |>
    filter(db_name %in% names(property_contact_details)) |>
    select(df_name, db_name) |>
    deframe()

  ## Assign result to 'data' object
  data <- property_contact_details |>
    select(all_of(pretty_col_names))

  ## Add property name
  data <- data |>
    left_join(prop_ref, join_by(ID == id)) |>
    rename(`Property Name` = property_name) |>
    relocate(`Property Name`, .after = ID)

  ## Add ordering attribute for DT table
  attr(data, "order_column") <- 2
  attr(data, "order_direction") <- "asc"

  return(data)
}
