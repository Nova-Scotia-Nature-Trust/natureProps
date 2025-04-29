prep_view_communications <- function(df_view_meta, selected_view, db_con) {
  ## Fetch raw landowner communication data
  raw_df <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT * FROM landowner_communication;",
      .con = db_con
    )
  ) |>
    as_tibble()

  ## Create a list of lookup tables
  db_lookup_tables <- list(
    communication_purpose = dbReadTable(db_con, "communication_purpose"),
    communication_method = dbReadTable(db_con, "communication_method")
  )

  ## Get the lookup function mapping data from metadata file
  join_lookup_fields <- df_view_meta |>
    filter(!!sym(selected_view) == TRUE) |>
    filter(!is.na(lookup_table)) |>
    select(lookup_table, df_key, lookup_key, lookup_value, new_col_name)

  ## Iterate the function over lookup fields in parcels table
  ## "raw_df" & "db_lookup_tables" are constants in pmap
  lookup_results <- pmap(
    join_lookup_fields,
    join_lookup_fn,
    idcol = "id",
    raw_df,
    db_lookup_tables
  )

  ## Combine all lookup function results & rename fields
  join_dfs <- function(x, y) left_join(x, y, by = join_by(id))
  lookup_combined <- purrr::reduce(lookup_results, join_dfs) |>
    rename_with(~ str_replace(.x, "_value$", "_id"), ends_with("_value"))

  ## Transform to pretty column names
  pretty_col_names <- df_view_meta |>
    filter(group == "landowner_communication") |>
    filter(db_name %in% names(raw_df)) |>
    select(df_name, db_name) |>
    deframe()

  # Combine with main dataframe
  data <- raw_df |>
    select(-all_of(setdiff(names(lookup_combined), "id"))) |>
    left_join(lookup_combined, join_by(id))

  landowner_ids <- data$landowner_contact_id

  property_info <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT p.landowner_contact_id, p.pid, pr.property_name 
            FROM parcels p
            LEFT JOIN properties pr ON p.property_id = pr.id
            WHERE p.landowner_contact_id IN ({landowner_ids*});",
      .con = db_con
    )
  ) |>
    as_tibble() |>
    group_by(landowner_contact_id) |>
    summarise(
      pid = paste(pid, collapse = ", "),
      property_id = paste(unique(property_name), collapse = ", "),
      .groups = "drop"
    )

  # Add a column to the data frame with the matching PIDs
  data <- data |>
    left_join(property_info, by = "landowner_contact_id")

  data <- data |>
    select(all_of(pretty_col_names), pid, property_id) |>
    rename(PIDs = pid, Property = property_id)

  # Query the landowner_details table to get first and last names
  landowner_details <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT id, name_first AS first_name, name_last AS last_name 
            FROM landowner_details
            WHERE id IN ({landowner_ids*});",
      .con = db_con
    )
  ) |>
    as_tibble()

  data <- data |>
    left_join(
      landowner_details,
      join_by(`Landowner Contact ID` == "id")
    ) |>
    relocate(first_name, last_name, .after = `Landowner Contact ID`) |>
    rename(`First Name` = first_name, `Last Name` = last_name) |>
    relocate(Property, .after = ID)

  attr(data, "order_column") <- 1
  attr(data, "order_direction") <- "asc"

  return(data)
}
