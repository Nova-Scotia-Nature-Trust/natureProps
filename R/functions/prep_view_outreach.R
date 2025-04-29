prep_view_outreach <- function(df_view_meta, selected_view, db_con) {
  ## Fetch raw outreach data
  raw_df <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT * FROM outreach;",
      .con = db_con
    )
  ) |>
    as_tibble()

  ## Create a list of lookup tables
  db_lookup_tables <- list(
    communication_purpose = dbReadTable(db_con, "communication_purpose"),
    communication_method = dbReadTable(db_con, "communication_method"),
    parcels = dbGetQuery(db_con, "SELECT id, pid FROM parcels;")
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
    filter(group == "outreach") |>
    filter(db_name %in% names(raw_df)) |>
    select(df_name, db_name) |>
    deframe()

  # Combine with main dataframe
  data <- raw_df |>
    select(-all_of(setdiff(names(lookup_combined), "id"))) |>
    left_join(lookup_combined, join_by(id)) |>
    select(all_of(pretty_col_names))

  attr(data, "order_column") <- 4
  attr(data, "order_direction") <- "desc"

  return(data)
}
