prep_view_pid <- function(df_view_meta, selected_view, db_con) {
  ## Get list of parcel table fields needed for view
  fields_to_fetch <- df_view_meta |>
    filter(!!sym(selected_view) == TRUE) |>
    select(db_name) |>
    pull()

  ## Fetch raw parcel data
  raw_df <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT {`fields_to_fetch`*} FROM parcels;",
      .con = db_con
    )
  )

  ## Create a list of lookup tables
  db_lookup_tables <- list(
    phase = dbReadTable(db_con, "phase"),
    ranking = dbReadTable(db_con, "ranking"),
    acquisition_type = dbReadTable(db_con, "acquisition_type"),
    properties = dbReadTable(db_con, "properties"),
    action_item_status = dbReadTable(db_con, "action_item_status")
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
    idcol = "pid",
    raw_df,
    db_lookup_tables
  )

  ## Combine all lookup function results & rename fields
  join_dfs <- function(x, y) left_join(x, y, by = join_by(pid))
  lookup_combined <- purrr::reduce(lookup_results, join_dfs) |>
    rename_with(~ str_replace(.x, "_value$", "_id"), ends_with("_value"))

  ## Transform to pretty column names
  pretty_col_names <- df_view_meta |>
    filter(group == "parcels") |>
    filter(db_name %in% fields_to_fetch) |>
    select(df_name, db_name) |>
    deframe()

  ## Combine the lookup columns with the non-lookup data
  ## Rename fields
  data <- raw_df |>
    select(-all_of(setdiff(names(lookup_combined), "pid"))) |>
    left_join(lookup_combined, join_by(pid)) |>
    select(all_of(pretty_col_names))

  if (selected_view %in% c("pid_view_01", "pid_view_04")) {
    ## Query to get PID sizes for
    pid_size <- dbGetQuery(
      db_con,
      statement = '
              SELECT pa.pid AS "PID",
                    info.area_ha AS "Size (ha)",
                    ROUND(info.area_ha * 2.47105, 2) AS "Size (acre)"
              FROM parcels AS pa
              LEFT JOIN parcel_info AS info ON pa.id = info.parcel_id;
            '
    ) |>
      as_tibble()

    ## Add PID sizes
    ## Assign result to 'data' object
    data <- data |>
      left_join(pid_size, join_by(PID)) |>
      relocate(contains("size"), .after = `Date Updated`)

    ## Add ordering attribute for DT table
    attr(data, "order_column") <- 1
    attr(data, "order_direction") <- "desc"
  } else if (selected_view == "pid_view_02") {
    ## Add ordering attribute for DT table
    attr(data, "order_column") <- 1
    attr(data, "order_direction") <- "asc"
  }

  return(data)
}
