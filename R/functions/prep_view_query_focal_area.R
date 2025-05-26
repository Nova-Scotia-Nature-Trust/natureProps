prep_view_query_focal_area <- function(
  df_view_meta,
  selected_view,
  db_con,
  focal_area
) {
  focal_area_id <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id FROM focus_area_internal WHERE internal_value IN ({focal_area*})",
      .con = db_con
    )
  )

  focal_properties <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id, property_name FROM properties WHERE focus_area_internal_id IN ({focal_area_id*})",
      .con = db_con
    )
  )

  focal_pids <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id, pid, priority_securement_ranking_id FROM parcels WHERE property_id IN ({focal_properties$id*}) AND priority_securement_ranking_id <= 3;",
      .con = db_con
    )
  )

  fields_to_fetch <- df_view_meta |>
    filter(!!sym(selected_view) == TRUE) |>
    select(db_name) |>
    pull()

  raw_df <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT {`fields_to_fetch`*} FROM parcels WHERE pid IN ({focal_pids$pid*});",
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

  ## Add focal area to dataframe
  focal_ref <- focal_area_from_prop_name(
    prop_name_vec = data$`Property Name`,
    db_con
  )

  data <- data |>
    mutate(`Focal Area (Internal)` = focal_ref) |>
    relocate(`Focal Area (Internal)`) |>
    arrange(`Focal Area (Internal)`, `Property Name`)

  return(list(df = data, focal_pids = focal_pids$pid))
}
