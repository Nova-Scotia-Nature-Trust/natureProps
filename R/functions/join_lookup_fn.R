join_lookup_fn <- function(
  lookup_table_name,
  df_key,
  lookup_key,
  lookup_value,
  new_col_name,
  idcol,
  df,
  db_lookup_tables
) {
  lookup_table <- db_lookup_tables[[lookup_table_name]]

  output <- df |>
    left_join(
      lookup_table,
      by = join_by(!!sym(df_key) == !!sym(lookup_key))
    ) |>
    rename(!!new_col_name := !!sym(lookup_value)) |>
    select(-all_of(df_key)) |>
    select(all_of(idcol), !!new_col_name)

  return(output)
}
