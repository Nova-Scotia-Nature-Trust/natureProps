prep_view_query_insurance <- function(
  df_view_meta,
  selected_view,
  db_con
) {
  # insurance_props <- dbGetQuery(
  #   conn = db_con,
  #   statement = glue_sql(
  #     "SELECT id, property_name, property_name_public,
  #     acquisition_securement_type_id, ownership_id,
  #     date_closed, owner_name
  #      FROM properties WHERE ownership_id != 9",
  #     .con = db_con
  #   )
  # ) |>
  #   as_tibble()

  # insurance_props |>
  #   glimpse()

  insurance_props <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT 
      p.id, 
      p.property_name, 
      p.property_name_public, 
      at.acquisition_value AS acquisition_securement_type_id,
      o.ownership_value AS ownership_id,
      p.date_closed, 
      p.owner_name,
      MIN(pp.padd_county) AS county
    FROM properties p
    LEFT JOIN parcels pa ON p.id = pa.property_id
    LEFT JOIN parcel_padd pp ON pa.id = pp.parcel_id
    LEFT JOIN ownership o ON p.ownership_id = o.id
    LEFT JOIN acquisition_type at ON p.acquisition_securement_type_id = at.id
    WHERE p.ownership_id != 9
    GROUP BY p.id, p.property_name, p.property_name_public, 
            at.acquisition_value, o.ownership_value, 
            p.date_closed, p.owner_name",
      .con = db_con
    )
  ) |>
    as_tibble() |>
    mutate(county = str_to_title(county)) |>
    mutate(county = str_remove(county, " County"))

  size_table <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id, property_id, size_confirmed_acres
     FROM parcels WHERE property_id IN ({insurance_props$id*});",
      .con = db_con
    )
  ) |>
    as_tibble()

  glimpse(size_table)

  pid_info_size <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT parcel_id, area_ha * 2.471053 AS size_acres
     FROM parcel_info WHERE parcel_id IN ({size_table$id*});",
      .con = db_con
    )
  ) |>
    as_tibble()

  glimpse(pid_info_size)

  size_table <- size_table |>
    left_join(pid_info_size, join_by(id == parcel_id)) |>
    mutate(
      size_acres = if_else(
        is.na(size_confirmed_acres),
        size_acres,
        size_confirmed_acres
      )
    ) |>
    select(property_id, size_acres) |>
    group_by(property_id) |>
    summarise(size_acres = sum(size_acres))

  insurance_props <- insurance_props |>
    left_join(size_table, join_by(id == property_id)) |>
    as_tibble()

  # df_view_meta <- read_xlsx(
  #   "inputs/field and function mapping tables/df_views.xlsx"
  # )

  # selected_view = "insurance_view"

  ## Create a list of lookup tables
  # db_lookup_tables <- list(
  #   acquisition_type = dbReadTable(db_con, "acquisition_type"),
  #   ownership = dbReadTable(db_con, "ownership")
  # )

  ## Get the lookup function mapping data from metadata file
  # join_lookup_fields <- df_view_meta |>
  #   filter(!!sym(selected_view) == TRUE) |>
  #   filter(!is.na(lookup_table)) |>
  #   select(lookup_table, df_key, lookup_key, lookup_value, new_col_name)

  ## Iterate the function over lookup fields in parcels table
  ## "raw_df" & "db_lookup_tables" are constants in pmap
  # lookup_results <- pmap(
  #   join_lookup_fields,
  #   join_lookup_fn,
  #   idcol = "id",
  #   insurance_props,
  #   db_lookup_tables
  # )

  ## Combine all lookup function results & rename fields
  # join_dfs <- function(x, y) left_join(x, y, by = join_by(id))
  # lookup_combined <- purrr::reduce(lookup_results, join_dfs) |>
  #   rename_with(~ str_replace(.x, "_value$", "_id"), ends_with("_value"))

  ## Transform to pretty column names
  pretty_col_names <- df_view_meta |>
    filter(db_name %in% setdiff(names(insurance_props), "id")) |>
    select(df_name, db_name) |>
    deframe()

  ## Combine the lookup columns with the non-lookup data
  ## Rename fields
  # data <- insurance_props |>
  #   select(-all_of(setdiff(names(lookup_combined), "id"))) |>
  #   left_join(lookup_combined, join_by(id)) |>
  #   select(all_of(pretty_col_names)) |>
  #   relocate(`Property Name`, Ownership, Owner)

  data <- insurance_props |>
    select(all_of(setdiff(names(insurance_props), "id"))) |>
    select(all_of(pretty_col_names)) |>
    relocate(`Property Name`, County, Ownership, Owner)

  glimpse(data)

  return(data)
}
