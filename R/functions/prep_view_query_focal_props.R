prep_view_query_focal_props <- function(
  df_view_meta,
  db_con,
  focal_area
) {
  # focal_area <- "St. Mary's River"

  focal_areas <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id, internal_value FROM focus_area_internal WHERE internal_value IN ({focal_area*})",
      .con = db_con
    )
  )

  focal_properties <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id, property_name_public, 
      price_purchase, date_closed_fiscal, focus_area_internal_id 
      FROM properties 
      WHERE focus_area_internal_id IN ({focal_areas$id*}) AND ownership_id != 9;",
      .con = db_con
    )
  )

  focal_properties

  size_table <- dbGetQuery(
    conn = db_con,
    statement = glue_sql(
      "SELECT id, property_id, size_confirmed_acres
     FROM parcels WHERE property_id IN ({focal_properties$id*});",
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

  focal_properties <- focal_properties |>
    left_join(size_table, join_by(id == property_id)) |>
    as_tibble() |>
    mutate(price_per_acre = price_purchase / size_acres)

  # df_view_meta <- read_xlsx(
  #   "inputs/field and function mapping tables/df_views.xlsx"
  # )

  focal_properties <- focal_properties |>
    left_join(focal_areas, join_by(focus_area_internal_id == id)) |>
    select(-focus_area_internal_id) |>
    rename(focus_area_internal = internal_value)

  names(focal_properties)

  pretty_col_names <- df_view_meta |>
    filter(db_name %in% setdiff(names(focal_properties), "id")) |>
    select(df_name, db_name) |>
    deframe()

  # pretty_col_names

  focal_properties <- focal_properties |>
    select(all_of(pretty_col_names)) |>
    relocate(
      `Focus Area (Internal)`,
      `Property Name`,
      `Date Closed (Fiscal)`,
      `Size (acres)`,
      `Purchase Price`,
      `Price per acre`
    ) |>
    arrange(`Focus Area (Internal)`, `Property Name`)

  return(focal_properties)
}
