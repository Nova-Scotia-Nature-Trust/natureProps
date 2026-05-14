prep_view_cons_lands <- function(cons_lands, table_type) {
  col_order <- c(
    "Public Property Name",
    "PID",
    "Internal Record ID",
    "Property ID",
    "Securement Property Name",
    "Project Region",
    "Focus Area",
    "Acquisition Securement Type",
    "Ownership",
    "Ecogift Number",
    "Fiscal Year Closed",
    "Public View",
    "Sensitivity Notes",
    "Size (Acres)",
    "Size (Hectares)",
    "Coastal Island",
    "Coastal Island ID",
    "Coastline Length",
    "Freshwater Island",
    "Shoreline Length",
    "Old Growth Forest Area",
    "Karst Forest Area",
    "Waterbird Colony ID"
  )

  if (table_type == "ungrouped") {
    data <- cons_lands |>
      select(all_of(col_order))
  } else if (table_type == "grouped") {
    data <- cons_lands |>
      summarise(
        PID = paste(unique(na.omit(PID)), collapse = ", "),

        `Size (Acres)` = sum(
          ifelse(
            is.na(`Size (Acres)`),
            `Size (Hectares)` * 2.471,
            `Size (Acres)`
          ),
          na.rm = TRUE
        ),
        `Size (Hectares)` = sum(`Size (Hectares)`, na.rm = TRUE),
        `Coastal Island` = any(`Coastal Island`),
        `Coastal Island ID` = paste(
          unique(na.omit(`Coastal Island ID`)),
          collapse = ", "
        ),
        `Coastline Length` = sum(`Coastline Length`, na.rm = TRUE),
        `Freshwater Island` = any(`Freshwater Island`, na.rm = TRUE),
        `Shoreline Length` = sum(`Shoreline Length`, na.rm = TRUE),
        `Old Growth Forest Area` = sum(
          `Old Growth Forest Area`,
          na.rm = TRUE
        ),
        `Karst Forest Area` = sum(
          `Karst Forest Area`,
          na.rm = TRUE
        ),
        `Waterbird Colony ID` = paste(
          unique(na.omit(`Waterbird Colony ID`)),
          collapse = ", "
        ),
        .by = c(
          `Public Property Name`,
          `Property ID`,
          `Internal Record ID`,
          `Securement Property Name`,
          `Project Region`,
          `Focus Area`,
          `Acquisition Securement Type`,
          Ownership,
          `Ecogift Number`,
          `Fiscal Year Closed`,
          `Public View`,
          `Sensitivity Notes`
        )
      ) |>
      mutate(
        across(
          c(
            `Size (Acres)`,
            `Size (Hectares)`,
            `Coastline Length`,
            `Shoreline Length`,
            `Old Growth Forest Area`,
            `Karst Forest Area`
          ),
          ~ round(.x, 2)
        )
      ) |>
      mutate(across(
        c(`Waterbird Colony ID`, `Coastal Island ID`),
        ~ na_if(., "")
      )) |>
      select(all_of(col_order))
  } else if (table_type == "spatial") {
    spatial_cols <- c(
      "property_name_public",
      "pid",
      "internal_record_id",
      "property_id",
      "property_name",
      "project_region",
      "focus_area",
      "acquisition_securement_type",
      "ownership_value",
      "ecogift_number",
      "date_closed_fiscal",
      "public_view",
      "notes_sensitivity",
      "size_confirmed_ha",
      "size_confirmed_acres",
      "coastal_island",
      "coastal_island_id",
      "coastline_length",
      "freshwater_island",
      "shoreline_length",
      "old_growth_forest_area",
      "karst_forest_area",
      "waterbird_colony_id"
    )

    data <- cons_lands |>
      janitor::clean_names() |>
      rename(
        property_name_public = public_property_name,
        property_name = securement_property_name,
        ownership_value = ownership,
        date_closed_fiscal = fiscal_year_closed,
        notes_sensitivity = sensitivity_notes,
        size_confirmed_ha = size_hectares,
        size_confirmed_acres = size_acres
      ) |>
      select(all_of(spatial_cols))
  }

  return(data)
}
