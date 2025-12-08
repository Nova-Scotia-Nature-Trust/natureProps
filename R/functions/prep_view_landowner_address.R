prep_view_landowner_address <- function(db_con) {
  parcel_ids <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT property_name, pid, pa.id as parcel_id FROM parcels as pa
      LEFT JOIN properties as pr ON pr.id = pa.property_id;",
      pids = pids,
      .con = db_con
    )
  ) |>
    as_tibble()

  # Get all landowners for these parcels
  owners <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT * FROM landowners WHERE parcel_id IN ({ids*})",
      ids = parcel_ids$parcel_id,
      .con = db_con
    )
  ) |>
    as_tibble()

  # Format each owner's name - vectorized approach
  formatted_owners <- owners |>
    mutate(
      # Build individual name from components - vectorized
      individual_name = paste(
        owner_name_first,
        owner_name_middle,
        owner_name_last
      ) |>
        str_remove_all("\\bNA\\b") |>
        str_trim() |>
        str_squish(),
      individual_name = na_if(individual_name, ""),

      # Use corp name if available, otherwise individual name
      owner_display = coalesce(owner_name_corp, individual_name)
    ) |>
    filter(!is.na(owner_display)) |>
    select(parcel_id, owner_display)

  # Group by parcel and combine multiple owners with commas
  owners_collapsed <- formatted_owners |>
    group_by(parcel_id) |>
    summarize(
      landowner_names = paste(owner_display, collapse = ", "),
      .groups = "drop"
    )

  # Get all addresses for these parcels
  addresses <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT * FROM parcel_madd WHERE parcel_id IN ({ids*})",
      ids = parcel_ids$parcel_id,
      .con = db_con
    )
  ) |>
    as_tibble()

  # Format each address as a single line
  formatted_addresses <- addresses |>
    mutate(
      # Build street address line
      street_line = paste(madd_number, madd_street, madd_street_type) |>
        str_remove_all("\\bNA\\b") |>
        str_trim() |>
        str_squish() |>
        na_if(""),

      # Apartment line
      apt_line = if_else(
        !is.na(madd_apartment_num),
        paste("Apt.", madd_apartment_num),
        NA_character_
      ),

      # Province and postal code
      prov_postal = paste(madd_province, madd_postal_code) |>
        str_remove_all("\\bNA\\b") |>
        str_trim() |>
        str_squish() |>
        na_if("")
    ) |>
    rowwise() |>
    mutate(
      # Combine non-NA address components with commas
      mailing_address = paste(
        na.omit(c(street_line, apt_line, madd_city, prov_postal, madd_country)),
        collapse = ", "
      )
    ) |>
    ungroup() |>
    select(parcel_id, mailing_address)

  data <- parcel_ids |>
    left_join(owners_collapsed, join_by(parcel_id)) |>
    left_join(formatted_addresses, join_by(parcel_id)) |>
    select(
      `Property Name` = property_name,
      PID = pid,
      `Landowner(s)` = landowner_names,
      `Mailing Address` = mailing_address
    )

  attr(data, "order_column") <- 0
  attr(data, "order_direction") <- "asc"

  data
}
