prep_view_secured_properties <- function(db_con, gis_con) {
  data <- dbGetQuery(
    conn = db_con,
    "SELECT 
      p.id,
      p.property_name_public, 
      p.property_name,       
      at.acquisition_value AS acquisition_securement_type_id,
      o.ownership_value AS ownership_id,
      p.date_closed, 
      p.date_closed_fiscal,
      p.owner_name,
      p.ecogift_number,
      p.donor_vendor,
      p.llt_funding_secured,
      ca.campaign_value
    FROM properties p
    LEFT JOIN ownership o ON p.ownership_id = o.id
    LEFT JOIN acquisition_type at ON p.acquisition_securement_type_id = at.id
    LEFT JOIN campaign ca ON p.campaign_id = ca.id
    WHERE p.ownership_id IS NOT NULL;
    ",
  ) |>
    as_tibble()

  pids <- dbGetQuery(
    db_con,
    "SELECT
    pa.property_id,
    pa.pid,
    COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471) AS size_acres,
    COALESCE(pa.size_confirmed_acres/2.471, pi.area_ha) AS size_ha
  FROM
    properties pr
    JOIN parcels pa ON pr.id = pa.property_id
    LEFT JOIN parcel_info pi ON pa.id = pi.parcel_id
  WHERE
    pr.ownership_id IS NOT NULL;
    "
  ) |>
    group_by(property_id) |>
    reframe(
      pids = paste(pid, collapse = ", "),
      size_acres = round(sum(size_acres), 2),
      size_ha = round(sum(size_ha), 2)
    )

  landscape_id <- dbReadTable(gis_con, "nsnt_property_names_ref") |>
    as_tibble() |>
    mutate(
      landscape_id = ifelse(
        is.na(landscape_url) | landscape_url == "",
        "",
        paste0(
          '<a href="',
          landscape_url,
          '" target="_blank">',
          external_id_landscape,
          '</a>'
        )
      )
    ) |>
    select(property_name, landscape_id)

  data <- data |>
    left_join(pids, join_by(id == property_id)) |>
    select(-id) |>
    left_join(landscape_id, join_by(property_name))

  data <- data |>
    arrange(property_name_public) |>
    relocate(pids, size_acres, size_ha, .after = property_name) |>
    relocate(owner_name, .after = ownership_id) |>
    relocate(landscape_id, .after = pids) |>
    rename(
      "Property Name" = property_name,
      "Public Property Name" = property_name_public,
      "Acquisition Type" = acquisition_securement_type_id,
      "Ownership" = ownership_id,
      "Date Closed" = date_closed,
      "Fiscal Year Closed" = date_closed_fiscal,
      "Owner Name" = owner_name,
      "Ecogift Number" = ecogift_number,
      "Donor/Vendor" = donor_vendor,
      "LLT Funding Secured" = llt_funding_secured,
      "Campaign" = campaign_value,
      "PIDs" = pids,
      "Size (Acres)" = size_acres,
      "Size (Hectares)" = size_ha,
      "Landscape ID" = landscape_id
    )

  attr(data, "order_column") <- 0
  attr(data, "order_direction") <- "asc"

  return(data)
}
