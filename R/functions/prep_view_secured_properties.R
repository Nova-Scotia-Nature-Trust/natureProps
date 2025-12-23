prep_view_secured_properties <- function(db_con, gis_con) {
  data <- dbGetQuery(conn = db_con, "SELECT * FROM view_secured_properties")

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
    left_join(landscape_id, join_by(`Property Name` == property_name)) |>
    relocate(landscape_id, .after = PIDs) |>
    rename("Landscape ID" = landscape_id) |>
    as_tibble()

  attr(data, "order_column") <- 0
  attr(data, "order_direction") <- "asc"

  return(data)
}
