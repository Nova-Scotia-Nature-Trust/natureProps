# Takes a vector of property names and returns a vector of focal areas

focal_area_from_prop_name <- function(prop_name_vec, db_con) {
  focal_internal <- dbReadTable(db_con, "focus_area_internal")
  properties <- dbReadTable(db_con, "properties")

  focal_vec <- enframe(
    prop_name_vec,
    name = NULL,
    value = "property_name"
  ) |>
    left_join(
      properties |>
        select(focus_area_internal_id, property_name),
      join_by(property_name)
    ) |>
    select(focus_area_internal_id) |>
    left_join(focal_internal, join_by(focus_area_internal_id == id)) |>
    pull(internal_value)

  return(focal_vec)
}
