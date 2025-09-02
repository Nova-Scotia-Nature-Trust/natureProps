#' Retrieve Focal Areas from Property Names
#'
#' This function takes a vector of property names and a database connection, and returns a vector of corresponding focal areas.
#'
#' @param prop_name_vec A character vector of property names.
#' @param db_con A database connection object.
#'
#' @return A character vector of focal areas corresponding to the input property names.
#'
#' @details The function performs the following steps:
#' - Reads the "focus_area_internal" and "properties" tables from the database.
#' - Matches the input property names with the "properties" table to retrieve the associated focus area IDs.
#' - Joins the focus area IDs with the "focus_area_internal" table to retrieve the internal values of the focal areas.
#' - Returns the internal values as a character vector.
#'
#' @export
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
