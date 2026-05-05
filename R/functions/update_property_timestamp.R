#' Update the date_updated field on the properties table
#'
#' Sets `date_updated` to today's date for the given property.
#' Call after any successful write to the properties table.
#'
#' @param con A DBI-compatible database connection or pool object.
#' @param property_id Integer ID of the property to update.
#'
#' @return Invisible NULL. Called for its side effect.
update_property_timestamp <- function(con, property_id) {
  dbx::dbxUpdate(
    conn = con,
    table = "properties",
    records = tibble::tibble(
      id = as.integer(property_id),
      date_updated = as.character(Sys.Date())
    ),
    where_cols = "id"
  )
}
