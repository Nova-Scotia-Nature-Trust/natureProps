#' Create a Database Connection
#'
#' This function establishes a connection to a PostgreSQL database using the
#' `RPostgres` package. It requires the database name and uses predefined
#' connection parameters such as host, port, user, and password.
#'
#' @param database A string specifying the name of the database to connect to.
#' @return A `DBIConnection` object representing the connection to the database.
#' @examples
#' \dontrun{
#'   # Example usage:
#'   db_con <- create_db_con("my_database")
#'   # Perform database operations...
#'   dbDisconnect(db_con) # Disconnect when done
#' }
#' @importFrom RPostgres Postgres
#' @export
create_db_con <- function(database) {
  db_con <- dbConnect(
    RPostgres::Postgres(),
    dbname = database,
    host = "192.168.1.51",
    port = 5432,
    user = "dominic",
    password = "s80d80d300"
  )
  return(db_con)
}
