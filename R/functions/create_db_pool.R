#' Create a Database Pool
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
#'   db_con <- create_db_pool("my_database")
#'   # Perform database operations...
#'   poolClose(db_con) # Disconnect when done
#' }
#' @importFrom RPostgres Postgres
#' @export
create_db_pool <- function(database) {
  db_con <- pool::dbPool(
    RPostgres::Postgres(),
    dbname = database,
    host = Sys.getenv("POSTGRES_HOST"),
    # host = "localhost",
    port = 5432,
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD")
  )
  return(db_con)
}
