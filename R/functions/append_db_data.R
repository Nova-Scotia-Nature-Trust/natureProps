#' Append Data to a Database Table
#'
#' This function appends data to a specified database table within a transaction. 
#' If an error occurs during the operation, the transaction is rolled back, and 
#' an appropriate error message is displayed. On success, the transaction is committed.
#'
#' @param db_table_name A character string specifying the name of the database table 
#'   to which the data will be appended.
#' @param data A data frame containing the data to be appended to the database table.
#' @param con A database connection object created using `DBI::dbConnect`.
#' @param silent A logical value indicating whether to suppress success messages. 
#'   If `FALSE`, a success message will be displayed using `shinyalert`.
#'
#' @return Returns `TRUE` if the data is successfully appended. If an error occurs, 
#'   the function stops execution and raises the error.
#'
#' @details
#' The function uses a transaction to ensure that either all data is appended 
#' successfully, or no changes are made to the database in case of an error. 
#' If a unique constraint violation occurs (e.g., duplicate keys), a specific 
#' error message is displayed. For other errors, a generic error message is shown.
#'
#' @importFrom DBI dbBegin dbAppendTable dbCommit dbRollback
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "my_database")
#' data <- data.frame(id = 1:5, value = letters[1:5])
#' append_db_data("my_table", data, con, silent = FALSE)
#' DBI::dbDisconnect(con)
#' }
#'
#' @export

append_db_data <- function(db_table_name, data, con, silent) {
  # Start transaction
  dbBegin(con)

  result <- tryCatch(
    {
      # Append the data
      dbAppendTable(con,
        name = db_table_name,
        value = data,
        row.names = NULL
      )

      # If successful, commit the transaction
      dbCommit(con)
      message("Data appended successfully.")
      TRUE # Return TRUE for success
    },
    error = function(e) {
      # Rollback if any error occurs
      dbRollback(con)

      if (str_detect(e$message, 'duplicate key value violates unique constraint "parcels_pid_unique"')) {
        error_msg <- "One or more PIDs already exist in the database"
        message(error_msg)
      } else {
        error_msg <- paste("Error occurred, no data was appended:", e$message)
        message(error_msg)
      }

      # Show shiny alert with the error
      shinyalert(
        title = "Database Error",
        text = error_msg,
        type = "error",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE
      )

      # Return the error to be re-thrown
      return(e)
    }
  )

  # If result is an error, stop execution
  if (inherits(result, "error")) {
    stop(result)
  }

  # Show success message
  if (silent == FALSE) {
    shinyalert(
      title = "Success",
      text = "Data was successfully appended to the database",
      type = "success",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      timer = 10000 # Auto-close after 10 seconds
    )
  }

  return(result)
}
