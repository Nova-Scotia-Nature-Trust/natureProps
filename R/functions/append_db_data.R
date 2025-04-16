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
