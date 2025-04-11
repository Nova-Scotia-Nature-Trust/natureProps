append_db_data <- function(db_table_name, data, con) {
  # Start transaction
  dbBegin(con)
  
  tryCatch(
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
    },
    error = function(e) {
      # Rollback if any error occurs
      dbRollback(con)
      message("Error occurred, no data was appended: ", e$message)
    }
  )
}
