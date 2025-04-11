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
