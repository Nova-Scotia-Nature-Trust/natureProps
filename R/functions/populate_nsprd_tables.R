#' Populate NS Property Database Tables
#'
#' This function populates various tables in the NS Property Database by extracting data from a source database
#' and transforming it to match the target schema.
#'
#' @param pid_list A vector of property IDs (PIDs) to process.
#' @param prd_con A database connection object for the source database (e.g., PRD database).
#' @param db_con A database connection object for the target database (e.g., NS Property Database).
#'
#' @details
#' The function performs the following steps for each table:
#' - Extracts raw data from the source database using SQL queries.
#' - Maps the raw data fields to the target schema using a reference file.
#' - Joins the data with the `parcels` table in the target database to retrieve parcel IDs.
#' - Appends the transformed data to the corresponding table in the target database.
#'
#' The following tables are populated:
#' - `parcel_info`: General parcel information.
#' - `parcel_madd`: Mailing address information.
#' - `parcel_padd`: Physical address information.
#' - `landowners`: Landowner information.
#'
#' @examples
#' # Example usage:
#' pid_list <- c("12345", "67890")
#' prd_con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "prd_db")
#' db_con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "ns_property_db")
#' populate_nsprd_tables(pid_list, prd_con, db_con)
#' DBI::dbDisconnect(prd_con)
#' DBI::dbDisconnect(db_con)
#'
#' @importFrom DBI dbGetQuery dbReadTable
#' @importFrom dplyr as_tibble filter select left_join mutate
#' @importFrom tidyr deframe
#' @importFrom readxl read_xlsx
#' @importFrom glue glue_sql
#' @importFrom tibble glimpse
populate_nsprd_tables <- function(pid_list, prd_con, db_con) {
  # pid_list = pid_input

  prd_field_names <- read_xlsx("inputs/field and function mapping tables/nsprd_field_names.xlsx")

  ## parcel_info ----
  pid_info_raw <- dbGetQuery(
    prd_con,
    glue_sql("SELECT * FROM pidmstrs WHERE pid IN ({pid_list*})", .con = prd_con)
  ) |>
    as_tibble()

  rename_ref <- prd_field_names |>
    filter(group == "parcel_info") |>
    select(-group) |>
    deframe()

  pid_info <- pid_info_raw |>
    select(all_of(rename_ref))

  glimpse(pid_info)

  parcels_db <- dbReadTable(db_con, "parcels") |>
    as_tibble() |>
    filter(pid %in% pid_list) |>
    select(id, pid)

  pid_info <- pid_info |>
    left_join(parcels_db, join_by(pid)) |>
    mutate(parcel_id = id) |>
    select(-pid, -id)

  append_db_data("parcel_info", pid_info, db_con, silent = TRUE)

  ## parcel_madd ----
  pid_mailing_address_raw <- dbGetQuery(
    prd_con,
    glue_sql("SELECT * FROM maddress WHERE pid IN ({pid_list*})", .con = prd_con)
  ) |>
    as_tibble()

  pid_mailing_address <- pid_mailing_address_raw |>
    distinct(pid, .keep_all = TRUE)

  rename_ref <- prd_field_names |>
    filter(group == "mailing_address") |>
    select(-group) |>
    deframe()

  pid_mailing_address <- pid_mailing_address |>
    select(all_of(rename_ref))

  glimpse(pid_mailing_address)

  pid_mailing_address <- pid_mailing_address |>
    left_join(parcels_db, join_by(pid)) |>
    mutate(parcel_id = id) |>
    select(-pid, -id)

  append_db_data("parcel_madd", pid_mailing_address, db_con, silent = TRUE)

  ## parcel_padd ----
  pid_address_raw <- dbGetQuery(
    prd_con,
    glue_sql("SELECT * FROM pidaddress WHERE pid IN ({pid_list*})", .con = prd_con)
  ) |>
    as_tibble()

  rename_ref <- prd_field_names |>
    filter(group == "physical_address") |>
    select(-group) |>
    deframe()

  pid_address <- pid_address_raw |>
    select(all_of(rename_ref))

  glimpse(pid_address)

  pid_address <- pid_address |>
    left_join(parcels_db, join_by(pid)) |>
    mutate(parcel_id = id) |>
    select(-pid, -id)

  append_db_data("parcel_padd", pid_address, db_con, silent = TRUE)

  # landowners  ----
  pid_names_raw <- dbGetQuery(
    prd_con,
    glue_sql("SELECT * FROM pidnames WHERE pid IN ({pid_list*})", .con = prd_con)
  ) |>
    as_tibble()

  rename_ref <- prd_field_names |>
    filter(group == "landowner") |>
    select(-group) |>
    deframe()

  pid_names <- pid_names_raw |>
    select(all_of(rename_ref))

  pid_names <- pid_names |>
    left_join(parcels_db, join_by(pid)) |>
    mutate(parcel_id = id) |>
    select(-pid, -id)

  glimpse(pid_names)

  append_db_data("landowners", pid_names, db_con, silent = TRUE)
}
