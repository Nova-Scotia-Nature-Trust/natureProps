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
