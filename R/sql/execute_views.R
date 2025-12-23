DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_focal_areas_outreach.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_pid.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_action_items.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_comms_history.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_outreach.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_property_contacts.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_property_descriptions.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_historical_comms.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_focal_areas_securement.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_insurance.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_property_sizes.sql"),
  immediate = TRUE
)
DBI::dbExecute(
  db_con,
  readr::read_file("R/sql/create_view_secured_properties.sql"),
  immediate = TRUE
)
