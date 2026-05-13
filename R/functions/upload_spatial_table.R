upload_spatial_table <- function(x, geom_type, table_name, db_con) {
  if (!is.null(geom_type)) {
    x <- st_cast(x, geom_type)
  }
  x <- st_make_valid(x)
  assertthat::assert_that(
    all(st_is_valid(x)),
    msg = "INVALID GEOMETRIES DETECTED"
  )
  st_geometry(x) <- "geom"
  table_index <- str_glue("geom_idx_{table_name}")
  if (!is.null(geom_type)) {
    x <- st_cast(x, geom_type)
  }

  empty_ref <- which(st_is_empty(x))
  if (length(empty_ref) > 0) {
    x <- x[-(which(st_is_empty(x))), ]
  }

  dbWriteTable(db_con, table_name, x, overwrite = TRUE)
  dbExecute(
    db_con,
    glue_sql(
      "SELECT UpdateGeometrySRID('{DBI::SQL(glue({table_name}))}','geom', 4326);",
      .con = db_con
    )
  )
  dbExecute(
    db_con,
    glue_sql(
      "ALTER TABLE {`table_name`} ADD COLUMN id SERIAL PRIMARY KEY;",
      .con = db_con
    )
  )

  dbExecute(
    db_con,
    glue_sql(
      "DROP INDEX IF EXISTS {`table_index`}",
      .con = db_con
    )
  )

  dbExecute(
    db_con,
    glue_sql(
      "CREATE INDEX {`table_index`} ON {`table_name`} USING GIST (geom);",
      .con = db_con
    )
  )

  dbExecute(
    db_con,
    glue_sql(
      "GRANT SELECT, INSERT, UPDATE, DELETE
      ON {`table_name`}
      TO gisuser;",
      .con = db_con
    )
  )
}
