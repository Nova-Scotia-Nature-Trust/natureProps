update_spatial_table <- function(x, geom_type = NULL, table_name, db_con) {
  # Prepare geometry -----
  if (!is.null(geom_type)) {
    x <- sf::st_cast(x, geom_type)
  }

  x <- sf::st_make_valid(x)

  assertthat::assert_that(
    all(sf::st_is_valid(x)),
    msg = "INVALID GEOMETRIES DETECTED"
  )

  sf::st_geometry(x) <- "geom"

  empty_ref <- which(sf::st_is_empty(x))
  if (length(empty_ref) > 0) {
    x <- x[-empty_ref, ]
  }

  table_index <- stringr::str_glue("geom_idx_{table_name}")

  # Create table if it doesn't exist ----

  if (!DBI::dbExistsTable(db_con, table_name)) {
    message("Creating table: ", table_name)

    sf::st_write(
      obj = x,
      dsn = db_con,
      layer = table_name,
      append = FALSE,
      quiet = TRUE
    )

    DBI::dbExecute(
      db_con,
      glue::glue_sql(
        "SELECT UpdateGeometrySRID({table_name}, 'geom', 4326);",
        table_name = DBI::SQL(table_name),
        .con = db_con
      )
    )

    DBI::dbExecute(
      db_con,
      glue::glue_sql(
        "ALTER TABLE {`table_name`}
         ADD COLUMN id SERIAL PRIMARY KEY;",
        .con = db_con
      )
    )

    DBI::dbExecute(
      db_con,
      glue::glue_sql(
        "CREATE INDEX {`table_index`}
         ON {`table_name`}
         USING GIST (geom);",
        .con = db_con
      )
    )

    DBI::dbExecute(
      db_con,
      glue::glue_sql(
        "GRANT SELECT, INSERT, UPDATE, DELETE
         ON {`table_name`}
         TO gisuser;",
        .con = db_con
      )
    )

    DBI::dbExecute(
      db_con,
      glue::glue_sql(
        "
        ALTER TABLE public.{`table_name`}
        OWNER TO dominic;
        ",
        .con = db_con
      )
    )
  } else {
    message("Refreshing table: ", table_name)

    pool::poolWithTransaction(db_con, function(conn) {
      DBI::dbExecute(
        conn,
        glue::glue_sql(
          "TRUNCATE TABLE {`table_name`};",
          .con = conn
        )
      )

      sf::st_write(
        obj = x,
        dsn = conn,
        layer = table_name,
        append = TRUE,
        quiet = TRUE
      )
    })
  }
}
