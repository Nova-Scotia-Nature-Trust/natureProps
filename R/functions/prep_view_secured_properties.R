prep_view_secured_properties <- function(db_con, gis_con) {
  data <- dbGetQuery(conn = db_con, "SELECT * FROM view_secured_properties")

  data <- data |>
    mutate(
      `Landscape ID` = ifelse(
        is.na(`Landscape ID`) | `Landscape ID` == "",
        "",
        paste0(
          '<a href="',
          landscape_url,
          '" target="_blank">',
          `Landscape ID`,
          '</a>'
        )
      )
    ) |>
    select(-landscape_url)

  attr(data, "order_column") <- 0
  attr(data, "order_direction") <- "asc"

  return(data)
}
