prep_view_property_descriptions <- function(db_con) {
  data <- dbGetQuery(
    db_con,
    statement = "
    WITH prop_with_pids AS (
      SELECT 
        prop.property_name, 
        prop.property_description, 
        string_agg(par.pid, ', ') AS pids, 
        prop.focus_area_internal_id 
      FROM 
        properties AS prop 
        LEFT JOIN parcels AS par ON prop.id = par.property_id 
      GROUP BY 
        prop.property_name, 
        prop.property_description, 
        focus_area_internal_id 
      ORDER BY 
        prop.property_name
    ) 
    SELECT 
      pwp.property_name, 
      fa.internal_value AS focus_area, 
      pwp.property_description, 
      pwp.pids 
    FROM 
      prop_with_pids AS pwp 
      LEFT JOIN focus_area_internal AS fa ON pwp.focus_area_internal_id = fa.id;
    "
  ) |>
    rename(
      `Property Name` = property_name,
      `Internal Focal Area` = focus_area,
      `PIDs` = pids,
      `Property Description` = property_description
    )

  attr(data, "order_column") <- 0
  attr(data, "order_direction") <- "asc"

  return(data)
}
