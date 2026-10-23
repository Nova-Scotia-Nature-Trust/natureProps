# Summarise which properties had securement action items updated in the last `days` days
get_updated_properties <- function(con, days = 7) {
  property_list <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT
       sai.property_id,
       p.property_name,
       p.property_name_public,
       COALESCE(p.anticipated_closing_year, 'Unassigned') AS anticipated_closing_year,
       COALESCE(sp.probability_value, 'Unassigned') AS probability_value
     FROM securement_action_items sai
     JOIN properties p ON p.id = sai.property_id
     LEFT JOIN phase ph ON ph.id = p.phase_id
     LEFT JOIN securement_probability sp ON sp.id = p.securement_probability_id
     WHERE ph.phase_value IS DISTINCT FROM 'Secured';"
  )

  recent_updates <- DBI::dbGetQuery(
    con,
    "SELECT
       (a.new_data->>'property_id')::int AS property_id,
       t.type_value,
       a.changed_at
     FROM audit_log a
     LEFT JOIN action_item_type t
       ON t.id = (a.new_data->>'action_item_type_id')::int
     WHERE a.table_name = 'securement_action_items'
       AND a.operation = 'UPDATE'
       AND a.old_data IS DISTINCT FROM a.new_data
       AND a.changed_at >= now() - ($1::int * interval '1 day');",
    params = list(days)
  )

  update_summary <- recent_updates |>
    arrange(desc(changed_at)) |>
    summarise(
      n_updates = n(),
      last_updated = as.Date(max(changed_at)),
      fields_updated = paste(sort(unique(type_value)), collapse = ", "),
      .by = property_id
    )

  check_df <- property_list |>
    left_join(update_summary, by = "property_id") |>
    mutate(
      updated = !is.na(n_updates),
      n_updates = coalesce(n_updates, 0L)
    ) |>
    arrange(updated, property_name) |>
    transmute(
      `Property Name` = property_name,
      `Public Property Name` = property_name_public,
      `Anticipated Closing Year` = anticipated_closing_year,
      `Closing Probability` = probability_value,
      `Updated` = if_else(updated, "Yes", "No"),
      `Number of Updates` = n_updates,
      `Fields Updated` = fields_updated,
      `Last Updated` = last_updated
    )

  return(check_df)
}

# List "Active - Securement" properties with no securement action items
get_untracked_properties <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT
       p.property_name AS \"Property Name\",
       p.property_name_public AS \"Public Property Name\",
       COALESCE(p.anticipated_closing_year, 'Unassigned') AS \"Anticipated Closing Year\",
       COALESCE(sp.probability_value, 'Unassigned') AS \"Closing Probability\"
     FROM properties p
     JOIN phase ph ON ph.id = p.phase_id
     LEFT JOIN securement_probability sp ON sp.id = p.securement_probability_id
     WHERE ph.phase_value = 'Active - Securement'
       AND NOT EXISTS (
         SELECT 1 FROM securement_action_items sai
         WHERE sai.property_id = p.id
       )
     ORDER BY p.property_name;"
  )
}

# List properties with action items that have no status assigned
get_missing_status_counts <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT
       p.property_name AS \"Property Name\",
       p.property_name_public AS \"Public Property Name\",
       COALESCE(p.anticipated_closing_year, 'Unassigned') AS \"Anticipated Closing Year\",
       COALESCE(sp.probability_value, 'Unassigned') AS \"Closing Probability\",
       count(*)::int AS \"Missing Status Count\"
     FROM securement_action_items sai
     JOIN properties p ON p.id = sai.property_id
     LEFT JOIN phase ph ON ph.id = p.phase_id
     LEFT JOIN securement_probability sp ON sp.id = p.securement_probability_id
     WHERE sai.action_item_status_id IS NULL
       AND ph.phase_value IS DISTINCT FROM 'Secured'
     GROUP BY p.id, p.property_name, p.property_name_public,
       p.anticipated_closing_year, sp.probability_value
     ORDER BY \"Missing Status Count\" DESC, p.property_name;"
  )
}
