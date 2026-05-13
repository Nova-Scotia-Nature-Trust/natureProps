DROP VIEW IF EXISTS view_conservation_lands_grouped;

CREATE VIEW view_conservation_lands_grouped AS
SELECT
  pr.property_name_public AS "Public Property Name",
  STRING_AGG(DISTINCT pa.pid::text, ', ') AS "PIDs",
  FORMAT(
  '<a href="%s" target="_blank">%s</a>',
  pr.landscape_url,
  pr.internal_record_id
) AS "Internal Record ID",
  pa.property_id AS "Property ID",
  pr.property_name AS "Securement Property Name",
  reg.region_value AS "Project Region",
  fae.external_value AS "Focus Area",
  ast.acquisition_value AS "Acquisition Securement Type",
  o.ownership_value AS "Ownership",
  pr.ecogift_number AS "Ecogift Number",
  pr.date_closed_fiscal AS "Fiscal Year Closed",
  pr.public_view AS "Public View",
  pr.notes_sensitivity AS "Sensitivity Notes",

  SUM(
    COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471)
  )::numeric(10,2) AS "Size (Acres)",

  SUM(
    COALESCE(pa.size_confirmed_ha, pi.area_ha)
  )::numeric(10,2) AS "Size (Hectares)",

  BOOL_OR(pa.coastal_island) AS "Coastal Island",

  STRING_AGG(
    DISTINCT pa.coastal_island_id::text,
    ', '
  ) AS "Coastal Island ID",

  SUM(
    COALESCE(pa.coastline_length, 0)
  )::numeric(10,2) AS "Coastline Length",

  BOOL_OR(pa.freshwater_island) AS "Freshwater Island",

  SUM(
    COALESCE(pa.shoreline_length, 0)
  )::numeric(10,2) AS "Shoreline Length",

  SUM(
    COALESCE(pa.old_growth_forest_area, 0)
  )::numeric(10,2) AS "Old Growth Forest Area",

  SUM(
    COALESCE(pa.karst_forest_area, 0)
  )::numeric(10,2) AS "Karst Forest Area",

  STRING_AGG(
    DISTINCT pa.waterbird_colony_id::text,
    ', '
  ) AS "Waterbird Colony ID"

FROM properties pr

LEFT JOIN ownership o
  ON pr.ownership_id = o.id

LEFT JOIN project_region reg
  ON pr.project_region_id = reg.id

LEFT JOIN focus_area_internal fai
  ON pr.focus_area_internal_id = fai.id

LEFT JOIN focus_area_external fae
  ON fai.focus_area_external_id = fae.id

LEFT JOIN acquisition_securement_type ast
  ON pr.acquisition_securement_type_id = ast.id

LEFT JOIN parcels pa
  ON pa.property_id = pr.id

LEFT JOIN parcel_info pi
  ON pi.parcel_id = pa.id

WHERE
  pr.ownership_id IS NOT NULL
  AND pr.ownership_id NOT IN (7, 11, 13, 14)

GROUP BY
  pr.property_name_public,
  pr.internal_record_id,
  pr.landscape_url,
  pa.property_id,
  pr.property_name,
  reg.region_value,
  fae.external_value,
  ast.acquisition_value,
  o.ownership_value,
  pr.ecogift_number,
  pr.date_closed_fiscal,
  pr.public_view,
  pr.notes_sensitivity

ORDER BY pr.property_name_public;