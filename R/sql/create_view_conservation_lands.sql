DROP VIEW IF EXISTS view_conservation_lands;
CREATE VIEW view_conservation_lands AS
SELECT
  pr.property_name_public AS "Public Property Name",
  pa.pid AS "PID",
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
  pa.size_confirmed_ha AS "Size (Hectares)",
  pa.size_confirmed_acres AS "Size (Acres)",
  pa.size_confirmed_notes AS "Size Confirmed Notes",
  pa.coastal_island AS "Coastal Island",
  pa.coastal_island_id AS "Coastal Island ID",
  pa.coastline_length AS "Coastline Length",
  pa.freshwater_island AS "Freshwater Island",
  pa.shoreline_length AS "Shoreline Length",
  pa.old_growth_forest_area AS "Old Growth Forest Area",
  pa.karst_forest_area AS "Karst Forest Area",
  pa.waterbird_colony_id AS "Waterbird Colony ID"
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
WHERE 
  pr.ownership_id IS NOT NULL 
  AND pr.ownership_id NOT IN (7, 11, 13, 14)
ORDER BY pr.property_name_public, pa.pid;