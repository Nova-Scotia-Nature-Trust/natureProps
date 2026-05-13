DROP VIEW IF EXISTS view_conservation_lands_spatial;
CREATE VIEW view_conservation_lands_spatial AS
SELECT
  pr.property_name_public, 
  pa.pid,
  pr.internal_record_id,
  pa.property_id,
  pr.property_name,  
  reg.region_value AS project_region,
  fae.external_value AS focus_area,
  ast.acquisition_value AS acquisition_securement_type,
  o.ownership_value,
  pr.ecogift_number,
  pr.date_closed_fiscal,
  pr.public_view,
  pr.notes_sensitivity,
  pa.size_confirmed_ha,
  pa.size_confirmed_acres,
  pa.size_confirmed_notes,
  pa.coastal_island,
  pa.coastal_island_id,
  pa.coastline_length,
  pa.freshwater_island,
  pa.shoreline_length,
  pa.old_growth_forest_area,
  pa.karst_forest_area,
  pa.waterbird_colony_id
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