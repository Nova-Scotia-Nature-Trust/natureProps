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
  pr.owner_name AS "Owner Name",
  pr.ecogift_number AS "Ecogift Number",
  pr.donor_vendor AS "Donor / Vendor",
  pr.date_closed_fiscal AS "Fiscal Year Closed",
  pr.date_closed AS "Date Closed",
  pr.llt_funding_secured AS "LLT Funding Secured",
  ca.campaign_value AS "Campaign", 
  pr.public_view AS "Public View",
  pr.notes_sensitivity AS "Sensitivity Notes",
  pa.size_confirmed_ha AS "Size (Hectares)",
  pa.size_confirmed_acres AS "Size (Acres)",
  pa.size_confirmed_notes AS "Size Confirmed Notes"
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
LEFT JOIN campaign ca 
  ON pr.campaign_id = ca.id 
LEFT JOIN parcels pa 
  ON pa.property_id = pr.id
WHERE 
  pr.ownership_id IS NOT NULL 
  AND pr.ownership_id NOT IN (7, 11, 13, 14)
ORDER BY pr.property_name_public, pa.pid;