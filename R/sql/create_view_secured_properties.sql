DROP VIEW IF EXISTS view_secured_properties;

CREATE VIEW view_secured_properties AS
SELECT
   pr.property_name_public AS "Public Property Name",
   pr.property_name AS "Property Name",
   STRING_AGG(pa.pid::text, ', ') AS "PIDs",
   SUM(COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471))::numeric(10, 2) AS "Size (Acres)",
   SUM(COALESCE(pa.size_confirmed_ha, pi.area_ha))::numeric(10, 2) AS "Size (Hectares)",
   at.acquisition_value AS "Acquisition Type",
   o.ownership_value AS "Ownership",
   pr.owner_name AS "Owner Name",
   pr.date_closed AS "Date Closed",
   pr.date_closed_fiscal AS "Fiscal Year Closed",
   pr.ecogift_number AS "Ecogift Number",
   pr.donor_vendor AS "Donor / Vendor",
   pr.llt_funding_secured AS "LLT Funding Secured",
   ca.campaign_value AS "Campaign" 
FROM
   properties pr 
   LEFT JOIN
      ownership o 
      ON pr.ownership_id = o.id 
   LEFT JOIN
      acquisition_type at 
      ON pr.acquisition_securement_type_id = at.id 
   LEFT JOIN
      campaign ca 
      ON pr.campaign_id = ca.id 
   LEFT JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN
      parcel_info pi 
      ON pi.parcel_id = pa.id 
WHERE
   pr.ownership_id IS NOT NULL AND pr.ownership_id NOT IN (7, 14) 
GROUP BY
   pr.id,
   pr.property_name_public,
   pr.property_name,
   at.acquisition_value,
   o.ownership_value,
   pr.owner_name,
   pr.date_closed,
   pr.date_closed_fiscal,
   pr.ecogift_number,
   pr.donor_vendor,
   pr.llt_funding_secured,
   ca.campaign_value 
ORDER BY
   pr.property_name_public;