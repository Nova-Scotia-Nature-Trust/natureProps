DROP VIEW IF EXISTS view_property_sizes;

CREATE VIEW view_property_sizes AS
SELECT
   pr.property_name AS "Property",
   pa.pid AS "PID",
   CASE
      WHEN
         pa.size_confirmed_acres IS NULL 
      THEN
         'Unconfirmed' 
      ELSE
         'Confirmed' 
   END
   AS "Size status", 
   pa.size_confirmed_acres AS "Confirmed Size (acres)", 
   pa.size_confirmed_ha AS "Confirmed Size (ha)", 
   ROUND(pi.area_ha * 2.471, 2) AS "POL Size (acres)", 
   ROUND(pi.area_ha, 2) AS "POL Size (ha)", 
   pa.size_confirmed_notes AS "Notes", 
   ROUND(COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471),2) AS "Reporting Size (acres)", 
   ROUND(COALESCE(pa.size_confirmed_ha, pi.area_ha), 2) AS "Reporting Size (ha)" 
FROM
   properties pr 
   JOIN
      parcels pa 
      ON pr.id = pa.property_id 
   LEFT JOIN
      parcel_info pi 
      ON pa.id = pi.parcel_id 
WHERE
   pr.ownership_id IS NOT NULL 
   AND pr.ownership_id != 7 
ORDER BY
   pr.property_name, pid;