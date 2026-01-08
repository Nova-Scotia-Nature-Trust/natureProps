DROP VIEW IF EXISTS view_insurance;

CREATE VIEW view_insurance AS 
SELECT
   pr.property_name_public AS "Property Name",
   INITCAP( TRIM( REGEXP_REPLACE( MIN(pp.padd_county), '\s*county\s*', '', 'i' ) ) ) AS "County",
   o.ownership_value AS "Ownership",
   pr.owner_name AS "Owner",
   at.acquisition_value AS "Acquistition Type",
   pr.date_closed AS "Date Closed",
   SUM( COALESCE(pa.size_confirmed_ha, ROUND(info.area_ha, 2)) ) AS "Size (ha)",
   SUM( COALESCE( pa.size_confirmed_acres, ROUND(info.area_ha * 2.47105, 2) ) ) AS "Size (acres)" 
FROM
   properties pr 
   LEFT JOIN
      parcels pa 
      ON pr.id = pa.property_id 
   LEFT JOIN
      parcel_padd pp 
      ON pa.id = pp.parcel_id 
   LEFT JOIN
      ownership o 
      ON pr.ownership_id = o.id 
   LEFT JOIN
      acquisition_type at 
      ON pr.acquisition_securement_type_id = at.id 
   LEFT JOIN
      parcel_info info 
      ON pa.id = info.parcel_id 
WHERE
   pr.ownership_id NOT IN 
   (
      11,
      12,
      13,
      14
   )
GROUP BY
   pr.id,
   pr.property_name,
   pr.property_name_public,
   at.acquisition_value,
   o.ownership_value,
   pr.date_closed,
   pr.owner_name
ORDER BY 
   pr.date_closed desc;