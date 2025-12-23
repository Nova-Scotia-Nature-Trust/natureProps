DROP VIEW IF EXISTS view_focal_areas_securement;

CREATE VIEW view_focal_areas_securement AS 
SELECT
   fo.internal_value AS "Focus Area (Internal)",
   pr.property_name_public AS "Property Name",
   pr.date_closed_fiscal AS "Fiscal Closing Date",
   SUM( COALESCE(pa.size_confirmed_ha, ROUND(info.area_ha, 2)) ) AS "Size (ha)",
   SUM( COALESCE(pa.size_confirmed_acres, ROUND(info.area_ha * 2.47105, 2)) ) AS "Size (acres)",
   ROUND(pr.price_purchase, 0) AS "Purchase Price",
   ROUND( pr.price_purchase / SUM( COALESCE(pa.size_confirmed_ha, info.area_ha) * 2.47105 ), 0 ) AS "Price Per Acre" 
FROM
   focus_area_internal fo 
   INNER JOIN
      properties pr 
      ON pr.focus_area_internal_id = fo.id 
   INNER JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN
      parcel_info info 
      ON pa.id = info.parcel_id 
WHERE
   pr.ownership_id NOT IN 
   (
      11,
      12,
      13
   )
GROUP BY
   fo.internal_value,
   pr.property_name_public,
   pr.date_closed_fiscal,
   pr.price_purchase 
ORDER BY
   fo.internal_value,
   pr.property_name_public;