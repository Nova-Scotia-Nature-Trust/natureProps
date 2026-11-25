DROP VIEW IF EXISTS view_secured_properties_pricing;

CREATE VIEW view_secured_properties_pricing AS 
SELECT
   COALESCE(fo.internal_value, 'Unassigned') AS "Focus Area (Internal)",
   pr.property_name_public AS "Property Name",
   pr.date_closed_fiscal AS "Fiscal Closing Date",
   SUM( COALESCE(pa.size_confirmed_ha, ROUND(info.area_ha, 2)) ) AS "Size (ha)",
   SUM( COALESCE(pa.size_confirmed_acres, ROUND(info.area_ha * 2.47105, 2)) ) AS "Size (acres)",
   ROUND(pr.price_purchase, 0) AS "Purchase Price",
   ROUND(pr.price_purchase / SUM( COALESCE(pa.size_confirmed_ha, info.area_ha) * 2.47105 ), 0 ) AS "Price Per Acre",
   ROUND(AVG(ap.fmv), 0) AS "Fair Market Value (CAD)",
   ROUND(
       AVG(ap.fmv) / SUM( COALESCE(pa.size_confirmed_ha, info.area_ha) * 2.47105 ), 0
   ) AS "FMV Price Per Acre"  
FROM
   properties pr 
   LEFT JOIN
      focus_area_internal fo 
      ON pr.focus_area_internal_id = fo.id 
   INNER JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN
      parcel_info info 
      ON pa.id = info.parcel_id 
   LEFT JOIN
      appraisals ap
      ON ap.property_id = pr.id
WHERE
   pr.ownership_id NOT IN (11, 12, 13, 14)
GROUP BY
   COALESCE(fo.internal_value, 'Unassigned'),
   pr.property_name_public,
   pr.date_closed_fiscal,
   pr.price_purchase
ORDER BY
   COALESCE(fo.internal_value, 'Unassigned'),
   pr.property_name_public;
