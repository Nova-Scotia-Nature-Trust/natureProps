 DROP VIEW IF EXISTS view_property_descriptions;

CREATE VIEW view_property_descriptions AS 
WITH prop_with_pids AS 
(
   SELECT
      pr.property_name,
      pr.property_description,
      STRING_AGG(pa.pid, ', ' 
   ORDER BY
      pa.pid) AS pids,
      pr.focus_area_internal_id 
   FROM
      properties pr 
      LEFT JOIN
         parcels pa 
         ON pr.id = pa.property_id 
   GROUP BY
      pr.property_name,
      pr.property_description,
      focus_area_internal_id 
   ORDER BY
      pr.property_name 
)
SELECT
   pwp.property_name AS "Property Name",
   fa.internal_value AS "Internal Focus Area",
   pwp.property_description AS "Property & Opportunity Description",
   pwp.pids AS "PIDs" 
FROM
   prop_with_pids AS pwp 
   LEFT JOIN
      focus_area_internal AS fa 
      ON pwp.focus_area_internal_id = fa.id;