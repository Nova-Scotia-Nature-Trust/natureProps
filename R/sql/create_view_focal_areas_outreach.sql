DROP VIEW IF EXISTS view_focal_areas_outreach;

CREATE VIEW view_focal_areas_outreach AS 
SELECT
   fo.internal_value AS "Focus Area (Internal)",
   pa.pid AS "PID",
   pr.property_name AS "Property Name",
   ac.acquisition_value AS "Acquisition Type",
   ph.phase_value AS "Phase",
   rk_sec.ranking_value AS "Securement Priority",
   rk_eco.ranking_value AS "Ecological Priority" 
FROM
   focus_area_internal fo 
   INNER JOIN
      properties pr 
      ON pr.focus_area_internal_id = fo.id 
   INNER JOIN
      parcels pa 
      ON pa.property_id = pr.id 
      AND pa.priority_securement_ranking_id <= 3 
      AND pa.priority_ecological_ranking_id <= 3 
   LEFT JOIN
      ranking rk_sec 
      ON pa.priority_securement_ranking_id = rk_sec.id 
   LEFT JOIN
      ranking rk_eco 
      ON pa.priority_ecological_ranking_id = rk_eco.id 
   LEFT JOIN
      phase ph 
      ON pr.phase_id = ph.id 
   LEFT JOIN
      acquisition_type ac 
      ON pa.acquisition_type_id = ac.id 
ORDER BY
   fo.internal_value,
   pr.property_name,
   pa.pid;