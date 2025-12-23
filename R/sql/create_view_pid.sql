DROP VIEW IF EXISTS view_pid;

CREATE VIEW view_pid AS 
SELECT
   pa.pid AS "PID",
   pr.property_name AS "Property Name",
   pa.date_added AS "Date Added",
   pa.date_updated AS "Date Updated",   
   info.area_ha AS "Size (ha)",
   ROUND(info.area_ha * 2.47105, 2) AS "Size (acres)",
   ac.acquisition_value AS "Acquisition Type",
   ph.phase_value AS "Phase",
   rk_sec.ranking_value AS "Securement Priority",
   rk_eco.ranking_value AS "Ecological Priority",
   rk_own.ranking_value AS "Landowner Interest"
FROM
     properties pr 
   INNER JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN
      ranking rk_sec 
      ON pa.priority_securement_ranking_id = rk_sec.id 
   LEFT JOIN
      ranking rk_eco 
      ON pa.priority_ecological_ranking_id = rk_eco.id 
   LEFT JOIN
      ranking rk_own 
      ON pa.landowner_interest_ranking_id = rk_own.id
   LEFT JOIN
      phase ph 
      ON pr.phase_id = ph.id 
   LEFT JOIN
      acquisition_type ac 
      ON pa.acquisition_type_id = ac.id 
   LEFT JOIN parcel_info info 
      ON pa.id = info.parcel_id
ORDER BY
   pr.property_name,
   pa.pid;