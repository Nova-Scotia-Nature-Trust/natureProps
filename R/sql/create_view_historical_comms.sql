DROP VIEW IF EXISTS view_historical_communications;

CREATE VIEW view_historical_communications AS 
SELECT
   pa.pid AS "PID",
   pr.property_name AS "Property Name",
   pa.historical_landowner_notes AS "Landowner History",
   pa.historical_securement_notes AS "Securement History" 
FROM
   parcels pa 
   LEFT JOIN
      properties pr 
      ON pr.id = pa.property_id 
ORDER BY
   pr.property_name asc;