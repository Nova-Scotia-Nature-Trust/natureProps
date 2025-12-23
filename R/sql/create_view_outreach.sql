DROP VIEW IF EXISTS view_outreach;

CREATE VIEW view_outreach AS 
SELECT
   pa.pid AS "PID",
   pur.purpose_value AS "Communication Purpose",
   me.method_value AS "Communication Method",
   ou.outreach_description AS "Description",
   ou.date_contacted AS "Date Contacted",
   ou.date_follow_up AS "Date Follow Up",
   ou.dnc AS "DNC" 
FROM
   outreach ou 
   LEFT JOIN
      parcels pa 
      ON ou.parcel_id = pa.id 
   LEFT JOIN
      communication_method me 
      ON ou.communication_method_id = me.id 
   LEFT JOIN
      communication_purpose pur 
      ON ou.communication_purpose_id = pur.id;