DROP VIEW IF EXISTS view_communication_history;

CREATE VIEW view_communication_history AS 
WITH property_info AS 
(
   SELECT
      pa.property_contact_id,
      STRING_AGG(pa.pid::text, ', ' 
   ORDER BY
      pa.pid) AS pids,
      STRING_AGG(DISTINCT pr.property_name, ', ' 
   ORDER BY
      pr.property_name) AS property_names 
   FROM
      parcels pa 
      LEFT JOIN
         properties pr 
         ON pa.property_id = pr.id 
   WHERE
      pa.property_contact_id IS NOT NULL 
   GROUP BY
      pa.property_contact_id 
)
SELECT
   com.property_contact_id AS "Property Conact ID",
   property_info.property_names AS "Property Name(s)",
   con.name_first AS "First Name",
   con.name_last AS "Last Name",
   pur.purpose_value AS "Communication Purpose",
   me.method_value AS "Communication Method",
   com.date_contacted AS "Date Contacted",
   com.communication_description AS "Description",
   com.date_follow_up AS "Follow Up Date",
   property_info.pids AS "PIDs" 
FROM
   property_contact_communication com 
   LEFT JOIN
      property_contact_details con 
      ON com.property_contact_id = con.id 
   LEFT JOIN
      communication_method me 
      ON com.communication_method_id = me.id 
   LEFT JOIN
      communication_purpose pur 
      ON com.communication_purpose_id = pur.id 
   LEFT JOIN
      property_info 
      ON com.property_contact_id = property_info.property_contact_id 
ORDER BY
   property_info.property_names;
