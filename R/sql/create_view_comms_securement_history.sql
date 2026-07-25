DROP VIEW IF EXISTS view_securement_communication_history;
CREATE VIEW view_securement_communication_history AS 
SELECT
   p.property_name AS "Property Name",
   com.property_contact_id AS "Property Contact ID",
   con.name_first AS "First Name",
   con.name_last AS "Last Name",
   pur.purpose_value AS "Communication Purpose",
   me.method_value AS "Communication Method",
   com.date_contacted AS "Date Contacted",
   com.communication_description AS "Description",
   com.date_follow_up AS "Follow Up Date",
   pid.pids AS "PIDs" 
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
      properties p 
      ON com.property_id = p.id 
   LEFT JOIN
      (
         SELECT
            property_id,
            string_agg(pid::text, ', ' 
         ORDER BY
            pid) AS pids 
         FROM
            parcels 
         GROUP BY
            property_id 
      )
      pid 
      ON p.id = pid.property_id 
WHERE
   pur.purpose_value = 'Securement' 
ORDER BY
   p.property_name;