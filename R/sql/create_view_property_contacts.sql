DROP VIEW IF EXISTS view_property_contacts;

CREATE VIEW view_property_contacts AS 
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
	pr.property_names AS "Property Name",
	pr.pids AS "Property Contact PIDs",
	con.name_last AS "Last Name", 
	con.name_first AS "First Name",
	con.email AS "Email",
	con.phone_home AS "Home Phone",
	con.phone_cell AS "Mobile Phone",
	con.property_contact_description AS "Contact Description",
	con.dnc AS "DNC"
FROM property_contact_details con 
LEFT JOIN property_info pr ON con.id = pr.property_contact_id;