DROP VIEW IF EXISTS view_landowner_changes;
CREATE VIEW view_landowner_changes AS
SELECT
   lc.pid                AS "PID",
   lc.property_name      AS "Property Name",
   lc.phase              AS "Phase",
   lc.status             AS "Status",
   lc.owner_name_last    AS "Last Name",
   lc.owner_name_first   AS "First Name",
   lc.owner_name_middle  AS "Middle Name",
   lc.owner_name_desig   AS "Designation",
   lc.owner_name_corp    AS "Corporation Name",
   lc.interest           AS "Interest",
   lc.date_of_change     AS "Date of Change"
FROM
   landowner_changes lc
ORDER BY
   lc.date_of_change DESC,
   lc.pid,
   CASE lc.status
      WHEN 'Previous Owner' THEN 1
      WHEN 'New Owner'      THEN 2
      WHEN 'No Change'      THEN 3
      ELSE 4
   END;
