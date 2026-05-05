DROP VIEW IF EXISTS view_llt_projects;

CREATE VIEW view_llt_projects AS
SELECT
   pr.property_name_public AS "Property Name",
   ll.legacy_property_name AS "LLT Property Name",
   pr.date_closed AS "Date Protected",
   ll.date_funding_received AS "Date LLT Funding Rec'd ",
   ll.funding_value AS "Endowement Funding Amount",
   ll.endowment_notes AS "Notes on Endowment",
   CASE
      WHEN
         ll.stewardship_plan_complete = TRUE 
      THEN
         'Yes' 
      ELSE
         'In Progress' 
   END
   AS "Stewardship Plan Completed", 
   ll.stewardship_plan_notes AS "Stewardship Plan Notes", 
   'Yes' AS "Annual Monitoring Program" 
FROM
   llt_projects ll 
   LEFT JOIN
      properties pr 
      ON ll.property_id = pr.id 
ORDER BY
   pr.date_closed ASC;