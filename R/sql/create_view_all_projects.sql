DROP VIEW IF EXISTS view_all_projects;

CREATE VIEW view_all_projects AS
SELECT p.property_name AS "Property Name",
       p.property_name_public AS "Property Name Public",
       p.securement_status AS "Securement Status",
       tl.team_value AS "Team Lead",
       sp.probability_value AS "Securement Probability",
       p.anticipated_closing_year AS "Anticipated Closing Year",
       ph.phase_value AS "Phase",
       p.phase_id_description AS "Phase Description"
FROM properties p
   LEFT JOIN securement_probability sp ON p.securement_probability_id = sp.id
   LEFT JOIN phase ph ON p.phase_id = ph.id
   LEFT JOIN team_lead tl ON p.team_lead_id = tl.id
WHERE sp.probability_value IS NOT NULL AND ph.phase_value != 'Secured'
ORDER BY p.property_name;
   
