DROP VIEW IF EXISTS view_unresolved_inquiries;

CREATE VIEW view_unresolved_inquiries AS
SELECT
    pr.id AS "ID",
    pr.property_name AS "Property Name",
    pr.date_added AS "Date Added",
    ph.phase_value AS "Phase",
    pr.phase_id_description AS "Phase Description",
    pr.phase_id_change AS "Date of Phase Change",
    pr.property_description AS "Property Description"
  FROM properties pr
  JOIN phase ph ON pr.phase_id = ph.id
  WHERE pr.date_added IS NOT NULL
    AND pr.date_added < CURRENT_DATE - INTERVAL '14 days'
    AND pr.date_added >= '2026-06-01'
    AND NOT EXISTS (
      SELECT 1
      FROM team_lead_actions tla
      WHERE tla.property_id = pr.id
    )
ORDER BY pr.date_added;