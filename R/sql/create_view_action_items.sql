DROP VIEW IF EXISTS view_securement_action_items;

CREATE VIEW view_securement_action_items AS 
SELECT
    pr.property_name AS "Property Name",
    pr.property_name_public AS "Property Name Public",
    STRING_AGG(pa.pid::text, ', ') AS "PIDs",    
    ait.type_value AS "Action Item",
    sai.action_due_date AS "Due Date",
    tl.team_value AS "Team Lead",
    ais.status_value AS "Status",
    sai.action_completed_date AS "Date Completed",
    sai.action_item_notes AS "Notes"
FROM
    properties pr
    JOIN parcels pa
        ON pa.property_id = pr.id
    JOIN securement_action_items sai
        ON sai.property_id = pr.id
    LEFT JOIN action_item_type ait
        ON sai.action_item_type_id = ait.id
    LEFT JOIN action_item_status ais
        ON sai.action_item_status_id = ais.id
    LEFT JOIN team_lead tl
        ON sai.team_lead_id = tl.id
    LEFT JOIN phase ph
        ON pr.phase_id = ph.id
WHERE
    pr.securement_probability_id IS NOT NULL OR ph.phase_value = 'Secured'
GROUP BY
    pr.property_name,
    pr.property_name_public,
    ait.id,
    ait.type_value,
    sai.action_due_date,
    tl.team_value,
    ais.status_value,
    sai.action_completed_date,
    sai.action_item_notes
ORDER BY
    pr.property_name_public,
    ait.id;
