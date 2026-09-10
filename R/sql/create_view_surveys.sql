DROP VIEW IF EXISTS view_surveys;

CREATE VIEW view_surveys AS
SELECT
    p.property_name AS "Property Name",
    p.property_name_public AS "Property Name Public",
    s.company AS "Survey Company",
    s.timeline AS "Timeline",
    s.amount_quote AS "Quoted Amount",
    s.amount_paid AS "Amount Paid",
    s.paid_date AS "Invoice Paid Date",
    s.survey_notes AS "Survey Notes"
FROM surveys s
LEFT JOIN properties p
    ON s.property_id = p.id;