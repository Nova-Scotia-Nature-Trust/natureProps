DROP VIEW IF EXISTS view_appraisals;

CREATE VIEW view_appraisals AS
SELECT
   pr.property_name AS "Property Name",
   STRING_AGG(pa.pid::text, ', ') AS "PIDs",
   ap.appraisal_effective_date AS "Appraisal Effective Date",
   ap.appraiser_name AS "Name of Appraiser",
   ap.fmv AS "Fair Market Value (CAD)",
   ap.appraisal_notes AS "Notes" 
FROM
   appraisals ap 
   LEFT JOIN
      properties pr 
      ON pr.id = ap.property_id 
   LEFT JOIN
      parcels pa 
      ON pa.property_id = pr.id 
GROUP BY
   pr.property_name,
   ap.appraisal_effective_date,
   ap.appraiser_name,
   ap.fmv,
   ap.appraisal_notes 
ORDER BY
   pr.property_name;