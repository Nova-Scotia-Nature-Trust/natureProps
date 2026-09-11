DROP VIEW IF EXISTS view_appraisals;

CREATE VIEW view_appraisals AS
 SELECT
   pr.property_name AS "Property Name",
   pr.property_name_public AS "Property Name Public",
   STRING_AGG(pa.pid::text, ', ') AS "PIDs",
   ap.appraisal_effective_date AS "Appraisal Effective Date",
   ap.appraiser_name AS "Name of Appraiser",
   ROUND(ap.fmv, 2) AS "Fair Market Value (CAD)",
   ROUND(ap.fmv / NULLIF(SUM(pi.area_ha * 2.471), 0), 2) AS "FMV/acre",
   ap.authoritative AS "Authoritative Appraisal",
   ap.appraisal_notes AS "Notes" 
FROM
   appraisals ap 
   LEFT JOIN
      properties pr 
      ON pr.id = ap.property_id 
   LEFT JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN 
      parcel_info pi 
      ON pa.id = pi.parcel_id
GROUP BY
   pr.property_name,
   ap.appraisal_effective_date,
   ap.appraiser_name,
   ap.fmv,
   ap.authoritative,
   ap.appraisal_notes,
   pr.property_name_public
ORDER BY
   pr.property_name;