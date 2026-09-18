DROP VIEW IF EXISTS view_property_pricing;

CREATE VIEW view_property_pricing AS
SELECT
    pr.property_name AS "Property Name",
       pr.property_name_public AS "Property Name Public",
    STRING_AGG(DISTINCT pa.pid::text, ', ') AS "PIDs",
    fai.internal_value AS "Focus Area",

    ROUND(ac.acres, 2) AS "Acres",

    ROUND(pr.price_asking, 2) AS "Asking",
    ROUND(pr.price_asking / NULLIF(ac.acres, 0), 2) AS "Asking/acre",

    ROUND(ap.fmv, 2) AS "FMV",
    ROUND(ap.fmv / NULLIF(ac.acres, 0), 2) AS "FMV/acre",
    ap.appraisal_effective_date AS "FMV Effective Date",

    ROUND(pr.price_offer, 2) AS "Offer",
    ROUND(pr.price_offer / NULLIF(ac.acres, 0), 2) AS "Offer/acre",

    ROUND(pr.price_purchase, 2) AS "Purchase",
    ROUND(pr.price_purchase / NULLIF(ac.acres, 0), 2) AS "Purchase/acre",

    ROUND(pr.donated_value, 2) AS "Donated Value",
    ROUND(pr.donated_value / NULLIF(ac.acres, 0), 2) AS "Donated/acre",

    ROUND(pr.unpaid_land_value, 2) AS "Unpaid Land Value",
    ROUND(pr.unpaid_land_value / NULLIF(ac.acres, 0), 2) AS "Unpaid/acre",
    
    pr.price_offer_history AS "Offer Price History"

FROM properties pr

LEFT JOIN focus_area_internal fai ON fai.id = pr.focus_area_internal_id

LEFT JOIN parcels pa
    ON pa.property_id = pr.id

LEFT JOIN appraisals ap
    ON ap.property_id = pr.id
    AND ap.authoritative = TRUE

LEFT JOIN (
    SELECT
        pa.property_id,
        SUM(pi.area_ha * 2.471) AS acres
    FROM parcels pa
    JOIN parcel_info pi
        ON pa.id = pi.parcel_id
    GROUP BY pa.property_id
) ac
    ON ac.property_id = pr.id

WHERE
    pr.price_asking IS NOT NULL
    OR ap.fmv IS NOT NULL
    OR pr.price_offer IS NOT NULL
    OR pr.price_purchase IS NOT NULL
    OR pr.donated_value IS NOT NULL
    OR pr.unpaid_land_value IS NOT NULL

GROUP BY
    pr.id,
    pr.property_name,
    pr.price_asking,
    ap.fmv,
    pr.price_offer,
    pr.price_offer_history,
    pr.price_purchase,
    pr.donated_value,
    pr.unpaid_land_value,
    ac.acres,
    fai.internal_value,
    ap.appraisal_effective_date,
    pr.property_name_public
ORDER BY
    pr.property_name;