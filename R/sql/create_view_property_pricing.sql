DROP VIEW IF EXISTS view_property_pricing;

CREATE VIEW view_property_pricing AS
SELECT
   pr.property_name AS "Property Name",
   STRING_AGG(pa.pid::text, ', ') AS "PIDs",
   pr.price_asking AS "Asking Price",
   ap.fmv AS "Fair Market Value",   
   pr.price_offer AS "Offer Price",
   pr.price_offer_history AS "Offer Price History", 
   pr.price_purchase AS "Purchase Price",
   pr.donated_value AS "Donated Value",
   pr.unpaid_land_value AS "Unpaid Land Value",
   pr.hst AS "HST"   
FROM
    properties pr 
   LEFT JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN
      appraisals ap
      ON ap.property_id = pr.id
GROUP BY
   pr.property_name,
   pr.price_asking,
   ap.fmv,   
   pr.price_offer,
   pr.price_offer_history, 
   pr.price_purchase,
   pr.donated_value,
   pr.unpaid_land_value,
   pr.hst
ORDER BY
   pr.property_name;
