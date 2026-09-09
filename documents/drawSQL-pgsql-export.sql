CREATE TABLE "action_item_status"(
    "id" SERIAL NOT NULL,
    "status_value" TEXT NOT NULL
);
ALTER TABLE
    "action_item_status" ADD PRIMARY KEY("id");
ALTER TABLE
    "action_item_status" ADD CONSTRAINT "action_item_status_status_value_unique" UNIQUE("status_value");
CREATE TABLE "outreach"(
    "id" SERIAL NOT NULL,
    "parcel_id" INTEGER NOT NULL,
    "dnc" BOOLEAN NOT NULL DEFAULT FALSE,
    "communication_purpose_id" INTEGER NOT NULL,
    "communication_method_id" INTEGER NOT NULL,
    "date_contacted" DATE NULL,
    "outreach_description" TEXT NULL,
    "date_follow_up" DATE NULL
);
ALTER TABLE
    "outreach" ADD PRIMARY KEY("id");
COMMENT
ON COLUMN
    "outreach"."dnc" IS 'Do Not Contact (DNC) where TRUE indicates that contact should not be made. Add the following constraint to ensure only true/false values: ALTER TABLE outreach ADD CONSTRAINT chk_dnc CHECK (dnc IN (TRUE, FALSE));';
CREATE TABLE "landowners"(
    "id" SERIAL NOT NULL,
    "parcel_id" INTEGER NOT NULL,
    "owner_name_last" TEXT NULL,
    "owner_name_first" TEXT NULL,
    "owner_name_middle" TEXT NULL,
    "owner_name_desig" TEXT NULL,
    "owner_name_corp" TEXT NULL,
    "interest" TEXT NULL,
    "qualifier" TEXT NULL
);
COMMENT
ON TABLE
    "landowners" IS 'Add constraint

DROP INDEX IF EXISTS "public"."unique_landowner";

CREATE UNIQUE INDEX unique_landowner
ON "public"."landowners" ("parcel_id", "owner_name_last", "owner_name_first", "owner_name_middle", 
                           "owner_name_desig", "owner_name_corp", "interest", "qualifier")
NULLS NOT DISTINCT;';
ALTER TABLE
    "landowners" ADD PRIMARY KEY("id");
COMMENT
ON COLUMN
    "landowners"."qualifier" IS 'Qualifier for Interest Holder';
CREATE TABLE "communication_method"(
    "id" SERIAL NOT NULL,
    "method_value" TEXT NOT NULL
);
ALTER TABLE
    "communication_method" ADD PRIMARY KEY("id");
ALTER TABLE
    "communication_method" ADD CONSTRAINT "communication_method_method_value_unique" UNIQUE("method_value");
CREATE TABLE "focus_area_internal"(
    "id" SERIAL NOT NULL,
    "internal_value" TEXT NOT NULL,
    "focus_area_external_id" INTEGER NULL
);
ALTER TABLE
    "focus_area_internal" ADD PRIMARY KEY("id");
ALTER TABLE
    "focus_area_internal" ADD CONSTRAINT "focus_area_internal_internal_value_unique" UNIQUE("internal_value");
CREATE TABLE "project_theme"(
    "id" SERIAL NOT NULL,
    "theme_value" TEXT NOT NULL
);
ALTER TABLE
    "project_theme" ADD PRIMARY KEY("id");
ALTER TABLE
    "project_theme" ADD CONSTRAINT "project_theme_theme_value_unique" UNIQUE("theme_value");
CREATE TABLE "project_region"(
    "id" SERIAL NOT NULL,
    "region_value" TEXT NOT NULL
);
ALTER TABLE
    "project_region" ADD PRIMARY KEY("id");
ALTER TABLE
    "project_region" ADD CONSTRAINT "project_region_region_value_unique" UNIQUE("region_value");
CREATE TABLE "properties"(
    "id" SERIAL NOT NULL,
    "property_name" TEXT NOT NULL,
    "property_name_public" TEXT NULL,
    "property_description" TEXT NULL,
    "date_added" DATE NULL,
    "date_updated" DATE NULL,
    "internal_record_id" TEXT NULL,
    "landscape_legacy_name" TEXT NULL,
    "landscape_url" TEXT NULL,
    "source_id" INTEGER NULL,
    "phase_id" INTEGER NULL,
    "phase_id_description" TEXT NULL,
    "phase_id_followup" DATE NULL,
    "phase_id_change" DATE NULL,
    "team_lead_id" INTEGER NULL,
    "securement_probability_id" INTEGER NULL,
    "securement_status" TEXT NULL,
    "date_securement_status" DATE NULL,
    "anticipated_closing_date" DATE NULL,
    "anticipated_closing_year" TEXT NULL,
    "aps_conditions_date" DATE NULL,
    "project_region_id" INTEGER NULL,
    "focus_area_internal_id" INTEGER NULL,
    "acquisition_securement_type_id" INTEGER NULL,
    "donor_vendor" TEXT NULL,
    "ownership_id" INTEGER NULL,
    "owner_name" TEXT NULL,
    "ecogift_number" TEXT NULL,
    "date_closed_fiscal" TEXT NULL,
    "date_closed" DATE NULL,
    "llt_funding_secured" BOOLEAN NULL,
    "campaign_id" INTEGER NULL,
    "public_view" BOOLEAN NULL,
    "notes_sensitivity" TEXT NULL,
    "stewardship_concerns" TEXT NULL,
    "price_asking" DECIMAL(12, 2) NULL,
    "price_offer" DECIMAL(12, 2) NULL,
    "price_offer_history" TEXT NULL,
    "price_purchase" DECIMAL(12, 2) NULL,
    "donated_value" DECIMAL(12, 2) NULL,
    "unpaid_land_value" DECIMAL(12, 2) NULL,
    "hst" BOOLEAN NULL,
    "structure" TEXT NULL,
    "structure_details" TEXT NULL,
    "project_feasibility_ranking_id" INTEGER NULL,
    "project_feasibility_ranking_reason" INTEGER NOT NULL
);
ALTER TABLE
    "properties" ADD PRIMARY KEY("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_property_name_unique" UNIQUE("property_name");
COMMENT
ON COLUMN
    "properties"."anticipated_closing_year" IS 'Add the following check to ensure valid values like ("2024/2025")

ALTER TABLE parcels
ADD CONSTRAINT closing_year_format_check 
CHECK (closing_year ~ ''^[0-9]{4}/[0-9]{4}$'' AND 
       substring(closing_year FROM 1 FOR 4)::INTEGER + 1 = 
       substring(closing_year FROM 6 FOR 4)::INTEGER);';
CREATE TABLE "phase"(
    "id" SERIAL NOT NULL,
    "phase_value" TEXT NOT NULL
);
ALTER TABLE
    "phase" ADD PRIMARY KEY("id");
ALTER TABLE
    "phase" ADD CONSTRAINT "phase_phase_value_unique" UNIQUE("phase_value");
COMMENT
ON COLUMN
    "phase"."phase_value" IS 'Refers to phase of securement';
CREATE TABLE "acquisition_type"(
    "id" SERIAL NOT NULL,
    "acquisition_value" TEXT NOT NULL
);
ALTER TABLE
    "acquisition_type" ADD PRIMARY KEY("id");
ALTER TABLE
    "acquisition_type" ADD CONSTRAINT "acquisition_type_acquisition_value_unique" UNIQUE("acquisition_value");
CREATE TABLE "parcels"(
    "id" SERIAL NOT NULL,
    "pid" CHAR(8) NOT NULL,
    "property_id" INTEGER NULL,
    "acquisition_type_id" INTEGER NULL,
    "priority_securement_ranking_id" INTEGER NULL,
    "priority_securement_ranking_reason" TEXT NULL,
    "priority_ecological_ranking_id" INTEGER NULL,
    "priority_ecological_ranking_reason" TEXT NULL,
    "af_transaction" BOOLEAN NULL,
    "size_confirmed_ha" DECIMAL(8, 2) NULL,
    "size_confirmed_acres" DECIMAL(8, 2) NULL,
    "size_confirmed_notes" TEXT NULL,
    "historical_securement_action" TEXT NULL,
    "historical_landowner_notes" TEXT NULL,
    "historical_securement_notes" TEXT NULL,
    "tax_exempt" BOOLEAN NULL,
    "tax_exempt_year" INTEGER NULL
);
ALTER TABLE
    "parcels" ADD PRIMARY KEY("id");
ALTER TABLE
    "parcels" ADD CONSTRAINT "parcels_pid_unique" UNIQUE("pid");
COMMENT
ON COLUMN
    "parcels"."pid" IS 'Add CHECK  after creating table.

ALTER TABLE parcels
ADD CONSTRAINT pid_numeric_8_digits
CHECK (pid ~ ''^\d{8}$'');';
COMMENT
ON COLUMN
    "parcels"."af_transaction" IS 'Add the following constraint to ensure only true/false values: ALTER TABLE parcels ADD CONSTRAINT chk_af_transaction CHECK (af_transaction IN (TRUE, FALSE));';
CREATE TABLE "ownership"(
    "id" SERIAL NOT NULL,
    "ownership_value" TEXT NOT NULL
);
ALTER TABLE
    "ownership" ADD PRIMARY KEY("id");
ALTER TABLE
    "ownership" ADD CONSTRAINT "ownership_ownership_value_unique" UNIQUE("ownership_value");
CREATE TABLE "property_theme"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "project_theme_id" INTEGER NOT NULL
);
ALTER TABLE
    "property_theme" ADD PRIMARY KEY("id");
CREATE TABLE "focus_area_external"(
    "id" SERIAL NOT NULL,
    "external_value" TEXT NOT NULL,
    "public_visible" BOOLEAN NOT NULL
);
ALTER TABLE
    "focus_area_external" ADD PRIMARY KEY("id");
ALTER TABLE
    "focus_area_external" ADD CONSTRAINT "focus_area_external_external_value_unique" UNIQUE("external_value");
CREATE TABLE "ranking"(
    "id" SERIAL NOT NULL,
    "ranking_value" TEXT NOT NULL
);
ALTER TABLE
    "ranking" ADD PRIMARY KEY("id");
ALTER TABLE
    "ranking" ADD CONSTRAINT "ranking_ranking_value_unique" UNIQUE("ranking_value");
CREATE TABLE "source"(
    "id" SERIAL NOT NULL,
    "source_value" TEXT NOT NULL
);
ALTER TABLE
    "source" ADD PRIMARY KEY("id");
ALTER TABLE
    "source" ADD CONSTRAINT "source_source_value_unique" UNIQUE("source_value");
CREATE TABLE "communication_purpose"(
    "id" SERIAL NOT NULL,
    "purpose_value" TEXT NOT NULL
);
ALTER TABLE
    "communication_purpose" ADD PRIMARY KEY("id");
ALTER TABLE
    "communication_purpose" ADD CONSTRAINT "communication_purpose_purpose_value_unique" UNIQUE("purpose_value");
CREATE TABLE "parcel_padd"(
    "id" SERIAL NOT NULL,
    "parcel_id" INTEGER NOT NULL,
    "padd_num" TEXT NULL,
    "padd_num_suffix" TEXT NULL,
    "padd_direction" TEXT NULL,
    "padd_street" TEXT NULL,
    "padd_street_type" TEXT NULL,
    "padd_apartment" TEXT NULL,
    "padd_city" TEXT NULL,
    "padd_lot_type" TEXT NULL,
    "padd_lot" TEXT NULL,
    "padd_municipal" TEXT NULL,
    "padd_county" TEXT NULL
);
ALTER TABLE
    "parcel_padd" ADD PRIMARY KEY("id");
CREATE TABLE "securement_probability"(
    "id" SERIAL NOT NULL,
    "probability_value" TEXT NOT NULL
);
ALTER TABLE
    "securement_probability" ADD PRIMARY KEY("id");
ALTER TABLE
    "securement_probability" ADD CONSTRAINT "securement_probability_probability_value_unique" UNIQUE("probability_value");
CREATE TABLE "parcel_info"(
    "id" SERIAL NOT NULL,
    "parcel_id" INTEGER NOT NULL,
    "parcel_type" TEXT NULL,
    "area" DECIMAL(8, 2) NULL,
    "area_unit" TEXT NULL,
    "area_ha" DECIMAL(8, 2) NULL,
    "area_certified" TEXT NULL,
    "owner_status" TEXT NULL,
    "pid_status" INTEGER NULL,
    "pid_creation_date" TEXT NULL,
    "land_title" INTEGER NULL
);
ALTER TABLE
    "parcel_info" ADD PRIMARY KEY("id");
ALTER TABLE
    "parcel_info" ADD CONSTRAINT "parcel_info_parcel_id_unique" UNIQUE("parcel_id");
COMMENT
ON COLUMN
    "parcel_info"."parcel_type" IS '(blank) Standard parcel
	AE	Arbitrary Railroad
	AR	Arbitrary Road Parcel
	AW	Arbitrary Water Parcel
	CC	Condominium Common
	CU	Condominium Unit
	IR	Indian Reservation
	LC	Local Common
	LW	Land and Water
	MP	Marsh Parcel
	PR	Private Road
	RD	Road Parcel
	RM	Remainder Parcel
	RR	Railway Parcel
	SP	Subdivision Problem
	UA	Unresolved Area
	UP	Unresolved Parcel
	UT	Unresolved Title
	WL	Water Lot
	DP	Dummy PID
	PM	Pending Migration
	PD	Pending Document
	XE	Spatial Element
	XP	Spatial Parcel';
COMMENT
ON COLUMN
    "parcel_info"."area_unit" IS 'A	ACRES
	F	SQUARE FEET
	H	HECTARES
	M	SQUARE METRES
	V	CUBIC METRES (VOLUME)';
COMMENT
ON COLUMN
    "parcel_info"."area_certified" IS 'A	ASSESSMENT
 	D	DEED
	G	GIS
	M	PLANIMETER
	P	PLAN
	S	SCALED
	U	UNKNOWN
	V	VERIFIED';
COMMENT
ON COLUMN
    "parcel_info"."owner_status" IS '(blank) = UNKNOWN
JT = JOINT TENANT
TC = TENANTS IN COMMON';
COMMENT
ON COLUMN
    "parcel_info"."pid_status" IS 'Status of PID - "1" = Active, "2" = Retired';
COMMENT
ON COLUMN
    "parcel_info"."land_title" IS 'Registered under LAND TITLES ACT  or Not. "1" = Land Registration, "2" = Not Land Registration';
CREATE TABLE "parcel_madd"(
    "id" SERIAL NOT NULL,
    "parcel_id" INTEGER NOT NULL,
    "aan" TEXT NULL,
    "madd_number" TEXT NULL,
    "madd_number_suffix" TEXT NULL,
    "madd_street" TEXT NULL,
    "madd_street_type" TEXT NULL,
    "madd_apartment_num" TEXT NULL,
    "madd_city" TEXT NULL,
    "madd_province" TEXT NULL,
    "madd_country" TEXT NULL,
    "madd_postal_code" TEXT NULL
);
COMMENT
ON TABLE
    "parcel_madd" IS 'Add constraint 

DROP INDEX IF EXISTS "public"."unique_parcel_madd_address";

CREATE UNIQUE INDEX unique_parcel_madd_address
ON "public"."parcel_madd" ("parcel_id", "aan", "madd_number", "madd_number_suffix", 
                            "madd_street", "madd_street_type", "madd_apartment_num", 
                            "madd_city", "madd_province", "madd_country", "madd_postal_code")
NULLS NOT DISTINCT;';
ALTER TABLE
    "parcel_madd" ADD PRIMARY KEY("id");
CREATE TABLE "property_contact_details"(
    "id" SERIAL NOT NULL,
    "name_last" TEXT NULL,
    "name_first" TEXT NULL,
    "email" TEXT NULL,
    "phone_home" TEXT NULL,
    "phone_cell" TEXT NULL,
    "dnc" BOOLEAN NOT NULL DEFAULT FALSE,
    "property_contact_description" TEXT NULL,
    "re_constituent_id" TEXT NULL,
    "address_line1" TEXT NULL,
    "address_line2" TEXT NULL,
    "city" TEXT NULL,
    "state_province_code" TEXT NULL,
    "postal_code" TEXT NULL,
    "country_code" CHAR(2) NULL
);
ALTER TABLE
    "property_contact_details" ADD PRIMARY KEY("id");
COMMENT
ON COLUMN
    "property_contact_details"."dnc" IS 'Do Not Contact (DNC) where TRUE indicates that contact should not be made. Add the following constraint to ensure only true/false values: ALTER TABLE outreach ADD CONSTRAINT chk_dnc CHECK (dnc IN (TRUE, FALSE));';
CREATE TABLE "property_contact_communication"(
    "id" SERIAL NOT NULL,
    "property_contact_id" INTEGER NOT NULL,
    "property_id" INTEGER NOT NULL,
    "communication_purpose_id" INTEGER NOT NULL,
    "communication_method_id" INTEGER NOT NULL,
    "date_contacted" DATE NULL,
    "communication_description" TEXT NULL,
    "date_follow_up" DATE NULL
);
ALTER TABLE
    "property_contact_communication" ADD PRIMARY KEY("id");
CREATE TABLE "fund_federal"(
    "id" SERIAL NOT NULL,
    "federal_value" TEXT NOT NULL
);
ALTER TABLE
    "fund_federal" ADD PRIMARY KEY("id");
ALTER TABLE
    "fund_federal" ADD CONSTRAINT "fund_federal_federal_value_unique" UNIQUE("federal_value");
CREATE TABLE "campaign"(
    "id" SERIAL NOT NULL,
    "campaign_value" TEXT NOT NULL
);
ALTER TABLE
    "campaign" ADD PRIMARY KEY("id");
ALTER TABLE
    "campaign" ADD CONSTRAINT "campaign_campaign_value_unique" UNIQUE("campaign_value");
CREATE TABLE "acquisition_securement_type"(
    "id" SERIAL NOT NULL,
    "acquisition_value" TEXT NOT NULL
);
ALTER TABLE
    "acquisition_securement_type" ADD PRIMARY KEY("id");
ALTER TABLE
    "acquisition_securement_type" ADD CONSTRAINT "acquisition_securement_type_acquisition_value_unique" UNIQUE("acquisition_value");
CREATE TABLE "team_lead"(
    "id" SERIAL NOT NULL,
    "team_value" TEXT NOT NULL
);
ALTER TABLE
    "team_lead" ADD PRIMARY KEY("id");
ALTER TABLE
    "team_lead" ADD CONSTRAINT "team_lead_team_value_unique" UNIQUE("team_value");
CREATE TABLE "internal_communications"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "date" DATE NOT NULL,
    "communication_description" TEXT NOT NULL
);
ALTER TABLE
    "internal_communications" ADD PRIMARY KEY("id");
CREATE TABLE "team_lead_actions"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "team_lead_id" INTEGER NOT NULL,
    "action_item_description" TEXT NOT NULL,
    "due_date" DATE NULL,
    "action_complete" BOOLEAN NOT NULL DEFAULT FALSE,
    "date_completed" DATE NOT NULL
);
ALTER TABLE
    "team_lead_actions" ADD PRIMARY KEY("id");
CREATE TABLE "appraisals"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "appraisal_effective_date" DATE NOT NULL,
    "appraiser_name" TEXT NOT NULL,
    "fmv" DECIMAL(12, 2) NOT NULL,
    "appraisal_notes" TEXT NULL
);
ALTER TABLE
    "appraisals" ADD PRIMARY KEY("id");
CREATE TABLE "securement_action_items"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "action_item_type_id" INTEGER NOT NULL,
    "action_due_date" DATE NULL,
    "team_lead_id" INTEGER NULL,
    "action_item_status_id" INTEGER NULL,
    "action_completed_date" DATE NULL,
    "action_item_notes" TEXT NULL
);
ALTER TABLE
    "securement_action_items" ADD PRIMARY KEY("id");
CREATE TABLE "action_item_type"(
    "id" SERIAL NOT NULL,
    "type_value" TEXT NOT NULL
);
ALTER TABLE
    "action_item_type" ADD PRIMARY KEY("id");
ALTER TABLE
    "action_item_type" ADD CONSTRAINT "action_item_type_type_value_unique" UNIQUE("type_value");
CREATE TABLE "properties_contact"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "property_contact_id" INTEGER NOT NULL
);
COMMENT
ON TABLE
    "properties_contact" IS 'Need to add a unique constraint for parcel and prop contact id';
ALTER TABLE
    "properties_contact" ADD PRIMARY KEY("id");
CREATE TABLE "property_fund_federal"(
    "id" SERIAL NOT NULL,
    "fund_federal_id" INTEGER NOT NULL,
    "property_id" INTEGER NOT NULL
);
ALTER TABLE
    "property_fund_federal" ADD PRIMARY KEY("id");
CREATE TABLE "fund"(
    "id" SERIAL NOT NULL,
    "fund_value" INTEGER NOT NULL
);
ALTER TABLE
    "fund" ADD PRIMARY KEY("id");
ALTER TABLE
    "fund" ADD CONSTRAINT "fund_fund_value_unique" UNIQUE("fund_value");
CREATE TABLE "property_fund"(
    "id" SERIAL NOT NULL,
    "fund_id" INTEGER NOT NULL,
    "property_id" INTEGER NOT NULL
);
ALTER TABLE
    "property_fund" ADD PRIMARY KEY("id");
CREATE TABLE "llt_projects"(
    "id" SERIAL NOT NULL,
    "property_id" INTEGER NOT NULL,
    "legacy_property_name" TEXT NULL,
    "date_funding_received" DATE NULL,
    "funding_value" INTEGER NOT NULL,
    "endowment_notes" TEXT NULL,
    "stewardship_plan_complete" BOOLEAN NOT NULL,
    "stewardship_plan_notes" TEXT NULL
);
ALTER TABLE
    "llt_projects" ADD PRIMARY KEY("id");
ALTER TABLE
    "property_fund_federal" ADD CONSTRAINT "property_fund_federal_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_ownership_id_foreign" FOREIGN KEY("ownership_id") REFERENCES "ownership"("id");
ALTER TABLE
    "outreach" ADD CONSTRAINT "outreach_communication_method_id_foreign" FOREIGN KEY("communication_method_id") REFERENCES "communication_method"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_acquisition_securement_type_id_foreign" FOREIGN KEY("acquisition_securement_type_id") REFERENCES "acquisition_securement_type"("id");
ALTER TABLE
    "property_contact_communication" ADD CONSTRAINT "property_contact_communication_communication_purpose_id_foreign" FOREIGN KEY("communication_purpose_id") REFERENCES "communication_purpose"("id");
ALTER TABLE
    "property_fund_federal" ADD CONSTRAINT "property_fund_federal_fund_federal_id_foreign" FOREIGN KEY("fund_federal_id") REFERENCES "fund_federal"("id");
ALTER TABLE
    "parcels" ADD CONSTRAINT "parcels_priority_ecological_ranking_id_foreign" FOREIGN KEY("priority_ecological_ranking_id") REFERENCES "ranking"("id");
ALTER TABLE
    "parcel_madd" ADD CONSTRAINT "parcel_madd_parcel_id_foreign" FOREIGN KEY("parcel_id") REFERENCES "parcels"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_phase_id_foreign" FOREIGN KEY("phase_id") REFERENCES "phase"("id");
ALTER TABLE
    "securement_action_items" ADD CONSTRAINT "securement_action_items_team_lead_id_foreign" FOREIGN KEY("team_lead_id") REFERENCES "team_lead"("id");
ALTER TABLE
    "parcel_info" ADD CONSTRAINT "parcel_info_parcel_id_foreign" FOREIGN KEY("parcel_id") REFERENCES "parcels"("id");
ALTER TABLE
    "team_lead_actions" ADD CONSTRAINT "team_lead_actions_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "securement_action_items" ADD CONSTRAINT "securement_action_items_action_item_status_id_foreign" FOREIGN KEY("action_item_status_id") REFERENCES "action_item_status"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_source_id_foreign" FOREIGN KEY("source_id") REFERENCES "source"("id");
ALTER TABLE
    "properties_contact" ADD CONSTRAINT "properties_contact_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_project_region_id_foreign" FOREIGN KEY("project_region_id") REFERENCES "project_region"("id");
ALTER TABLE
    "outreach" ADD CONSTRAINT "outreach_parcel_id_foreign" FOREIGN KEY("parcel_id") REFERENCES "parcels"("id");
ALTER TABLE
    "property_theme" ADD CONSTRAINT "property_theme_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "focus_area_internal" ADD CONSTRAINT "focus_area_internal_focus_area_external_id_foreign" FOREIGN KEY("focus_area_external_id") REFERENCES "focus_area_external"("id");
ALTER TABLE
    "securement_action_items" ADD CONSTRAINT "securement_action_items_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_securement_probability_id_foreign" FOREIGN KEY("securement_probability_id") REFERENCES "securement_probability"("id");
ALTER TABLE
    "parcels" ADD CONSTRAINT "parcels_acquisition_type_id_foreign" FOREIGN KEY("acquisition_type_id") REFERENCES "acquisition_type"("id");
ALTER TABLE
    "outreach" ADD CONSTRAINT "outreach_communication_purpose_id_foreign" FOREIGN KEY("communication_purpose_id") REFERENCES "communication_purpose"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_focus_area_internal_id_foreign" FOREIGN KEY("focus_area_internal_id") REFERENCES "focus_area_internal"("id");
ALTER TABLE
    "llt_projects" ADD CONSTRAINT "llt_projects_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "landowners" ADD CONSTRAINT "landowners_parcel_id_foreign" FOREIGN KEY("parcel_id") REFERENCES "parcels"("id");
ALTER TABLE
    "parcel_padd" ADD CONSTRAINT "parcel_padd_parcel_id_foreign" FOREIGN KEY("parcel_id") REFERENCES "parcels"("id");
ALTER TABLE
    "parcels" ADD CONSTRAINT "parcels_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "parcels" ADD CONSTRAINT "parcels_priority_securement_ranking_id_foreign" FOREIGN KEY("priority_securement_ranking_id") REFERENCES "ranking"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_campaign_id_foreign" FOREIGN KEY("campaign_id") REFERENCES "campaign"("id");
ALTER TABLE
    "property_contact_communication" ADD CONSTRAINT "property_contact_communication_property_contact_id_foreign" FOREIGN KEY("property_contact_id") REFERENCES "property_contact_details"("id");
ALTER TABLE
    "appraisals" ADD CONSTRAINT "appraisals_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_project_feasibility_ranking_id_foreign" FOREIGN KEY("project_feasibility_ranking_id") REFERENCES "ranking"("id");
ALTER TABLE
    "properties_contact" ADD CONSTRAINT "properties_contact_property_contact_id_foreign" FOREIGN KEY("property_contact_id") REFERENCES "property_contact_details"("id");
ALTER TABLE
    "properties" ADD CONSTRAINT "properties_team_lead_id_foreign" FOREIGN KEY("team_lead_id") REFERENCES "team_lead"("id");
ALTER TABLE
    "property_theme" ADD CONSTRAINT "property_theme_project_theme_id_foreign" FOREIGN KEY("project_theme_id") REFERENCES "project_theme"("id");
ALTER TABLE
    "securement_action_items" ADD CONSTRAINT "securement_action_items_action_item_type_id_foreign" FOREIGN KEY("action_item_type_id") REFERENCES "action_item_type"("id");
ALTER TABLE
    "team_lead_actions" ADD CONSTRAINT "team_lead_actions_team_lead_id_foreign" FOREIGN KEY("team_lead_id") REFERENCES "team_lead"("id");
ALTER TABLE
    "property_fund" ADD CONSTRAINT "property_fund_fund_id_foreign" FOREIGN KEY("fund_id") REFERENCES "fund"("id");
ALTER TABLE
    "internal_communications" ADD CONSTRAINT "internal_communications_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "property_fund" ADD CONSTRAINT "property_fund_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "property_contact_communication" ADD CONSTRAINT "property_contact_communication_property_id_foreign" FOREIGN KEY("property_id") REFERENCES "properties"("id");
ALTER TABLE
    "property_contact_communication" ADD CONSTRAINT "property_contact_communication_communication_method_id_foreign" FOREIGN KEY("communication_method_id") REFERENCES "communication_method"("id");