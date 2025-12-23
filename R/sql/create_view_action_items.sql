DROP VIEW IF EXISTS view_action_items;

CREATE VIEW view_action_items AS 
SELECT
   pa.pid AS "PID",
   pr.property_name AS "Property Name",
   info.area_ha AS "Size (ha)",
   ROUND(info.area_ha * 2.47105, 2) AS "Size (acres)",
   pa.securement_action_description AS "Securement Action Description",
   isa.action_value AS "ISA",
   isa_report.action_value AS "ISA Report",
   intent_letter.action_value AS "Letter of Intent",
   ps_agree.action_value AS "PS Agreement",
   easement.action_value AS "Easement",
   env_investigation.action_value AS "Environmental Investigation",
   title_certificate.action_value AS "Certificate of Title",
   appraisal.action_value AS "Appraisal",
   survey.action_value AS "Survey",
   boundary_agreement.action_value AS "Boundary Agreement",
   legal_title_mig.action_value AS "Legal Title Migration",
   ecogifts.action_value AS "Ecogifts",
   approval_cc.action_value AS "CC Approval",
   approval_board.action_value AS "Board Approval",
   af_phase_01.action_value AS "AF Phase 01",
   af_phase_02.action_value AS "AF Phase 02",
   af_phase_03.action_value AS "AF Phase 03",
   af_ease_transfer.action_value AS "AF Easement Transfer",
   af_remoteness_letter.action_value AS "AF Remoteness",
   af_field_verified.action_value AS "AF Field Verified",
   llt_funding.action_value AS "LLT Funding" 
FROM
   properties pr 
   INNER JOIN
      parcels pa 
      ON pa.property_id = pr.id 
   LEFT JOIN
      parcel_info info 
      ON pa.id = info.parcel_id 
   LEFT JOIN
      action_item_status isa 
      ON pa.isa_id = isa.id 
   LEFT JOIN
      action_item_status isa_report 
      ON pa.isa_report_id = isa_report.id 
   LEFT JOIN
      action_item_status intent_letter 
      ON pa.intent_letter_id = intent_letter.id 
   LEFT JOIN
      action_item_status ps_agree 
      ON pa.ps_agree_id = ps_agree.id 
   LEFT JOIN
      action_item_status easement 
      ON pa.easement_id = easement.id 
   LEFT JOIN
      action_item_status env_investigation 
      ON pa.env_investigation_id = env_investigation.id 
   LEFT JOIN
      action_item_status title_certificate 
      ON pa.title_certificate_id = title_certificate.id 
   LEFT JOIN
      action_item_status appraisal 
      ON pa.appraisal_id = appraisal.id 
   LEFT JOIN
      action_item_status survey 
      ON pa.survey_id = survey.id 
   LEFT JOIN
      action_item_status boundary_agreement 
      ON pa.boundary_agreement_id = boundary_agreement.id 
   LEFT JOIN
      action_item_status legal_title_mig 
      ON pa.legal_title_mig_id = legal_title_mig.id 
   LEFT JOIN
      action_item_status ecogifts 
      ON pa.ecogifts_id = ecogifts.id 
   LEFT JOIN
      action_item_status approval_cc 
      ON pa.approval_cc_id = approval_cc.id 
   LEFT JOIN
      action_item_status approval_board 
      ON pa.approval_board_id = approval_board.id 
   LEFT JOIN
      action_item_status af_phase_01 
      ON pa.af_phase_01_id = af_phase_01.id 
   LEFT JOIN
      action_item_status af_phase_02 
      ON pa.af_phase_02_id = af_phase_02.id 
   LEFT JOIN
      action_item_status af_phase_03 
      ON pa.af_phase_03_id = af_phase_03.id 
   LEFT JOIN
      action_item_status af_ease_transfer 
      ON pa.af_ease_transfer_id = af_ease_transfer.id 
   LEFT JOIN
      action_item_status af_remoteness_letter 
      ON pa.af_remoteness_letter_id = af_remoteness_letter.id 
   LEFT JOIN
      action_item_status af_field_verified 
      ON pa.af_field_verified_id = af_field_verified.id 
   LEFT JOIN
      action_item_status llt_funding 
      ON pa.llt_funding_id = llt_funding.id 
ORDER BY
   pr.property_name,
   pa.pid;
;