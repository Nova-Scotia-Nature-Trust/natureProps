#' Generate a "Closing Details" Excel workbook for a property
#'
#' Builds a two-sheet workbook (`Property` and `Appraisers`) pre-filled
#' with property details pulled from the database, along with data
#' validation dropdowns for fields that expect controlled values.
#'
#' @param db_con Database connection/pool.
#' @param property_id Integer. `properties.id` of the property to export.
#' @param output_file Character. Destination path for the saved `.xlsx`.
generate_closing_details_xlsx <- function(db_con, property_id, output_file) {
  fields <- c(
    "Property Internal Name",
    "Property Public Name",
    "Donor/Vendor Name",
    "Donor/Vendor Address",
    "Donor/Vendor Email",
    "Donor/Vendor Phone",
    "Should they receive solicitations?",
    "Should they receive Gratitude Report?",
    "Should they receive monthly e-newsletter?",
    "If a donation, should it be anonymous?",
    "Is Donor/Vendor still Priority Landowner?",
    "",
    "PID(s)",
    "Type of Protection",
    "Property Size",
    "FMV",
    "FMV after easement (if applicable)",
    "Purchase Price (if applicable)",
    "EcoGifts?",
    "Purchase/Donation Date",
    "Name and Address of appraiser"
  )

  field_map <- c(
    "Property Internal Name" = "property_name",
    "Property Public Name" = "property_name_public",
    "Donor/Vendor Name" = "donor_vendor",
    "PID(s)" = "pids",
    "Property Size" = "size",
    "FMV" = "fmv",
    "Purchase Price (if applicable)" = "price_purchase",
    "Purchase/Donation Date" = "date_closed"
  )

  # ---- Data ----

  prop_details <- dbGetQuery(
    db_con,
    statement = glue_sql(
      "SELECT pr.property_name,
              pr.property_name_public,
              STRING_AGG(pa.pid::text, ', ') AS pids,
              pr.donor_vendor,
              pr.price_purchase,
              pr.date_closed,
              SUM(COALESCE(pa.size_confirmed_acres, pi.area_ha * 2.471))::numeric(10, 2) AS size
      FROM properties pr
      LEFT JOIN parcels pa ON pr.id = pa.property_id
      LEFT JOIN parcel_info pi ON pi.parcel_id = pa.id
      WHERE pr.id = {property_id}
      GROUP BY
        pr.property_name,
        pr.property_name_public,
        pr.donor_vendor,
        pr.price_purchase,
        pr.date_closed;
      ",
      .con = db_con
    )
  ) |>
    as_tibble()

  prop_details <- prop_details |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(
      cols = everything(),
      names_to = "db_name",
      values_to = "Value"
    ) |>
    left_join(
      field_map |>
        enframe(name = "excel_name", value = "db_name"),
      by = "db_name"
    ) |>
    select(Field = excel_name, Value)

  data <- fields |>
    enframe(name = NULL, value = "Field") |>
    left_join(prop_details, join_by(Field)) |>
    replace_na(list(Value = ""))

  # ---- Workbook ----

  wb <- wb_workbook()

  wb$add_worksheet("Property")

  wb$add_data(
    sheet = "Property",
    x = data,
    start_col = 1,
    start_row = 1
  )

  # ---- Header formatting ----

  wb$add_font(
    sheet = "Property",
    dims = "A1:B1",
    bold = TRUE,
    color = wb_color(hex = "FFFFFFFF")
  )

  wb$add_fill(
    sheet = "Property",
    dims = "A1:B1",
    color = wb_color(hex = "A3BCE9")
  )

  wb$add_cell_style(
    sheet = "Property",
    dims = "A1:B1",
    horizontal = "center",
    vertical = "center"
  )

  # ---- Column widths ----

  wb$set_col_widths(
    sheet = "Property",
    cols = 1:2,
    widths = c(45, 35)
  )

  # ---- Freeze header row ----

  wb$freeze_pane(
    sheet = "Property",
    first_active_row = 2
  )

  # ---- Dropdowns ----

  # Fields with Yes - Mail / Yes - Email / No
  mail_email_fields <- c(
    "Should they receive solicitations?",
    "Should they receive Gratitude Report?"
  )

  # Fields with Yes / No / N/A
  na_fields <- c(
    "Should they receive monthly e-newsletter?"
  )

  # Fields with Yes / No
  yes_no_fields <- c(
    "If a donation, should it be anonymous?",
    "Is Donor/Vendor still Priority Landowner?",
    "EcoGifts?"
  )

  for (i in seq_along(fields)) {
    field <- fields[i]

    # Excel row is i + 1 because row 1 contains headers
    excel_row <- i + 1

    if (field %in% mail_email_fields) {
      wb$add_data_validation(
        sheet = "Property",
        dims = paste0("B", excel_row),
        type = "list",
        value = '"Yes - Mail,Yes - Email,No"'
      )
    } else if (field %in% na_fields) {
      wb$add_data_validation(
        sheet = "Property",
        dims = paste0("B", excel_row),
        type = "list",
        value = '"Yes,No,N/A"'
      )
    } else if (field %in% yes_no_fields) {
      wb$add_data_validation(
        sheet = "Property",
        dims = paste0("B", excel_row),
        type = "list",
        value = '"Yes,No"'
      )
    }
  }

  # ---- Appraisers sheet ----

  appraisers <- c(
    "James Stephens, Hillside Consulting Ltd., 6828 NS-105 Baddeck, NS",
    "Turner Drake & Partners Ltd, 6182 North St Halifax, NS B3K 1P5",
    "Ingram Varner and Associates, 310-15 Dartmouth Rd Bedford NS B4A 3X6",
    "Barkhouse Appraisals, 27 Spruce Lane Antigonish NS B2G 2J7",
    "Stephen Horswill, Alderney Appraisals, 165 Portland St, Dartmouth, NS, B2Y1J2"
  )

  wb$add_worksheet("Appraisers")

  wb$add_data(
    sheet = "Appraisers",
    x = data.frame(Appraisers = appraisers),
    start_col = 1,
    start_row = 1
  )

  # Format appraiser header
  wb$add_font(
    sheet = "Appraisers",
    dims = "A1",
    bold = TRUE,
    color = wb_color(hex = "FFFFFFFF")
  )

  wb$add_fill(
    sheet = "Appraisers",
    dims = "A1",
    color = wb_color(hex = "A3BCE9")
  )

  # Make appraiser column wide enough to see values
  wb$set_col_widths(
    sheet = "Appraisers",
    cols = 1,
    widths = 80
  )

  # ---- Appraiser dropdown ----

  appraiser_row <- which(data$Field == "Name and Address of appraiser") + 1

  wb$add_data_validation(
    sheet = "Property",
    dims = paste0("B", appraiser_row),
    type = "list",
    value = str_glue("Appraisers!$A$2:$A${length(appraisers) + 1}")
  )

  # ---- Save ----

  wb$save(output_file)

  invisible(output_file)
}
