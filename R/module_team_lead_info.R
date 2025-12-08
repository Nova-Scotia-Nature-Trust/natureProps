# UI ----
module_team_lead_info_UI <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6),

    # Left card: All indicator boxes
    card(
      height = "auto",
      card_header(
        h5("Action Items")
      ),
      card_body(
        "Content to come"
      )
    ),

    # Right card: NSNT ownership information
    card(
      height = "auto",
      card_header(
        h5("Team Lead Propety List")
      ),
      card_body(
        "Content to come"
      )
    )
  )
}

# Server ----
module_team_lead_info_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {})
}
