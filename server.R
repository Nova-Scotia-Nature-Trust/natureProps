# Server logic ----
server <- function(input, output, session) {
  if (USE_AUTH) {
    # Authentication
    res_auth <- secure_server(
      check_credentials = check_credentials(
        "postgres_auth_config.yml"
      ),
      timeout = 0,
    )

    # Get current user info
    output$auth_output <- renderText({
      reactiveValuesToList(res_auth)
    })

    # Set current user in database - wrap in observe()
    observe({
      req(res_auth$user) # Wait for user to be available
      DBI::dbExecute(
        db_con,
        paste0("SET session \"app.current_user\" = '", res_auth$user, "'")
      )
    })
  }

  # Toggle sidebar when gear icon is clicked
  observeEvent(input$toggle_sidebar, {
    # Use toggle_sidebar with the correct sidebar ID
    bslib::toggle_sidebar("main_sidebar")
  })

  # observeEvent(input$dark_toggle, {
  #   toggle_dark_mode(if (input$dark_toggle) "dark" else "light")
  # })

  observeEvent(input$dark_toggle, {
    mode <- if (input$dark_toggle) "dark" else "light"
    toggle_dark_mode(mode)
  })

  module_pol_viewer_server(
    "pol_webpage",
    db_con,
    db_updated
  )

  db_updated <- reactiveVal(0)
  focal_pid_rv <- reactiveVal(NULL)

  module_property_stats_server(
    "home_page",
    db_con,
    db_updated
  )

  module_property_details_server(
    "property_details_form",
    db_con,
    prd_con,
    db_updated
  )

  module_property_contact_server(
    "property_contact_form",
    db_con,
    db_updated
  )

  module_assign_priorities_server(
    "assign_priorities",
    db_con,
    db_updated
  )

  module_review_data_viewer_server(
    "review_data",
    db_con,
    db_updated
  )

  module_data_viewer_server(
    "records_view",
    db_con,
    db_updated,
    prop_filter = NULL,
    focal_pid_rv
  )
  module_action_item_tracking_server("action_items", db_con, db_updated)
  module_data_viewer_server("securement_records_view", db_con, db_updated)
  module_property_contact_communication_server(
    "property_contact_communication",
    db_con,
    db_updated
  )
  module_outreach_queries_server(
    "outreach_query",
    db_con,
    db_updated,
    focal_pid_rv
  )
  module_edit_records_server("edit_records", db_con, db_updated)
  module_securement_queries_server(
    "securement_query",
    db_con,
    db_updated,
    focal_pid_rv
  )
  module_review_projects_server("project_review", db_con, db_updated)
}
