# Server logic ----
server <- function(input, output, session) {
  # Authentication ----
  user_base <- get_users_from_db(db_con)

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = function() get_sessionids_from_db(db_con),
    cookie_setter = function(user, sessionid) {
      add_sessionid_to_db(db_con, user, sessionid)
    },
    log_out = reactive(logout_init())
  )

  # Revoke the remember-me token on logout. Priority 1 so it runs before
  # shinyauthr clears credentials()$info.
  observeEvent(
    logout_init(),
    {
      req(credentials()$user_auth)

      sessionid <- credentials()$info$sessionid
      if (!is.null(sessionid) && nzchar(sessionid)) {
        delete_sessionid_from_db(db_con, sessionid)
      }
    },
    priority = 1
  )

  # Failed-login message. Low priority so it runs after shinyauthr has
  # evaluated the login attempt.
  observeEvent(
    input[["login-button"]],
    {
      if (!isTRUE(credentials()$user_auth)) {
        shinyjs::show("login_error")
        shinyjs::delay(5000, shinyjs::hide("login_error"))
      }
    },
    priority = -1
  )

  # Toggle login screen / app
  observe({
    shinyjs::toggle("login_page", condition = !isTRUE(credentials()$user_auth))
    shinyjs::toggle("app_content", condition = isTRUE(credentials()$user_auth))
  })

  # Set current user in database
  observe({
    req(credentials()$user_auth)
    DBI::dbExecute(
      db_con,
      "SELECT set_config('app.current_user', $1, false)",
      params = list(
        # Fall back to username if name is missing
        dplyr::coalesce(credentials()$info$name, credentials()$info$user)
      )
    )
  })

  # Modules start only after login ----
  # req() sits in the event expression so a FALSE value doesn't consume `once`
  observeEvent(
    req(credentials()$user_auth),
    {
      # Setup Cons Lands processing ----
      db_updated <- reactiveVal(0)

      cons_lands_data_rv <- reactiveVal(NULL)

      # Runs on startup and whenever any edit module signals a change (db_updated),
      # otherwise cons_lands_data_rv stays frozen at its startup value (e.g. stale public_view)
      observeEvent(db_updated(), {
        cons_lands_data_rv(
          prep_cons_lands_data(
            gis_con,
            db_con
          )
        )
      })

      module_pol_viewer_server(
        "pol_webpage",
        db_con,
        db_updated
      )
      focal_pid_rv <- reactiveVal(NULL)
      prop_spp_rv <- reactiveVal(NULL)

      module_prop_stats_server(
        "prop_stats",
        db_con,
        gis_con,
        db_updated
      )

      module_securement_focus_areas_server(
        "securement_focus_areas",
        db_con,
        db_updated
      )

      module_add_property_record_server(
        "property_details_form",
        db_con,
        prd_con,
        db_updated
      )

      module_add_property_contact_server(
        "property_contact_form",
        db_con,
        db_updated
      )

      module_assign_securement_values_server(
        "assign_priorities",
        db_con,
        db_updated
      )

      # module_review_data_viewer_server(
      #   "review_data",
      #   db_con,
      #   db_updated
      # )

      module_data_viewer_server(
        "records_view",
        db_con,
        db_updated,
        prop_filter = NULL,
        focal_pid_rv,
        panel_id = "outreach_panel"
      )
      module_securement_action_items_server("action_items", db_con, db_updated)

      module_data_viewer_server(
        "securement_records_view",
        db_con,
        db_updated,
        panel_id = "securement_panel"
      )

      module_data_viewer_server(
        "cons_lands_view",
        db_con,
        db_updated,
        panel_id = "cons_lands_panel",
        cons_lands_data = cons_lands_data_rv,
      )

      module_add_property_contact_communication_server(
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
      module_securement_queries_server(
        "securement_query",
        db_con,
        db_updated,
        focal_pid_rv
      )
      module_project_overview_server("project_review", db_con, db_updated)

      module_property_mapbox_server(
        "properties_map",
        db_con = db_con,
        gis_con = gis_con,
        db_updated
      )

      module_review_queries_server("review_queries", db_con, db_updated)

      module_eco_highlights_server("ecological_highlights", db_con, db_updated)

      module_team_lead_info_server("team_lead_info", db_con, db_updated)

      module_team_lead_actions_server("team_lead_actions", db_con, db_updated)

      module_edit_team_actions_server("edit_team", db_con, db_updated)

      module_edit_property_contact_communication_server(
        "edit_property_contact_communication",
        db_con,
        db_updated
      )

      module_edit_pricing_server("edit_pricing", db_con, db_updated)

      module_edit_closing_details_server(
        "edit_closing",
        db_con,
        db_updated,
        cons_lands_data = cons_lands_data_rv
      )
      module_edit_funding_server("edit_funding", db_con, db_updated)

      module_edit_securement_properties_server(
        "edit_securement_properties",
        db_con,
        db_updated
      )
      module_edit_securement_parcels_server(
        "edit_securement_parcels",
        db_con,
        db_updated
      )

      module_species_properties_server(
        "species_properties",
        db_con,
        gis_con,
        db_updated,
        prop_spp_rv
      )

      module_edit_appraisals_server("edit_appraisals", db_con, db_updated)

      module_edit_surveys_server("edit_surveys", db_con, db_updated)

      module_edit_property_contacts_server(
        "edit_property_contact_details",
        db_con,
        db_updated
      )

      module_active_projects_review_server(
        "securement_review",
        db_con,
        db_updated
      )

      module_all_projects_review_server("all_projects", db_con, db_updated)

      module_edit_llt_projects_server(
        "edit_llt_projects",
        db_con,
        db_updated
      )

      module_add_securement_communication_server(
        "securement_comms",
        db_con,
        db_updated
      )

      module_admin_server(
        "admin",
        db_con,
        cons_lands_data = cons_lands_data_rv
      )

      module_securement_actions_quality_control_server(
        "securement_actions_qc",
        db_con,
        db_updated
      )

      module_secured_properties_mapbox_server(
        "secured_properties_map",
        db_con,
        gis_con,
        db_updated
      )
    },
    once = TRUE
  )
}
