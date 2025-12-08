ui <- page_navbar(
  title = "Nature Trust Property Database Manager",
  useShinyjs(),
  # Add the CSS here for all modules
  tags$head(
    tags$style(HTML(
      "
      .popover {
        max-width: 600px !important;
        width: auto !important;
      }
      .popover-body {
        white-space: pre-wrap;
        word-wrap: break-word;
        line-height: 1.4;
      }
    "
    ))
  ),
  id = "main_navbar",
  selected = "Home",
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "united"),
  fillable = TRUE,
  sidebar = sidebar(
    id = "main_sidebar",
    open = FALSE,
    width = 250,
    title = NULL,
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Property Online Access",
        icon = bs_icon("link-45deg"),
        module_pol_viewer_ui("pol_webpage")
      ),
      accordion_panel(
        title = "Display Settings",
        icon = bs_icon("palette"),
        input_switch(
          id = "dark_toggle",
          label = "Dark Mode",
          value = FALSE
        )
      ),
      accordion_panel(
        title = "App Info",
        icon = bs_icon("info-circle"),
        p(str_glue("Version: {VERSION}"))
      )
    )
  ),
  nav_panel(
    title = "Home",
    icon = icon("home"),
    module_value_boxes_UI("home_page")
  ),
  nav_panel(
    title = "Outreach",
    icon = bs_icon("person-lines-fill"),
    navset_card_tab(
      height = "100%",
      # nav_panel(
      #   title = "Initialise PID",
      #   module_property_intake_ui("property_form")
      # ),
      nav_panel(
        title = "Add Property Record",
        module_property_details_ui("property_details_form")
      ),
      nav_panel(
        title = "Add Property Contact",
        module_property_contact_ui("property_contact_form")
      ),
      nav_panel(
        title = "Add Outreach & Communication",
        module_property_contact_communication_ui(
          "property_contact_communication"
        )
      ),
      nav_panel(
        title = "Data Viewer",
        module_data_viewer_ui("records_view", panel_id = "panel_01")
      ),
      nav_panel(
        title = "Queries",
        module_outreach_queries_ui("outreach_query")
      )
    )
  ),
  nav_panel(
    title = "Securement",
    icon = bs_icon("geo-alt"),
    navset_card_tab(
      height = "100%",
      nav_panel(
        title = "Action Items",
        module_action_item_tracking_ui("action_items")
      ),
      nav_panel(
        title = "Data Viewer",
        module_data_viewer_ui(
          "securement_records_view",
          panel_id = "panel_02"
        )
      ),
      nav_panel(
        title = "Queries",
        module_securement_queries_ui("securement_query")
      )
    )
  ),
  nav_panel(
    title = "Review",
    icon = bs_icon("clipboard-data"),
    navset_card_tab(
      height = "100%",
      nav_panel(
        title = "Project Overview",
        module_review_projects_ui("project_review")
      ),
      nav_panel(
        title = "Assign Priorities",
        module_assign_priorities_ui("assign_priorities")
      ),
      nav_panel(
        title = "Data Viewer",
        module_review_data_viewer_ui("review_data")
      ),
      nav_panel(
        title = "Queries",
        module_review_queries_ui("review_queries")
      ),
      nav_panel(
        title = "Ecological Features",
        module_eco_highlights_ui("ecological_highlights")
      )
    )
  ),
  nav_panel(
    title = "Edit Records",
    icon = bs_icon("pencil-square"),
    navset_card_tab(
      height = "100%",
      nav_panel(
        title = "Properties/Parcels",
        module_edit_records_ui("edit_records")
      ),
      nav_panel(
        title = "Tab 2",
      ),
      nav_panel(
        title = "Tab 3"
      )
    )
  ),
  nav_panel(
    title = "Mapping",
    icon = bs_icon("map"),
    navset_card_tab(
      height = "100%",
      nav_panel(
        title = "Priority Map",
        module_property_map_ui("property_map")
      ),
      nav_panel(
        title = "ArcGIS Online Map",
        card(
          full_screen = TRUE,
          card_body(
            tags$iframe(
              src = "https://nsnt.maps.arcgis.com/apps/instant/basic/index.html?appid=36ea149a7296409583d023812ac482d1",
              style = "width:100%; height:80vh; border:0;"
            )
          )
        )
      ),
      nav_panel(
        title = "Tab 3"
      )
    )
  ),
  nav_spacer(),
  nav_item(
    actionBttn(
      inputId = "toggle_sidebar",
      label = "",
      icon = icon("gear"),
      style = "simple",
      size = "s"
    )
  )
)

if (USE_AUTH) {
  ui <- secure_app(ui)
} else {
  ui <- ui
}
