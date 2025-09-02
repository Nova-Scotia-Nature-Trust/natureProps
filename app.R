library(shiny)
library(bslib)
library(DT)
library(DBI)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(glue)
library(bsicons)
library(shinyvalidate)
library(shinyjs)
library(shinyalert)
library(dbx)
library(janitor)
conflicted::conflict_scout()
walk(list.files("R/functions", full.names = TRUE), source)
# walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)
# options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")

# Create database connection
db_con <- create_db_con("dummydb")
prd_con <- create_db_con("nsprd")

ui <- page_navbar(
  title = "Nature Trust Property Database Manager",
  id = "main_navbar",
  selected = "Securement",
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "united"),
  fillable = TRUE,
  sidebar = sidebar(
    id = "main_sidebar",
    open = FALSE,
    width = 250,
    title = "Sidebar things",
    input_switch(
      id = "dark_toggle",
      label = "Dark Mode",
      value = TRUE
    ),
    selectInput(
      inputId = "mySelectInput",
      label = "List",
      choices = list("Do this" = "a", "Do that" = "b")
    ),
    actionButton(inputId = "myButton", label = "Do action!"),
    hr(),
    h4("App Info"),
    p("Version: 0.0.1")
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
      nav_panel(
        title = "Initialise PID",
        module_property_intake_ui("property_form")
      ),
      nav_panel(
        title = "Outreach & Communication",
        module_landowner_communication_ui("landowner_communication")
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
        module_data_viewer_ui("securement_records_view", panel_id = "panel_02")
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
        title = "Assign Priorities",
        p(
          "UI for assigning ecological and securement 
          priority ranking to each property."
        )
      ),
      nav_panel(
        title = "Data Viewer",
      ),
      nav_panel(
        title = "Queries"
      )
    )
  ),
  nav_panel(
    title = "Edit Records",
    icon = bs_icon("pencil-square"),
    navset_card_tab(
      height = "100%",
      nav_panel(
        title = "Parcels",
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

# Server logic ----
server <- function(input, output, session) {
  # Toggle sidebar when gear icon is clicked
  observeEvent(input$toggle_sidebar, {
    # Use toggle_sidebar with the correct sidebar ID
    bslib::toggle_sidebar(id = "main_sidebar")
  })

  observeEvent(input$dark_toggle, {
    toggle_dark_mode(if (input$dark_toggle) "dark" else "light")
  })

  db_updated <- reactiveVal(0)
  focal_pid_rv <- reactiveVal(NULL)

  module_property_stats_server("home_page", db_con, db_updated)
  module_property_intake_server("property_form", db_con, prd_con, db_updated)
  module_data_viewer_server(
    "records_view",
    db_con,
    db_updated,
    prop_filter = NULL,
    focal_pid_rv
  )
  module_action_item_tracking_server("action_items", db_con, db_updated)
  module_data_viewer_server("securement_records_view", db_con, db_updated)
  module_landowner_communication_server(
    "landowner_communication",
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
}

# Run the application
shinyApp(ui = ui, server = server)
