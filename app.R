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

walk(list.files("R/functions", full.names = TRUE), source)

# Create database connection
db_con <- create_db_con("dummydb")
prd_con <- create_db_con("nsprd")

# Source modules
source("R/module_property_intake.R")
source("R/module_data_viewer.R")
source("R/module_value_boxes.R")

ui <- page_navbar(
  title = "Nature Trust Property Database Manager",
  id = "main_navbar",
  selected = "Outreach",
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
        title = "Commmunication & Outreach"
      ),
      nav_panel(
        title = "Data Viewer",
        module_data_viewer_ui("records_view", panel_id = "panel_01")
      ),
      nav_panel(
        title = "Queries"
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
        title = "Queries"
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

# Server logic
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

  module_property_stats_server("home_page", db_con, db_updated)
  module_property_intake_server("property_form", db_con, prd_con, db_updated)
  module_data_viewer_server("records_view", db_con, db_updated)
  module_action_item_tracking_server("action_items", db_con, db_updated)
  module_data_viewer_server("securement_records_view", db_con, db_updated)
}

# Run the application
shinyApp(ui = ui, server = server)
