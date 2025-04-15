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
  selected = "Outreach",
  collapsible = TRUE,
  theme = bs_theme(bootswatch = "united"),
  sidebar = sidebar(
    open = FALSE,
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
  ),
  nav_panel(
    title = "Home",
    module_value_boxes_UI("home_page")
  ),
  nav_panel(
    title = "Outreach",
    tabsetPanel(
      module_property_intake_ui("property_form"),
      nav_panel(
        title = "Commmunication & Outreach"
      ),
      module_data_viewer_ui("records_view"),
      nav_panel(
        title = "Queries"
      )
    )
  ),
  nav_panel(
    title = "Securement",
    tabsetPanel(
      nav_panel(
        title = "Action Items"
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  observeEvent(input$dark_toggle, {
    toggle_dark_mode(if (input$dark_toggle) "dark" else "light")
  })

  db_updated <- reactiveVal(0)
  
  module_property_stats_server("home_page", db_con, db_updated)
  module_property_intake_server("property_form", db_con, prd_con, db_updated)
  module_data_viewer_server("records_view", db_con, db_updated)
}

# Run the application
shinyApp(ui = ui, server = server)
