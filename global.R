library(shiny)
library(bslib)
library(DT)
library(DBI)
library(pool)
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
library(shinymanager)
library(markdown)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(sf)
library(thematic)
conflicted::conflict_scout()
walk(list.files("R/functions", full.names = TRUE), source)

# Create database connection
db_con <- create_db_pool("dummydb")
prd_con <- create_db_pool("nsprd")
gis_con <- create_db_pool("nsnt_gis")
# gis_con <- create_db_pool("gis")

# Register cleanup when app stops
onStop(function() {
  poolClose(db_con)
  poolClose(prd_con)
})

USE_AUTH <- FALSE

if (USE_AUTH) {
  shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "Nature Trust Property Manager",
    "Login" = "Sign in"
  )
}

VERSION <- "2.4.5"

thematic_shiny(font = "auto")

# Tester PIDs:
# 05311154
# 35219047
# 65192890
# 70037155
# 45112554
# 40202681
# 25193996
# 35222843
# 70037155
# 15735657
# 45035656
# 20356556
# 20356598
# 20493565
# 25103565
# 30356547
# 30356596
# 37535655
