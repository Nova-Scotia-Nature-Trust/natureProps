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
library(shinyauthr)
library(sodium)
library(markdown)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(sf)
library(thematic)
library(mapgl)
library(shinycssloaders)
library(future)
library(promises)
library(quarto)
library(openxlsx2)
conflicted::conflict_scout()
walk(list.files("R/functions", full.names = TRUE), source)

# Database environment switching
is_dev <- Sys.getenv("APP_ENV") == "development"
db_con <- create_db_pool(
  if (is_dev) "nsnt-properties-test" else "nsnt-properties",
  port = 5432
)
prd_con <- create_db_pool("nsprd", port = 5432)
gis_con <- create_db_pool("nsnt_gis", port = 5432)

# Register cleanup when app stops
onStop(function() {
  poolClose(db_con)
  poolClose(prd_con)
})

thematic_shiny(font = "auto")
