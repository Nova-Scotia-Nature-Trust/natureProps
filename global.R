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
library(shinymanager)
library(markdown)
conflicted::conflict_scout()
walk(list.files("R/functions", full.names = TRUE), source)
# walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)
# options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")

# Create database connection
db_con <- create_db_con("dummydb")
prd_con <- create_db_con("nsprd")

DOCKER_CON <- FALSE
USE_AUTH <- FALSE

if (USE_AUTH) {
  shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "Nature Trust Property Manager",
    "Login" = "Sign in"
  )
}

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
