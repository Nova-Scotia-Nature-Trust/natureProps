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
conflicted::conflict_scout()
walk(list.files("R/functions", full.names = TRUE), source)
# walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)
# options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")

# Create database connection
db_con <- create_db_con("dummydb")
prd_con <- create_db_con("nsprd")

USE_AUTH <- FALSE

if (USE_AUTH) {
  shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "Nature Trust Property Manager",
    "Login" = "Sign in"
  )
}
