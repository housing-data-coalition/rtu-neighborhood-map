library(shiny)
library(shinyjs)
library(shinyauthr) # remotes::install_github("paulc91/shinyauthr")
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(leaflet)
library(leafpop)
library(leaflet.mapboxgl) # remotes::install_github("rstudio/leaflet.mapboxgl")
library(gt)

# Encrypted user base for login screen
user_base <- readRDS("data/user_base.rds")

# Set API token for MapboxGL basemap
options(mapbox.accessToken = read_rds("data/mapbox_token.rds"))

# Dataset with tract shapes and indicator values
rw_tracts <- read_rds("data/ridgewood-tracts-indicators.rds")

# Table of variable display names, formatting info, etc for table
indic_info <- read_csv("data/indicator-info.csv", col_types = "icccc")

# Create named vector for variables to use in dropdown menu
var_inputs <- indic_info %>% 
  filter(var_group != "Totals") %>% 
  transmute(
    # remove year info for readability
    label = paste0(gsub("\\s\\(.*?\\)", "", var_group), ": ", display_name),
    var_name
  ) %>% 
  {setNames(.$var_name, .$label)}

# NOTE: we don't load the member data here, but rather in server.R, because we
# need to wait until login authorization is complete.
