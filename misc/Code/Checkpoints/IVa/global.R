# Checkpoint IIIa: Leaflet
# In this lab we explore different reactive functions to improve the user experience
# Setup ------------
# Load the libraries
library(shiny)
library(dplyr) # For data manipulation
library(ggplot2) # For figures
library(STNet) # Where the data is
library(shinydashboard) # To create the UI
library(sf) # To manipulate spatial data
library(plotly) # For interactive plots
library(DT)
library(leaflet)

## Load the data ------------
data("vac") # Data for vaccination
data("vigilancia") # Data for surveillance
data("captura") # Data for captures

# Loading the spatial data from the package
MxShp <- st_read(system.file("data/MxShp.shp", package = "STNet")) %>% 
  filter(CVE_ENT %in% c('15', '12', '16')) # We filter to only the study region

# Project our points
capturaSp <- captura %>% 
  st_as_sf(crs = st_crs(4326), coords = c('LONG', 'LATITUD'))