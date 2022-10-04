# Checkpoint IIa: Improving the user interface
# In this app we modified the user interface using the package shinydash
# Setup ------------
# Load the libraries
library(shiny)
library(dplyr) # For data manipulation
library(ggplot2) # For figures
library(STNet) # Where the data is
library(shinydashboard) # To create the UI

## Load the data ------------
data("vac") # Data for vaccination
data("vigilancia") # Data for surveillance
data("captura") # Data for captures