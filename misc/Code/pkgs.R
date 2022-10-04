# This file has all the packages we will be using during the workshop

packages = c("shiny", # library to make the apps
             'shinydashboard',# Library for user interface functions
             'ggplot2', # For plots
             "dplyr", # For data manipulation
             'ggpubr', # To make arrangements and other tools for visualization
             "sf", # For spatial object manipulation
             'raster', # For raster manipulation
             "lubridate", # To manipulate dates
             'plotly', # For interactive plots
             'leaflet', # For interactive maps
             'DT', # For interactive tables
             'rsconnect', # To authorize shiny
             'tidygraph', # For network data manipulation
             'devtools', # To install packages from github
             'visNetwork' # For interactive network visualization
)

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

devtools::install_github("rstudio/d3heatmap") # To make interactive heatmaps
# devtools::install_github("rstudio/httpuv") # TO run shiny in Rstudio Cloud
devtools::install_github('jpablo91/STNet') # To access some data used in the workshop
