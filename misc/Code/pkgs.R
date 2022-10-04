# Este archivo contiene todos los paquetes necesarios para el curso de Visualización interactiva de bases de datos complejas mediante la plataforma Shiny

packages = c("shiny", # Libreria para hacer apps
             'shinydashboard',
             'ggplot2', # Para graficos
             "dplyr", # Para manipular datos
             'ggpubr', # Para hacer arreglos de graficas
             "sf", # Para manipular objetos espaciales
             "lubridate", # Para manipular fechas 
             'stars',
             'tmap',
             'plotly', # Para graficos interactivos
             'DT', # Para hacer tablas interactivas
             'rsconnect', # Para authorizar la cuenta de shiny 
             'tidygraph', # Para manipular redes
             'devtools', # para instalar paquetes de github
             'visNetwork' # para visualizacion de redes dinamicas
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

devtools::install_github("rstudio/d3heatmap") # para hacer heatmaps interactivos
devtools::install_github("rstudio/httpuv") # para correr shiny en rstudio cloud
devtools::install_github('jpablo91/STNet') # para acceder a los datos del curso