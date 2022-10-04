# Checkpoint IIIa:
# En esta aplicacion introduciremos figuras interactivas
library(shiny)
library(dplyr) # Para manipulacion de datos
library(ggplot2) # Para las figuras
library(shinydashboard) # para crear un dashboard
library(STNet)
library(sf)
library(plotly)

# Cargar los datos del paquete STNet
data("vac") # Datos de vacunacion
data("vigilancia") # datos de vigilancia
data("captura") # Datos de captura
data('mov') # cargamos los datos
# Loading the spatial data from the package
MxShp <- st_read(system.file("data/MxShp.shp", package = "STNet")) %>% 
  filter(CVE_ENT %in% c('15', '12', '16')) # filtramos la base de datos para solo usar los estados 15, 12 y 16

# proyectar nuestros puntos
capturaSp <- captura %>% 
  st_as_sf(crs = st_crs(4326), coords = c('LONG', 'LATITUD')) %>% 
  st_transform(crs = st_crs(MxShp))

# Definir Interfaz
# Encabezado -----------
header <- dashboardHeader(title = 'Nueva aplicacion')
# Sidebar -------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    'Este es un menu para navegar la app', # Texto que aparecera en nuestra app
    menuItem("Vacunacion", tabName = "T1"), # Primer tab de nuestra app
    menuItem("Vigilancia", tabName = "T2"), # Segundo tab de nuestra app
    menuItem("Capturas", tabName = "T3"),
    menuItem("Movimientos", tabName = 'T4'),
    br(),
    selectInput(inputId = "Mun", label = "Municipios:", 
                choices = unique(vac$NOM_MUN), multiple = T, 
                selected = unique(vac$NOM_MUN)),
    sliderInput(inputId = 'year', label = 'Año', 
                min = min(as.numeric(vac$YEAR)), max = max(as.numeric(vac$YEAR)), 
                value = range(vac$YEAR)),
    actionButton(inputId = 'filter', label = 'Filtrar datos')
  )
)

# Body -----------
body <- dashboardBody(
  tabItems(
    # Primer tab
    tabItem(tabName = 'T1',
            fluidRow(
              column(width = 12, 
                     box(title = 'Vacunacion', width = 6,
                         plotlyOutput("VacMun"), # Figura de serie de tiempo
                     ),
                     box(title = 'Vacunacion boxplot', width = 6,
                         plotlyOutput("VacBoxplot"), # Figura de serie de tiempo
                     )
              )
            )
    ),
    
    tabItem(tabName = 'T2',
            fluidRow(
              column(width = 12,
                     box(title = 'Vigilancia', width = 12,
                         plotlyOutput("VigMun"),
                     )
              )
            )
    ),
    tabItem(tabName = 'T3',
            fluidRow(
              column(width = 12,
                     box(title = 'Captura', width = 6,
                         plotlyOutput("CapMun")
                     ),
                     box(title = 'Localizaciones de capturas', width = 6,
                         plotlyOutput('CapturaMap'))
              )
            )
    ),
    tabItem(tabName = 'T4',
            fluidRow(
              box(title = 'Motivo', width = 12,
                  selectInput(inputId = 'Motivo', label = 'Motivo: ',
                              multiple = T,
                              choices = unique(mov$MOTIVO), 
                              selected = unique(mov$MOTIVO))),
              box(title = 'Mapa de movimientos',
                  plotlyOutput('MovMap'))
              )
            )
  )
)

# Definir ui -----------------
# Integrar los componentes de la interfaz
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactividad ------------------
  x <- eventReactive(input$filter, {
    p <- vac %>% # base de datos
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filtramos los datos
  })
  
  observeEvent(x(), {
    showModal(modalDialog("Plots Updated", easyClose = T))
  })
  
  y <- eventReactive(input$filter, {
    y <- vigilancia %>% # base de datos
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filtramos los datos
  })
  
  z <- eventReactive(input$filter, {
    p <- capturaSp %>% # base de datos
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filtramos los datos
  })
  
  
  Mov <- eventReactive(input$filter, {
    mov %>% 
      filter(MOTIVO %in% input$Motivo,
             year %in% input$year)
  })
  
  edges <- eventReactive(input$filter,{
    edges <- Mov() %>% 
      select(ORIGEN, DESTINO, LONG_O, LAT_O, LONG_D, LAT_D) %>% 
      distinct()
  })
  
  nodes <- eventReactive(input$filter, {
    list(n1 = distinct(edges(), ID = ORIGEN, LAT = LAT_O, LONG = LONG_O),
         n2 = distinct(edges(), ID = DESTINO, LAT = LAT_D, LONG = LONG_D)) %>%
      do.call(rbind, .) %>%
      distinct()
    })
  
  # Outputs ---------------
  output$VacMun <- renderPlotly({
    p <- x() %>% 
      group_by(YEAR) %>% 
      summarise(Vac = sum(VAC_BOV)) %>% 
      ggplot(aes(x = YEAR, y=Vac)) +
      geom_bar(position="dodge", stat="identity", fill = "deepskyblue4") +
      labs(x = "Año", y = "Dosis de vacuna aplicados",
           title = "Aplicación de vacuna antirrábica")
    
    ggplotly(p)
  })
  
  output$VacBoxplot <- renderPlotly({
    p <- x() %>% 
      group_by(YEAR, NOM_MUN) %>% 
      summarise(Hatos = sum(TOTAL_HATOS, na.rm = T),
                Vacunados = sum(HATOS_VAC, na.rm = T)) %>% 
      mutate(pVac = Vacunados/Hatos) %>% 
      ggplot() +
      geom_boxplot(aes(x = YEAR, y = pVac)) +
      geom_jitter(aes(x = YEAR, y = pVac), width = 0.1)
    
    ggplotly(p)
  })
  
  output$VigMun <- renderPlotly({
    p <- y() %>%
      group_by(YEAR, RESULTADO) %>%
      summarise(N = n()) %>%
      ggplot(aes(x = YEAR, y=N, fill=RESULTADO)) +
      geom_bar(position="dodge", stat="identity")+
      theme(legend.position = "top") +
      labs(x = "Año", y = "Muestras enviadas al laboratorio",
           title = "Vigilancia epidemiológica de rabia paralítica en el suroeste del Estado de México")
    
    ggplotly(p)
  })
  
  output$CapMun <- renderPlotly({
    z() %>%
      group_by(YEAR) %>%
      summarise(N = n()) %>%
      ggplot(aes(x = YEAR, y=N)) +
      geom_bar(position="dodge", stat="identity", fill = "red4") +
      labs(x = "Año", y = "Eventos de captura de murciélago hematófago",
           title = "Control de población de murciélago hematófago")
  })
  
  output$CapturaMap <- renderPlotly({
    p <- ggplot() +
      geom_sf(data = MxShp, fill = 'grey60', col = 'grey20') +
      geom_sf(data = z(), aes(size = CAPTURADOS), alpha = 0.5, col = 'red4') +
      theme_void()
    
    ggplotly(p)
  })
  
  
  output$MovMap <- renderPlotly({
    plot_mapbox() %>% 
      add_segments(
        data = edges(), alpha = 0.1, size = I(1),
        color = I('blue'),
        x = ~LONG_O, xend = ~LONG_D,
        y = ~LAT_O, yend = ~LAT_D
      ) %>% 
      add_markers(data = nodes(), type = 'scatter',
                  x = ~LONG, y = ~LAT, color = I('red')) %>% 
      layout(mapbox = list(style = 'open-street-map',
                           center = list(lon=mean(nodes()$LONG), lat=mean(nodes()$LAT)),
                           zoom = 3))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)