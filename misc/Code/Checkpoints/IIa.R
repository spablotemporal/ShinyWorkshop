# Checkpoint IIa:
# In this app we modified the user interface using the package shinydash
# Load the libraries
library(shiny)
library(dplyr) # For data manipulation
library(ggplot2) # For figures
library(STNet) # Where the data is
library(shinydashboard) # To create the UI

# Load the data from STNet 
data("vac") # Data for vaccination
data("vigilancia") # Data for surveillance
data("captura") # Data for captures

# Define interface
# Header -----------
header <- dashboardHeader(title = 'New app')
# Sidebar -------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    'This is the sidebar of the app', # Some text
    # Tabs in our app
    menuItem("Vaccination", tabName = "tab1"),
    menuItem("Surveillance", tabName = "tab2"),
    menuItem("Captures", tabName = "tab3"),
    br(), # this is to leave some space after the tab names
    selectInput(inputId = "Mun", label = "Municipios:", 
                choices = unique(vac$NOM_MUN), multiple = T, 
                selected = unique(vac$NOM_MUN)),
    sliderInput(inputId = 'year', label = 'Año', 
                min = min(as.numeric(vac$YEAR)), max = max(as.numeric(vac$YEAR)), 
                value = range(vac$YEAR))
  )
)

# Body -----------
body <- dashboardBody(
  tabItems(
    # Primer tab
    tabItem(tabName = 'tab1',
            fluidRow(
              column(width = 12, 
                     box(title = 'Vacunacion', width = 6,
                         plotOutput("VacMun"), # Figura de serie de tiempo
                     ),
                     box(title = 'Vacunacion boxplot', width = 6,
                         plotOutput("VacBoxplot"), # Figura de serie de tiempo
                     )
              )
            )
    ),
    
    tabItem(tabName = 'tab2',
            fluidRow(
              column(width = 12,
                     box(title = 'Vigilancia', width = 6,
                         plotOutput("VigMun"),
                     )
              )
            )
    ),
    tabItem(tabName = 'tab3',
            fluidRow(
              column(width = 12,
                     box(title = 'Captura', width = 6,
                         plotOutput("CapMun"),
                     )
              )
            )
    )
  )
)

# Integrar los componentes de la interfaz
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  x <- reactive({
    p <- vac %>% # base de datos
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filtramos los datos
  })
  
  output$VacMun <- renderPlot({
    x() %>% 
      group_by(YEAR) %>% 
      summarise(Vac = sum(VAC_BOV)) %>% 
      ggplot(aes(x = YEAR, y=Vac)) +
      geom_bar(position="dodge", stat="identity", fill = "deepskyblue4") +
      labs(x = "Año", y = "Dosis de vacuna aplicados",
           title = "Aplicación de vacuna antirrábica")
  })
  
  output$VacBoxplot <- renderPlot({
    x() %>% 
      group_by(YEAR, NOM_MUN) %>% 
      summarise(Hatos = sum(TOTAL_HATOS, na.rm = T),
                Vacunados = sum(HATOS_VAC, na.rm = T)) %>% 
      mutate(pVac = Vacunados/Hatos) %>% 
      ggplot() +
      geom_boxplot(aes(x = YEAR, y = pVac)) +
      geom_jitter(aes(x = YEAR, y = pVac), width = 0.1)
  })
  
  y <- reactive({
    p <- vigilancia %>% # base de datos
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filtramos los datos
  })
  
  output$VigMun <- renderPlot({
    y() %>%
      group_by(YEAR, RESULTADO) %>%
      summarise(N = n()) %>%
      ggplot(aes(x = YEAR, y=N, fill=RESULTADO)) +
      geom_bar(position="dodge", stat="identity")+
      theme(legend.position = "top") +
      labs(x = "Año", y = "Muestras enviadas al laboratorio",
           title = "Vigilancia epidemiológica de rabia paralítica en el suroeste del Estado de México")
  })
  
  z <- reactive({
    p <- captura %>% # base de datos
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filtramos los datos
  })
  
  output$CapMun <- renderPlot({
    z() %>%
      group_by(YEAR) %>%
      summarise(N = n()) %>%
      ggplot(aes(x = YEAR, y=N)) +
      geom_bar(position="dodge", stat="identity", fill = "red4") +
      labs(x = "Año", y = "Eventos de captura de murciélago hematófago",
           title = "Control de población de murciélago hematófago")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)