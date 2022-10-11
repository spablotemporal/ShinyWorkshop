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
    ## Inputs ------------
    selectInput(inputId = "Mun", label = "Municipalities:", 
                choices = unique(vac$NOM_MUN), multiple = T, 
                selected = unique(vac$NOM_MUN)),
    sliderInput(inputId = 'year', label = 'Year', 
                min = min(as.numeric(vac$YEAR)), max = max(as.numeric(vac$YEAR)), 
                value = range(vac$YEAR)),
    actionButton(inputId = 'filter', label = 'Filter')
  )
)

# Body ---------
body <- dashboardBody(
  tabItems(
    ## First tab ----------
    tabItem(tabName = 'tab1',
            fluidRow(
              column(width = 12, 
                     box(title = 'Vaccination', width = 6,
                         plotOutput("VacMun"), # Figura de serie de tiempo
                     ),
                     box(title = 'Vacunacion boxplot', width = 6,
                         plotOutput("VacBoxplot"), # Figura de serie de tiempo
                     )
              )
            )
    ),
    ## Second tab ----------
    tabItem(tabName = 'tab2',
            fluidRow(
              column(width = 12,
                     box(title = 'Surveillance', width = 6,
                         plotOutput("VigMun"),
                     ),
                     box(
                       DTOutput('TblSurv')
                     )
              )
            )
    ),
    ## Third tab ----------
    tabItem(tabName = 'tab3',
            fluidRow(
              column(width = 12,
                     box(title = 'Captures', width = 6,
                         plotOutput("CapMun"),
                     ),
                     box(title = 'Capture locations', width = 6,
                         leafletOutput('CapturaMap')
                         ),
                     box(title = 'Add New Observation', width = 12,
                         checkboxInput(inputId = 'NewEntry', label = 'Add New Observation'),
                         # 'Latitude/Longitude: ', shiny::textOutput('MapLoc'),
                         uiOutput(outputId = 'NameOut'),
                         tableOutput('LocationsDF')
                         )
              )
            )
    )
  )
)

# Define the UI
dashboardPage(header = header, sidebar = sidebar, body = body) %>% 
  shiny::shinyUI()