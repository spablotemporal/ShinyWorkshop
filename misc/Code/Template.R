# Initial setup ------------------
# Load th elibraries you will be using for the app
library(shiny)
library(shinydashboard) # library for UI elements

## Load the data ----------
# here you can load the data will be using in the app

# UI -----------------
## Header -----------
# You can define your header
header <- dashboardHeader(title = 'App')

## Sidebar-----------
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Menu for the different tabs
    menuItem(text = 'Tab 1', tabName = 'T1'),
    hr()
    # define your global inputs
    )
  )

## Body ------------
body <- dashboardBody(
  tabItems(
    # Add the tabs of the app body
    tabItem(tabName = 'T1',
            # Add more details for the UI here (i.e. text, inputs, outputs, etc)
            )
    )
)

## Integrate UI ----------
ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body)

# server ------------------
server <- function(input, output){
  ## Reactividad ---------------
  # Define your reactive objects
  
  ## Outputs ------------------
  # Define the outputs for your app
  
}

# Run the app --------
shinyApp(ui = ui, server = server)