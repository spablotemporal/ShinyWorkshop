# Server -----------
# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Reactive objects ------------
  x <- reactive({
    p <- vac %>% 
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2]))
  })
  
  y <- reactive({
    p <- vigilancia %>% 
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2]))
  })
  
  z <- reactive({
    p <- captura %>%
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2]))
  })
  
  ## Outputs ---------
  output$VacMun <- renderPlot({
    x() %>% 
      group_by(YEAR) %>% 
      summarise(Vac = sum(VAC_BOV)) %>% 
      ggplot(aes(x = YEAR, y=Vac)) +
      geom_bar(position="dodge", stat="identity", fill = "deepskyblue4") +
      labs(x = "Year", y = "Vaccine Doses",
           title = "Rabies vaccines applied")
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
  
  output$VigMun <- renderPlot({
    y() %>%
      group_by(YEAR, RESULTADO) %>%
      summarise(N = n()) %>%
      ggplot(aes(x = YEAR, y=N, fill=RESULTADO)) +
      geom_bar(position="dodge", stat="identity")+
      theme(legend.position = "top") +
      labs(x = "Year", y = "Tests",
           title = "Rabies surveillance")
  })
  
  output$CapMun <- renderPlot({
    z() %>%
      group_by(YEAR) %>%
      summarise(N = n()) %>%
      ggplot(aes(x = YEAR, y=N)) +
      geom_bar(position="dodge", stat="identity", fill = "red4") +
      labs(x = "Year", y = "Bat captures",
           title = "Bat population control")
  })
}