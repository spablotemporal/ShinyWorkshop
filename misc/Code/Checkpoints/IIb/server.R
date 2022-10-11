# Server -----------
# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Reactive objects ------------
  x <- eventReactive(input$filter, {
    p <- vac %>% # data set
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filter the data
  }, ignoreNULL = FALSE) # This is for render on load
  
  observeEvent(x(), {
    showModal(modalDialog("Plots Updated", easyClose = T))
  })
  
  y <- eventReactive(input$filter, {
    y <- vigilancia %>% # data set
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filter the data
  }, ignoreNULL = FALSE)
  
  z <- eventReactive(input$filter, {
    p <- capturaSp %>% # data set
      filter(NOM_MUN %in% input$Mun,
             between(YEAR, input$year[1], right = input$year[2])) # filter the data
  }, ignoreNULL = FALSE)
  
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
  
  output$CapturaMap <- renderPlotly({
    p <- ggplot() +
      geom_sf(data = MxShp, fill = 'grey60', col = 'grey20') +
      geom_sf(data = z(), aes(size = CAPTURADOS), alpha = 0.5, col = 'red4') +
      theme_void()
    
    ggplotly(p)
  })
}