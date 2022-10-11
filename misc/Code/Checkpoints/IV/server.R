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
  
  Loc <- reactive({
    c(input$CapturaMap_click[[1]], input$CapturaMap_click[[2]])
  })
  
  observeEvent(input$submit, {
    showModal(modalDialog("Uploading data", footer=NULL))
    # Save the data
    data.frame(Name = input$NameIn,
               Lat = Loc()[1],
               Lon = Loc()[2]) %>% 
      sheet_append(ss = Geolocations, data = .)
    removeModal()
    
    
    # Show message
    showModal(modalDialog(
      title = "Submision complete",
      paste0('Thanks for submiting the information'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  dGL <- eventReactive(input$submit, {
    read_sheet(ss = Geolocations) 
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
  
  output$TblSurv <- renderDT({
    y() %>% 
      DT::datatable(
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          buttons = list("copy",
                         list(extend = "collection",
                              buttons = c("csv", "excel", "pdf"),
                              text = "Download")))
      )
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
  
  output$CapturaMap <- renderLeaflet({
    # Creating a popup
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addMarkers(data = dGL(), lat = ~Lat, lng = ~Lon,
                 popup = ~paste('<h3>Name:', Name, '</h3>')) %>% 
      addMarkers( 
        data = z(),
        clusterOptions = markerClusterOptions(),
        # We can format the popup using some html tags
        popup = ~paste('<h3>Name:', NOM_LOC, '</h3>', 
                       "<br>Captures:", CAPTURADOS,
                       "<br>Location:", REFUGIO_NOM
        ) # The variable we are using for the label
      )
  })
  
  # Render the new inputs
  output$NameOut <- renderUI({
    if(input$NewEntry){
      tagList(
        'Lat/Long:', shiny::textOutput('MapLoc'),
        shiny::textInput(inputId = 'NameIn', label = 'Name'),
        actionButton(inputId = 'submit', label = 'Submit')
      )
    }
  })
  
  output$MapLoc <- renderText({
    if(is.null(input$CapturaMap_click)){ 
      'Click on the map to get the coordinates'
    }else{
      Loc()
    }
  })
  
  output$LocationsDF <- renderTable({
    dGL()
  })
}