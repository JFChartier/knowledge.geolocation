#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(shinythemes)
library(leaflet)
############
#QM.data = readRDS("QM.data.rds")
source("functionsForGeoKnowledgeApp.R", local = T)
all.data=readRDS("all.data.rds")

mySVD=readRDS("2Orgs_approxReducedMatrix-2019-04-03.rds")
latentNormedDocSpace = as.matrix(mySVD$u %*% solve(diag((mySVD$d)))) %>% normRowVectors()




ui<-navbarPage(theme = shinytheme("paper"), inverse=F, windowTitle= "Knowledge Geo", title = "Dashboard Knowledge Geo: ",
               
           navbarMenu("About",
                      
                      tabPanel(title = "Project",
                               source("ui_project.R", local = T, encoding = 'UTF-8')$value
                      ),
                      tabPanel("Team",
                               source("ui_team.R", local = T, encoding = 'UTF-8')$value
                      ),
                      tabPanel("Contact",
                               source("ui_contact.R", local = T, encoding = 'UTF-8')$value
                      )
           ),
           
           tabPanel("Event geolocations",
                    source("ui_eventsGeo.R", local = T, encoding = 'UTF-8')$value
           )#,
           
           #tabPanel("Knowledge geolocation"#,
                    #source("ui_knowledgeGeo.R", local = T, encoding = 'UTF-8')$value
           #),
           #tabPanel("Knowledge change"#,
                    #source("ui_knowledgeChange.R", local = T, encoding = 'UTF-8')$value
           #)
           
)

# Define server 
server <- function(input, output, session) {
  
  if ("shiny" %in% installed.packages()==FALSE)
  {
    install.packages('shiny',dependencies = TRUE)
  }
  
  library(shiny)
  
  if ("leaflet" %in% installed.packages()==FALSE)
  {
    install.packages('leaflet',dependencies = TRUE)
  }
  
  library(leaflet)
  library(dplyr)
  library(magrittr)
  library(scales)
  
  
  myColors = c("black","blue","red","green","brown","pink","orange", "yellow", "lightblue", "gray")
  #myColors = topo.colors(10)
  pal <- leaflet::colorFactor(palette = myColors,
                              levels = unique(all.data$CATEGORY))
  
  
  ########################################
  #render event geolocation
  
  #query_select<-reactive({
    #print(input$query)
    #input$query
    
  #})
  
  # Creating reactive Values:
  relevantSegments <- reactiveValues()
  #initialize as if all segment were perfectly similar to the query
  relevantSegments$relevance=rep(1, nrow(all.data))
  
  
  #create eventReactive
  #similarityWithQuery<-eventReactive(input$button, ignoreNULL=F, valueExpr = {
  observeEvent(input$button, {
    print(str(input$query))
    #similarityWithQuery1<-reactive({
    cat("inside similarityWithQuery")
    print("inside similarityWithQuery")
    
    queryVector=buildQuery(queryTokens = input$query, mySVD = mySVD)
    
    #calculate similarity between query vector and segment vectors
    withProgress(message = 'searching...', value = .1, {
      latentSimilarityWithQuery=proxy::simil(x=queryVector, y = latentNormedDocSpace, by_rows=T, method=dotProduct, convert_distances = FALSE)
      
    })
    
    n=sum(latentSimilarityWithQuery>relavanceMin())
    print(c("number of relevant documents: ", n))
    
    
    relevantSegments$relevance=latentSimilarityWithQuery[1,]   
  })
  
  relavanceMin<-reactive({input$minima})
  
  #category render
  category_select1<-reactive({
    #QM.data
    #QM.data %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity)) #%>%select(c(1:3))
    i=relevantSegments$relevance>=relavanceMin()
    x=all.data[i,]
    x$relevance=relevantSegments$relevance[i]
    x=x %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity & INCIDENT.DATE <=input$time & organization %in% input$organization))
    #x$relevance=relevantSegments$relevance[i]
    x
  })
  
  ##test
  #output$query <- renderText({
    #str(input$query)
    #input$query
  #})
  
  output$eventMap1 <- renderLeaflet(
    {
      map <- category_select1()%>%
        leaflet() %>%
        addProviderTiles("CartoDB") %>%
        # Use dc_hq to add the hq column as popups
        addCircleMarkers(lng = ~LONGITUDE, 
                         lat = ~LATITUDE, 
                         popup = category_select1()$DESCRIPTION, 
                         radius=2,
                         color=~pal(category_select1()$CATEGORY),
                         label=~INCIDENT.TITLE,
                         fill=T,
                         opacity = .9,
                         popupOptions=c(maxWidth = 400, minWidth = 50, maxHeight = 300,
                                      autoPan = TRUE, keepInView = FALSE, closeButton = TRUE)) %>%
        addLegend(position = "bottomright",
                pal = pal,
                values = unique(category_select1()$CATEGORY))
      map
    })
  
  # Create data table of relevant events
  output$relevantEvents <- DT::renderDataTable({
    #idDoc=relevantSegments$similWithQuery>=relavanceMin()
    #x=cbind(all.data[idDoc, c(2,4,5,8,9)], "relevance"=relevantSegments$similWithQuery[idDoc])
    x=category_select1()[,c(2,4,5,8,9,11)]
    #idDoc = order(relevantSegments$similWithQuery>=relavanceMin(), decreasing = T)
    DT::datatable(data = x, 
                  options = list(pageLength = 5), 
                  rownames = FALSE, escape = F)
  })
  
  
}
#library(shinyShortcut)
#shinyShortcut(shinyDirectory = getwd(), OS = .Platform$OS.type, gitIgnore = FALSE)


# Run the application 
shinyApp(ui = ui, server = server)

