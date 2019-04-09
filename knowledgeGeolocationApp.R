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
all.data=readRDS("all.unique.data.rds")

mySVD=readRDS("2Orgs_approxReducedMatrix-2019-04-03.rds")
latentNormedDocSpace = as.matrix(mySVD$u %*% solve(diag((mySVD$d)))) %>% normRowVectors()




ui<-navbarPage(theme = shinytheme("paper"), inverse=F, windowTitle= "Knowledge Geo", title = "Dashboard Knowledge Geo: ",
               tabPanel("Event geolocations",
                        source("ui_eventsGeo.R", local = T, encoding = 'UTF-8')$value
               ),
               
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
               )
           
           #tabPanel("Knowledge geolocation"#,
                    #source("ui_knowledgeGeo.R", local = T, encoding = 'UTF-8')$value
           #),
           #tabPanel("Knowledge change"#,
                    #source("ui_knowledgeChange.R", local = T, encoding = 'UTF-8')$value
           #)
           
)

# Define server 
server <- function(input, output, session) {
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(magrittr)
  library(scales)
  library(ggplot2)
  
  
  myColors = c("black","blue","red","green","brown","pink","orange", "yellow", "lightblue", "gray")
  #myColors = topo.colors(10)
  pal <- leaflet::colorFactor(palette = myColors,
                              levels = unique(all.data$CATEGORY))
  
  # Creating reactive Values:
  relevantSegments <- reactiveValues()
  #initialize as if all segment were perfectly similar to the query
  relevantSegments$relevance=rep(1, nrow(all.data))
  
  
  
  observeEvent(input$button, {
    #initialize as if all segment were perfectly similar to the query
    relevantSegments$relevance=rep(1, nrow(all.data))
    print(paste("input: ", input$query))
    str(input$query)
    if(is.null(input$query)==F){
      #print(str(input$query))
      #similarityWithQuery1<-reactive({
      #cat("inside similarityWithQuery")
      print("inside similarityWithQuery")
      
      queryVector=buildQuery(queryTokens = input$query, mySVD = mySVD)
      
      #calculate similarity between query vector and segment vectors
      withProgress(message = 'searching...', value = .1, {
        latentSimilarityWithQuery=proxy::simil(x=queryVector, y = latentNormedDocSpace, by_rows=T, method=dotProduct, convert_distances = FALSE)
        
      })
      
      n=sum(latentSimilarityWithQuery>=relavanceMin())
      print(c("number of relevant documents: ", n))
      relevantSegments$relevance=latentSimilarityWithQuery[1,]  
      
    }
     
  })
  
  relavanceMin<-reactive({input$minima})
  
  #category render
  category_select1<-reactive({
    #QM.data
    #QM.data %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity)) #%>%select(c(1:3))
    i=relevantSegments$relevance>=relavanceMin()
    print(paste0(sum(i), " number de relevant doc from ", length(relevantSegments$relevance)))
    x=all.data[i,]
    x$RELEVANCE=relevantSegments$relevance[i]
    x=x %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity & INCIDENT.DATE <=input$time & organization %in% input$organization))
    #initialize again all segments as perfectly similar to the query
    #relevantSegments$relevance=rep(1, nrow(all.data))
    x
  })
  
  ####### create map of selected reports #########################################################
  output$eventMap1 <- renderLeaflet(
    {
      map <- category_select1()%>%
        leaflet(options = leafletOptions(
          # Set minZoom and dragging 
          dragging = T)) %>%
        addProviderTiles("CartoDB") %>%
        # Use dc_hq to add the hq column as popups
        addCircleMarkers(lng = ~jitter(LONGITUDE, amount = 0.0005), #jitter
                         lat = ~jitter(LATITUDE, amount=0.0005), #jitter
                         popup = category_select1()$DESCRIPTION, 
                         radius=~((normVector(category_select1()$RELEVANCE)+1)**6)*2, #(((RELEVANCE+1)**4)/2),
                         color=~pal(category_select1()$CATEGORY),
                         label=~INCIDENT.TITLE,
                         fill=T,
                         opacity = .8,
                         #clusterOptions = markerClusterOptions(),
                         popupOptions=c(maxWidth = 400, minWidth = 50, maxHeight = 300,
                                      autoPan = TRUE, keepInView = F, closeButton = TRUE)) %>%
        addLegend(position = "bottomright",
                pal = pal,
                values = unique(category_select1()$CATEGORY))
      map
    })
  ################################################################################################
  
  ###### Create data table of relevant reports ########################################################
  output$relevantEvents <- DT::renderDataTable(server = F, {
    x=category_select1()[order(category_select1()$RELEVANCE, decreasing = T),c(2,4,5,8,10,11)]
    colnames(x)=toupper(colnames(x))
    
    x$RELEVANCE=round(x$RELEVANCE, 3)
    
    DT::datatable(data = x, 
                  rownames = FALSE,
                  escape = F,
                  extensions="Buttons",
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 #scrollY = 300,
                                 #scroller = TRUE,
                                 pageLength = 5)
    )
  })
  ######################################################################################
  
  output$organizationReports<- renderPlot(expr = {
    ggplot(data = category_select1()$organization%>%as.data.frame(.)) +
      geom_bar(mapping = aes(x = ., y = ..count.., group = 1), stat = "count") + 
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Count")+xlab("Organizations")
    
  })
  
  output$relevantReports<- renderPlot(expr = {
    
    ggplot(data=category_select1()$RELEVANCE%>%as.data.frame(.), aes(.)) + 
      geom_histogram(breaks=seq(from = 0,to = 1, by = 0.1))+
      xlab("Relevance score")+ylab("Count")+
      scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.2), labels=c("0 \n very low", 0.2, 0.4, 0.6, 0.8, "1 \n very high" ))
    
  })
  
  output$reportsByCategory<- renderPlot(expr = {
    
    dat=table(category_select1()$CATEGORY)%>%as.data.frame(., stringsAsFactors=F)
    colnames(dat)=c("cat", "freq")
    
    ggplot(data = dat, aes(x = cat, y = freq)) +
      geom_bar(stat = "identity") + 
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Count")+
      xlab("Report category")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
      #geom_text(aes(y=..prop.., label = scales::percent(..prop..)), stat= "count", vjust = -.5, size = 3)
    
  })
  
  output$reportsByGranularity<- renderPlot(expr = {
    
    dat=table(category_select1()$GRANULARITY)%>%as.data.frame(., stringsAsFactors=F)
    colnames(dat)=c("gran", "freq")
    
    ggplot(data = dat, aes(x = gran, y = freq)) +
      geom_bar(stat = "identity") + 
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Count")+
      xlab("Report granularity")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #geom_text(aes(y=..prop.., label = scales::percent(..prop..)), stat= "count", vjust = -.5, size = 3)
    
  })
  
  ############ category and time #############################
  output$categoryByTime <- renderPlot(expr = {
    dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$CATEGORY)%>%set_colnames(c("date", "CATEGORY"))
    dat=reshape2::dcast(dat, date~CATEGORY)
    dat=reshape2::melt(dat,id=c("date"), variable.name="CATEGORY")
    str(dat)
    ggplot(data = dat, aes(x = date, y = value, colour=CATEGORY))+
      geom_line(size=1)+
      ylab("Count")+
      xlab("Dates")+
      scale_color_manual(values = pal(unique(dat$CATEGORY)))
      #theme(legend.position = "right")
    
  })
  
  output$granularityByTime <- renderPlot(expr = {
    dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$GRANULARITY)%>%set_colnames(c("date", "GRANULARITY"))
    dat=reshape2::dcast(dat, date~GRANULARITY)
    dat=reshape2::melt(dat,id=c("date"), variable.name="GRANULARITY")
    str(dat)
    ggplot(data = dat, aes(x = date, y = value, colour=GRANULARITY))+
      geom_line(size=1)+
      ylab("Count")+
      xlab("Dates")+
      #scale_color_manual(values = pal(unique(dat$category)))+
      scale_color_brewer(palette = "Accent")
    #theme(legend.position = "right")
    
  })
  
  
    
  
}
#library(shinyShortcut)
#shinyShortcut(shinyDirectory = getwd(), OS = .Platform$OS.type, gitIgnore = FALSE)


# Run the application 
shinyApp(ui = ui, server = server)

