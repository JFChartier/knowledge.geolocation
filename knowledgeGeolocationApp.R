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
#add i row for facilitating the subsetting
all.data$i=seq(1:nrow(all.data))


mySVD=readRDS("2Orgs_approxReducedMatrix-2019-04-03.rds")
latentNormedDocSpace = as.matrix(mySVD$u %*% solve(diag((mySVD$d)))) %>% normRowVectors()
doc.term.matrix=readRDS("2Orgs_sparseMatrix-2019-04-03.rds")


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
  library(mapview)
  
  
  myColors = c("black","blue","red","green","brown","pink","orange", "yellow", "lightblue", "gray")
  #myColors = topo.colors(10)
  pal <- leaflet::colorFactor(palette = myColors,
                              levels = unique(all.data$CATEGORY))
  
  palOrg<-leaflet::colorFactor(palette = c("lightgrey", "black"),
                               levels = unique(all.data$organization))
  
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
      #print("inside similarityWithQuery")
      
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
  
  selecMap=reactive({input$map})
  
  center <- reactive({
    current.center=input$eventMap1_center
    current.center
    #if(is.null(input$eventMap1_center)){
      #return(c(179.462, -20.64275))
    #}else{
      #return(input$eventMap1_center)
    #}
    
  })
  
  
  #category render
  category_select1<-reactive({
    #QM.data
    #QM.data %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity)) #%>%select(c(1:3))
    i=relevantSegments$relevance>=relavanceMin()
    print(paste0(sum(i), " number de relevant doc from ", length(relevantSegments$relevance)))
    sub.data=all.data[i,]
    sub.data$RELEVANCE=relevantSegments$relevance[i]
    sub.data=sub.data %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity & INCIDENT.DATE <=input$time & organization %in% input$organization))
    sub.data
  })
  
  ####### create map of selected reports #########################################################
  output$eventMap1 <- renderLeaflet(
    {
      #print(paste("current center: ", center()))
      map <- category_select1()%>%
        leaflet(options = leafletOptions(
          # Set minZoom and dragging 
          dragging = T, zoomControl = T, minZoom = 6, maxZoom = 6)) %>%
        #setView(lng = -73.98575, lat = 40.74856, zoom = 10) %>%
        addProviderTiles(selecMap()) %>%
       addCircleMarkers(lng = ~jitter(LONGITUDE, amount = 0.0005), #jitter
                         lat = ~jitter(LATITUDE, amount=0.0005), #jitter
                         popup = category_select1()$DESCRIPTION, 
                         radius=~((normVector(category_select1()$RELEVANCE)+1)**6)*5, #(((RELEVANCE+1)**4)/2),
                         color=~pal(category_select1()$CATEGORY),
                         label=~INCIDENT.TITLE,
                         fill=T,
                         opacity = 1,
                         fillOpacity = .8,
                         weight = 1,
                         fillColor=~palOrg(category_select1()$organization),
                         #clusterOptions = markerClusterOptions(),
                         popupOptions=c(maxWidth = 400, minWidth = 50, maxHeight = 300,
                                      autoPan = TRUE, keepInView = F, closeButton = TRUE)) %>%
        
        addLegend(position = "bottomright",
                pal = pal,
                values = unique(category_select1()$CATEGORY),
                title="Categories",
                opacity = .8)%>%
        addLegend(position = "bottomleft",
                  pal = palOrg,
                  values = unique(category_select1()$organization),
                  title="Organizations", 
                  opacity = .8)%>%
        addMouseCoordinates(style = "basic")
        #addLayersControl(overlayGroups = c("circles"))
      map
    })
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  #https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
  output$selectReport <- DT::renderDataTable(server = F, {
    x=category_select1()[order(category_select1()$RELEVANCE, decreasing = T),c(2,4,5,8,9,10,11)]
    colnames(x)=toupper(colnames(x))
    
    x$RELEVANCE=round(x$RELEVANCE, 3)
    
    my_place=data_of_click$clickedMarker$id
    
    print(my_place)
    prin(data_of_click)
    DT::datatable(data = x[which(x$ID==my_place),], 
                  rownames = FALSE,
                  escape = F#,
                  #extensions="Buttons",
                  #options = list(dom = 'Bfrtip',
                                 #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 #scrollY = 300,
                                 #scroller = TRUE,
                                 #pageLength = 5
                                 #)
    )
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
      #scale_fill_manual(values = c("sky blue", "grey30"))
    
  })
  
  output$relevantReports<- renderPlot(expr = {
    
    ggplot(data=category_select1()$RELEVANCE%>%as.data.frame(.), aes(.)) + 
      geom_histogram(breaks=seq(from = 0,to = 1, by = 0.1))+
      xlab("Relevance score")+ylab("Count")+
      scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.2), labels=c("0 \n very low", 0.2, 0.4, 0.6, 0.8, "1 \n very high" ))
    
    #alternative stacked barplot
    #dat=cbind(category_select1()$RELEVANCE%>%as.data.frame(.), category_select1()$organization)
    #colnames(dat)=c("RELEVANCE", "ORGANIZATION")
    
    #ggplot(data=dat, aes(x = RELEVANCE, fill=ORGANIZATION)) + 
      #geom_histogram(position="stack", breaks=seq(from = 0,to = 1, by = 0.1))+
      #xlab("Relevance score")+ylab("Count")+
      #scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.2), labels=c("0 \n very low", 0.2, 0.4, 0.6, 0.8, "1 \n very high" ))+
      #scale_fill_manual(values=c("sky blue", "grey30"))+
      #theme(legend.position = "top")
    
    
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
    #str(dat)
    ggplot(data = dat, aes(x = date, y = value, colour=CATEGORY))+
      geom_line(size=1)+
      ylab("Count")+
      xlab("Dates")+
      scale_color_manual(values = pal(unique(dat$CATEGORY)))+
      theme(legend.position = "none")
    
  })
  
  output$categoryByTime2 <- renderPlot(expr = {
    dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$CATEGORY)%>%set_colnames(c("date", "CATEGORY"))
    dat=reshape2::dcast(dat, date~CATEGORY)
    dat=reshape2::melt(dat,id=c("date"), variable.name="CATEGORY")
    #str(dat)
    ggplot(data = dat, aes(x = date, y = value, fill=CATEGORY))+
      geom_bar(stat="identity", position="fill")+
      ylab("Proportion")+
      xlab("Dates")+
      scale_y_continuous(labels = percent_format())+
      scale_fill_manual(values = pal(unique(dat$CATEGORY)))
    #theme(legend.position = "right")
    
  })
  
  output$granularityByTime <- renderPlot(expr = {
    dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$GRANULARITY)%>%set_colnames(c("date", "GRANULARITY"))
    dat=reshape2::dcast(dat, date~GRANULARITY)
    dat=reshape2::melt(dat,id=c("date"), variable.name="GRANULARITY")
    ggplot(data = dat, aes(x = date, y = value, colour=GRANULARITY))+
      geom_line(size=1)+
      ylab("Count")+
      xlab("Dates")+
      #scale_color_manual(values = pal(unique(dat$category)))+
      scale_color_brewer(palette = "Accent")
    #theme(legend.position = "right")
    
  })
  
  output$granularityByTime2 <- renderPlot(expr = {
    dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$GRANULARITY)%>%set_colnames(c("date", "GRANULARITY"))
    dat=reshape2::dcast(dat, date~GRANULARITY)
    dat=reshape2::melt(dat,id=c("date"), variable.name="GRANULARITY")
    
    ggplot(data = dat, aes(x = date, y = value, fill=GRANULARITY))+
      geom_bar(stat="identity", position="fill")+
      ylab("Proportion")+
      xlab("Dates")+
      scale_y_continuous(labels = percent_format())+
      scale_fill_brewer(palette = "Accent")
    #theme(legend.position = "right")
    
  })
  
  ######## lexical specificities in time ##########################
  # Create reactive data frame
  specificities_organizations <- reactive({
    
    #calculer les specificites
    specificites = quanteda::textstat_keyness(x=doc.term.matrix[category_select1()$i,], target=category_select1()$organization=="Quake Map", measure="chi2", sort=TRUE)
    
    #filter NA
    specificites=specificites[is.na(specificites$chi2)==F,]
    
    #set p-value to 0.05, but could be 0.01 if we want to select only reliable specificities 
    specificites=specificites[specificites$p<0.05,]
    specificites
  })
  
  
  output$keynessplot <- renderPlot(
    {
      #set to 30 the number of displayed specificities, but could be change if it is too much
      quanteda::textplot_keyness(x=specificities_organizations(), n=25, show_legend = T)+ #color = c("sky blue", "grey30")
        #geom_col(position=position_dodge(0.5))+
        theme(legend.position="top")+
        scale_fill_manual(values=c("sky blue", "grey30"), 
                          name="ORGANIZATIONS",
                          labels=c("Quake Map", "Doctors without borders"))
        #theme(legend.title = element_text("ORGANIZATIONS"))
        
      
        
        #coord_fixed(ylim=c(0,5000))
      
    })
  
  output$keynessplot2 <- renderPlot({
    n=30
    top.spec=top_n(x = specificities_organizations(), n=n, wt=chi2)
    top.spec=top.spec[top.spec$chi2>0,]
    top.spec$ORGANIZATION="Quake Map"
    bottom.spec=top_n(x = specificities_organizations(), n=-n, wt=chi2)
    bottom.spec=bottom.spec[bottom.spec$chi2<0,]
    bottom.spec$ORGANIZATION="Doctors without\nborders"
    
    dat=rbind(top.spec,bottom.spec)
    #dat$feature=paste(dat$feature, "\n")
    ggplot(data = dat, aes(x = reorder(feature, chi2), y = chi2, fill=ORGANIZATION)) +
      geom_bar(stat = "identity", width = .9, position = position_dodge(width = 2)) +
      #geom_text(aes(label=feature,vjust=0.25, hjust=ifelse(chi2 >= 0, -0.25, 1.25)))+
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Specificity score (chi2)")+
      xlab("Lexical Specificities")+
      #theme(axis.text.x = element_text(angle = 90))+
      coord_flip()+
      scale_fill_manual(values = c("sky blue", "grey28"))
      #ylim(c((min(dat$chi2)-300), (max(dat$chi2)+300)))
      #scale_x_continuous(drop=FALSE) 
    
      #coord_cartesian(xlim=c(,23))
    #geom_text(aes(label=feature), nudge_y = 1, vjust=0.25)
    
    
  })
      
  
  #####################################################################
  
  
    
  
}
#library(shinyShortcut)
#shinyShortcut(shinyDirectory = getwd(), OS = .Platform$OS.type, gitIgnore = FALSE)


# Run the application 
shinyApp(ui = ui, server = server)

