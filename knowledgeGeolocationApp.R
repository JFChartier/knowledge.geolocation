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

source("functionsForGeoKnowledgeApp.R", local = T)
all.data=readRDS("all.unique.data.rds")
#add i row for facilitating the subsetting
all.data$i=seq(1:nrow(all.data))


mySVD=readRDS("2Orgs_approxReducedMatrix-2019-04-03.rds")
latentNormedDocSpace = as.matrix(mySVD$u %*% solve(diag((mySVD$d)))) %>% normRowVectors()
doc.term.matrix=readRDS("2Orgs_sparseMatrix-2019-04-03.rds")

simil.bet.org=readRDS("simil.bet.org.of.same.category.rds")


ui<-navbarPage(theme = shinytheme("journal"), inverse=F, windowTitle= "Knowledge Geo", title = "Knowledge Geo: ",
               tabPanel("Dashboard",
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
  library(ggrepel)
  
  
  myColors = c("black","blue","red","green","brown","pink","orange", "yellow", "lightblue", "gray")
  #myColors = topo.colors(10)
  pal <- leaflet::colorFactor(palette = myColors,
                              levels = unique(all.data$CATEGORY))
  
  palOrg<-leaflet::colorFactor(palette = c("black", "white"),
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
  
  
  zoom<-reactive({input$zoom})
  
  centerLat=reactive({input$lalitude})
  
  centerLon=reactive({input$longitude})
  
  
  #category render
  category_select1<-reactive({
    i=relevantSegments$relevance>=relavanceMin()
    print(paste0(sum(i), " number de relevant doc from ", length(relevantSegments$relevance)))
    sub.data=all.data[i,]
    sub.data$RELEVANCE=relevantSegments$relevance[i]
    sub.data=sub.data %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity & INCIDENT.DATE <=input$time & organization %in% input$organization))
    #use this next line if the time slider has 2 thresholds
    #sub.data=sub.data %>% filter((CATEGORY %in% input$category & GRANULARITY %in% input$granularity & INCIDENT.DATE >=input$time[1] & INCIDENT.DATE <=input$time[2] & organization %in% input$organization))
    
    sub.data
  })
  
  ####### create map of selected reports #########################################################
  output$eventMap1 <- renderLeaflet(
    {
      #print(paste("current center: ", center()))
      map <- category_select1()%>%
        leaflet(options = leafletOptions(
          # Set minZoom and dragging 
          dragging = T, zoomControl = T, minZoom = zoom()[1], maxZoom = zoom()[2])) %>%
        setView(lng = centerLon(), lat = centerLat(), zoom = zoom()[1]) %>%
        addProviderTiles(selecMap()) %>%
       addCircleMarkers(lng = ~jitter(LONGITUDE, amount = 0.0005), #jitter
                         lat = ~jitter(LATITUDE, amount=0.0005), #jitter
                         popup = category_select1()$DESCRIPTION, 
                         radius=~((normVector(category_select1()$RELEVANCE)+1)**6)*5, #(((RELEVANCE+1)**4)/2),
                         color=~pal(category_select1()$CATEGORY),
                         label=~INCIDENT.TITLE,
                         fill=T,
                         opacity = 1,
                         fillOpacity = 1,
                         weight = 2,
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
  # data_of_click <- reactiveValues(clickedMarker=NULL)
  # store the click
  # observeEvent(input$map_marker_click,{
  #   print(input$map_marker_click)
  #   data_of_click$clickedMarker <- input$map_marker_click
  # })
  
  #https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
  # output$selectReport <- DT::renderDataTable(server = F, {
  #   x=category_select1()[order(category_select1()$RELEVANCE, decreasing = T),c(2,4,5,8,9,10,11)]
  #   colnames(x)=toupper(colnames(x))
  #   
  #   #x$RELEVANCE=round(x$RELEVANCE, 3)
  #   
  #   my_place=data_of_click$clickedMarker$id
  #   
  #   print(my_place)
  #   print(data_of_click)
  #   DT::datatable(data = x[which(x$ID==my_place),], 
  #                 rownames = FALSE,
  #                 escape = F#,
  #                 #extensions="Buttons",
  #                 #options = list(dom = 'Bfrtip',
  #                                #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
  #                                # scrollY = 300,
  #                                # scroller = TRUE,
  #                                # pageLength = 5
  #                                # )
  #   )
  # })
  
  ################################################################################################
  
  ###### Create data table of relevant reports ########################################################
  output$relevantEvents <- DT::renderDataTable(server = F, {
    x=category_select1()[order(category_select1()$RELEVANCE, decreasing = T),c(2,4,5,8,10,12)]
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
  
  output$propReports<-renderPlot(expr = {
    n=nrow(all.data)
    m=nrow(category_select1())
    
    dat=data.frame("Reports"=c("Remaining reports", "Selected reports"), Count=c(n-m, m))
    dat$Percentage=c(dat$Count[1]/n*100, dat$Count[2]/dat$Count[1]*100)
    
    # ggplot(dat, aes(x="", y=Count, fill=Reports))+
    #   geom_bar(width = 1, stat = "identity", colour="black")+
    #   coord_polar("y", start=0)+
    #   scale_fill_manual(values=c("white", "grey30"))
    
    ggplot(dat, aes(x=2, y=Count, fill=Reports))+
      geom_bar(stat="identity", colour="black")+
      coord_polar(theta = "y", start = 0)+
      #geom_text(aes(y = Count, label = Reports), color = "black")+
      xlim(0.5, 2.5)+
      theme(axis.ticks=element_blank()) +
      #theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      scale_fill_manual(values=c("white", "grey30"))+
      xlab("")+ylab("")+
      #geom_label_repel(colour="white", segment.colour="black")
      #geom_label_repel(aes(label = paste(round(Percentage,2),"%")), colour=c("black", "white"), segment.colour="black", show.legend = F)+
      geom_label_repel(aes(label = paste(Count,"reports")), colour=c("black", "white"), segment.colour="black", show.legend = F, size=5)
    
      
  })
  
  
  
  output$organizationReports<- renderPlot(expr = {
    dat=category_select1()$organization%>%table()%>%as.data.frame()%>%set_colnames(c("ORGANIZATION", "COUNT"))
    ggplot(data = dat, aes(x = ORGANIZATION, y = COUNT))+
      geom_bar(aes(fill = ORGANIZATION), stat = "identity", colour="black") + 
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Count")+
      xlab("")+
      #theme(axis.text=element_blank())+
      scale_fill_manual(values = c("white", "grey30"))+
      theme(legend.position = "right")
    
    
    # ggplot(data = category_select1()$organization%>%as.data.frame(.)) +
    #   geom_bar(mapping = aes(x = ., y = ..count.., group = 1), stat = "count", colour="black", fill=c("white", "grey30")) + 
    #   #scale_y_continuous(labels = scales::percent_format())+
    #   ylab("Count")+
    #   xlab("Organizations")+
    #   scale_fill_manual(values = c("white", "grey30"))+
    #   theme(legend.position = "right")
    
  })
  
  output$relevantReports<- renderPlot(expr = {
    
    # ggplot(data=category_select1()$RELEVANCE%>%as.data.frame(.), aes(.)) + 
    #   geom_histogram(breaks=seq(from = 0,to = 1, by = 0.1))+
    #   xlab("Relevance score")+ylab("Count")+
    #   scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.2), labels=c("0 \n very low", 0.2, 0.4, 0.6, 0.8, "1 \n very high" ))
    
    #alternative stacked barplot
    dat=cbind(category_select1()$RELEVANCE%>%as.data.frame(.), category_select1()$organization)
    colnames(dat)=c("RELEVANCE", "ORGANIZATION")
    
    ggplot(data=dat, aes(x = RELEVANCE, fill=ORGANIZATION)) + 
      geom_histogram(position="stack", breaks=seq(from = 0,to = 1, by = 0.1), colour="black")+
      xlab("Relevance score")+ylab("Count")+
      scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, by=.2), labels=c("0 \n very low", 0.2, 0.4, 0.6, 0.8, "1 \n very high" ))+
      scale_fill_manual(values=c("white", "grey30"))+
      theme(legend.position = "right")
    
    
  })
  
  output$reportsByCategory<- renderPlot(expr = {
    
    dat=category_select1()[,c("CATEGORY", "organization")]
    dat=reshape2::dcast(dat, CATEGORY~organization)
    dat=reshape2::melt(dat,id.vars=c("CATEGORY"), variable.name="Organization", value.name="Count")
    
    ggplot(data = dat, aes(x = CATEGORY, y = Count, fill=Organization)) +
      geom_bar(stat = "identity", colour="black") + 
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Count")+
      xlab("Report category")+
      scale_fill_manual(values=c("white", "grey30"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    #old format
    #dat=table(category_select1()$CATEGORY)%>%as.data.frame(., stringsAsFactors=F)
    #colnames(dat)=c("cat", "freq")
    
    
    # ggplot(data = dat, aes(x = cat, y = freq)) +
    #   geom_bar(stat = "identity", colour="black") + 
    #   #scale_y_continuous(labels = scales::percent_format())+
    #   ylab("Count")+
    #   xlab("Report category")+
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #   #geom_text(aes(y=..prop.., label = scales::percent(..prop..)), stat= "count", vjust = -.5, size = 3)
    
  })
  
  output$reportsByGranularity<- renderPlot(expr = {
    dat=category_select1()[,c("GRANULARITY", "organization")]
    dat=reshape2::dcast(dat, GRANULARITY~organization)
    dat=reshape2::melt(dat,id.vars=c("GRANULARITY"), variable.name="Organization", value.name="Count")
    
    ggplot(data = dat, aes(x = GRANULARITY, y = Count, fill=Organization)) +
      geom_bar(stat = "identity", colour="black") + 
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Count")+
      xlab("Report granularity")+
      scale_fill_manual(values=c("white", "grey30"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #old format
    
    # dat=table(category_select1()$GRANULARITY)%>%as.data.frame(., stringsAsFactors=F)
    # colnames(dat)=c("gran", "freq")
    # 
    # ggplot(data = dat, aes(x = gran, y = freq)) +
    #   geom_bar(stat = "identity", colour="black") + 
    #   #scale_y_continuous(labels = scales::percent_format())+
    #   ylab("Count")+
    #   xlab("Report granularity")+
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # #geom_text(aes(y=..prop.., label = scales::percent(..prop..)), stat= "count", vjust = -.5, size = 3)
    # 
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
  
  output$reportsByTime <- renderPlot(expr = {
    dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$organization)%>%set_colnames(c("date", "ORGANIZATION"))
    dat=reshape2::dcast(dat, date~ORGANIZATION)
    dat=reshape2::melt(dat,id=c("date"), variable.name="ORGANIZATION")
    #str(dat)
    ggplot(data = dat, aes(x = date, y = value, colour=ORGANIZATION))+
      geom_line(size=2)+
      ylab("Count")+
      xlab("Dates")+
      scale_color_manual(values=c("white", "grey30"))+
      theme(legend.position = "right")
      #scale_color_brewer(palette = "Accent")
    
  })
  
  # ouput$reportsByTime<-renderPlot(expr = {
  #   dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$GRANULARITY)%>%set_colnames(c("date", "GRANULARITY"))
  #   dat=reshape2::dcast(dat, date~GRANULARITY)
  #   dat=reshape2::melt(dat,id=c("date"), variable.name="GRANULARITY")
  #   ggplot(data = dat, aes(x = date, y = value, colour=GRANULARITY))+
  #     geom_line(size=1)+
  #     ylab("Count")+
  #     xlab("Dates")+
  #     #scale_color_manual(values = pal(unique(dat$category)))+
  #     scale_color_brewer(palette = "Accent")
  #   #theme(legend.position = "right")
  #   
  # })
  
  # output$granularityByTime <- renderPlot(expr = {
  #   dat=cbind(category_select1()$INCIDENT.DATE %>% as.Date(.)%>%as.data.frame(), category_select1()$GRANULARITY)%>%set_colnames(c("date", "GRANULARITY"))
  #   dat=reshape2::dcast(dat, date~GRANULARITY)
  #   dat=reshape2::melt(dat,id=c("date"), variable.name="GRANULARITY")
  #   ggplot(data = dat, aes(x = date, y = value, colour=GRANULARITY))+
  #     geom_line(size=1)+
  #     ylab("Count")+
  #     xlab("Dates")+
  #     #scale_color_manual(values = pal(unique(dat$category)))+
  #     scale_color_brewer(palette = "Accent")
  #   #theme(legend.position = "right")
  #   
  # })
  
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
    #keep only word with prevalence larger than 5. Necessery for the chi2 since the statistic has erratic behavior for low frequency
    specificites=specificites[(specificites$n_target+specificites$n_reference)>=5,]
    specificites
  })
  
  #show specificities with quanteda's plot function
  #comes with bugs when we want to modify the legend
  # output$keynessplot <- renderPlot(
  #   {
  #     #set to 30 the number of displayed specificities, but could be change if it is too much
  #     quanteda::textplot_keyness(x=specificities_organizations(), n=25, show_legend = T)+ #color = c("sky blue", "grey30")
  #       #geom_col(position=position_dodge(0.5))+
  #       theme(legend.position="top")+
  #       scale_fill_manual(values=c("sky blue", "grey30"), 
  #                         name="ORGANIZATIONS",
  #                         labels=c("Quake Map", "Doctors without borders"))
  #       #theme(legend.title = element_text("ORGANIZATIONS"))
  #       
  #     
  #       
  #       #coord_fixed(ylim=c(0,5000))
  #     
  #   })
  
  output$keynessplot2 <- renderPlot({
    #keep only significant specifities, where p-value < 0.05. Could be 0.01 too, if you feel less generous 
    specificites=specificities_organizations()[specificities_organizations()$p<0.01,]
    
    n=30
    top.spec=dplyr::top_n(x = specificites, n=n, wt=chi2)
    top.spec=top.spec[top.spec$chi2>0,]
    top.spec$ORGANIZATION="Quake Map"
    bottom.spec=top_n(x = specificites, n=-n, wt=chi2)
    bottom.spec=bottom.spec[bottom.spec$chi2<0,]
    bottom.spec$ORGANIZATION="Doctors without\nborders"
    
    dat=rbind(top.spec,bottom.spec)
    #dat$feature=paste(dat$feature, "\n")
    ggplot(data = dat, aes(x = reorder(feature, chi2), y = chi2, fill=ORGANIZATION)) +
      geom_bar(stat = "identity", width = .9, position = position_dodge(width = 2), colour="black") +
      #geom_text(aes(label=feature,vjust=0.25, hjust=ifelse(chi2 >= 0, -0.25, 1.25)))+
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Specificity score (chi2)")+
      xlab("Lexical Specificities")+
      #theme(axis.text.x = element_text(angle = 90))+
      coord_flip()+
      scale_fill_manual(values = c("white", "grey28"))+
      theme(axis.text = element_text(size = 15, colour = "black"))
      #ylim(c((min(dat$chi2)-300), (max(dat$chi2)+300)))
      #scale_x_continuous(drop=FALSE) 
    
      #coord_cartesian(xlim=c(,23))
    #geom_text(aes(label=feature), nudge_y = 1, vjust=0.25)
    
    
  })
  
  
  output$keynessplotNeg <- renderPlot({
    #View(specificities_organizations())  
    #keeping only non-significant specifities, where p-value > 0.05. Could be 0.01 too if fell more generous 
    specificites=specificities_organizations()[specificities_organizations()$p<0.01,]
    
    #View(specificites, title = "significant specif") 
    top.spec=specificites[specificites$chi2>=0,]
    
    #=-30
    
    top.spec=dplyr::top_n(x = top.spec, n=-30, wt=chi2)
    top.spec$ORGANIZATION="Quake Map"
    
    bottom.spec=specificites[specificites$chi2<=0,]
    bottom.spec=top_n(x = bottom.spec, n=30, wt=chi2)
    #bottom.spec=bottom.spec[order(bottom.spec$chi2, decreasing = T),]
    bottom.spec$ORGANIZATION="Doctors without\nborders"
    
    dat=rbind(top.spec,bottom.spec)
    #dat$feature=paste(dat$feature, "\n")
    ggplot(data = dat, aes(x = reorder(feature, chi2), y = chi2, fill=ORGANIZATION)) +
      geom_bar(stat = "identity", width = .9, position = position_dodge(width = 2), colour="black") +
      #geom_text(aes(label=feature,vjust=0.25, hjust=ifelse(chi2 >= 0, -0.25, 1.25)))+
      #scale_y_continuous(labels = scales::percent_format())+
      ylab("Specificity score (chi2)")+
      xlab("Lexical Specificities")+
      #theme(axis.text.x = element_text(angle = 90))+
      coord_flip()+
      scale_fill_manual(values = c("white", "grey28"))+
      theme(axis.text = element_text(size = 15, colour = "black"))
    #ylim(c((min(dat$chi2)-300), (max(dat$chi2)+300)))
    #scale_x_continuous(drop=FALSE) 
    
    #coord_cartesian(xlim=c(,23))
    #geom_text(aes(label=feature), nudge_y = 1, vjust=0.25)
    
    
  })
  
  commonWordsInClounds <- reactive({
    
    dat=category_select1()[category_select1()$organization=="Quake Map",]
    dat=doc.term.matrix[dat$i,]
    topWordsQM=quanteda::topfeatures(dat,n=50, decreasing=T)
    
    dat=category_select1()[category_select1()$organization=="Doctors Without Borders",]
    dat=doc.term.matrix[dat$i,]
    topWordsDWB=quanteda::topfeatures(dat,n=50, decreasing=T)
    
    common=intersect(names(topWordsQM), names(topWordsDWB))
    #print(common)
    common
  })
  
  
  output$wordcoundQM <- renderPlot({
    dat=category_select1()[category_select1()$organization=="Quake Map",]
    dat=doc.term.matrix[dat$i,]
    topWords=quanteda::topfeatures(dat,n=50, decreasing=T)
    #dat=quanteda::dfm_select(dat,selection="keep", pattern=names(topWords), valuetype="fixed", case_insensitive=T)
    
    myCols=rep("black", length(topWords))
    #match_value<-match(commonWordsInClounds(), quanteda::featnames(dat))
    match_value<-match(commonWordsInClounds(), names(topWords))
    #print(quanteda::featnames(dat)[match_value])
    #print(topWords[match_value])
    myCols[match_value]="darkred"
    #View(cbind(topWords,myCols), "QM")
    #quanteda::textplot_wordcloud(x = dat, min_count=1, color=myCols)
    wordcloud::wordcloud(words = names(topWords), freq = topWords, color=myCols, ordered.colors=T, use.r.layout = T, scale=c(3,1))
    
  })
  
  output$wordcouldDWB <- renderPlot({
    dat=category_select1()[category_select1()$organization=="Doctors Without Borders",]
    dat=doc.term.matrix[dat$i,]
    topWords=quanteda::topfeatures(dat,n=50, decreasing=T)
    
    dat=data.frame(myCols=rep("black", length(topWords)), freq=topWords, word=names(topWords), stringsAsFactors = F)
    match_value<-match(commonWordsInClounds(), names(topWords))
    dat$myCols[match_value]="darkred"
    #wordcloud2::wordcloud2Output(as.data.frame(dat), color=myCols)
    wordcloud::wordcloud(words = dat$word, freq = dat$freq, color=dat$myCols, ordered.colors=T, use.r.layout=T, scale=c(3,1))
    
  })
  
  output$similOrg<-renderPlot({
    i= (simil.bet.org$id.doctors %in% category_select1()$i) & (simil.bet.org$id.QM %in% category_select1()$i)
    #str(i)
    #print(sum(i))
    dat=simil.bet.org[i,]
    ggplot(dat, aes(x=cat.doc, y=simil,  fill=cat.doc)) + geom_boxplot()+ 
      #facet_grid(.~cat.doc)+
      stat_summary(fun.y=mean, geom="point", shape=4, size=2)+
      labs(subtitle="x indicates the average similariy.\nOnly categories with reports from both organizations are shown")+scale_y_continuous("Similarity score (cosine)")+
      scale_x_discrete("")+
      theme(axis.text = element_text(size = 10, colour = "black"))+
      scale_fill_manual("CATEGORY", values = pal(unique(dat$cat.doc)))
    
    #alternative plot with histograms instead
    # ggplot(dat, aes(x=simil)) + geom_histogram(aes(y = ..ncount.., colour=cat.doc, fill=cat.doc),alpha=0.5,position="identity")+
    #   facet_grid(.~cat.doc)+
    #   labs(title = "", y="")+ labs(fill='Agreement')+ labs(color='Agreement')

  })
      
  
  #####################################################################
  
  
    
  
}
#library(shinyShortcut)
#shinyShortcut(shinyDirectory = getwd(), OS = .Platform$OS.type, gitIgnore = FALSE)


# Run the application 
shinyApp(ui = ui, server = server)

