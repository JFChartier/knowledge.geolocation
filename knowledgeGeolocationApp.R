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
QM.data = readRDS("QM.data.rds")

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
           ),
           
           tabPanel("Knowledge geolocation"#,
                    #source("ui_knowledgeGeo.R", local = T, encoding = 'UTF-8')$value
           ),
           tabPanel("Knowledge change"#,
                    #source("ui_knowledgeChange.R", local = T, encoding = 'UTF-8')$value
           )
           
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
  #install.packages("scales")
  
  
  #####################################
  #load data
  QM.data = readRDS("QM.data.rds")
  
  myColors = c("black","blue","red","green","brown","pink","orange", "yellow", "lightblue", "gray")
  #myColors = topo.colors(10)
  pal <- leaflet::colorFactor(palette = myColors,
                              levels = unique(QM.data$CATEGORY))
  
  
  
  ########################################
  #render event geolocation
  
  #category render
  category_select1<-reactive({
      #QM.data
     QM.data %>% filter(CATEGORY %in% input$category) #%>%select(c(1:3))
    })
  
  
  output$eventMap1 <- renderLeaflet(
    {
      map <- category_select1()%>%
        leaflet() %>%
        addProviderTiles("CartoDB") %>%
        # Use dc_hq to add the hq column as popups
        addCircleMarkers(lng = ~LONGITUDE, 
                         lat = ~LATITUDE, 
                         popup = category_select1()$LOCATION, 
                         radius=2,
                         #the fuck is here
                         color=~pal(category_select1()$CATEGORY),
                         label=~INCIDENT.TITLE,
                         fill=T,
                         opacity = .9) %>%
        addLegend(position = "topright",
                pal = pal,
                values = unique(category_select1()$CATEGORY))
      map
    })
  
  
}
#library(shinyShortcut)
#shinyShortcut(shinyDirectory = getwd(), OS = .Platform$OS.type, gitIgnore = FALSE)


# Run the application 
shinyApp(ui = ui, server = server)

