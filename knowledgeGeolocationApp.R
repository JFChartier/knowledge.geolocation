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
############
QM.data = readRDS("QM.data.rds")

ui<-navbarPage(theme = shinytheme("paper"), inverse=F, windowTitle= "RegCan", title = "Dashboard RegCan: ",
               
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
  
  
  #####################################
  #load data
  QM.data = readRDS("QM.data.rds")
  
  myColors = c("black","blue","red","green","white","gray","orange", "yellow", "ligthblue", "pink")
  pal <- colorFactor(palette = myColors,
                     levels = unique(QM.data$CATEGORY))
  
  
  
  ########################################
  #render event geolocation
  
  #category render
  category_select1<-reactive({
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
                         #popup = category_select1()$LOCATION, 
                         radius=1,
                         #the fuck is here
                         color=~pal(CATEGORY),
                         label=~INCIDENT.TITLE) #%>%
        #addLegend(position = "bottomright",
                #pal = pal,
                #values = unique(QM.data$CATEGORY))
      map
    })
  
  
}
#library(shinyShortcut)
#shinyShortcut(shinyDirectory = getwd(), OS = .Platform$OS.type, gitIgnore = FALSE)


# Run the application 
shinyApp(ui = ui, server = server)

