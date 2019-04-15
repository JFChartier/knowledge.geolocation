ui_topic<-fluidPage(
  fluidRow(
    
  sidebarLayout(
  
      # Input(s)
      sidebarPanel(width = 3,
                   
                   # Text instructions
                   #HTML("Select topics"),
                   
                   selectInput(inputId = "map",
                               label="Map:",
                               choices = c("OpenStreetMap", "CartoDB", "Stamen.TerrainBackground"),
                               multiple = F,
                               selected="CartoDB"),
                   numericInput(inputId = "lalitude",
                                label = "Latitude",
                                value = mean(all.data$LATITUDE),
                                min=min(all.data$LATITUDE),
                                max=max(all.data$LATITUDE)),
                   numericInput(inputId = "longitude",
                                label = "longitude",
                                value= mean(all.data$LONGITUDE),
                                min=min(all.data$LONGITUDE),
                                max=max(all.data$LONGITUDE)),
                   
                   selectInput(inputId = "organization",
                               label = "Organization:",
                               choices = unique(all.data$organization),
                               multiple= T,
                               selected=all.data$organization),
                   selectInput(inputId = "category",
                               label = "Event category:",
                               choices = unique(all.data$CATEGORY),
                               multiple= T,
                               selected=all.data$CATEGORY),
                   
                   selectInput(inputId = "granularity",
                               label = "Event granularity:",
                               choices = unique(all.data$GRANULARITY),
                               multiple= T,
                               selected=all.data$GRANULARITY),
                   #HTML("Select topics"),
                   selectInput(inputId = "query",
                               label = "Keyword(s) in event description:",
                               choices = mySVD$original.features, # a vector containing all available keywords
                               multiple= T),
                   
                   # Action button to show
                   actionButton(inputId = "button", 
                                label = ("Run Search")),
                   br(),br(),
                   # Set minimal Threshold
                   sliderInput(inputId = "minima", 
                               label = "Relevance threshold slider:", 
                               min = 0, max = 1, 
                               value = 0, step = 0.05),
                   sliderInput(inputId = "time",
                               label = "Time slider:",
                               min=min(as.Date(all.data$INCIDENT.DATE)),
                               max=max(as.Date(all.data$INCIDENT.DATE)),
                               value = max(as.Date(all.data$INCIDENT.DATE)),
                               #value = c(min(as.Date(QM.data$INCIDENT.DATE)), max(as.Date(QM.data$INCIDENT.DATE))),
                               step = 1,
                               animate=TRUE,
                               width="100%")
                   #br(), br(),
                   
      ),
      
      # Output(s)
      mainPanel(width = 9,
                tabsetPanel(
                  #tabPanel(title="Summary"
                  #),
                   
                  tabPanel(title="Map of Selected Reports",
                           #HTML("<h4><b>Map of selected events</b></h3>"),
                           #textOutput("query"),
                           leafletOutput(outputId = "eventMap1",height = 500),
                           # Show data table
                           #br(),
                           
                           br(),
                           #HTML("<h4><b>Table of seleted events</b></h3>"),
                           DT::dataTableOutput(outputId = "selectReport")
                           #br()
                  ),
                  tabPanel(title="Table of Selected Reports",
                           DT::dataTableOutput(outputId = "relevantEvents")
                           
                  ),
                  tabPanel(title="Analytic of Selected Reports",
                           source("ui_analytics.R", local = T, encoding = 'UTF-8')$value
                           
                  )
                )
                #h4("Map of Events"),
                #leafletOutput(outputId = "eventMap1",height = 500)
                
      )
    ))
)
