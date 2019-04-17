ui_topic<-fluidPage(
  fluidRow(
    
  sidebarLayout(
  
      # Input(s)
      sidebarPanel(width = 3,
                   
                   # Text instructions
                   #HTML("Select topics"),
                   wellPanel(
                     h5("Map features"),
                     selectInput(inputId = "map",
                                 label="Map:",
                                 choices = c("OpenStreetMap", "CartoDB", "Stamen.TerrainBackground"),
                                 multiple = F,
                                 selected="CartoDB"),
                     # Show data table
                     checkboxInput(inputId = "show_frame_param",
                                   label = "Show zooming parameters",
                                   value = F),
                     conditionalPanel(condition="input.show_frame_param == 1",
                                      sliderInput(inputId = "zoom",
                                                  label = "Zoom slider:",
                                                  min=1,
                                                  max=15,
                                                  value = c(5, 15),
                                                  step = 1,
                                                  width="100%"),
                                      numericInput(inputId = "lalitude",
                                                   label = "Latitude",
                                                   value = round(mean(all.data$LATITUDE, na.rm = T), digits = 2),
                                                   min=round(min(all.data$LATITUDE, na.rm = T), digits = 2),
                                                   max=round(max(all.data$LATITUDE, na.rm = T), digits = 2),
                                                   step=.1),
                                      numericInput(inputId = "longitude",
                                                   label = "Longitude",
                                                   value= round(mean(all.data$LONGITUDE, na.rm = T), digits = 2),
                                                   min=round(min(all.data$LONGITUDE, na.rm = T), digits = 2),
                                                   max=round(max(all.data$LONGITUDE, na.rm = T), digits = 2),
                                                   step=.1)
                                      
                     )
                   #   sliderInput(inputId = "zoom",
                   #               label = "Zoom slider:",
                   #               min=1,
                   #               max=15,
                   #               value = c(5, 15),
                   #               step = 1,
                   #               width="100%"),
                   #   numericInput(inputId = "lalitude",
                   #                label = "Latitude",
                   #                value = mean(all.data$LATITUDE, na.rm = T),
                   #                min=min(all.data$LATITUDE, na.rm = T),
                   #                max=max(all.data$LATITUDE, na.rm = T),
                   #                step=.5),
                   #   numericInput(inputId = "longitude",
                   #                label = "Longitude",
                   #                value= mean(all.data$LONGITUDE, na.rm = T),
                   #                min=min(all.data$LONGITUDE, na.rm = T),
                   #                max=max(all.data$LONGITUDE, na.rm = T),
                   #                step=.5)
                   ),
                   wellPanel(
                     h5("Events selection parameters"),
                   
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
                     sliderInput(inputId = "time",
                                 label = "Time slider:",
                                 min=min(as.Date(all.data$INCIDENT.DATE)),
                                 max=max(as.Date(all.data$INCIDENT.DATE)),
                                 value = max(as.Date(all.data$INCIDENT.DATE)),
                                 #value = c(min(as.Date(QM.data$INCIDENT.DATE)), max(as.Date(QM.data$INCIDENT.DATE))),
                                 step = 1,
                                 animate=TRUE,
                                 width="100%")
                     ),
                   wellPanel(
                     
                      h5("Semantic search engine"),
                   
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
                                 value = 0, step = 0.05)
                   )
      ),
      
      # Output(s)
      mainPanel(width = 9,
                tabsetPanel(
                  #tabPanel(title="Summary"
                  #),
                   
                  tabPanel(title="Map",
                           #HTML("<h4><b>Map of selected events</b></h3>"),
                           #textOutput("query"),
                           leafletOutput(outputId = "eventMap1",height = 600)
                           # Show data table
                           #br(),
                           
                           #showing a table of a selected point in the map
                           #br(),
                           #HTML("<h4><b>Table of seleted events</b></h3>"),
                           #DT::dataTableOutput(outputId = "selectReport")
                           #br()
                  ),
                  tabPanel(title="Table",
                           DT::dataTableOutput(outputId = "relevantEvents")
                           
                  ),
                  tabPanel(title="Analytics",
                           source("ui_analytics.R", local = T, encoding = 'UTF-8')$value
                           
                  )
                )
                #h4("Map of Events"),
                #leafletOutput(outputId = "eventMap1",height = 500)
                
      )
    ))
)

