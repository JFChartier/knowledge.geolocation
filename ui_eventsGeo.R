ui_topic<-fluidPage(
  fluidRow(
    
  sidebarLayout(
  
      # Input(s)
      sidebarPanel(width = 2,
                   
                   # Text instructions
                   #HTML("Select topics"),
                   
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
                   # Set minimal Threshold
                   sliderInput(inputId = "minima", 
                               label = "Relevance threshold slider:", 
                               min = 0, max = 1, 
                               value = 0.1, step = 0.05),
                   #br(), br(),
                   sliderInput(inputId = "time",
                               label = "Time slider:",
                               min=min(as.Date(all.data$INCIDENT.DATE)),
                               max=max(as.Date(all.data$INCIDENT.DATE)),
                               value = max(as.Date(all.data$INCIDENT.DATE)),
                               #value = c(min(as.Date(QM.data$INCIDENT.DATE)), max(as.Date(QM.data$INCIDENT.DATE))),
                               step = 1,
                               animate=TRUE)
                   
                      
      ),
      
      # Output(s)
      mainPanel(width = 10,
                tabsetPanel(
                  #tabPanel(title="Summary"
                  #),
                   
                  tabPanel(title="Map of Events",
                           #textOutput("query"),
                           leafletOutput(outputId = "eventMap1",height = 500),
                           # Show data table
                           br(),br(),
                           HTML("<h3><b>Seleted events</b></h3>"),
                           br(),br(),
                           DT::dataTableOutput(outputId = "relevantEvents"),
                           br()
                  ),
                  tabPanel(title="Selected Event Analysis"
                           
                  )
                )
                #h4("Map of Events"),
                #leafletOutput(outputId = "eventMap1",height = 500)
                
      )
    ))
)
