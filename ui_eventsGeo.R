library(d3heatmap)
ui_topic<-fluidPage(
  fluidRow(
    
  sidebarLayout(
  
      # Input(s)
      sidebarPanel(width = 2,
                   
                   # Text instructions
                   #HTML("Select topics"),
                   
                   # Select variable for y-axis
                   selectInput(inputId = "category",
                               label = "Select category:",
                               choices = unique(QM.data$CATEGORY),
                               multiple= T,
                               selected=QM.data$CATEGORY),
                   
                   selectInput(inputId = "granularity",
                               label = "Select granularity:",
                               choices = unique(QM.data$GRANULARITY),
                               multiple= T,
                               selected=QM.data$GRANULARITY),
                   sliderInput(inputId = "time",
                               label = "Time Slider:",
                               min=min(as.Date(QM.data$INCIDENT.DATE)),
                               max=max(as.Date(QM.data$INCIDENT.DATE)),
                               value = max(as.Date(QM.data$INCIDENT.DATE)),
                               #value = c(min(as.Date(QM.data$INCIDENT.DATE)), max(as.Date(QM.data$INCIDENT.DATE))),
                               step = 1,
                               animate=TRUE)
                   
                      
      ),
      
      # Output(s)
      mainPanel(width = 10,
                h4("Explore events by category"),
                leafletOutput(outputId = "eventMap1",height = 500)
      )
    ))
)