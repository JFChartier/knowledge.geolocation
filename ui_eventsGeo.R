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
                               selected=QM.data$CATEGORY[1])
                   
                      
      ),
      
      # Output(s)
      mainPanel(width = 10,
                h4("Explore events by category"),
                leafletOutput(outputId = "eventMap1",height = 1000)
      )
    ))
)