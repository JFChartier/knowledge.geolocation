ui_analytics<-fluidPage(
  fluidRow(
    column(6,
           h5("Distribution of reports by organization"),
           plotOutput(outputId = "organizationReports")),
    column(6,
           h5("Relevance of reports for the query"),
           plotOutput(outputId = "relevantReports"))
    
  ),
  
  fluidRow(
    column(6,
           h5("Distribution of reports by category"),
           plotOutput(outputId = "reportsByCategory")),
    column(6,
           h5("Distribution of reports by event granularity"),
           plotOutput(outputId = "reportsByGranularity"))
    
  ),
  fluidRow(
    h5("Distribution of report categories in time"),
    #column(4,
           #h5("Distribution of reports by category"),
           #plotOutput(outputId = "categoryByTime")),
    #column(8,
           #h5("Distribution of reports by event granularity"),
           #plotOutput(outputId = "categoryByTime2"))
    
    #h5("Distribution of report categories in time"),
    #plotOutput(outputId = "categoryByTime"),
    plotOutput(outputId = "categoryByTime2")
    
  ),
  fluidRow(
    
     h5("Distribution of report granularities in time"),
     #plotOutput(outputId = "granularityByTime")
     plotOutput(outputId = "granularityByTime2")
  )
  
)
