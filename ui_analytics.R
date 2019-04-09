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
    plotOutput(outputId = "categoryByTime")
  ),
  fluidRow(
    
     h5("Distribution of report granularities in time"),
     plotOutput(outputId = "granularityByTime")
  )
  
)
