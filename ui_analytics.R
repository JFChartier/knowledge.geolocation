ui_analytics<-fluidPage(
  fluidRow(
    column(6,
           h5("Distribution of reports by organization"),
           plotOutput(outputId = "organizationReports", height = "300px")),
    column(6,
           h5("Relevance of reports for the query"),
           plotOutput(outputId = "relevantReports", height = "300px"))
  ),
  fluidRow(
    column(6,
           h5("Distribution of reports by category"),
           plotOutput(outputId = "reportsByCategory", height = "300px")),
    column(6,
           h5("Distribution of reports in time"),
           plotOutput(outputId = "reportsByTime", height = "300px"))

  ),
  fluidRow(
    
    column(6,
           h5("Distribution of reports by event granularity"),
           plotOutput(outputId = "reportsByGranularity", height = "300px"))#,
    # column(4,
    #        h5("Distribution of reports by event granularity"),
    #        plotOutput(outputId = "reportsByGranularity", height = "300px"))
    # 
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
    plotOutput(outputId = "categoryByTime2", height = "300px")
    
  ),
  fluidRow(
    
     h5("Distribution of report granularities in time"),
     #plotOutput(outputId = "granularityByTime")
     plotOutput(outputId = "granularityByTime2", height = "300px")
  ),
  fluidRow(
    
    h5("Lexical specificities of organizations"),
    #plotOutput(outputId = "keynessplot")
    plotOutput(outputId = "keynessplot2", height = "600px")
  )
  
)
