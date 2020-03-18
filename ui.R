library(DT)

ui <- fluidPage(
  
  titlePanel("COVID-19 Türkiye"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Veri seçin:",
                  choices = c("Özet", "Tüm")),
      
      # sliderInput("expTime", "Gün", value = 7, min = 1, max = 15, step = 1),
      
      checkboxInput(inputId = "expModelPlot", label = "Üstel model eğrisi ekle", value = TRUE),
      checkboxInput(inputId = "expModelSummary", label = "Üstel model sonuçlarını görüntüle", value = FALSE)
      
    ),
    
    mainPanel(
      
      h3(textOutput("caption", container = span)),
      
        DT::dataTableOutput("resultTable"),
        plotOutput("plotTotalCases"),
        plotOutput("logPlotTotalCases"),
        # plotOutput("plotTotalDeats"),
        plotOutput("barPlotNewCases"),
        plotOutput("barPlotNewDeaths"),
        verbatimTextOutput('summaryModel')

      

    )
  )
)