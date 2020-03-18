library(DT)

ui <- fluidPage(
  
  titlePanel("COVID-19 Türkiye"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Veri seçin:",
                  choices = c("Özet", "Tüm")),
      
      
      
      checkboxInput(inputId = "expModelPlot", label = "Üstel model eğrisi ekle", value = TRUE),
      checkboxInput(inputId = "addCI", label = "%95 güven aralığı ekle", value = FALSE),
      checkboxInput(inputId = "expModelSummary", label = "Üstel model sonuçlarını görüntüle", value = FALSE),
      sliderInput("expTime", "Gün (daha sonraki günler için üstel dağılım model kestirimleri elde edilebilir)", value = 8, min = 1, max = 10, step = 1)
      
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