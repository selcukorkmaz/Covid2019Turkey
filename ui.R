library(DT)

ui <- fluidPage(
  
  titlePanel("COVID-19 Türkiye"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Veri seçin:",
                  choices = c("Özet", "Tüm")),
      
      
      checkboxInput(inputId = "totalDeaths", label = "Ölüm vakalarını ekle", value = TRUE),
      checkboxInput(inputId = "expModelPlot", label = "Üstel model eğrisi ekle", value = FALSE),
      checkboxInput(inputId = "addCI", label = "%95 güven aralığı ekle", value = FALSE),
      checkboxInput(inputId = "expModelSummary", label = "Üstel model sonuçlarını görüntüle", value = FALSE),
      sliderInput("expTime", "Gün (daha sonraki günler için üstel dağılım model kestirimleri elde edilebilir)", value = 13, min = 1, max = 15, step = 1),
      checkboxInput(inputId = "compare", label = "Ülkeleri karşılaştır", value = FALSE),
      
      conditionalPanel(condition="input.compare",

        selectizeInput("countries", "Ülkeleri seçiniz", choices = NULL, multiple = TRUE)

      ),
      
      HTML('<p><b>COVID-19 yayınları:<b></p>'),
      
      # tags$div(id = "main", style = "width: 50%"),
      
      HTML('<a href="https://www.nejm.org/coronavirus">NEJM</a>'),
      HTML('<a href="https://www.thelancet.com/coronavirus">Lancet</a>'),
      HTML('<a href="https://www.nature.com/collections/hajgidghjb">Nature</a>'),
      HTML('<a href="https://www.bmj.com/coronavirus">BMJ</a>'),
      HTML('<a href="https://jamanetwork.com/journals/jama/pages/coronavirus-alert">JAMA</a>'),
      HTML('<a href="https://www.cell.com/2019-nCOV">Cell</a>'),
      HTML('<a href="https://academic.oup.com/journals/pages/coronavirus">Oxford</a>'),
      HTML('<a href="https://www.elsevier.com/connect/coronavirus-information-center">Elsevier</a>'),
      HTML('<a href="https://novel-coronavirus.onlinelibrary.wiley.com/">Wiley</a>'),
      HTML('<a href="https://www.scientificamerican.com/tag/The+Coronavirus+Outbreak/">SciAm</a>'),
      
      HTML('<br>'),
      HTML('<br>'),
      
      HTML('<p>Son veri güncelleme: 22.03.2020, 22:15</p>')
        
 
      
    ),
    
    mainPanel(
      
      # h3(textOutput("caption", container = span)),
      
        DT::dataTableOutput("resultTable"),
        plotOutput("plotTotalCases"),
        plotOutput("logPlotTotalCases"),
        # plotOutput("plotTotalDeats"),
        plotOutput("barPlotNewCases"),
        plotOutput("barPlotNewDeaths"),
        plotOutput("compareConfirmed"),
        plotOutput("compareDeaths"),
        plotOutput("compareRecovered"),
        verbatimTextOutput('summaryModel')

      

    )
  )
)