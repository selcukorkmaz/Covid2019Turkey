library(DT)

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel("COVID-19 Türkiye"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Türkiye İstatistikleri"),
      selectInput(inputId = "dataset",
                  label = "Veri seçiniz",
                  choices = c("Özet", "Tüm")),
      
      # checkboxInput(inputId = "totalDeaths", label = "Ölüm vakalarını ekle", value = TRUE),
      checkboxInput(inputId = "expModelPlot", label = "Üstel model eğrisi ekle", value = FALSE),
      # checkboxInput(inputId = "addCI", label = "%95 güven aralığı ekle", value = FALSE),
      checkboxInput(inputId = "expModelSummary", label = "Üstel model sonuçlarını görüntüle", value = FALSE),
      # sliderInput("expTime", "Gün (daha sonraki günler için üstel dağılım model kestirimleri elde edilebilir)", value = 18, min = 1, max = 20, step = 1),
      
      h4("Dünya İstatistikleri"),
      # checkboxInput(inputId = "compare", label = "Ülkeleri karşılaştır", value = TRUE),
      
      # conditionalPanel(condition="input.compare",

        # checkboxInput(inputId = "firstCase", label = "Verileri ilk görülen vakadan başlat", value = TRUE),
        selectizeInput("countries", "Ülke seçiniz", choices = NULL, multiple = TRUE),
        checkboxInput(inputId = "logTransform", label = "Logaritmik dönüşüm uygula", value = TRUE),
        checkboxInput(inputId = "trajectory", label = "Katlanma doğrularını ekle", value = TRUE),
        numericInput("filter", "En az X vaka olan ülkeleri göster", value = 100),
        numericInput("firstCase", "Verileri en az X adet görülen vakadan başlat (toplam vaka grafiği için)", value = 100),
        numericInput("firstDeath", "Verileri en az X adet ölümden başlat  (toplam ölüm grafiği için)", value = 10),
        numericInput("firstRecover", "Verileri en az X adet iyileşmeden başlat  (toplam iyileşme grafiği için)", value = 5)
        # checkboxInput(inputId = "population", label = "Nüfusa göre düzeltme uygula", value = FALSE),
        
        

      # ),
      
      
      
      # tags$div(id = "main", style = "width: 50%"),
      
     

      
   
      
    ),
    
    mainPanel(
      
      # h3(textOutput("caption", container = span)),
      
        h4(textOutput(outputId = "table1")),
        DT::dataTableOutput("resultTable"),
        plotOutput("plotTotalCases"),
        plotOutput("plotTotalDeatsRecovered"),
        # plotOutput("plotTotalDeats"),
        plotOutput("plotTotalTests"),
        plotOutput("testVsCasePlot"),
        plotOutput("plotTotalICU"),
        plotOutput("barPlotNewCases"),
        plotOutput("barPlotNewDeaths"),
        plotOutput("barPlotNewRecovered"),
        h4(textOutput(outputId = "table2")),
        DT::dataTableOutput("countryTable"),
        HTML('<br>'),
        HTML('<br>'),
        
        plotOutput("compareConfirmed"),
        plotOutput("compareDeaths"),
        plotOutput("compareRecovered"),
        # plotOutput("testComparisonPlot"),
        # plotOutput("testComparisonPopAdjustedPlot"),
        plotOutput("testComparisonPopTestAdjustedPlot"),
        verbatimTextOutput('summaryModel'),
        
        # WHERE YOUR FOOTER GOES
        hr(),
        HTML('<p><b>Kaynak kodlar: <a href = "https://github.com/selcukorkmaz/Covid2019Turkey" target="_blank">GitHub</a></b></p>'),
        HTML('<p><b>Türkiye verileri son güncelleme: 30.03.2020, 20:00</b></p>'),
        HTML("<p><b>Türkiye verileri <a href = 'https://covid19.saglik.gov.tr' target='_blank'>T.C. Sağlık Bakanlığı</a>'ndan alınmaktadır.</b></p>"),
        HTML('<p><b>Dünya verileri <a href = "https://github.com/CSSEGISandData/COVID-19" target="_blank">JHU CSSE</a> veritabanından alınmaktadır.</b></p>'),
        HTML("<p><b>Katlanma grafikleri <a href = 'https://twitter.com/jburnmurdoch' target='_blank'>John Burn-Murdoch</a>'dan uyarlanmıştır.</b></p>"),
        HTML("<p><b>Ülkelerin test sayısı ve vaka sayısı karşılaştırma grafiği 28-30 Mart 2020 arasında <a href = 'https://en.wikipedia.org/wiki/COVID-19_testing' target='_blank'> paylaşılan veriler</a> kullanılarak oluşturulmuştur.</b></p>"),
        
        HTML('<b>COVID-19 yayınları:<b>'),
        HTML('<a href="https://www.nejm.org/coronavirus" target="_blank">NEJM</a>'),
        HTML('<a href="https://www.thelancet.com/coronavirus" target="_blank">Lancet</a>'),
        HTML('<a href="https://www.nature.com/collections/hajgidghjb" target="_blank">Nature</a>'),
        HTML('<a href="https://www.bmj.com/coronavirus" target="_blank">BMJ</a>'),
        HTML('<a href="https://jamanetwork.com/journals/jama/pages/coronavirus-alert" target="_blank">JAMA</a>'),
        HTML('<a href="https://www.cell.com/2019-nCOV" target="_blank">Cell</a>'),
        HTML('<a href="https://academic.oup.com/journals/pages/coronavirus" target="_blank">Oxford</a>'),
        HTML('<a href="https://www.elsevier.com/connect/coronavirus-information-center" target="_blank">Elsevier</a>'),
        HTML('<a href="https://novel-coronavirus.onlinelibrary.wiley.com/" target="_blank">Wiley</a>'),
        HTML('<a href="https://www.scientificamerican.com/tag/The+Coronavirus+Outbreak/" target="_blank">SciAm</a></p>'),

        HTML('<p><b>Hata ve öneri bildirimlerinizi <a href = "mailto: selcukorkmaz@gmail.com">selcukorkmaz@gmail.com</a> adresine gönderebilirsiniz.</b></p>')
        
        
        

      

    )
  )
)