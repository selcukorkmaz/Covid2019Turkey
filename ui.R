library(DT)
library(markdown)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinyjs)



appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"


ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel("COVID-19 Türkiye"),
  
  # setBackgroundColor(color = "ghostwhite"),
  useShinydashboard(),
  
  useShinyjs(),
  inlineCSS(appCSS),
  
  div(
    id = "loading-content",
    h2("Yükleniyor...Lütfen Bekleyiniz...")
  ),
  hidden(
    div(
      id = "app-content",
  sidebarLayout(

    sidebarPanel(
      
      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
      
      
      h4("Türkiye İstatistikleri"),
      selectInput(inputId = "dataset",
                  label = "Veri seçiniz",
                  choices = c("Özet", "Tüm")),
      
      checkboxInput(inputId = "kalmanFilter", label = "Toplam vaka için sonraki gün tahmini ekle", value = FALSE),
      
      checkboxInput(inputId = "hideStats", label = "Türkiye istatistiklerini gizle", value = FALSE),

      # checkboxInput(inputId = "totalDeaths", label = "Ölüm vakalarını ekle", value = TRUE),
      # checkboxInput(inputId = "expModelPlot", label = "Üstel model eğrisi ekle", value = FALSE),
      # checkboxInput(inputId = "addCI", label = "%95 güven aralığı ekle", value = FALSE),
      # checkboxInput(inputId = "expModelSummary", label = "Üstel model sonuçlarını görüntüle", value = FALSE),
      # sliderInput("expTime", "Gün (daha sonraki günler için üstel dağılım model kestirimleri elde edilebilir)", value = 18, min = 1, max = 20, step = 1),

      h4("Dünya İstatistikleri"),
      # checkboxInput(inputId = "compare", label = "Ülkeleri karşılaştır", value = TRUE),

      # conditionalPanel(condition="input.compare",

        # checkboxInput(inputId = "firstCase", label = "Verileri ilk görülen vakadan başlat", value = TRUE),
        selectizeInput("countries", "Ülke seçiniz", choices = NULL, multiple = TRUE),
        checkboxInput(inputId = "highlightCountry", label = "Ülke vurgula", value = TRUE),
        conditionalPanel(condition="input.highlightCountry",

            selectizeInput("highlightCountries", "", choices = NULL, multiple = FALSE)
        ),

  
      
      checkboxInput(inputId = "settings", label = "Ayarlar", value = FALSE),
      conditionalPanel(condition="input.settings",
                       checkboxInput(inputId = "logTransform", label = "Logaritmik dönüşüm uygula", value = TRUE),
                       checkboxInput(inputId = "trajectory", label = "Katlanma doğrularını ekle", value = TRUE),
                      
                       numericInput("filter", "En az X vaka olan ülkeleri göster", value = 100),
                       numericInput("firstCase", "Verileri en az X adet görülen vakadan başlat (toplam vaka grafiği için)", value = 100),
                       numericInput("firstDeath", "Verileri en az X adet ölümden başlat  (toplam ölüm grafiği için)", value = 10),
                       # numericInput("firstRecover", "Verileri en az X adet iyileşmeden başlat  (toplam iyileşme grafiği için)", value = 10),
                       numericInput("weeklyNewCase", "Verileri en az X adet ortalama yeni vakadan başlat  (haftalık vaka grafiği için)", value = 200),
                       numericInput("weeklyDeath", "Verileri en az X adet ortalama yeni ölümden başlat  (haftalık ölüm grafiği için)", value = 20)
                       
                       
      ),
      
      HTML('<a href="https://twitter.com/selcukorkmaz?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-show-count="false">Follow @selcukorkmaz</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
      


    ),

    mainPanel(

      h3(textOutput("caption", container = span)),

      conditionalPanel(condition = "!input.hideStats",
      
        # verbatimTextOutput("dimension"),
        h3(textOutput(outputId = "table1")),
        
        
        conditionalPanel(condition = "input.dataset == 'Özet'",
                         
        uiOutput("totalCase"),
        uiOutput("totalDeath"),
        uiOutput("totalRecovered"),
        uiOutput("totalActiveCases"),
        uiOutput("deathRate"),
        uiOutput("totalTest"),
        uiOutput("entCases"),
        uiOutput("deathRateClosedCases"),
        uiOutput("icuCases"),
        
        uiOutput("totalCaseMillion"),
        uiOutput("totalCaseThousandTest"),
        
        
        uiOutput("totalTesMillion")
        
        
       ),
        
        DT::dataTableOutput("resultTable"),
        # HTML('<br>'),
        # HTML('<br>'),
         htmlOutput("turkeyMap"),
       # htmlOutput("inc"),
       
       plotlyOutput("plotTotalCases", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
       plotlyOutput("plotTotalDeatsRecovered", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        # plotOutput("plotTotalDeats"),
        plotlyOutput("plotTotalActiveCases", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
       plotlyOutput("plotTotalTests", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
       plotlyOutput("totalClosedDeathRate", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("testVsCasePlot", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("plotTotalICU", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("plotTestCaseAdjusted", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("plotTotalTestCaseAdjusted", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
       plotlyOutput("plotTestCaseGrowt", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("barPlotNewCases", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("barPlotNewDeaths", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("barPlotNewRecovered", height = 'auto', width = 'auto'),
       HTML('<br>'),
       HTML('<br>'),
        plotlyOutput("barPlotNewTest", height = 'auto', width = 'auto')
        
      ),
        
        
        h3(textOutput(outputId = "table2")),
        DT::dataTableOutput("countryTable"),
        # tags$head(includeHTML(("worldMap.html"))),
        HTML('<br>'),
        HTML('<br>'),
      
      h3(textOutput(outputId = "worldMapText")),
        htmlOutput("worldMap"),
      HTML('Harita kaynağına erişmek için <a href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6" target="_blank"> tıklayınız.</a>'),
      
      
      HTML('<br>'),
      HTML('<br>'),
      plotlyOutput("worldStatistics", height = 'auto', width = 'auto'),
      HTML('<br>'),
      HTML('<br>'),
      plotlyOutput("worldStatisticsPopAdjusted", height = 'auto', width = 'auto'),
      HTML('<br>'),
      HTML('<br>'),
        plotOutput("compareConfirmed"),
        HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             vaka sayılarının X adet (varsayılan 100) vaka tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. Y ekseni logaritmik veya
             doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. Katlanma doğruları ile birlikte incelendiğinde
             ülkelerin toplam vakalarının zamana bağlı artışları daha iyi karşılaştırılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        
        plotOutput("compareDeaths"),
        HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             ölüm sayılarının X adet (varsayılan 10) ölüm tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. Y ekseni logaritmik veya
             doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. Katlanma doğruları ile birlikte incelendiğinde
             ülkelerin toplam ölümlerinin zamana bağlı artışları daha iyi karşılaştırılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        
        # plotOutput("compareRecovered"),
        # HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
        #      iyileşme sayılarının X adet (varsayılan 10) iyileşme tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. Y ekseni logaritmik veya
        #      doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
        #      incelenmesi önerilmektedir. Katlanma doğruları ile birlikte incelendiğinde
        #      ülkelerin toplam iyileşmelerinin zamana bağlı artışları daha iyi karşılaştırılabilir.</p>'),
        # HTML('<br>'),
        # HTML('<br>'),
        
        
        # plotOutput("testComparisonPlot"),
        # plotOutput("testComparisonPopAdjustedPlot"),
        plotOutput("meanWeeklyNewCases"),
        HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             haftalık ortalama yeni vaka sayılarının haftalık X adet (varsayılan 200) vaka tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. 
             Y ekseni logaritmik veya doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. X eksenindeki her bir gün için kedisinden önceki 1 haftanın hareketli ortalaması hesaplanmıştır. Böylece, ülkelerin
             haftalık ortalama yeni vaka sayıları karşılaştırılarak salgının gidişatı değerlendirilebilir ve ülkeler arası karşılaştırmalar yapılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        plotOutput("meanWeeklyNewDeaths"),
        HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             haftalık ortalama yeni ölüm sayılarının haftalık X adet (varsayılan 20) ölüm tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. 
             Y ekseni logaritmik veya doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. X eksenindeki her bir gün için kedisinden önceki 1 haftanın hareketli ortalaması hesaplanmıştır. Böylece, ülkelerin
             haftalık ortalama yeni ölüm sayıları karşılaştırılarak salgının gidişatı değerlendirilebilir ve ülkeler arası karşılaştırmalar yapılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
      
      
        
        plotOutput("growtRate"),
        HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik ülkelerin ortalama
             haftalık toplam vaka değişiminin X adet (varsayılan 100) vaka tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. 
            X eksenindeki her bir gün için kedisinden önceki 1 haftanın hareketli ortalaması hesaplanmıştır. Bu grafik incelenerek salgının anlık 
             gidişatının hangi yönde olduğu değerlendirilebilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
      
      plotOutput("growthFactor"),
      HTML("<p align= 'justify'><b>Grafik Açıklaması:</b>Bu grafikte y-ekseninde ülkelerin haftalık ortalama büyüme faktörleri
           x-ekseninde ise logaritmik ölçekte toplam vaka sayıları bulunmaktadır. 
           Büyüme faktörü 1'in üzerinde ise günlük yeni vaka sayısının bir önceki günden daha yüksek olduğu, büyüme faktörü 1'in altında ise günlük yeni vaka sayısının bir önceki günden daha düşük olduğu,
           büyüme faktörü 1'e eşit ise günlük yeni vaka sayısının bir önceki güne göre değişmediği anlamına gelir.</p>"),
      
      HTML('<br>'),
      HTML('<br>'),
        
        plotOutput("testComparisonPopTestAdjustedPlot"),
      HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafikte toplam vaka sayıları en az 10 bin olan ülkelerin 1 milyon nüfusta tespit ettikleri vaka sayıları ile
             1 milyon nüfusa yaptıkları test sayıları için saçılım grafiği oluşturulmuştur. Burada test başına tespit edilen 
           vaka sayısı ile salgının toplumdaki düzeyi, 1 milyon nüfusa yapılan test sayısı ile toplumun ne kadarına test yapıldığı
           elde edilmeye çalışılmıştır (R-kare = 0.4018). </p>'),
        HTML('<br>'),
        HTML('<br>'),
        plotlyOutput("testComparisonPopTestAdjustedPlot2", height = 'auto', width = 'auto'),
      HTML('<p align= "justify"><b>Grafik Açıklaması:</b> Bu grafik yukarıda oluşturulan saçılım grafiğinin çubuk grafiği
           olarak oluşturulmuş şeklidir.</p>'),
      HTML('<br>'),
      HTML('<br>'),
        h3(textOutput(outputId = "table3")),
        DT::dataTableOutput("underReportingTable"),

        HTML('<br>'),
        HTML('<br>'),
        plotOutput("underReportingPlot"),
        HTML('<p align= "justify"><b>Grafik Açıklaması:</b><a href = "https://www.lshtm.ac.uk/research/centres/centre-mathematical-modelling-infectious-diseases" target="_blank">The London School of Hygiene & Tropical 
             Medicine’de bulunan Centre for the Mathematical Modeling of Infectious 
             Diseases</a> 
             birimindeki araştırmacılar tarafından gerçekleştirilen bir ön çalışmada (pre-print)* 
             ülkelerin raporladıkları semptomatik vakaların yüzdesi kestirilmeye çalışılmıştır. 
             İlgili çalışmaya <a href="https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html" target="_blank">buradan</a> erişilebilir.</p>'),

        HTML('<br>'),
        HTML('<br>'),
  
        
        HTML('<p align= "justify"><b>Uyarı:</b> Burada oluşturulan grafiklerden ve elde edilen istatistiklerden yapılan çıkarsamalar hesaba katılamayan bir
             çok faktör (ülkelerin sağlık harcamaları, nüfus yoğunlukları, yaş dağılımları, hastane ve yoğun bakım yatak sayıları, sağlık çalışanı sayıları, kültürel farklılıklar
             vb.) nedeniyle eksiktir. Bu faktörlerin hesaba katılarak sonuçların dikkatli yorumlanması önerilmektedir.</p>'),
        
      HTML('<br>'),
      
      HTML('<p align= "justify"><b>*Ön çalışma (Pre-print):</b> Akademik yayıncılıkta, hakemli bir bilimsel
           dergide resmi akran değerlendirmesi (peer-review) yapılmadan yayınlanan
           bilimsel makalenin bir versiyonudur.</p>'),
      
        # verbatimTextOutput('summaryModel'),

        # WHERE YOUR FOOTER GOES
        hr(),
        HTML('<p align= "justify"><b>Kaynak kodlar: <a href = "https://github.com/selcukorkmaz/Covid2019Turkey" target="_blank">GitHub</a></b></p>'),
        HTML('<p align= "justify"><b>Türkiye verileri son güncelleme: 01.05.2020, 19:30</b></p>'),
        HTML("<p><b>Türkiye verileri <a href = 'https://covid19.saglik.gov.tr' target='_blank'>T.C. Sağlık Bakanlığı</a>'ndan alınmaktadır.</b></p>"),
        HTML('<p align= "justify"><b>Dünya verileri <a href = "https://github.com/CSSEGISandData/COVID-19" target="_blank">JHU CSSE</a> veritabanından alınmaktadır.</b></p>'),
        HTML("<p><b>Katlanma grafikleri <a href = 'https://twitter.com/jburnmurdoch' target='_blank'>John Burn-Murdoch</a>'dan uyarlanmıştır.</b></p>"),
        HTML("<p><b>Ülkelerin test sayısı ve vaka sayısı karşılaştırma grafiği 27 Nisan-1 Mayıs 2020 tarihleri arasında <a href = 'https://en.wikipedia.org/wiki/COVID-19_testing' target='_blank'> paylaşılan veriler</a> kullanılarak oluşturulmuştur.</b></p>"),

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

        HTML('<p align= "justify"><b>Yararlı Linkler:</b></p>'),
        HTML('<a href="https://covidtracking.com/" target="_blank">The COVID Tracking Project</a></p>'),
        HTML('<a href="http://www.healthdata.org/" target="_blank">Institute for Health Metrics and Evaluation</a></p>'),
        HTML('<a href="https://www.cdc.gov/" target="_blank">Centers for Disease Control and Prevention</a></p>'),
        HTML('<a href="https://www.ecdc.europa.eu/en" target="_blank">European Centre for Disease Prevention and Control</a></p>'),
      
        HTML('<p align= "justify"><b>Hata ve öneri bildirimlerinizi <a href = "mailto: selcukorkmaz@gmail.com">selcukorkmaz@gmail.com</a> adresine gönderebilirsiniz.</b></p>')



    ))


    )
  )
)
