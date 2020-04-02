library(DT)
library(markdown)

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel("COVID-19 Türkiye"),

  sidebarLayout(

    sidebarPanel(
      h4("Türkiye İstatistikleri"),
      selectInput(inputId = "dataset",
                  label = "Veri seçiniz",
                  choices = c("Özet", "Tüm")),
      
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
                       numericInput("firstRecover", "Verileri en az X adet iyileşmeden başlat  (toplam iyileşme grafiği için)", value = 10),
                       numericInput("weeklyNewCase", "Verileri en az X adet ortalama yeni vakadan başlat  (haftalık vaka grafiği için)", value = 200),
                       numericInput("weeklyDeath", "Verileri en az X adet ortalama yeni ölümden başlat  (haftalık ölüm grafiği için)", value = 20)
                       
                       
      )
      


    ),

    mainPanel(

      # h3(textOutput("caption", container = span)),

      conditionalPanel(condition = "!input.hideStats",
      
        
        h4(textOutput(outputId = "table1")),
        DT::dataTableOutput("resultTable"),
        HTML('<br>'),
        HTML('<br>'),
        htmlOutput("frame"),
        plotOutput("plotTotalCases"),
        plotOutput("plotTotalDeatsRecovered"),
        # plotOutput("plotTotalDeats"),
        plotOutput("plotTotalTests"),
        plotOutput("testVsCasePlot"),
        plotOutput("plotTotalICU"),
        plotOutput("plotTestCaseAdjusted"),
        plotOutput("barPlotNewCases"),
        plotOutput("barPlotNewDeaths"),
        plotOutput("barPlotNewRecovered"),
        plotOutput("barPlotNewTest")
        
      ),
        
        
        h4(textOutput(outputId = "table2")),
        DT::dataTableOutput("countryTable"),
        HTML('<br>'),
        HTML('<br>'),

        plotOutput("compareConfirmed"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             vaka sayılarının X adet (varsayılan 100) vaka tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. Y ekseni logaritmik veya
             doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. Katlanma doğruları ile birlikte incelendiğinde
             ülkelerin toplam vakalarının zamana bağlı artışları daha iyi karşılaştırılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        
        plotOutput("compareDeaths"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             ölüm sayılarının X adet (varsayılan 10) ölüm tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. Y ekseni logaritmik veya
             doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. Katlanma doğruları ile birlikte incelendiğinde
             ülkelerin toplam ölümlerinin zamana bağlı artışları daha iyi karşılaştırılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        
        plotOutput("compareRecovered"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             iyileşme sayılarının X adet (varsayılan 10) iyileşme tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. Y ekseni logaritmik veya
             doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. Katlanma doğruları ile birlikte incelendiğinde
             ülkelerin toplam iyileşmelerinin zamana bağlı artışları daha iyi karşılaştırılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        
        # plotOutput("testComparisonPlot"),
        # plotOutput("testComparisonPopAdjustedPlot"),
        plotOutput("meanWeeklyNewCases"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             haftalık ortalama yeni vaka sayılarının haftalık X adet (varsayılan 200) vaka tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. 
             Y ekseni logaritmik veya doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. X eksenindeki her bir gün için kedisinden önceki 1 haftanın hareketli ortalaması hesaplanmıştır. Böylece, ülkelerin
             haftalık ortalama yeni vaka sayıları karşılaştırılarak salgının gidişatı değerlendirilebilir ve ülkeler arası karşılaştırmalar yapılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        plotOutput("meanWeeklyNewDeaths"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafik ülkelerin resmi
             haftalık ortalama yeni ölüm sayılarının haftalık X adet (varsayılan 20) ölüm tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. 
             Y ekseni logaritmik veya doğrusal ölçekte çizilebilmektedir. Gidişatın daha iyi anlaşılabilmesi adına grafiğin logaritmik ölçekte 
             incelenmesi önerilmektedir. X eksenindeki her bir gün için kedisinden önceki 1 haftanın hareketli ortalaması hesaplanmıştır. Böylece, ülkelerin
             haftalık ortalama yeni ölüm sayıları karşılaştırılarak salgının gidişatı değerlendirilebilir ve ülkeler arası karşılaştırmalar yapılabilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        plotOutput("growtRate"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafik ülkelerin ortalama
             haftalık toplam vaka değişiminin X adet (varsayılan 100) vaka tespit edildikten sonraki geçen zamana bağlı değişimini göstermektedir. 
            X eksenindeki her bir gün için kedisinden önceki 1 haftanın hareketli ortalaması hesaplanmıştır. Bu grafik incelenerek salgının anlık 
             gidişatının hangi yönde olduğu değerlendirilebilir.</p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        plotOutput("testComparisonPopTestAdjustedPlot"),
        HTML('<p><b>Grafik Açıklaması:</b> Bu grafikte ülkelerin bin test başına tespit ettikleri vaka sayıları ile
             1 milyon nüfusa yaptıkları test sayıları ile saçılım grafiği oluşturulmuştur. Burada test başına tespit edilen 
             vaka sayısı ile salgının toplumdaki düzeyi, 1 milyon nüfusa yapılan test sayısı ile toplumun ne kadarına test yapıldığı
             elde edilmeye çalışılmıştır. </p>'),
        HTML('<br>'),
        HTML('<br>'),
        
        HTML('<p><b>Uyarı:</b> Burada oluşturulan grafiklerden ve elde edilen istatistiklerden yapılan çıkarsamalar hesaba katılamayan bir
             çok faktör (ülkelerin sağlık harcamaları, nüfus yoğunlukları, yaş dağılımları, hastane ve yoğun bakım yatak sayıları, sağlık çalışanı sayıları, kültürel farklılıklar
             vb.) nedeniyle eksiktir. Bu faktörlerin hesaba katılarak sonuçların dikkatli yorumlanması önerilmektedir.</p>'),
        
        # verbatimTextOutput('summaryModel'),

        # WHERE YOUR FOOTER GOES
        hr(),
        HTML('<p><b>Kaynak kodlar: <a href = "https://github.com/selcukorkmaz/Covid2019Turkey" target="_blank">GitHub</a></b></p>'),
        HTML('<p><b>Türkiye verileri son güncelleme: 01.04.2020, 20:00</b></p>'),
        HTML("<p><b>Türkiye verileri <a href = 'https://covid19.saglik.gov.tr' target='_blank'>T.C. Sağlık Bakanlığı</a>'ndan alınmaktadır.</b></p>"),
        HTML('<p><b>Dünya verileri <a href = "https://github.com/CSSEGISandData/COVID-19" target="_blank">JHU CSSE</a> veritabanından alınmaktadır.</b></p>'),
        HTML("<p><b>Katlanma grafikleri <a href = 'https://twitter.com/jburnmurdoch' target='_blank'>John Burn-Murdoch</a>'dan uyarlanmıştır.</b></p>"),
        HTML("<p><b>Ülkelerin test sayısı ve vaka sayısı karşılaştırma grafiği 29-31 Mart 2020 tarihleri arasında <a href = 'https://en.wikipedia.org/wiki/COVID-19_testing' target='_blank'> paylaşılan veriler</a> kullanılarak oluşturulmuştur.</b></p>"),

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
