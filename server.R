library(DT)

server <- function(input, output) {


  #### Veri yükleme ####
  dataset <- reactive({
    
    data <- read.table("www/data/covid_cases.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Tarih", "Toplam Vaka", "Yeni Vaka", "Toplam Ölüm", "Yeni Ölüm", "Time")
    return(data)
    
  })
  
  summaryData <- reactive({
    
    data <- read.table("www/data/summary.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Toplam Vaka", "Toplam Ölüm", "Aktif Vaka", "Ölüm Oranı (%)", "Toplam Vaka/1M Nüfus")
    return(data)
    
  })
  
  
  
  #### Tablo ####
  result <- reactive({
    
    if(input$dataset == 'Özet'){
      
      res = summaryData()
      
    }
    
    if(input$dataset == 'Tüm'){
      
      res <- dataset()[-ncol(dataset())]

    }
    
    return(res)
    
  })
  
  output$resultTable <- DT::renderDataTable({
    
    
    datatable(result(), extensions = c('Buttons','KeyTable', 'Responsive'), rownames= FALSE,options = list(pageLength = 100, 
      info = FALSE,bFilter = FALSE, paging = FALSE, dom = 'Bfrtip',buttons = list(list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),             
                                                          text = 'İndir')), keys = TRUE
    ))

  })
  
  
  #### Üstel model ####
  
  exponentialModel <- reactive({
    
    data = dataset()
    
    y = dataset()[,"Toplam Vaka"]
    x = dataset()[,"Time"]
    
    expmodel <- lm(log(y)~ x)
    
    return(expmodel)
    
  })
    
  output$summaryModel <- renderPrint({
    
    if(input$expModelSummary){
    
    summary(exponentialModel())
    
    }
  })
  
  #### Grafik ####
  
  ##### Toplam Vaka Grafiği ####
  
  output$plotTotalCases <- renderPlot({
    
    if(!input$expModelPlot){

    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(dataset()[,"Toplam Vaka"]))
    legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"])
    
    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Vaka", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Türkiye'deki Toplam Koronavirüs Vakaları")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Vaka"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")

    
    legend(1, legendPosition, legend=c("Vaka"),
           col=c("blue"), lty=1)
    
    if(input$totalDeaths){
    
        lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "violet", xlab = "Time (s)",
              ylab = "Counts")
        
        
        legend(1, legendPosition, legend=c("Vaka","Ölüm"),
               col=c("blue", "violet"), lty=1)
        
    }
    
    }
    
    if(input$expModelPlot){
      
      
      times <- seq(1,input$expTime, 1)
      predictions <- exp(predict(exponentialModel(),
                                 list(x=times),interval = "confidence"))
      
      xlimit = c(1, max(nrow(dataset()), max(times)))
      if(!input$addCI){
      
        ylimit = c(1,max(max(predictions[,"fit"]), max(dataset()[,"Toplam Vaka"])))
        legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], max(predictions[,"fit"]))
        
      }
      
      else if(input$addCI){
        
        ylimit = c(1,max(max(predictions[,"upr"]), max(dataset()[,"Toplam Vaka"])))
        legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], max(predictions[,"upr"]))
        
        
      }
      
      
      plot.new()
      plot(1, type="n", xlab="Gün", ylab="Toplam Vaka", xlim=xlimit, ylim=ylimit, panel.first = grid(),
           main = "Türkiye'deki Toplam Koronavirüs Vakaları")
      
      lines(seq(1:nrow(dataset()))[1:input$expTime], dataset()[,"Toplam Vaka"][1:input$expTime], lwd=2, col = "blue", xlab = "Time (s)",
            ylab = "Counts")
      
      lines(times, predictions[,"fit"], lwd=2, col = "red", xlab = "Time (s)",
            ylab = "Counts")
      
      if(input$addCI){
        
        lines(times, predictions[,"lwr"], lwd=2, col = "black", xlab = "Time (s)",
              ylab = "Counts")
        
          lines(times, predictions[,"upr"], lwd=2, col = "black", xlab = "Time (s)",
              ylab = "Counts")
        
        legend(1, legendPosition, legend=c("Vaka", "Üstel model", "Güven aralığı (%95)"),
               col=c("blue", "red", "black"), lty=1)
        
        legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], max(predictions[,"upr"]))
        
      }else{
      
      legend(1, legendPosition, legend=c("Vaka", "Üstel model"),
             col=c("blue", "red"), lty=1)
      
      }
      
      if(input$totalDeaths){
        
        lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "violet", xlab = "Time (s)",
              ylab = "Counts")
        
        
        legend(1, legendPosition, legend=c("Vaka","Ölüm", "Üstel model"),
               col=c("blue", "violet", "red"), lty=1)
        
        if(input$addCI){
          
          lines(times, predictions[,"lwr"], lwd=2, col = "black", xlab = "Time (s)",
                ylab = "Counts")
          
          lines(times, predictions[,"upr"], lwd=2, col = "black", xlab = "Time (s)",
                ylab = "Counts")
          
          legend(1, legendPosition, legend=c("Vaka", "Ölüm", "Üstel model", "Güven aralığı (%95)"),
                 col=c("blue", "violet", "red", "black"), lty=1)
          
          legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], max(predictions[,"upr"]))
          
        }
        
      }
      
    }
    
    
  })
  

  ##### log(Toplam Vaka) Grafiği ####
  
  output$logPlotTotalCases <- renderPlot({
    
      
      xlimit = c(1, nrow(dataset()))
      ylimit = c(0,log(max(dataset()[,"Toplam Vaka"])))
      legendPosition = max(log(dataset()[nrow(dataset()),"Toplam Vaka"]))
      
      plot.new()
      plot(1, type="n", xlab="Gün", ylab="log(Toplam Vaka)", xlim=xlimit, ylim=ylimit, panel.first = grid(),
           main = "Türkiye'deki Toplam Koronavirüs Vakaları (logaritmik)")
      
      lines(seq(1:nrow(dataset())), log(dataset()[,"Toplam Vaka"]), lwd=2, col = "blue", xlab = "Time (s)",
            ylab = "Counts")
      
      
      legend(1, legendPosition, legend=c("log(Gözlenen)"),
             col=c("blue"), lty=1)
      
    
    
    
  })
  
  ##### Toplam Ölüm Grafiği #### 
  plotTotalDeats <- renderPlot({
    
    
    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(dataset()[,"Toplam Ölüm"]))
    legendPosition = max(dataset()[nrow(dataset()),"Toplam Ölüm"])
    
    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Ölüm", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Türkiye'deki Toplam Koronavirüs Ölümleri")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")
    
    
    legend(1, legendPosition, legend=c("Gözlenen"),
           col=c("blue"), lty=1)
    
    
    
    
  })
  
  
  ##### Günlük Yeni Vakalar ####
  
  output$barPlotNewCases <- renderPlot({
    
    barplot(dataset()[,"Yeni Vaka"]~seq(1, nrow(dataset()),1), data = dataset(),
            xlab = "Gün", ylab="Günlük Yeni Vaka Sayısı" ,main = "Günlük Yeni Vakalar", panel.first = grid())
    
    
  })
    
    

  ##### Günlük Yeni Ölümler ####
  
  output$barPlotNewDeaths <- renderPlot({
    
    barplot(dataset()[,"Yeni Ölüm"]~seq(1, nrow(dataset()),1), data = dataset(),
            xlab = "Gün", ylab="Günlük Yeni Ölüm Sayısı" ,main = "Günlük Yeni Ölümler", panel.first = grid())
    
    
  })
  
  
  
}