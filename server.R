library(DT)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(gghighlight)
library(plotly)
library(shinyjs)
library(pracma)
library(Metrics)
library(readr)
source("worldData2.R")
source("worldData3.R")
source("underReporting.R")
# source("scale_CFR.R")
# source("delay_distributions.R")
# source("table_of_estimates.R")

server <- function(input, output, session) {

  # Simulate work being done for 1 second
  Sys.sleep(1)
  
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  #### Veri yükleme ####

  #### Dünya verileri ####
  dataWorld <-reactive({
    
    # url = "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"
    filter = input$filter
    worldData2(filter)
    
  })

  ##### Ülke isimleri #####
  countries <-reactive({

    dataWorld()[[2]]

  })

  observe({



  updateSelectizeInput(session, "countries", choices =  as.character(countries()), selected = c("Turkey",
    "Germany", "Italy", "Spain","France","Iran", "US", "China", "United Kingdom", "Korea, South", 
    "Switzerland", "Austria", "Belgium", "Canada", "Netherlands", "Portugal", "Brazil", "Sweden", "Israel", "Japan",
      "Peru", "India"))


  })


  observe({



    updateSelectizeInput(session, "highlightCountries", choices =  input$countries, selected = input$input$countries[1])


  })

  #### Türkiye tüm veriler ####
  dataset <- reactive({

    data <- read.table("www/data/covid_cases.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Tarih", "Toplam Vaka", "Yeni Vaka", "Toplam Ölüm", "Yeni Ölüm",
                       "Toplam İyileşme", "Yeni İyileşme", "Aktif Vaka",  "Toplam Yoğun Bakım Hasta Sayısı",
                       "Toplam Entübe Hasta Sayısı", "Toplam Test", "Yeni Test", "Test Sayısı (Milyonda)",
                       "Test Başına Vaka (%)", "Toplam Vaka (Milyonda)", "Vaka Değişim Oranı (%)", 
                       "Yeni Vaka Tespit Etme Oranı (%)", "Ölüm Oranı (%)", "Kapanan Vakada Ölüm (%)", "Büyüme Faktörü", "Test Değişim Oranı (%)",
                       "Time")
    return(data)

  })

  #### Türkiye özet veriler ####
  summaryData <- reactive({

    data <- read.table("www/data/summary.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Toplam Vaka", "Toplam Ölüm", "Toplam İyileşen Vaka", "Toplam Aktif Vaka",
                       "Ölüm Oranı (%)","Toplam Test", "Toplam Vaka (Milyonda)",
                       "Test Sayısı (Milyonda)", "Tespit Edilen Test Başına Vaka (%)")
    return(data)

  })


  #### Dünya Test verileri ####
  dataTest <- reactive({test = read.table("www/data/testsNew.txt", header=FALSE, sep = "\t")
  
  head(test)
  
  colnames(test) = c("Country", "Date", "Test", "Positive", "Positive_Rate", "Test_million_population", "Positive_million_population", "Ref")
  head(test)
  
  combinedTestCountries = test#data.table::rbindlist(testCountries)
  
  combinedTestCountries = combinedTestCountries[complete.cases(combinedTestCountries),]
  
  plotData = dplyr::filter(combinedTestCountries, combinedTestCountries$Positive >=10000)
  
  plotData= plotData[plotData$Date %in% c("27 April", "28 April", "29 April", "30 April", "1 May"), ]
  plotData$Country=as.character(plotData$Country)
  
  plotData$Country[plotData$Country %in% "UnitedStates"] = "US" 
  plotData$Country[plotData$Country %in% "SouthKorea"] = "South Korea" 
  plotData$Country[plotData$Country %in% "UnitedKingdom[b]"] = "United Kingdom" 
  plotData = plotData[!(plotData$Country %in% c("UnitedArabEmirates")),]
  
  return(plotData)
  })

####### DASHBOARDS ###########
  output$totalCase <- renderUI({
    
    infoBox(
      "TOPLAM VAKA", summaryData()[1,1], icon = icon("hospital-symbol"), color = "yellow",
      fill = TRUE
    )
    
    })
  
  output$totalDeath <- renderUI({
    
    infoBox(
      "TOPLAM ÖLÜM", summaryData()[1,2], icon = icon("feather"), color = "red",
      fill = TRUE
    )
    
  })
  
  output$totalRecovered <- renderUI({
    
    infoBox(
      "TOPLAM İYİLEŞEN", summaryData()[1,3], icon = icon("check"), color = "green",
      fill = TRUE
    )
    
  })
  
  
  output$totalActiveCases <- renderUI({
    
    infoBox(
      "TOPLAM AKTİF VAKA", summaryData()[1,4], icon = icon("thermometer"), color = "yellow",
      fill = TRUE
    )
    
  })
  
  output$deathRate <- renderUI({
    
    infoBox(
      "ÖLÜM ORANI (%)", summaryData()[1,5], icon = icon("percentage"), color = "red",
      fill = TRUE
    )
    
  })
  
  output$totalTest <- renderUI({
    
    infoBox(
      "TOPLAM TEST", summaryData()[1,6], icon = icon("microscope"), color = "green",
      fill = TRUE
    )
    
  })
  
  output$totalCaseMillion <- renderUI({
    
    infoBox(
      "VAKA SIKLIĞI (MİLYONDA)", summaryData()[1,7], icon = icon("sort-numeric-up"), color = "yellow",
      fill = TRUE
    )
    
  })
  
  output$deathRateClosedCases <- renderUI({
    
    infoBox(
      "KAPANAN VAKADA ÖLÜM ORANI (%)", summaryData()[1,10], icon = icon("percentage"), color = "red",
      fill = TRUE
    )
    
  })
  
  output$totalTesMillion <- renderUI({

    infoBox(
      "TEST SIKLIĞI (MİLYONDA)", summaryData()[1,8], icon = icon("sort-numeric-up"), color = "green",
      fill = TRUE
    )
    
  })
  
  output$icuCases <- renderUI({
    
    infoBox(
      "YOĞUN BAKIM HASTA SAYISI", summaryData()[1,11], icon = icon("stethoscope"), color = "yellow",
      fill = TRUE
    )
    
  })
  
  output$totalCaseThousandTest <- renderUI({
    
    infoBox(
      "TEST BAŞINA VAKA (%)", summaryData()[1,9], icon = icon("percentage"), color = "red",
      fill = TRUE
    )
    
  })
  
  output$entCases <- renderUI({
    
    infoBox(
      "ENTÜBE HASTA SAYISI", summaryData()[1,12], icon = icon("stethoscope"), color = "yellow",
      fill = TRUE
    )
    
  })
  

  
  
  
  #### Tablo ####
  result <- reactive({

    if(input$dataset == 'Tüm'){

      dataset()[1:21]
      
    }


  })

  #### Türkiye istatistikleri #####
  output$resultTable <- DT::renderDataTable({


    datatable(result(), extensions = c('Buttons','KeyTable', 'Responsive'), rownames= FALSE,options = list(columnDefs = list(list(className = 'dt-center',targets='_all')),pageLength = 100,
      info = FALSE,bFilter = FALSE, paging = FALSE, dom = 'Bfrtip',buttons = list(list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                          text = 'İndir')), keys = TRUE
    ))

  })

  output$dimension <- renderPrint({
    
    cbind.data.frame(input$dimension[1], input$dimension[2])
  })
  
  #### Türkiye haritası #####
  output$turkeyMap <- renderUI({
  
    if(input$dimension < 550){
      
      h = input$dimension/2
      
    }else{
      
      h = input$dimension/2.5
      
    }
    
    
    tags$iframe(src="https://infogram.com/turkiye-koronavirus-vaka-sayilari-haritasi-1h7z2lgw10qx4ow", 
                width = "100%", height = h, 
                allowfullscreen="allowfullscreen",
                id = 'myIframe',frameborder=0,
                scrolling = 'yes')
    
  
    
    # <div class="infogram-embed" data-id="ee5d83f0-3c42-4112-a103-162b3770c9cd" 
    # data-type="interactive" data-title="Türkiye koronavirüs vaka sayıları haritası"></div><script>
    #   !function(e,i,n,s){var t="InfogramEmbeds",d=e.getElementsByTagName("script")[0];
    #   if(window[t]&&window[t].initialized)window[t].process&&window[t].process();
    #   else if(!e.getElementById(n)){var o=e.createElement("script");o.async=1,o.id=n,
    #   o.src="https://e.infogram.com/js/dist/embed-loader-min.js",d.parentNode.insertBefore(o,d)}}
    # (document,0,"infogram-async");
    # 
    # </script>
    
  })
  
  #### Dünya haritası #####
  output$worldMap <- renderUI({
    
    if(input$dimension < 550){
      
      h = input$dimension/2
      
    }else{
      
      h = input$dimension/2.5
      
    }
  
    
    tags$iframe(src="https://www.arcgis.com/apps/Embed/index.html?webmap=14aa9e5660cf42b5b4b546dec6ceec7c&extent=&zoom=true&previewImage=false&scale=true&disable_scroll=true&theme=light", 
                width = "100%", height = h, 
                allowfullscreen="allowfullscreen",
                id = 'myIframe2',frameborder=0,
                scrolling = 'no')
    
    
  })
  
  #### Üstel model ####
  exponentialModel <- reactive({

    data = dataset()

    vaka = dataset()[,"Toplam Vaka"][8:nrow(dataset())]
    zaman = dataset()[,"Time"][8:nrow(dataset())]
    vaka_tespit_oran = dataset()[,"Yeni Vaka Tespit Etme Oranı (%)"][8:nrow(dataset())]

    expmodel <- lm(log(vaka)~ zaman+vaka_tespit_oran)

    return(expmodel)

  })

  #### Üstel model özeti #####
  output$summaryModel <- renderPrint({

    if(input$expModelSummary){
      summary(exponentialModel())
    }
  })

  #### Grafik ####

  ##### Toplam Vaka Grafiği ####

  output$plotTotalCases <- renderPlotly({
   
    fig <- plot_ly(x = seq(1:nrow(dataset())), y = dataset()[,"Toplam Vaka"], type = 'scatter', name = "Vaka", mode = 'lines+markers'
                   , line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red')))
    
    
    if(input$kalmanFilter){
      
      
      all<- dataset()[1:2]
      
      colnames(all) = c("date", "Turkey")
      print(all)
      d = unlist(strsplit(as.character(all[nrow(all),1]), "\\."))
      print(d)
      d2 = paste0(d[3],"/",d[2],"/",d[1])
      print(d2)
      d2 = as.Date(paste0(d[3],"/",d[2],"/",d[1]))
      all$date =seq(as.Date("2020/3/11"), d2, by=1)
      
      original = all
      # all$X1<-NULL
      date<-all[1]
      future = 1
      z=1
      
      
      while(future >= z ){
        print(z)
        z= z+1  
        date<-all[1]
        
        date[nrow(date) + 1,1] <-all[nrow(all),1]+1
        pred_all<-NULL
        date$date = as.character(date$date)
        for (n in 2:ncol(all)-1) {
          Y<-ts(data = all[n+1], start = 1, end =nrow(all)+1)  
          sig_w<-0.1
          w<-sig_w*randn(1,10000) # acceleration which denotes the fluctuation (Q/R) rnorm(100, mean = 0, sd = 1)
          sig_v<-0.1
          v<-sig_v*randn(1,10000)   
          t<-0.45
          # t=t1+0.5*(z-1)
          phi<-matrix(c(1,0,t,1),2,2)
          gama<-matrix(c(0.5*(t)^2,t),2,1)
          H<-matrix(c(1,0),1,2)
          #Kalman
          x0_0<-p0_0<-matrix(c(0,0),2,1)
          p0_0<-matrix(c(1,0,0,1),2,2)
          Q<-0.1
          R<-0.1
          X<-NULL
          X2<-NULL
          pred<-NULL
          for (i in 0:nrow(all)) {
            namp <-paste("p", i+1,"_",i, sep = "")
            assign(namp, phi%*%(get(paste("p", i,"_",i, sep = "")))%*%t(phi)+gama%*%Q%*%t(gama))
            namk <- paste("k", i+1, sep = "")
            assign(namk,get(paste("p", i+1,"_",i, sep = ""))%*%t(H)%*%(1/(H%*%get(paste("p", i+1,"_",i, sep = ""))%*%t(H)+R)))
            namx <- paste("x", i+1,"_",i, sep = "")
            assign(namx,phi%*%get(paste("x", i,"_",i, sep = "")))
            namE <- paste("E", i+1, sep = "")
            assign(namE,Y[i+1]-H%*%get(paste("x", i+1,"_",i, sep = "")))
            namx2 <- paste("x", i+1,"_",i+1, sep = "")
            assign(namx2,get(paste("x", i+1,"_",i, sep = ""))+get(paste("k", i+1, sep = ""))%*%get(paste("E", i+1, sep = "")))
            namp2 <- paste("p", i+1,"_",i+1, sep = "")
            assign(namp2,(p0_0-get(paste("k", i+1, sep = ""))%*%H)%*%get(paste("p", i+1,"_",i, sep = "")))
            X<-rbind(X,get(paste("x", i+1,"_",i,sep = ""))[1])
            X2<-rbind(X2,get(paste("x", i+1,"_",i,sep = ""))[2])
            if(i>2){
              remove(list=(paste("p", i-1,"_",i-2, sep = "")))
              remove(list=(paste("k", i-1, sep = "")))
              remove(list=(paste("E", i-1, sep = "")))
              remove(list=(paste("p", i-2,"_",i-2, sep = "")))
              remove(list=(paste("x", i-1,"_",i-2, sep = "")))
              remove(list=(paste("x", i-2,"_",i-2, sep = "")))}
          }
          pred<-NULL
          pred<-cbind(Y,X,round(X2,4))
          pred<-as.data.frame(pred)
          pred$region<-colnames(all[,n+1])
          pred$date<-date$date
          pred$actual<-rbind(0,(cbind(pred[2:nrow(pred),1])/pred[1:nrow(pred)-1,1]-1)*100)
          pred$predict<-rbind(0,(cbind(pred[2:nrow(pred),2])/pred[1:nrow(pred)-1,2]-1)*100)
          pred$pred_rate<-(pred$X/pred$Y-1)*100
          pred$X2_change<-rbind(0,(cbind(pred[2:nrow(pred),3]-pred[1:nrow(pred)-1,3])))
          pred_all<-rbind(pred_all,pred)
        }
        pred_all<-cbind(pred_all[,4:5],pred_all[,1:3])
        names(pred_all)[5]<-"X2"
        pred_all<-pred_all[,c(1,3:5)]
        pred_all$days = 1:nrow(pred_all)
        sd(pred_all$Y[(nrow(pred_all)-7):nrow(pred_all)])
        pred_all$Y[nrow(pred_all)] = pred_all$X[nrow(pred_all)]
        
        all = pred_all[c(1,2)]
        all$date = as.Date(all$date)
        
        
      }
      
      all$days = seq(1,nrow(all))
      all$Y = as.numeric(formatC(all$Y, digits = 0, format = "f"))
      
      # fig <- plot_ly(x = seq(1:nrow(original)), y = original$Turkey, name = 'Vaka', type = 'scatter', 
      #                mode = 'lines+markers', line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
      # 
      fig <- fig %>% add_trace(x = all$days[(nrow(original)):(nrow(original)+future)], 
                               y =all$Y[(nrow(original)):(nrow(original)+future)], name = 'Sonraki Gün Tahmin', 
                               mode = 'lines+markers',
                               line = list(color = "blue"), marker = list(color = 'blue',line = list(color = 'blue'))) 
      

      
      
      
    }
    
    fig %>% layout(title = "<b>Toplam Resmi COVID-19 Vakaları</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Toplam Vaka"), showlegend = TRUE, autosize = TRUE,
                   margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })

  ##### Toplam Ölüm ve İyileşme Grafiği ####

  output$plotTotalDeatsRecovered <- renderPlotly({

# 
# 
#     xlimit = c(1, nrow(dataset()))
#     ylimit = c(1,max(max(dataset()[,"Toplam Ölüm"]), max(dataset()[,"Toplam İyileşme"])))
#     legendPosition = max(max(dataset()[nrow(dataset()),"Toplam Ölüm"]), max(dataset()[,"Toplam İyileşme"]))
# 
#     plot.new()
#     plot(1, type="n", xlab="Gün", ylab="Sayı", xlim=xlimit, ylim=ylimit, panel.first = grid(),
#          main = "Toplam Resmi COVID-19 Ölüm ve İyileşme Vakaları")
# 
#     lines(type="o",seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "red", xlab = "Time (s)",
#           ylab = "Counts")
# 
# 
#         lines(type="o",seq(1:nrow(dataset())), dataset()[,"Toplam İyileşme"], lwd=2, col = "blue", xlab = "Time (s)",
#               ylab = "Counts")
# 
# 
#         legend("topleft", legend=c("Ölüm","İyileşme"),
#                col=c("red", "blue"), lty=1)

        
        fig <- plot_ly(x = seq(1:nrow(dataset())), y = dataset()[,"Toplam İyileşme"], name = 'İyileşme', type = 'scatter', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y =dataset()[,"Toplam Ölüm"], name = 'Ölüm', mode = 'lines+markers',
                                 line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 

        fig%>% layout(title = "<b>Toplam Ölüm ve İyileşme Vakaları</b>", xaxis = list(title = "Gün"), 
                      yaxis=list(title = "Sayı"), showlegend = TRUE, autosize = TRUE,
                      margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
          layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        
  })

  ##### log(Toplam Vaka) Grafiği ####

  output$logPlotTotalCases <- renderPlot({


      xlimit = c(1, nrow(dataset()))
      ylimit = c(0,log(max(dataset()[,"Toplam Vaka"])))
      legendPosition = max(log(dataset()[nrow(dataset()),"Toplam Vaka"]))

      plot.new()
      plot(1, type="n", xlab="Gün", ylab="log(Toplam Vaka)", xlim=xlimit, ylim=ylimit, panel.first = grid(),
           main = "Toplam Resmi COVID-19 Vakaları (logaritmik)")

      lines(type="o",seq(1:nrow(dataset())), log(dataset()[,"Toplam Vaka"]), lwd=2, col = "blue", xlab = "Time (s)",
            ylab = "Counts")


      legend(1, legendPosition, legend=c("log(Vaka)"),
             col=c("blue"), lty=1)




  })

  ##### Toplam vs Kapanan Ölüm ####
  output$totalClosedDeathRate <- renderPlotly({
    
    
    data = dataset()
    
    time <- data$Time
    time2 = time[5:length(time)]
    deathRate <- data$`Ölüm Oranı (%)`[time2]
    deathRateClosedCases <- data$`Kapanan Vakada Ölüm (%)`[time2]
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Kapanan Vakada Ölüm Oranı (%)"
    )
    
    
    fig <- plot_ly(x = time2, y = deathRate, name = 'Toplam', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(x = time2, y =deathRateClosedCases, yaxis = "y2", name = 'Kapanan', mode = 'lines+markers',
                             line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
    
    fig%>% layout(title = "<b>Toplam ve Kapanan Vakada Ölüm (%)</b>", yaxis2 = ay,
                  xaxis = list(title = "Gün"), 
                  yaxis=list(title = "Ölüm Oranı (%)"), showlegend = TRUE, autosize = TRUE,
                  margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))%>% layout(yaxis2=list(fixedrange=TRUE))
    
    
    
    
  })
  
  
  ##### Test vs Vaka ####
  output$testVsCasePlot <- renderPlotly({


    data = dataset()
    
    time1 <- data$Time
    time = time1[8:length(time1)]
    newTest <- data$`Yeni Test`[time]
    newCaseRatio <- data$`Yeni Vaka Tespit Etme Oranı (%)`[time]
    
    meanDailyTest = list()
    
    meanDailyCaseRatio = list()
    
    for(i in 1:(length(time)-6)){
      
      j=i+6
      
      meanDailyTest[[i]] = mean(newTest[i:j], na.rm = TRUE)
      
      meanDailyCaseRatio[[i]] = mean(newCaseRatio[i:j], na.rm = TRUE)
      
    }
    
    
    meanDailyTests =  do.call(rbind.data.frame, meanDailyTest)
    colnames(meanDailyTests) = "Mean_Daily_New_Tests"
    
    meanDailyCaseRatio =  do.call(rbind.data.frame, meanDailyCaseRatio)
    colnames(meanDailyCaseRatio) = "Mean_Daily_Case_Ratio"
    
    
    dailyTestsCaseRatioMean = cbind.data.frame(Days =0:(nrow(meanDailyTests)-1), meanDailyTests, meanDailyCaseRatio)
    
    
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Yeni Vaka Tespit Oranı (%)"
    )
    
    time = dailyTestsCaseRatioMean$Days
    newTest = dailyTestsCaseRatioMean$Mean_Daily_New_Tests
    newCaseRatio = dailyTestsCaseRatioMean$Mean_Daily_Case_Ratio
    
    fig <- plot_ly(x = time, y = newTest, name = 'Yeni Test', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(x = time, y =newCaseRatio, yaxis = "y2", name = 'Yeni Vaka Oranı', mode = 'lines+markers',
                             line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
    
    fig%>% layout(title = "<b>Yeni Test-Vaka Oranı (7 Gün Ort.)</b>", yaxis2 = ay,
                  xaxis = list(title = "Gün"), 
                  yaxis=list(title = "Yeni Test Sayısı"), showlegend = TRUE, autosize = TRUE,
                  margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.0750, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))%>% layout(yaxis2=list(fixedrange=TRUE))
    

  })

  ##### Toplam Ölüm Grafiği ####
  plotTotalDeats <- renderPlot({


    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(dataset()[,"Toplam Ölüm"]))
    legendPosition = max(dataset()[nrow(dataset()),"Toplam Ölüm"])

    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Ölüm", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam COVID-19 Ölümleri")

    lines(type="o",seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")


    legend(1, legendPosition, legend=c("Vaka"),
           col=c("blue"), lty=1)




  })

  ##### Toplam Aktif Vaka ####
  
  output$plotTotalActiveCases <- renderPlotly({
    
  
    
    fig <- plot_ly(x = seq(1:nrow(dataset())), y = dataset()[,"Aktif Vaka"], type = 'scatter', name = "Aktif Vaka", mode = 'lines+markers'
                   , line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red')))
    
    fig %>% layout(title = "<b>Toplam Resmi Aktif Vaka Sayısı</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Aktif Vaka"), showlegend = TRUE, autosize = TRUE,
                   margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    
    
  })
  
  
  ##### Toplam Test Grafiği ####

  output$plotTotalTests <- renderPlotly({



    # xlimit = c(1, nrow(dataset()))
    # ylimit = c(1,max(dataset()[,"Toplam Test"], na.rm = TRUE))
    # 
    # plot.new()
    # plot(1, type="n", xlab="Gün", ylab="Toplam Test", xlim=xlimit, ylim=ylimit, panel.first = grid(),
    #      main = "")
    # 
    # lines(type="o",seq(1:nrow(dataset())), dataset()[,"Toplam Test"], lwd=2, col = "blue", xlab = "Time (s)",
    #       ylab = "Counts")
    # 
    # legend("topleft", legend=c("Test"),
    #        col=c("blue"), lty=1)

    fig <- plot_ly(x = seq(1:nrow(dataset())), y = dataset()[,"Toplam Test"], type = 'scatter', name = "Test", mode = 'lines+markers')
    
    fig %>% layout(title = "<b>Toplam Resmi COVID-19 Test Sayısı</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Toplam Test"), showlegend = TRUE, autosize = TRUE,
                   margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    


  })

  ##### Toplam Yoğun Bakım ve Entübe Hasta Sayısı ####

  output$plotTotalICU <- renderPlotly({



    # xlimit = c(1, nrow(dataset()))
    # ylimit = c(1,max(max(dataset()[,"Toplam Entübe Hasta Sayısı"]), max(dataset()[,"Toplam Yoğun Bakım Hasta Sayısı"])))
    # 
    # plot.new()
    # plot(1, type="n", xlab="Gün", ylab="Sayı", xlim=xlimit, ylim=ylimit, panel.first = grid(),
    #      main = "Toplam Resmi Yoğun Bakım ve Entübe Hasta Sayısı")
    # 
    # lines(type="o",seq(1:nrow(dataset())), dataset()[,"Toplam Yoğun Bakım Hasta Sayısı"], lwd=2, col = "red", xlab = "Time (s)",
    #       ylab = "Counts")
    # 
    # 
    # lines(type="o",seq(1:nrow(dataset())), dataset()[,"Toplam Entübe Hasta Sayısı"], lwd=2, col = "blue", xlab = "Time (s)",
    #       ylab = "Counts")
    # 
    # 
    # legend("topleft", legend=c("Yoğun Bakım","Entübe"),
    #        col=c("red", "blue"), lty=1)

    fig <- plot_ly(x = seq(1:nrow(dataset())), y = dataset()[,"Toplam Yoğun Bakım Hasta Sayısı"], name = 'Yoğun Bakım', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y =dataset()[,"Toplam Entübe Hasta Sayısı"], name = 'Entübe', mode = 'lines+markers',
                             line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
    
    fig%>% layout(title = "<b>Yoğun Bakım ve Entübe Hasta</b>", xaxis = list(title = "Gün"), 
                  yaxis=list(title = "Sayı"), showlegend = TRUE, autosize = TRUE,
                  margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

  })



  ##### Toplam Test (Milyonda) ve Vaka (Bin Test) ####

  output$plotTestCaseAdjusted <- renderPlotly({

    data = dataset()
    time <- data$Time
    test <- data$`Test Sayısı (Milyonda)`
    case <- data$`Test Başına Vaka (%)`
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Test Başına Vaka (%)"
    )
    
    
    fig <- plot_ly(x = time, y = test, name = 'Test (Milyonda)', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(x = time, y =case, yaxis = "y2", name = 'Vaka (%)', mode = 'lines+markers',
                             line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
    
    fig%>% layout(title = "<b>Test (Milyonda) ve Vaka (%)</b>", yaxis2 = ay,
                  xaxis = list(title = "Gün"), 
                  yaxis=list(title = "Test Sıklığı (Milyonda)"), showlegend = TRUE, autosize = TRUE,
                  margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))%>% layout(yaxis2=list(fixedrange=TRUE))
    

  })

  ##### Toplam Test vs Toplam Vaka Sayısı (Milyonda) #######
  
  output$plotTotalTestCaseAdjusted <- renderPlotly({
    
    data = dataset()
    time <- data$Time
    test <- data$`Test Sayısı (Milyonda)`
    case <- data$`Toplam Vaka (Milyonda)`
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Vaka Sıklığı (Milyonda)"
    )
    
    
    fig <- plot_ly(x = time, y = test, name = 'Test (Milyonda)', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(x = time, y =case, yaxis = "y2", name = 'Vaka (Milyonda)', mode = 'lines+markers',
                             line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
    
    fig%>% layout(title = "<b>Test ve Vaka Sıklığı (Milyonda)</b>", yaxis2 = ay,
                  xaxis = list(title = "Gün"), 
                  yaxis=list(title = "Test Sıklığı (Milyonda)"), showlegend = TRUE, autosize = TRUE,
                  margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))%>% layout(yaxis2=list(fixedrange=TRUE))
    
    
  })
  
  
  ##### Test ve Vaka Büyüme Oranları ####
  
  output$plotTestCaseGrowt <- renderPlotly({
    
    time = seq(1:nrow(dataset()))
    time2 = time[10:length(time)]
    
    library(plotly)
    fig <- plot_ly(x = time2, y = dataset()[,"Vaka Değişim Oranı (%)"][10:length(time)], name = 'Vaka Artış  (%)', type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y =dataset()[,"Test Değişim Oranı (%)"][10:length(time)], name = 'Test Artış  (%)', mode = 'lines+markers',
                             line = list(color = "red"), marker = list(color = 'red',line = list(color = 'red'))) 
    
    fig%>% layout(title = "<b>Toplam Test ve Vaka Artış Oranları</b>", xaxis = list(title = "Gün"), 
                  yaxis=list(title = "Artış Oranı (%)"), showlegend = TRUE, autosize = TRUE,
                  margin = list(l=50, r=50, b=0, t=50, pad=4))%>% 
      layout(legend = list(x = 0.040, y = 0.9))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
  })
  
  
  
   ##### Günlük Yeni Vakalar ####

  output$barPlotNewCases <- renderPlotly({

    # barplot(dataset()[,"Yeni Vaka"]~seq(1, nrow(dataset()),1),
    #         xlab = "Gün", ylab="Günlük Yeni Vaka Sayısı" ,main = "Günlük Yeni Vakalar", panel.first = grid())
    # 

   fig <- plot_ly(
      x = seq(1, nrow(dataset()),1),
      y = dataset()[,"Yeni Vaka"],
      type = "bar")
    
    fig %>% layout(title = "<b>Günlük Yeni Vakalar</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Günlük Yeni Vaka Sayısı"), showlegend = FALSE,margin = list(l=50, r=50, b=0, t=50, pad=4))%>% config(displayModeBar = F)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) 

    
    
    
    
  })



  ##### Günlük Yeni Ölümler ####

  output$barPlotNewDeaths <- renderPlotly({

    # barplot(dataset()[,"Yeni Ölüm"]~seq(1, nrow(dataset()),1),
    #         xlab = "Gün", ylab="Günlük Yeni Ölüm Sayısı" ,main = "Günlük Yeni Ölümler", panel.first = grid())

    fig <- plot_ly(
      x = seq(1, nrow(dataset()),1),
      y = dataset()[,"Yeni Ölüm"],
      type = "bar")
    
    fig %>% layout(title = "<b>Günlük Yeni Ölüm Sayısı</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Günlük Yeni Ölümler"), showlegend = FALSE,margin = list(l=50, r=50, b=0, t=50, pad=4))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
  })


  ##### Günlük Yeni İyileşmeler ####

  output$barPlotNewRecovered <- renderPlotly({

    # barplot(dataset()[,"Yeni İyileşme"]~seq(1, nrow(dataset()),1),
    #         xlab = "Gün", ylab="Günlük Yeni İyileşme Sayısı" ,main = "Günlük Yeni İyileşmeler", panel.first = grid())

    
    fig <- plot_ly(
      x = seq(1, nrow(dataset()),1),
      y = dataset()[,"Yeni İyileşme"],
      type = "bar")
    
    fig %>% layout(title = "<b>Günlük Yeni İyileşme Sayısı</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Günlük Yeni İyileşmeler"), showlegend = FALSE,margin = list(l=50, r=50, b=0, t=50, pad=4))%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    
    
  })


  ##### Günlük Yeni Test ####

  output$barPlotNewTest <- renderPlotly({

    # barplot(dataset()[,"Yeni Test"]~seq(1, nrow(dataset()),1),
    #         xlab = "Gün", ylab="Günlük Yeni Test Sayısı" ,main = "Günlük Yeni Test Sayısı", panel.first = grid())
    # 

    
    fig <- plot_ly(
      x = seq(1, nrow(dataset()),1),
      y = dataset()[,"Yeni Test"],
      type = "bar")
    
    fig %>% layout(title = "<b>Günlük Yeni Test Sayısı</b>", xaxis = list(title = "Gün"), 
                   yaxis=list(title = "Günlük Yeni Test Sayısı"), showlegend = FALSE,margin = list(l=50, r=50, b=0, t=50, pad=4))%>% config(displayModeBar = F)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) 
    
    
    
    
  })


  ##### Ülke Karşılaştırma ####

  #### Ülke verileri #####
  comparedCountries <- reactive({

    comparedCountries = input$countries
    indx = which(dataWorld()[[1]]$Country %in% comparedCountries)
    compareData = as.data.frame(dataWorld()[[1]][indx,])
    compareData$Country = as.character(compareData$Country)
    compareData$logConfirmed = log(compareData$Confirmed)
    compareData$logDeaths = log(compareData$Deaths)
    compareData$logRecovered = log(compareData$Recovered)
    return(compareData)

  })

  #### Ülke tablosu #####
  output$countryTable <- DT::renderDataTable({


    dataComparedCountries = dataWorld()[[4]][dataWorld()[[4]]$Country %in% input$countries,]
    colnames(dataComparedCountries) = c("Ülke", "Toplam Vaka", "Toplam Ölüm", "Toplam İyileşen", "Yeni Vaka", "Yeni Ölüm", "Yeni İyileşme",
                                        "Vaka Sıklığı (Milyonda)", "Ölüm Sıklığı (Milyonda)", "İyileşme Sıklığı (Milyonda)", "Nüfus", 
                                        "Yıllık Değişim", "Net Değişim", "Yoğunluk", "Yüzölüçümü", "Doğurganlık Oranı", "Ortanca Yaş",
                                        "Şehir Nüfusu", "Dünya Nüfusundaki Payı")

    
    allConfirmed = sum(dataWorld()[[3]]$MaxConfirmed[!is.na(dataWorld()[[3]]$MaxConfirmed) & !is.infinite(dataWorld()[[3]]$MaxConfirmed)], na.rm=TRUE)
    allDeaths = sum(dataWorld()[[3]]$MaxDeaths[!is.na(dataWorld()[[3]]$MaxDeaths) & !is.infinite(dataWorld()[[3]]$MaxDeaths)], na.rm=TRUE)
    allRecovered = sum(dataWorld()[[3]]$MaxRecovered[!is.infinite(dataWorld()[[3]]$MaxRecovered)], na.rm=TRUE)
    
    allNewCases = sum(dataWorld()[[3]]$NewCases[!is.na(dataWorld()[[3]]$NewCases) & !is.infinite(dataWorld()[[3]]$NewCases)], na.rm=TRUE)
    allNewDeaths = sum(dataWorld()[[3]]$NewDeaths[!is.na(dataWorld()[[3]]$NewDeaths) & !is.infinite(dataWorld()[[3]]$NewDeaths)], na.rm=TRUE)
    allNewRecovered = sum(dataWorld()[[3]]$NewRecovered[!is.na(dataWorld()[[3]]$NewRecovered) & !is.infinite(dataWorld()[[3]]$NewRecovered)], na.rm=TRUE)
    
    worldPopulation = 7777586140
    worldAdjustedCase = round(1000000/(worldPopulation/allConfirmed),0)
    worldAdjustedDeaths = round(1000000/(worldPopulation/allDeaths),0)
    worldpopAdjustedRecovered = round(1000000/(worldPopulation/allRecovered),0)
    

    
    
    world = cbind.data.frame("Global", allConfirmed, allDeaths, allRecovered, allNewCases,allNewDeaths,allNewRecovered,worldAdjustedCase
                             ,worldAdjustedDeaths,worldpopAdjustedRecovered,worldPopulation,NA,NA,NA,NA,NA,NA,NA,NA)
    colnames(world) = c("Ülke", "Toplam Vaka", "Toplam Ölüm", "Toplam İyileşen", "Yeni Vaka", "Yeni Ölüm", "Yeni İyileşme",
                        "Vaka Sıklığı (Milyonda)", "Ölüm Sıklığı (Milyonda)", "İyileşme Sıklığı (Milyonda)", "Nüfus", 
                        "Yıllık Değişim", "Net Değişim", "Yoğunluk", "Yüzölüçümü", "Doğurganlık Oranı", "Ortanca Yaş",
                        "Şehir Nüfusu", "Dünya Nüfusundaki Payı")

    dataComparedCountries = as.data.frame(rbind.data.frame(world, dataComparedCountries))
    dataComparedCountries = dataComparedCountries[order(dataComparedCountries[,"Toplam Vaka"], decreasing = TRUE),]


    datatable(dataComparedCountries,
              extensions = c('Buttons','KeyTable', 'Responsive'),
              rownames= FALSE,options = list(columnDefs = list(list(className = 'dt-center',targets='_all')),pageLength = 100, info = TRUE,bFilter = TRUE, paging = FALSE, 
                        dom = 'Bfrtip',buttons = list(list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                                                                                                                                                            text = 'İndir')), keys = TRUE
    ))

  })

  
  #### Ülke İstatistikleri Grafiği ##########
  
  output$worldStatistics <- renderPlotly({
    
    data2 = dataWorld()[[3]] 
    
    
    plotData = data2[(data2$Country %in% input$countries),]
    
    head(plotData)
    
    fig1 <- plot_ly(x = plotData$MaxConfirmed, y = ~reorder(plotData$Country, plotData$MaxConfirmed), name = 'Toplam Vaka',
                    type = 'bar', orientation = 'h',
                    marker = list(color = '#6093ca',
                                  line = list(color = '#6093ca', width = 1))) 
    
    # fig1 <- fig1 %>% layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
    #                         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
    fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                     x = plotData$MaxConfirmed * 0.95 + 10000,  y = plotData$Country,
                                     text = paste(round(plotData$MaxConfirmed, 2)),
                                     font = list(family = 'Arial', size = 12, color = 'black'),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    fig1
    
    
    fig2 <- plot_ly(x = plotData$MaxRecovered, y = ~reorder(plotData$Country, plotData$MaxConfirmed), name = 'Toplam İyileşen',
                    type = 'bar', orientation = 'h',
                    marker = list(color = '#77dd77',
                                  line = list(color = '#77dd77', width = 1))) 
    
    
    fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'y',
                                     x = plotData$MaxRecovered * 1.0 + 1500,  y = plotData$Country,
                                     text = paste(round(plotData$MaxRecovered, 2)),
                                     font = list(family = 'Arial', size = 12),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    fig2
    
    
    fig3 <- plot_ly(x = plotData$MaxDeaths, y = ~reorder(plotData$Country, plotData$MaxConfirmed), name = 'Toplam Ölüm',
                    type = 'bar', orientation = 'h',
                    marker = list(color = '#ff6961',
                                  line = list(color = '#ff6961', width = 1))) 
    
    
    fig3 <- fig3 %>% add_annotations(xref = 'x', yref = 'y',
                                     x = plotData$MaxDeaths * 1.0 + 1500,  y = plotData$Country,
                                     text = paste(round(plotData$MaxDeaths, 2)),
                                     font = list(family = 'Arial', size = 12),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    fig3
    
    fig <- subplot(fig1, fig2, fig3) 
    
    fig <- fig %>% layout(title = 'Mutlak sayı*', font = list(size=11),
                          legend = list(x = 0.009, y = 10.038,
                                        font = list(size = 12)),
                          margin = list(l = 100, r = 20, t = 70, b = 70),
                          paper_bgcolor = 'rgb(248, 248, 255)',
                          plot_bgcolor = 'rgb(248, 248, 255)')
    
    fig <- fig %>% add_annotations(xref = 'paper', yref = 'paper',
                                   x = 0, y = -0.15,
                                   text = paste('*Ülkelerin toplam vaka, iyileşen ve ölüm sayıları'),
                                   font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                                   showarrow = FALSE)
    
    fig%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
  })
  
  output$worldStatisticsPopAdjusted <- renderPlotly({
    
    data2 = dataWorld()[[4]] 
    
    
    plotData = data2[(data2$Country %in% input$countries),]
    
    head(plotData)
    
    fig1 <- plot_ly(x = plotData$popAdjustedCase, y = ~reorder(plotData$Country, plotData$popAdjustedCase), name = 'Vaka Sıklığı',
                    type = 'bar', orientation = 'h',
                    marker = list(color = '#6093ca',
                                  line = list(color = '#6093ca', width = 1))) 
    
    # fig1 <- fig1 %>% layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
    #                         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
    fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                     x = plotData$popAdjustedCase * 0.95 + 100,  y = plotData$Country,
                                     text = paste(round(plotData$popAdjustedCase, 2)),
                                     font = list(family = 'Arial', size = 12, color = 'black'),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    fig1
    
    
    fig2 <- plot_ly(x = plotData$popAdjustedRecovered, y = ~reorder(plotData$Country, plotData$popAdjustedCase), name = 'İyileşen Sıklığı',
                    type = 'bar', orientation = 'h',
                    marker = list(color = '#77dd77',
                                  line = list(color = '#77dd77', width = 1))) 
    
    
    fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'y',
                                     x = plotData$popAdjustedRecovered * 1.0 + 100,  y = plotData$Country,
                                     text = paste(round(plotData$popAdjustedRecovered, 2)),
                                     font = list(family = 'Arial', size = 12),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    fig2
    
    
    fig3 <- plot_ly(x = plotData$popAdjustedDeaths, y = ~reorder(plotData$Country, plotData$popAdjustedCase), name = 'Ölüm Sıklığı',
                    type = 'bar', orientation = 'h',
                    marker = list(color = '#ff6961',
                                  line = list(color = '#ff6961', width = 1))) 
    
    
    fig3 <- fig3 %>% add_annotations(xref = 'x', yref = 'y',
                                     x = plotData$popAdjustedDeaths * 1.0 + 10,  y = plotData$Country,
                                     text = paste(round(plotData$popAdjustedDeaths, 2)),
                                     font = list(family = 'Arial', size = 12),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    fig3
    
    fig <- subplot(fig1, fig2, fig3) 
    
    
    fig <- fig %>% layout(title = 'Nüfusa göre düzeltilmiş*', font = list(size=11),
                          legend = list(x = 0.009, y = 10.038,
                                        font = list(size = 12)),
                          margin = list(l = 100, r = 20, t = 70, b = 70),
                          paper_bgcolor = 'rgb(248, 248, 255)',
                          plot_bgcolor = 'rgb(248, 248, 255)')
    
    fig <- fig %>% add_annotations(xref = 'paper', yref = 'paper',
                                   x = 0, y = -0.15,
                                   text = paste('*Ülkelerin 1 milyon nüfustaki toplam vaka, iyileşen ve ölüm sıklıkları'),
                                   font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                                   showarrow = FALSE)
    
    fig%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
  })
  
  
  #### Ülke vaka karşılaştırması #####
  output$compareConfirmed <- renderPlot({
    options(scipen=999)
  compareData = comparedCountries()

            splitCompareData = split(compareData, compareData$Country)

            for(counts in 1:length(unique(compareData$Country))){

              if(max(splitCompareData[[counts]]$Confirmed) > input$firstCase){
                tmp = splitCompareData[[counts]]
                tmp2 = dplyr::filter(tmp, tmp$Confirmed >= input$firstCase)
                indx = which(splitCompareData[[counts]]$Date == tmp2$Date[1])-1
                splitCompareData[[counts]] = splitCompareData[[counts]][indx:nrow(splitCompareData[[counts]]),]
                # splitCompareData[[counts]]$Confirmed[[1]]  = input$firstCase
                splitCompareData[[counts]]$logConfirmed[[1]] = log(input$firstCase)
                splitCompareData[[counts]]$Days = 0:(nrow(data.frame(splitCompareData[[counts]]))-1)
              }else{

                splitCompareData[[counts]] = NA
              }

            }

            na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
            compareData = rbindlist(na.omit.list(splitCompareData))



            if(input$firstCase > 0){
              xlabel = paste0(input$firstCase,". Vaka Tespit Edildikten Sonra Geçen Gün")
            }else{
              xlabel = "Gün"
            }

            round.choose <- function(x, roundTo, dir = 1) {
              if(dir == 1) {  ##ROUND UP
                x + (roundTo - x %% roundTo)
              } else {
                if(dir == 0) {  ##ROUND DOWN
                  x - (x %% roundTo)
                }
              }
            }


        yValues = seq(input$firstCase,(max(compareData$Confirmed)+max(compareData$Confirmed)/8),(max(compareData$Confirmed)-input$firstCase)/5)
        roundValue = 5000
        if(max(yValues) <= 100){

          roundValue = 10

        }

        if(max(yValues) <= 1000 && max(yValues) > 100 ){

          roundValue = 100

        }

        if(max(yValues) <= 10000 && max(yValues) > 1000){

          roundValue = 1000

        }

        if(max(yValues) <= 100000 && max(yValues) > 10000){

          roundValue = 10000

        }


        if(max(yValues) > 1000000){

          roundValue = 50000

        }



        yValues = c(yValues[[1]],round.choose(yValues[-1]-yValues[[1]], roundTo = roundValue, 1))

        transform = ifelse(input$logTransform,"log", "identity")
        gtitle = ifelse(input$logTransform,"Ülkelerin Toplam Resmi Vaka Sayıları (Log)", "Ülkelerin Toplam Resmi Vaka Sayıları")


        if(input$logTransform){

          if("US" %in% input$countries){
          
          breakPoints = c(input$firstCase,input$firstCase+input$firstCase, input$firstCase+input$firstCase*4,
          input$firstCase+input$firstCase*9,input$firstCase+input$firstCase*19,input$firstCase+input$firstCase*49
          ,input$firstCase+input$firstCase*99,input$firstCase+input$firstCase*199,input$firstCase+input$firstCase*499
          ,input$firstCase+input$firstCase*999,input$firstCase+input$firstCase*2499,input$firstCase+input$firstCase*4999)

          }else{
            
            breakPoints = c(input$firstCase,input$firstCase+input$firstCase, input$firstCase+input$firstCase*4,
                            input$firstCase+input$firstCase*9,input$firstCase+input$firstCase*19,input$firstCase+input$firstCase*49
                            ,input$firstCase+input$firstCase*99,input$firstCase+input$firstCase*199,input$firstCase+input$firstCase*499
                            ,input$firstCase+input$firstCase*999, input$firstCase+input$firstCase*1499)
            
          }
          lim = c(min(compareData$Confirmed),max(compareData$Confirmed))

          }else{

            breakPoints = waiver()
            lim = c(input$firstCase,max(compareData$Confirmed))

          }


      p = ggplot(data = compareData, aes(x=Days, y=Confirmed)) + geom_line(aes(colour=Country),size = 1) +
          geom_point(aes(colour=Country), size=2) + ylab("Toplam Vaka") + xlab(xlabel)+
          scale_colour_discrete("Ülke")+ ggtitle(gtitle)+
          theme(text = element_text(size=14),legend.title=element_blank())+
          theme(legend.position="bottom") +scale_y_continuous(trans = transform,
                                                              breaks = breakPoints,
                                                               limits = lim)#+
        #scale_x_continuous(limits = c(0,50))


      if(input$trajectory){
        d = (input$firstCase)*2^(1:max(compareData$Days))
        d = c(input$firstCase, d[1:max(compareData$Days)])

        doublingEveryDay =  cbind.data.frame(Country = rep("2'ye katlanma (her gün)", length(d)), Confirmed = d,
                                             Days = 0:(length(d)-1))


        d2 = (input$firstCase)*1.4142137^(1:max(compareData$Days))
        d2 = c(input$firstCase, d2[1:max(compareData$Days)])

        doublingEveryTwoDays =  cbind.data.frame(Country = rep("2'ye katlanma (2 günde bir)", length(d2)), Confirmed = d2,
                                                 Days = 0:(length(d2)-1))


        d3 = (input$firstCase)*1.259921^(1:max(compareData$Days))
        d3 = c(input$firstCase, d3[1:max(compareData$Days)])

        doublingEveryThreeDays =  cbind.data.frame(Country = rep("2'ye katlanma (3 günde bir)", length(d3)), Confirmed = d3,
                                                   Days = 0:(length(d3)-1))


        d7 = (input$firstCase)*1.10408955^(1:max(compareData$Days))
        d7 = c(input$firstCase, d7[1:max(compareData$Days)])

        doublingEveryWeek =  cbind.data.frame(Country = rep("2'ye katlanma (haftada bir)", length(d7)), Confirmed = d7,
                                              Days = 0:(length(d7)-1))



        doublingData = plyr::rbind.fill(doublingEveryDay, doublingEveryTwoDays,doublingEveryThreeDays, doublingEveryWeek)


        p2 = p + geom_line(data =doublingData, aes(y=Confirmed, fill=Country),size=1, linetype="dashed")


        pg <- ggplot_build(p2)
        coordinates = pg$data[[3]]

        splitCoordinates = split(coordinates,coordinates$group)

        coordinateValues = matrix(NA,nrow=4,ncol=2)


        for(coord in 1:length(splitCoordinates)){

          df = splitCoordinates[[coord]][-c(9,10)]
          df2 = df[complete.cases(df),]

          coordinateValues[coord,1] = ifelse(input$logTransform, exp(max(df2$y)),max(df2$y))
          coordinateValues[coord,2] = max(df2$x)

        }

        if(input$highlightCountry){

        p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                      color="black")+
          annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                   color="black")+
          annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                   color="black")+
          annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                   color="black") +
          gghighlight(compareData$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)

        }else{

          p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                        color="black")+
            annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                     color="black")+
            annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                     color="black")+
            annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                     color="black")

        }
      }else{

          if(input$highlightCountry){
              p + gghighlight(compareData$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
          }else{

            p
          }
      }


  })

  #### Ülke ölüm karşılaştırması #####
  output$compareDeaths <- renderPlot({



      compareData = comparedCountries()

      splitCompareData = split(compareData, compareData$Country)

      for(counts in 1:length(unique(compareData$Country))){

        if(max(splitCompareData[[counts]]$Deaths) > input$firstDeath){

          tmp = splitCompareData[[counts]]
          tmp2 = dplyr::filter(tmp, tmp$Deaths >= input$firstDeath)
          indx = which(splitCompareData[[counts]]$Date == tmp2$Date[1])-1
          splitCompareData[[counts]] = splitCompareData[[counts]][indx:nrow(splitCompareData[[counts]]),]
          # splitCompareData[[counts]]$Deaths[[1]]  = input$firstDeath
          splitCompareData[[counts]]$logDeaths[[1]] = log(input$firstDeath)
          splitCompareData[[counts]]$Days = 0:(nrow(data.frame(splitCompareData[[counts]]))-1)

        }else{

          splitCompareData[[counts]] = NA
        }

      }

      na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
      compareData = rbindlist(na.omit.list(splitCompareData))




      round.choose <- function(x, roundTo, dir = 1) {
        if(dir == 1) {  ##ROUND UP
          x + (roundTo - x %% roundTo)
        } else {
          if(dir == 0) {  ##ROUND DOWN
            x - (x %% roundTo)
          }
        }
      }

      if(input$firstDeath > 0){
        xlabel = paste0(input$firstDeath,". Ölümden Sonra Geçen Gün")
      }else{
        xlabel = "Gün"
      }

      # }


        yValues = seq(input$firstDeath,(max(compareData$Deaths)+max(compareData$Deaths)/8),
                      (max(compareData$Deaths)-input$firstDeath)/5)

        if(max(yValues) <= 100){

          roundValue = 10

        }

        if(max(yValues) <= 1000 && max(yValues) > 100 ){

          roundValue = 100

        }

        if(max(yValues) <= 10000 && max(yValues) > 1000){

          roundValue = 1000

        }

        if(max(yValues) <= 100000 && max(yValues) > 10000){

          roundValue = 10000

        }


        if(max(yValues) > 1000000){

          roundValue = 50000

        }



        yValues = c(yValues[[1]],round.choose(yValues[-1]-yValues[[1]], roundTo = roundValue, 1))

        transform = ifelse(input$logTransform,"log", "identity")
        gtitle = ifelse(input$logTransform,"Ülkelerin Toplam Resmi Ölüm Sayıları (Log)", "Ülkelerin Toplam Resmi Ölüm Sayıları")


        if(input$logTransform){

          breakPoints = c(input$firstDeath,input$firstDeath+input$firstDeath, input$firstDeath+input$firstDeath*4,
                          input$firstDeath+input$firstDeath*9,input$firstDeath+input$firstDeath*19,input$firstDeath+input$firstDeath*49
                          ,input$firstDeath+input$firstDeath*99,input$firstDeath+input$firstDeath*199,input$firstDeath+input$firstDeath*499
                          ,input$firstDeath+input$firstDeath*999,input$firstDeath+input$firstDeath*1999,input$firstDeath+input$firstDeath*3999)
          lim = c(min(compareData$Deaths),max(compareData$Deaths))

        }else{

          breakPoints = waiver()
          lim = c(input$firstDeath,max(compareData$Deaths))

        }



      p =  ggplot(data = compareData, aes(x=Days, y=Deaths)) + geom_line(aes(colour=Country),size = 1) +
          geom_point(aes(colour=Country), size=2) +xlab(xlabel) + ylab("Toplam Ölüm") +
          scale_colour_discrete("Ülke")+ ggtitle(gtitle)+
          theme(text = element_text(size=14),legend.title=element_blank())+
          theme(legend.position="bottom") +
          scale_y_continuous(trans = transform, breaks = breakPoints,
                             limits = lim)#+
        #scale_x_continuous(limits = c(0,50))




      if(input$trajectory){

      d = (input$firstDeath)*2^(1:max(compareData$Days))
      d = c(input$firstDeath, d[1:max(compareData$Days)])

      doublingEveryDay =  cbind.data.frame(Country = rep("2'ye katlanma (her gün)", length(d)), Confirmed = d,
                                           Days = 0:(length(d)-1))


      d2 = (input$firstDeath)*1.4142137^(1:max(compareData$Days))
      d2 = c(input$firstDeath, d2[1:max(compareData$Days)])

      doublingEveryTwoDays =  cbind.data.frame(Country = rep("2'ye katlanma (2 günde bir)", length(d2)), Confirmed = d2,
                                               Days = 0:(length(d2)-1))


      d3 = (input$firstDeath)*1.259921^(1:max(compareData$Days))
      d3 = c(input$firstDeath, d3[1:max(compareData$Days)])

      doublingEveryThreeDays =  cbind.data.frame(Country = rep("2'ye katlanma (3 günde bir)", length(d3)), Confirmed = d3,
                                                 Days = 0:(length(d3)-1))


      d7 = (input$firstDeath)*1.10408955^(1:max(compareData$Days))
      d7 = c(input$firstDeath, d7[1:max(compareData$Days)])

      doublingEveryWeek =  cbind.data.frame(Country = rep("2'ye katlanma (haftada bir)", length(d7)), Confirmed = d7,
                                            Days = 0:(length(d7)-1))



      doublingData = plyr::rbind.fill(doublingEveryDay, doublingEveryTwoDays,doublingEveryThreeDays, doublingEveryWeek)


      p2 = p + geom_line(data =doublingData, aes(y=Confirmed, fill=Country),size=1, linetype="dashed")


      pg <- ggplot_build(p2)
      coordinates = pg$data[[3]]

      splitCoordinates = split(coordinates,coordinates$group)

      coordinateValues = matrix(NA,nrow=4,ncol=2)


      for(coord in 1:length(splitCoordinates)){

        df = splitCoordinates[[coord]][-c(9,10)]
        df2 = df[complete.cases(df),]

        coordinateValues[coord,1] = ifelse(input$logTransform, exp(max(df2$y)),max(df2$y))
        coordinateValues[coord,2] = max(df2$x)

      }


      if(input$highlightCountry){
            p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                          color="black")+
              annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                       color="black")+
              annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                       color="black")+
              annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                       color="black")+
              gghighlight(compareData$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
      }else{

            p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                          color="black")+
              annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                       color="black")+
              annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                       color="black")+
              annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                       color="black")

      }

      }else{
          if(input$highlightCountry){

            p+gghighlight(compareData$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)

          }else{

            p

          }
      }






  })

  #### Ülke iyileşme karşılaştırması #####
#   output$compareRecovered <- renderPlot({
# 
# 
# 
#       compareData = comparedCountries()
#       maxLimit = max(compareData$Recovered)
#       splitCompareData = split(compareData, compareData$Country)
# 
#       for(counts in 1:length(unique(compareData$Country))){
# 
#         if(max(splitCompareData[[counts]]$Recovered) > input$firstRecover){
# 
#           tmp = splitCompareData[[counts]]
#           tmp2 = dplyr::filter(tmp, tmp$Recovered >= input$firstRecover)
#           indx = which(splitCompareData[[counts]]$Date == tmp2$Date[1])-1
#           splitCompareData[[counts]] = splitCompareData[[counts]][indx:nrow(splitCompareData[[counts]]),]
#           # splitCompareData[[counts]]$Recovered[[1]]  = input$firstRecover
#           splitCompareData[[counts]]$logConfirmed[[1]] = log(input$firstRecover)
#           splitCompareData[[counts]]$Days = 0:(nrow(data.frame(splitCompareData[[counts]]))-1)
# 
#         }else{
# 
#           splitCompareData[[counts]] = NA
#         }
# 
#       }
# 
#       na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
#       compareData = rbindlist(na.omit.list(splitCompareData))
# 
#       round.choose <- function(x, roundTo, dir = 1) {
#         if(dir == 1) {  ##ROUND UP
#           x + (roundTo - x %% roundTo)
#         } else {
#           if(dir == 0) {  ##ROUND DOWN
#             x - (x %% roundTo)
#           }
#         }
#       }
#       if(input$firstRecover > 0){
#         xlabel = paste0(input$firstRecover,". İyileşmeden Sonra Geçen Gün")
#       }else{
#         xlabel = "Gün"
#       }
# 
#       # }
# 
#         yValues = seq(input$firstRecover,(max(compareData$Recovered)+max(compareData$Recovered)/8),
#                       (max(compareData$Recovered)-input$firstRecover)/5)
# 
#         if(max(yValues) <= 100){
# 
#           roundValue = 10
# 
#         }
# 
#         if(max(yValues) <= 1000 && max(yValues) > 100 ){
# 
#           roundValue = 100
# 
#         }
# 
#         if(max(yValues) <= 10000 && max(yValues) > 1000){
# 
#           roundValue = 1000
# 
#         }
# 
#         if(max(yValues) <= 100000 && max(yValues) > 10000){
# 
#           roundValue = 10000
# 
#         }
# 
# 
#         if(max(yValues) > 1000000){
# 
#           roundValue = 50000
# 
#         }
# 
# 
# 
#         yValues = c(yValues[[1]],round.choose(yValues[-1]-yValues[[1]], roundTo = roundValue, 1))
# 
#         transform = ifelse(input$logTransform,"log", "identity")
#         gtitle = ifelse(input$logTransform,"Ülkelerin Toplam Resmi İyileşme Sayıları (Log)", "Ülkelerin Toplam Resmi İyileşme Sayıları")
# 
#         if(input$logTransform){
# 
#           breakPoints = c(input$firstRecover,input$firstRecover+input$firstRecover, input$firstRecover+input$firstRecover*4,
#                           input$firstRecover+input$firstRecover*9,input$firstRecover+input$firstRecover*19,input$firstRecover+input$firstRecover*49
#                           ,input$firstRecover+input$firstRecover*99,input$firstRecover+input$firstRecover*199,input$firstRecover+input$firstRecover*499
#                           ,input$firstRecover+input$firstRecover*999,input$firstRecover+input$firstRecover*1999
# 
#                           )
# 
#           lim = c(min(compareData$Recover),maxLimit)
# 
#         }else{
# 
#           breakPoints = waiver()
#           lim = c(input$firstRecover,maxLimit)
# 
#         }
# 
#        p =  ggplot(data = compareData, aes(x=Days, y=Recovered)) + geom_line(aes(colour=Country),size = 1) +
#           geom_point(aes(colour=Country), size=2) +  xlab(xlabel) + ylab("Toplam İyileşme") +
#           scale_colour_discrete("Ülke")+ ggtitle(gtitle)+
#           theme(text = element_text(size=14),legend.title=element_blank())+
#           theme(legend.position="bottom") +
#           scale_y_continuous(trans = transform, breaks = breakPoints,
#                              limits = c(input$firstRecover,maxLimit))+
#          scale_x_continuous(limits = c(0,50))
# 
# 
#        if(input$trajectory){
#        d = (input$firstRecover)*2^(1:max(compareData$Days))
#        d = c(input$firstRecover, d[1:max(compareData$Days)])
# 
#        doublingEveryDay =  cbind.data.frame(Country = rep("2'ye katlanma (her gün)", length(d)), Confirmed = d,
#                                             Days = 0:(length(d)-1))
# 
# 
#        d2 = (input$firstRecover)*1.4142137^(1:max(compareData$Days))
#        d2 = c(input$firstRecover, d2[1:max(compareData$Days)])
# 
#        doublingEveryTwoDays =  cbind.data.frame(Country = rep("2'ye katlanma (2 günde bir)", length(d2)), Confirmed = d2,
#                                                 Days = 0:(length(d2)-1))
# 
# 
#        d3 = (input$firstRecover)*1.259921^(1:max(compareData$Days))
#        d3 = c(input$firstRecover, d3[1:max(compareData$Days)])
# 
#        doublingEveryThreeDays =  cbind.data.frame(Country = rep("2'ye katlanma (3 günde bir)", length(d3)), Confirmed = d3,
#                                                   Days = 0:(length(d3)-1))
# 
# 
#        d7 = (input$firstRecover)*1.10408955^(1:max(compareData$Days))
#        d7 = c(input$firstRecover, d7[1:max(compareData$Days)])
# 
#        doublingEveryWeek =  cbind.data.frame(Country = rep("2'ye katlanma (haftada bir)", length(d7)), Confirmed = d7,
#                                              Days = 0:(length(d7)-1))
# 
# 
# 
#        doublingData = plyr::rbind.fill(doublingEveryDay, doublingEveryTwoDays,doublingEveryThreeDays, doublingEveryWeek)
# 
# 
#        p2 = p + geom_line(data =doublingData, aes(y=Confirmed, fill=Country),size=1, linetype="dashed")
# 
# 
#        pg <- ggplot_build(p2)
#        coordinates = pg$data[[3]]
# 
#        splitCoordinates = split(coordinates,coordinates$group)
# 
#        coordinateValues = matrix(NA,nrow=4,ncol=2)
# 
# 
#        for(coord in 1:length(splitCoordinates)){
# 
#          df = splitCoordinates[[coord]][-c(9,10)]
#          df2 = df[complete.cases(df),]
# 
#          coordinateValues[coord,1] = ifelse(input$logTransform, exp(max(df2$y)),max(df2$y))
#          coordinateValues[coord,2] = max(df2$x)
# 
#        }
# 
# if(input$highlightCountry){
#        p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
#                      color="black")+
#          annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
#                   color="black")+
#          annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
#                   color="black")+
#          annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
#                   color="black")+
#          gghighlight(compareData$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
# 
# }else{
# 
#   p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
#                 color="black")+
#     annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
#              color="black")+
#     annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
#              color="black")+
#     annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
#              color="black")
# }
#        }else{
# 
#          if(input$highlightCountry){
#            p + gghighlight(compareData$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
#          }else{
# 
#            p
#          }
# 
#        }
# 
# 
# 
# 
#   })

  #### Ülke test karşılaştırması #####
  # output$testComparisonPlot <- renderPlot({
  # 
  #   ggplot(dataTest(), aes(x=Tests, y=Positive)) +
  #     geom_point() +
  #     ggrepel::geom_text_repel(aes(label = Country))+
  #     xlab("Test Sayısı") + ylab("Vaka Sayısı")+
  #     scale_y_continuous(breaks = seq(0,175000,25000))+
  #     scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Test Sayıları ve Vaka Sayıları")+
  #     theme(text = element_text(size=14),legend.title=element_blank())
  # 
  # 
  # })

  # #### Ülke milyon nüfusta test karşılaştırması #####
  # output$testComparisonPopAdjustedPlot <- renderPlot({
  # 
  #   ggplot(dataTest(), aes(x=Test_million_population, y=Positive)) +
  #     geom_point() +
  #     ggrepel::geom_text_repel(aes(label = Country))+
  #     xlab("Test Sayısı/1 Milyon Nüfus") + ylab("Vaka Sayısı")+
  #     scale_y_continuous(breaks = seq(0,175000,25000))+
  #     scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin 1 Milyon Nüfusta Test Sayıları ve Vaka Sayıları")+
  #     theme(text = element_text(size=14),legend.title=element_blank())
  # 
  # 
  # 
  # })

  #### Ülkelerin normalize edilmiş test/vaka karşılaştırması #####
  output$testComparisonPopTestAdjustedPlot <- renderPlot({

    ggplot(dataTest(), aes(x=Test_million_population, y=Positive_million_population)) +
      geom_point() +
      ggrepel::geom_text_repel(aes(label = Country))+
      xlab("Test Sıklığı (Milyonda)") + ylab("Vaka Sıklığı (Milyonda)")+
      scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Vaka ve Test Sıklığı Karşılaştırması")+
      theme(text = element_text(size=14),legend.title=element_blank())+ 
      geom_smooth(method='lm')



  })
  
  #### Ülkelerin normalize edilmiş test/vaka karşılaştırması (grafik) #####
  output$testComparisonPopTestAdjustedPlot2 <- renderPlotly({
    
    plotData = dataTest()
    
    
    fig1 <- plot_ly(x = plotData$Positive_million_population, y = ~reorder(plotData$Country, plotData$Positive_million_population), name = 'Vaka Sıklığı (Milyonda)',
                    type = 'bar', orientation = 'h', height = 700,
                    marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                  line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
    
    fig1 <- fig1 %>% layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                            xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
    fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                     x = plotData$Positive_million_population * 0.75 + 10,  y = plotData$Country,
                                     text = paste(round(plotData$Positive_million_population, 2)),
                                     font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    
    fig2 <- plot_ly(x = plotData$Test_million_population, y = ~reorder(plotData$Country, plotData$Positive_million_population), name = 'Test Sıklığı (Milyonda)',
                    type = 'bar', orientation = 'h') 
    
    fig2 <- fig2%>% layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = FALSE, domain= c(0, 0.85)),
                           xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
    
    
    fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'y',
                                     x = plotData$Test_million_population * 1.0 + 1150,  y = plotData$Country,
                                     text = paste(round(plotData$Test_million_population, 2)),
                                     font = list(family = 'Arial', size = 12),
                                     showarrow = FALSE)%>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    fig2
    
    fig <- subplot(fig1, fig2) 
    
    fig <- fig %>% layout(title = 'Vaka ve Test Sıklığı (Milyonda)*', font = list(size=11),
                          legend = list(x = 0.029, y = 1.038,
                                        font = list(size = 12)),
                          margin = list(l = 100, r = 20, t = 70, b = 70),
                          paper_bgcolor = 'rgb(248, 248, 255)',
                          plot_bgcolor = 'rgb(248, 248, 255)')
    
    fig <- fig %>% add_annotations(xref = 'paper', yref = 'paper',
                                   x = 0, y = -0.15,
                                   text = paste('*27 Nisan-1 Mayıs 2020 arasındaki veriler kullanılmıştır.'),
                                   font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                                   showarrow = FALSE)
    
    fig%>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
    
    
    
  })
  
  #### Ülkelerin vaka artış hızları #####
  output$growtRate <- renderPlot({
    
    data = dataWorld()[[1]][c("Country", "Confirmed", "Days")]
    
    
    splitData = split(data, data$Country)
    
    firstCase=input$firstCase
    
    meanGrowthCountries = list()
    
    for(counts in 1:length(splitData)){
      

      if(max(splitData[[counts]]$Confirmed) > firstCase){
        tmp = splitData[[counts]]
        tmp2 = dplyr::filter(tmp, tmp$Confirmed >= firstCase)
        
        if(nrow(tmp2) >= 14){
          
          if(splitData[[counts]]$Country[1] == "China"){
            
            indx = which(splitData[[counts]]$Days == tmp2$Days[1])-1
            
          }else{
            indx = which(splitData[[counts]]$Days == tmp2$Days[1])-2
          }
          splitData[[counts]] = splitData[[counts]][indx:nrow(splitData[[counts]]),]
          # splitData[[counts]]$Confirmed[[1]]  = firstCase
          
          splitData[[counts]]$Days = 0:(nrow(data.frame(splitData[[counts]]))-1)
          splitData[[counts]]$Growth = NA
          for(k in 2:nrow(splitData[[counts]])){
            
            splitData[[counts]]$Growth[k] = ((splitData[[counts]]$Confirmed[k]-splitData[[counts]]$Confirmed[k-1])/
                                               splitData[[counts]]$Confirmed[k-1])*100
            
          }
          
          splitData[[counts]] = splitData[[counts]][complete.cases(splitData[[counts]]),]
          
          meanGrowth = list()
          
          for(i in 1:(nrow(splitData[[counts]])-6)){
            
            j=i+6
            
            meanGrowth[[i]] = mean(splitData[[counts]][i:j, "Growth"], na.rm = TRUE)
            
            
          }
          
          meanGrowth =  do.call(rbind.data.frame, meanGrowth)
          colnames(meanGrowth) = "Mean_growth"
          
          growthMean = cbind.data.frame(Country = splitData[[counts]]$Country[1], Days =1:nrow(meanGrowth), meanGrowth)
          
          
        }else{
          
          growthMean = NULL
        }
        
      }else{
        
        growthMean = NULL
      }
      
      meanGrowthCountries[[counts]] = growthMean
    }
    
    
    newGrowthData = do.call(rbind.data.frame, meanGrowthCountries)
    head(newGrowthData)
    
    comparedCountries = input$countries
    
    newGrowthData2 = newGrowthData[as.character(newGrowthData$Country) %in% comparedCountries,]
    
    if(input$firstCase > 0){
      xlabel = paste0(input$firstCase,". Vaka Tespit Edildikten Sonra Geçen Gün")
    }else{
      xlabel = "Gün"
    }
    
    if(input$highlightCountry){
    ggplot(data = newGrowthData2, aes(x=Days, y=Mean_growth)) + geom_line(aes(colour=Country),size = 1) +
      geom_point(aes(colour=Country), size=2) + ylab("Ortalama Günlük Toplam Vaka Değişimi (%)") + xlab(xlabel)+
      scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Ortalama Haftalık Toplam Vaka Değişimi")+
      theme(text = element_text(size=14),legend.title=element_blank())+
      theme(legend.position="bottom") +
      #scale_x_continuous(limits = c(0,50))+
      gghighlight(newGrowthData2$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
      
    }else{
      
      ggplot(data = newGrowthData2, aes(x=Days, y=Mean_growth)) + geom_line(aes(colour=Country),size = 1) +
        geom_point(aes(colour=Country), size=2) + ylab("Ortalama Günlük Toplam Vaka Değişimi (%)") + xlab(xlabel)+
        scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Ortalama Günlük Toplam Vaka Değişimi (Önceki 7 Gün Ortalaması)")+
        theme(text = element_text(size=14),legend.title=element_blank())+
        theme(legend.position="bottom") #+
        #scale_x_continuous(limits = c(0,50))
      
    }
    

    
    
    
  })
  
  
  #### Ülkelerin haftalık yeni vaka artışları #####
  output$meanWeeklyNewCases <- renderPlot({
    
    data = dataWorld()[[1]][c("Country", "Confirmed", "Days")]
    
    splitData = split(data, data$Country)
    
    
    meanDailyCasesCountries = list()
    
    for(counts in 1:length(splitData)){
      

      
      tmp = splitData[[counts]]
      
      if(nrow(tmp) >= 14){
        
        indx = which(splitData[[counts]]$Days == tmp$Days[1])
        
        splitData[[counts]] = splitData[[counts]][indx:nrow(splitData[[counts]]),]
        
        splitData[[counts]]$Days = 0:(nrow(data.frame(splitData[[counts]]))-1)
        splitData[[counts]]$NewCases = NA
        for(k in 2:nrow(splitData[[counts]])){
          
          splitData[[counts]]$NewCases[k] = splitData[[counts]]$Confirmed[k]-splitData[[counts]]$Confirmed[k-1]
          
          
        }
        
        splitData[[counts]] = splitData[[counts]][complete.cases(splitData[[counts]]),]
        
        meanDailyCases = list()
        weeklyCases = list()
        
        for(i in 1:(nrow(splitData[[counts]])-6)){
          
          j=i+6
          
          meanDailyCases[[i]] = mean(splitData[[counts]][i:j, "NewCases"], na.rm = TRUE)
          weeklyCases[[i]] = sum(splitData[[counts]][i:j, "NewCases"], na.rm = TRUE)
          
        }
        
        meanDailyCases =  do.call(rbind.data.frame, meanDailyCases)
        weeklyCases =  do.call(rbind.data.frame, weeklyCases)
        
        colnames(meanDailyCases) = "Mean_Daily_New_Cases"
        colnames(weeklyCases) = "Weekly_New_Cases"
        
        dailyCasesMean = cbind.data.frame(Country = rep(as.character(splitData[[counts]]$Country[1]),nrow(meanDailyCases)), Days =0:(nrow(meanDailyCases)-1), meanDailyCases,weeklyCases)
        
        
      }else{
        
        dailyCasesMean = NULL
      }
      
      
      
      meanDailyCasesCountries[[counts]] = dailyCasesMean
    }
    
    
    newWeeklyData = do.call(rbind.data.frame, meanDailyCasesCountries)
    
    
    comparedCountries = input$countries
    
    newWeeklyData2 = newWeeklyData[as.character(newWeeklyData$Country) %in% comparedCountries,]
    newWeeklyData2$Country = as.character(newWeeklyData2$Country)
    
    splitNewWeeklyData = split(newWeeklyData2, newWeeklyData2$Country)
    weeklyNewData = list()
    
    for(compareCount in 1:length(splitNewWeeklyData)){
      
      
      tmp = splitNewWeeklyData[[compareCount]]
      tmp2 = dplyr::filter(tmp, tmp$Weekly_New_Cases >= input$weeklyNewCase)
      indx = which(splitNewWeeklyData[[compareCount]]$Days == tmp2$Days[1])-1
      splitNewWeeklyData[[compareCount]] = splitNewWeeklyData[[compareCount]][indx:nrow(splitNewWeeklyData[[compareCount]]),]
      splitNewWeeklyData[[compareCount]]$Days = 0:(nrow(data.frame(splitNewWeeklyData[[compareCount]]))-1)
      
      weeklyNewData[[compareCount]] = splitNewWeeklyData[[compareCount]]
    }
    
    newWeeklyData3 = do.call(rbind.data.frame, weeklyNewData)
    
    if(input$firstCase > 0){
      xlabel = paste0("Haftalık ",input$weeklyNewCase," Yeni Vaka Tespit Edildikten Sonra Geçen Gün")
    }else{
      xlabel = "Gün"
    }
    
    
    if(input$logTransform){
      
      breakPoints = c(input$firstCase,input$firstCase+input$firstCase, input$firstCase+input$firstCase*4,
                      input$firstCase+input$firstCase*9,input$firstCase+input$firstCase*19,input$firstCase+input$firstCase*49
                      ,input$firstCase+input$firstCase*99,input$firstCase+input$firstCase*199,input$firstCase+input$firstCase*499
                      ,input$firstCase+input$firstCase*999)
      
      
      
    }else{
      
      breakPoints = waiver()
      
    }
    
    transform = ifelse(input$logTransform,"log", "identity")
    gtitle = ifelse(input$logTransform,"Ülkelerin Toplam Resmi Vaka Sayıları (Log)", "Ülkelerin Toplam Resmi Vaka Sayıları")
    
    
    if(input$highlightCountry){
      ggplot(data = newWeeklyData3, aes(x=Days, y=Mean_Daily_New_Cases)) + geom_line(aes(colour=Country),size = 1) +
        geom_point(aes(colour=Country), size=2) + ylab("Haftalık Ortalama Yeni Vaka") + xlab(xlabel)+
        scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Haftalık Ortalama Yeni Vaka Değişimi")+
        theme(text = element_text(size=14),legend.title=element_blank())+
        theme(legend.position="bottom") + scale_y_continuous(trans = transform, breaks = breakPoints)+
        gghighlight(newWeeklyData3$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
      
    }else{
      
      ggplot(data = newWeeklyData3, aes(x=Days, y=Mean_Daily_New_Cases)) + geom_line(aes(colour=Country),size = 1) +
        geom_point(aes(colour=Country), size=2) + ylab("Haftalık Ortalama Yeni Vaka") + xlab(xlabel)+
        scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Haftalık Ortalama Yeni Vaka Değişimi")+
        theme(text = element_text(size=14),legend.title=element_blank())+
        theme(legend.position="bottom") + scale_y_continuous(trans = transform, breaks = breakPoints)
      
    }
    
    
  })
  
  
  #### Ülkelerin haftalık yeni ölüm artışları #####
  output$meanWeeklyNewDeaths <- renderPlot({
    
    
    data = dataWorld()[[1]][c("Country", "Deaths", "Days")]
    
    splitData = split(data, data$Country)
    
    
    meanDailyDeathsCountries = list()
    
    for(counts in 1:length(splitData)){
      

      
      tmp = splitData[[counts]]
      
      if(nrow(tmp) >= 14){
        
        indx = which(splitData[[counts]]$Days == tmp$Days[1])
        
        splitData[[counts]] = splitData[[counts]][indx:nrow(splitData[[counts]]),]
        
        splitData[[counts]]$Days = 0:(nrow(data.frame(splitData[[counts]]))-1)
        splitData[[counts]]$NewDeaths = NA
        for(k in 2:nrow(splitData[[counts]])){
          
          splitData[[counts]]$NewDeaths[k] = splitData[[counts]]$Deaths[k]-splitData[[counts]]$Deaths[k-1]
          
          
        }
        
        splitData[[counts]] = splitData[[counts]][complete.cases(splitData[[counts]]),]
        
        meanDailyDeaths = list()
        weeklyDeaths = list()
        
        for(i in 1:(nrow(splitData[[counts]])-6)){
          
          j=i+6
          
          meanDailyDeaths[[i]] = mean(splitData[[counts]][i:j, "NewDeaths"], na.rm = TRUE)
          weeklyDeaths[[i]] = sum(splitData[[counts]][i:j, "NewDeaths"], na.rm = TRUE)
          
        }
        
        meanDailyDeaths =  do.call(rbind.data.frame, meanDailyDeaths)
        weeklyDeaths =  do.call(rbind.data.frame, weeklyDeaths)
        
        colnames(meanDailyDeaths) = "Mean_Daily_New_Deaths"
        colnames(weeklyDeaths) = "Weekly_New_Deaths"
        
        dailyDeathsMean = cbind.data.frame(Country = rep(as.character(splitData[[counts]]$Country[1]),nrow(meanDailyDeaths)), Days =0:(nrow(meanDailyDeaths)-1), meanDailyDeaths,weeklyDeaths)
        
        
      }else{
        
        dailyDeathsMean = NULL
      }
      
      
      
      meanDailyDeathsCountries[[counts]] = dailyDeathsMean
    }
    
    
    newWeeklyData = do.call(rbind.data.frame, meanDailyDeathsCountries)
    
    
    comparedCountries = input$countries
    
    newWeeklyData2 = newWeeklyData[as.character(newWeeklyData$Country) %in% comparedCountries,]
    newWeeklyData2$Country = as.character(newWeeklyData2$Country)
    
    splitNewWeeklyData = split(newWeeklyData2, newWeeklyData2$Country)
    weeklyNewData = list()
    
    for(compareCount in 1:length(splitNewWeeklyData)){
      
      
      tmp = splitNewWeeklyData[[compareCount]]
      tmp2 = dplyr::filter(tmp, tmp$Weekly_New_Deaths >= input$weeklyDeath)
      indx = which(splitNewWeeklyData[[compareCount]]$Days == tmp2$Days[1])-1
      splitNewWeeklyData[[compareCount]] = splitNewWeeklyData[[compareCount]][indx:nrow(splitNewWeeklyData[[compareCount]]),]
      splitNewWeeklyData[[compareCount]]$Days = 0:(nrow(data.frame(splitNewWeeklyData[[compareCount]]))-1)
      
      weeklyNewData[[compareCount]] = splitNewWeeklyData[[compareCount]]
    }
    
    newWeeklyData3 = do.call(rbind.data.frame, weeklyNewData)
    
    if(input$firstCase > 0){
      xlabel = paste0("Haftalık ",input$weeklyDeath," Yeni Ölüm Tespit Edildikten Sonra Geçen Gün")
    }else{
      xlabel = "Gün"
    }
    
    
    if(input$logTransform){
      
      breakPoints = c(input$weeklyDeath,input$weeklyDeath+input$weeklyDeath, input$weeklyDeath+input$weeklyDeath*4,
                      input$weeklyDeath+input$weeklyDeath*9,input$weeklyDeath+input$weeklyDeath*19,input$weeklyDeath+input$weeklyDeath*49
                      ,input$weeklyDeath+input$weeklyDeath*99,input$weeklyDeath+input$weeklyDeath*199,input$weeklyDeath+input$weeklyDeath*499
                      ,input$weeklyDeath+input$weeklyDeath*999)
      
      
      
    }else{
      
      breakPoints = waiver()
      
    }
    
    transform = ifelse(input$logTransform,"log", "identity")
    
    
    if(input$highlightCountry){
      
      ggplot(data = newWeeklyData3, aes(x=Days, y=Mean_Daily_New_Deaths)) + geom_line(aes(colour=Country),size = 1) +
        geom_point(aes(colour=Country), size=2) + ylab("Haftalık Ortalama Yeni Ölüm") + xlab(xlabel)+
        scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Haftalık Ortalama Yeni Ölüm Değişimi")+
        theme(text = element_text(size=14),legend.title=element_blank())+
        theme(legend.position="bottom") + scale_y_continuous(trans = transform, breaks = breakPoints)+
        gghighlight(newWeeklyData3$Country == input$highlightCountries, keep_scales = TRUE, use_direct_label = TRUE)
      
    }else{
      
      ggplot(data = newWeeklyData3, aes(x=Days, y=Mean_Daily_New_Deaths)) + geom_line(aes(colour=Country),size = 1) +
        geom_point(aes(colour=Country), size=2) + ylab("Haftalık Ortalama Yeni Ölüm") + xlab(xlabel)+
        scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Haftalık Ortalama Yeni Ölüm Değişimi")+
        theme(text = element_text(size=14),legend.title=element_blank())+
        theme(legend.position="bottom") + scale_y_continuous(trans = transform, breaks = breakPoints)
      
    }
    
    
    
  })
  
  
  #### Ülkelerin raporlanan vakalarının yüzde kestirimi #####
  
  #### Tablo #####
  output$underReportingTable <- DT::renderDataTable({
    
    
    data = underReporting()
    
    data[data$country == "United States of America", ]$country = "US"
    data[data$country == "South Korea", ]$country = "Korea, South"
    
    
    
    data =data[data$country %in% input$countries,c(1,7)]
 
    colnames(data) = c("Ülke", "Tahmini Raporlanan Vaka (%95 Güven Aralığı) " )
      datatable(data, extensions = c('Buttons','KeyTable', 'Responsive'), rownames= FALSE,
                options = list(columnDefs = list(list(className = 'dt-center',targets='_all')), pageLength = 100, info = FALSE,bFilter = FALSE, paging = FALSE, 
                               dom = 'Bfrtip',buttons = list(list(extend = 'collection',
                                                                  buttons = c('csv', 'excel', 'pdf'),
                                               
                                                                                                                                               text = 'İndir')), keys = TRUE
      ))
      
    
  })
  
  
  #### Grafik ####
  output$underReportingPlot <- renderPlot({
    
    
    data = underReporting()
    
    data[data$country == "United States of America", ]$country = "US"
    data[data$country == "South Korea", ]$country = "Korea, South"
    
    
    data =data[data$country %in% input$countries,]
    
    data$country <- factor(data$country, levels = data$country[order(data$underreporting_estimate,decreasing = T)])
    data$underreporting_estimate = 100*data$underreporting_estimate
    data$lower = 100*data$lower
    data$upper = 100*data$upper
    
    ggplot(data, aes(x=country, y=underreporting_estimate)) + 
      geom_errorbar(aes(ymin=lower, ymax=upper),
                    width=0.1, size=1.1, col = "blue") +
      geom_point(col = "red") + coord_flip() +scale_y_continuous(limits = c(0,100))+
      ylab("Tahmini Vaka Raporlama Yüzdesi") + xlab("")+
      theme(text = element_text(size=14),legend.title=element_blank())+
      geom_hline(yintercept=0, linetype="dashed", size = 2)+
      geom_hline(yintercept=100, linetype="dashed", size = 2)
    
    
  })
  
  
 
#### Ülkelerin büyüme faktörleri ######
  output$growthFactor <- renderPlot({
    
    dataset = worldData3(0)
    data = dataset[[1]]

    
    splitData = split(data, data$Country)
    splitList = list()
    
    for(i in 1:length(splitData)){
      
      tmp = splitData[[i]]
      
      c = as.character(tmp$Country[[1]])

      tmp= tmp[order(as.Date(tmp$Date, format="%m/%d/%Y")),]
      
      if(nrow(tmp) > 0){
        tmp$newCases = NA
        tmp$growthFactor = NA
        
        for(j in 1:nrow(tmp)){
          # print(j)
          if(j < nrow(tmp)){
            tmp$newCases[[j+1]] = tmp$Confirmed[j+1]-tmp$Confirmed[j]
            tmp$growthFactor[j+1] =  (tmp$newCases[j+1])/tmp$newCases[j]
          }
          
        }
        
        splitList[[i]] = tmp
        
      }
    }
    
    growthFactorData = do.call(rbind.data.frame, splitList)
    

    splitTime = split(growthFactorData, data$Date)
    splitFull = list()
    
    
    for(i in 1:length(splitTime)){
      
      tmp = dplyr::filter(splitTime[[i]])
      tmp$dailyTotalCases = sum(splitTime[[i]]$Confirmed, na.rm = TRUE)
      tmp$dailyTotalDeaths = sum(splitTime[[i]]$Deaths, na.rm = TRUE)
      tmp$dailyTotalRecovered = sum(splitTime[[i]]$Recovered, na.rm = TRUE)
      splitFull[[i]] = tmp
    }
    
    growthFactorDataFull = do.call(rbind.data.frame, splitFull)
    
    
    comparedCountries = input$countries
    indx = which(growthFactorDataFull$Country %in% comparedCountries)
    compareData = as.data.frame(growthFactorDataFull[indx,])
    compareData$Country = as.character(compareData$Country)
    
    
    data = compareData[,c("Country", "Date", "growthFactor", "Confirmed", "newCases")]
    
    
    splitData = split(data, data$Country)
    listData = list()
    
    for(i in 1:length(splitData)){
      
      tmp = splitData[[i]]
      tmp2 = tmp[complete.cases(tmp),]
      
      tmp3 <- tmp2[is.finite(rowSums(tmp2[,3:4])),]
      tmp4 = tmp3[tmp3$growthFactor > 0 & tmp3$growthFactor < 5,]
      
      meanGrowth = list()
      tmp4$meanGrowth = NA
      
      for(j in 1:(nrow(tmp4)-6)){
        
        k=j+6
        
        tmp4$meanGrowth[k] = mean(tmp4[j:k, "growthFactor"], na.rm = TRUE)
        
        
      }
      
      listData[[i]] = tmp4[complete.cases(tmp4),]
      
    }
    
    fullData = do.call(rbind.data.frame, listData)
    fullData[fullData$Country == "France",]
    
    
    firstCase=1000
    breakPoints = c(firstCase/10,firstCase/10+firstCase/10, firstCase/10+(firstCase/10)*4,
                    2*firstCase, firstCase+firstCase*9,firstCase+firstCase*49
                    ,firstCase+firstCase*499)  
    
    p = ggplot(data = fullData, aes(x=Confirmed, y=meanGrowth)) + geom_line(aes(colour=Country),size = 1) +
      geom_point(aes(colour=Country), size=1) + 
      ylab("Haftalık Ortalama Büyüme Faktörü") + xlab("Toplam Vaka")+
      scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Haftalık Ortalama Büyüme Faktörü Değişimi")+
      theme(text = element_text(size=14),legend.title=element_blank())+
      scale_x_continuous(trans = "log", breaks = breakPoints) +
      scale_y_continuous(breaks = round(seq(0.80, 2.5, by = 0.1),3))+
      geom_hline(yintercept=1, linetype="dashed", size = 1)+
      theme(legend.position = "bottom")
    

    
    if(input$highlightCountry){
    
     p = p+
      gghighlight(fullData$Country == input$highlightCountries, keep_scales = FALSE, use_direct_label = TRUE)
    
      }
    
    
    return(p)
  })
  
  
######## Başlıklar ####################
  
  output$table1 <- renderText({

    "Bölüm 1. Türkiye'deki COVID-19 Vaka İstatistikleri"
  })

  output$table2 <- renderText({

    "Bölüm 2. Dünyadaki COVID-19 Vaka İstatistikleri"
  })


  
  output$table3 <- renderText({
    
    "Ülkelerin Tahmini Semptomatik Vaka Raporlama Yüzdeleri"
  })
  
  output$worldMapText <- renderText({
    
    "Dünya Haritası"
  })
  


}
