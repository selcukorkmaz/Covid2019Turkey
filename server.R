library(DT)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)


server <- function(input, output, session) {


  #### Veri yükleme ####
  
  #### Dünya verileri ####
  dataWorld <-reactive({
    
    url = "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"

    dataWorld = read.csv(url)
    
    # confirmedURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    # 
    # confirmed = read.csv(confirmedURL, header = TRUE, check.names = FALSE)
    # 
    # confirmedData = confirmed %>%
    #   gather(Date, Confirmed, -c("Province/State", "Country/Region","Lat", "Long")) 
    # 
    # ##################################
    # 
    # deathsURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    # 
    # deaths = read.csv(deathsURL, header = TRUE, check.names = FALSE)
    # 
    # deathsData = deaths %>%
    #   gather(Date, Deaths, -c("Province/State", "Country/Region","Lat", "Long")) 
    # 
    # ##################################
    # 
    # recoveredURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    # 
    # recovered = read.csv(recoveredURL, header = TRUE, check.names = FALSE)
    # head(recovered)
    # 
    # recoveredData = recovered %>%
    #   gather(Date, Recovered, -c("Province/State", "Country/Region","Lat", "Long")) 
    # 
    # recoveredData[recoveredData$`Country/Region` == "Turkey",]
    # 
    # ##########################
    # 
    # dataWorld = plyr::join_all(list(confirmedData,deathsData,recoveredData), by=c("Province/State","Country/Region",
    #                                                                               "Lat", "Long", "Date"), type='left')
    # 
    
    dataWorld$Province.State = as.character(dataWorld$Province.State)
    dataWorld$Province.State = noquote(dataWorld$Province.State)
    head(dataWorld)
    
    splitDataWorld = split(dataWorld, dataWorld$Country.Region)
    
    countryNanmes = names(splitDataWorld)
    newWorldData = list()
    
    
    for(i in 1:length(countryNanmes)){
      
      print(i)
      
      c = countryNanmes[i]
      d = nrow(splitDataWorld[[c]])
      pData = splitDataWorld[[c]]
      pData$Date = as.character(pData$Date)
      pData$MaxConfirmed = max(pData$Confirmed, na.rm = TRUE)
      pData$MaxRecovered = max(pData$Recovered, na.rm = TRUE)
      pData$MaxDeaths = max(pData$Deaths, na.rm = TRUE)
      
      if(c == "Turkey"){
        
        
        insertRow <- function(existingDF, newrow, r) {
          existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
          existingDF[r,] <- newrow
          existingDF
        }
        
        missingTurkey = cbind.data.frame( Date = as.factor("2020-01-21"),
                                          Country.Region = c("Turkey"),
                                          Province.State = c(NA),
                                          Lat = c(38.9637),
                                          Long = c(35.2433),
                                          Confirmed = c(947),
                                          Recovered = c(0),
                                          Deaths =  c(21),
                                          MaxConfirmed = max(pData$Confirmed, na.rm = TRUE),
                                          MaxRecovered = max(pData$Recovered, na.rm = TRUE),
                                          MaxDeaths = max(pData$Deaths, na.rm = TRUE),
                                          Days = c(0))
        missingTurkey$Date = as.character(missingTurkey$Date)
        pData = insertRow(pData, missingTurkey, r = nrow(pData[pData$Confirmed <= 1236,]))
        
        
        
      }
      
      if((length(pData$Province.State) > length(unique(dataWorld$Date)))  && c != "Turkey"){
        
        orderedData = setDT(pData)[order(Country.Region, Confirmed), .SD, .(Date)]
        orderedDataList = list()
        splitOrderedData = split(orderedData, orderedData$Date)
        
        for(j in 1:length(splitOrderedData)){
          
          orderedDataList[[j]] = cbind.data.frame(Province.State = NA,  
                                                  Country.Region = unique(splitOrderedData[[j]]$Country.Region),
                                                  Lat = unique(splitOrderedData[[j]]$Lat)[1],
                                                  Long = unique(splitOrderedData[[j]]$Long)[1],
                                                  Date = unique(splitOrderedData[[j]]$Date),
                                                  Confirmed = sum(splitOrderedData[[j]]$Confirmed, na.rm = TRUE),
                                                  Recovered = sum(splitOrderedData[[j]]$Recovered, na.rm = TRUE),
                                                  Deaths = sum(splitOrderedData[[j]]$Deaths, na.rm = TRUE),
                                                  MaxConfirmed = j,
                                                  MaxRecovered = j,
                                                  MaxDeaths = j,
                                                  Days = j
          )[1,]
          
        }
        
        tmpData = rbindlist(orderedDataList)
        tmpData$MaxConfirmed = max(tmpData$Confirmed, na.rm = TRUE)
        tmpData$MaxRecovered = max(tmpData$Recovered, na.rm = TRUE)
        tmpData$MaxDeaths = max(tmpData$Deaths, na.rm = TRUE)

        
        if(c == "China"){
          
          earlyChina = cbind.data.frame(Province.State = c(NA,NA,NA,NA),
                                        Country.Region = c("China","China","China","China"),
                                        Lat = c(37.8099,37.8099,37.8099,37.8099),
                                        Long = c(101.0583,101.0583,101.0583,101.0583),
                                        Date = c("2020-01-18","2020-01-19","2020-01-20","2020-01-21"),
                                        Confirmed = c(80, 216,235,386),
                                        Recovered = c(0,0,0,0),
                                        Deaths =  c(0,0,0,6),
                                        MaxConfirmed = c(max(tmpData$Confirmed, na.rm = TRUE),max(tmpData$Confirmed, na.rm = TRUE),max(tmpData$Confirmed, na.rm = TRUE),max(tmpData$Confirmed, na.rm = TRUE)),
                                        MaxRecovered = c(max(tmpData$Recovered, na.rm = TRUE),max(tmpData$Recovered, na.rm = TRUE),max(tmpData$Recovered, na.rm = TRUE),max(tmpData$Recovered, na.rm = TRUE)),
                                        MaxDeaths = c(max(tmpData$Deaths, na.rm = TRUE),max(tmpData$Deaths, na.rm = TRUE),max(tmpData$Deaths, na.rm = TRUE),max(tmpData$Deaths, na.rm = TRUE)),
                                        Days = c(0,0,0,0))
          
          tmpData = rbind.data.frame(earlyChina, tmpData)
          
        }
        
        newWorldData[[i]] = tmpData
        
      }else{
        
        pData$Province.State = NA
        pData$Days = seq(1, length(unique(pData$Date)), 1)

        newWorldData[[i]] = pData
        
      }
      
      
    }
    
    newWorldDataFull = rbindlist(newWorldData, use.names = TRUE, fill = TRUE)[,-"Province.State"]
    colnames(newWorldDataFull)[2] = "Country"
    # newWorldDataFull$Recovered[is.na(newWorldDataFull$Recovered)] <- 0
    
    population = FALSE

    
    if(population){  
    population <- read.table("www/data/population.txt", sep="\t", header=TRUE, comment.char="#",
                             na.strings=".", stringsAsFactors=FALSE,
                             quote="", fill=FALSE)
    

    mergedData <- left_join(newWorldDataFull, population, by = c("Country" ))

    newWorldDataFull$popAdjustedCase = 1000000/(mergedData$Population/mergedData$Confirmed)
    newWorldDataFull$popAdjustedDeaths = 1000000/(mergedData$Population/mergedData$Deaths)
    newWorldDataFull$popAdjustedRecovered = 1000000/(mergedData$Population/mergedData$Recovered)
    
  }
    
    
    dataCountries = newWorldDataFull[,c("Country", "MaxConfirmed", "MaxDeaths", "MaxRecovered")]
    dataCountries = dataCountries[!duplicated(dataCountries), ]
    
    newWorldDataFull = dplyr::filter(newWorldDataFull, MaxConfirmed >= input$filter)
    
    countries = as.character(newWorldDataFull$Country)
    
    data = list(newWorldDataFull, countries, dataCountries)
    
    
    
    return(data)
  })
  
  ##### Ülke isimleri #####
  countries <-reactive({
    
    dataWorld()[[2]]
    
  })
    
  observe({
    
    
      
      updateSelectizeInput(session, "countries", choices =  as.character(countries()), selected = c("Turkey", "Germany", "Italy", "Spain","France","Iran", "US", "China"))
    
    
  })
  
  #### Türkiye tüm veriler ####
  dataset <- reactive({
    
    data <- read.table("www/data/covid_cases.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Tarih", "Toplam Vaka", "Yeni Vaka", "Toplam Ölüm", "Yeni Ölüm", 
                       "Toplam İyileşme", "Yeni İyileşme",  "Toplam Yoğun Bakım Hasta Sayısı",
                       "Toplam Entübe Hasta Sayısı", "Toplam Test", "Yeni Test",
                       "Vaka Değişim Oranı (%)", "Yeni Vaka Tespit Etme Oranı (%)", "Time")
    return(data)
    
  })
  
  #### Türkiye özet veriler ####
  summaryData <- reactive({
    
    data <- read.table("www/data/summary.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Toplam Vaka", "Toplam Ölüm", "İyileşen Vaka", "Aktif Vaka", "Ölüm Oranı (%)", "Vaka Sayısı (Milyonda)")
    return(data)
    
  })
  
  
  #### Dünya Test verileri ####
  dataTest <- reactive({
    
    test = read.table("www/data/tests.txt", header=FALSE, comment.char="#",
                      na.strings=".", stringsAsFactors=FALSE,
                      quote="", fill=TRUE, sep = "\t")
    
    colnames(test) = c("Country", "Tests", "Positive", "Date", "Test_million_population", "Positive_Thousand_Test", "Ref")
    
    head(test)
    test$Country
    
    splitTest = split(test, test$Country)
    testCountries = list()
    
    for(i in 1:length(splitTest)){
      
      df = splitTest[i]
      
      if(grepl(":", names(df))){
        
        df = NULL
        
        
      }
      
      else if(grepl("\\(", names(df))){
        
        df[[1]]$Country =  gsub("\\(.*","",names(df))
      }
      
      testCountries[i] = df
    }
    
    
    combinedTestCountries = data.table::rbindlist(testCountries)
    
    combinedTestCountries = combinedTestCountries[complete.cases(combinedTestCountries),]
    
    testCompared = c(" Australia", " Canada", " Denmark", " Italy", " Japan", " Norway",
                     " Portugal", " South Korea", " Switzerland"," Turkey",
                     " United Kingdom", " United States ", " Finland"," Poland" 
    )
    plotData = combinedTestCountries[combinedTestCountries$Country %in% testCompared,]
    
    return(plotData)
    
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
  
  #### Türkiye istatistikleri #####
  output$resultTable <- DT::renderDataTable({
    
    
    datatable(result(), extensions = c('Buttons','KeyTable', 'Responsive'), rownames= FALSE,options = list(pageLength = 100, 
      info = FALSE,bFilter = FALSE, paging = FALSE, dom = 'Bfrtip',buttons = list(list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),             
                                                          text = 'İndir')), keys = TRUE
    ))

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
  
  output$plotTotalCases <- renderPlot({
    
    if(!input$expModelPlot){

    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(dataset()[,"Toplam Vaka"], na.rm = TRUE))
    legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], na.rm = TRUE)
    
    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Vaka", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam Resmi COVID-19 Vakaları")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Vaka"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")

    legend("topleft", legend=c("Vaka"),
           col=c("blue"), lty=1)
    
    # if(input$totalDeaths){
    # 
    #     lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "violet", xlab = "Time (s)",
    #           ylab = "Counts")
    #     
    #     
    #     legend(1, legendPosition, legend=c("Vaka","Ölüm"),
    #            col=c("blue", "violet"), lty=1)
    #     
    # }
    
    }
    
    if(input$expModelPlot){
      
      
      times <- seq(1,nrow(dataset()), 1)
      predictions <- exp(predict(exponentialModel(),
                                 list(x=times),interval = "confidence"))
      
      xlimit = c(1, max(nrow(dataset()), max(times, na.rm = TRUE), na.rm = TRUE))
      ylimit = c(1,max(max(predictions[,"fit"], na.rm = TRUE), max(dataset()[,"Toplam Vaka"], na.rm = TRUE), na.rm = TRUE))

      plot.new()
      plot(1, type="n", xlab="Gün", ylab="Toplam Vaka", xlim=xlimit, ylim=ylimit, panel.first = grid(),
           main = "Toplam Resmi COVID-19 Vakaları")
      
      lines(seq(1:nrow(dataset())), dataset()[,"Toplam Vaka"], lwd=2, col = "blue", xlab = "Time (s)",
            ylab = "Counts")
      
      lines(times[8:nrow(dataset())], predictions[,"fit"], lwd=2, col = "red", xlab = "Time (s)",
            ylab = "Counts")
      

        # lines(times[8:nrow(dataset())], predictions[,"lwr"], lwd=2, col = "black", xlab = "Time (s)",
        #       ylab = "Counts")
        # 
        # lines(times[8:nrow(dataset())], predictions[,"upr"], lwd=2, col = "black", xlab = "Time (s)",
        #       ylab = "Counts")
        # 

        
        # legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], max(predictions[,"fit"]))
        
        legend("topleft", legend=c("Vaka", "Üstel model"),
               col=c("blue", "red"), lty=1)

  
      
      # if(input$totalDeaths){
      #   
      #   lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "violet", xlab = "Time (s)",
      #         ylab = "Counts")
      #   
      #   
      #   # legend(1, legendPosition, legend=c("Vaka","Ölüm", "Üstel model"),
      #   #        col=c("blue", "violet", "red"), lty=1)
      #   
      #   if(input$addCI){
      #     
      #     lines(times, predictions[,"lwr"], lwd=2, col = "black", xlab = "Time (s)",
      #           ylab = "Counts")
      #     
      #     lines(times, predictions[,"upr"], lwd=2, col = "black", xlab = "Time (s)",
      #           ylab = "Counts")
      #     
      #     legend(1, legendPosition, legend=c("Vaka", "Ölüm", "Üstel model", "Güven aralığı (%95)"),
      #            col=c("blue", "violet", "red", "black"), lty=1)
      #     
      #     legendPosition = max(dataset()[nrow(dataset()),"Toplam Vaka"], max(predictions[,"upr"]))
      #     
      #   }
      #   
      # }
      
    }
    
    
  })
  
  ##### Toplam Ölüm ve İyileşme Grafiği ####
  
  output$plotTotalDeatsRecovered <- renderPlot({
    
    
    
    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(max(dataset()[,"Toplam Ölüm"]), max(dataset()[,"Toplam İyileşme"])))
    legendPosition = max(max(dataset()[nrow(dataset()),"Toplam Ölüm"]), max(dataset()[,"Toplam İyileşme"]))
    
    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Sayı", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam Resmi COVID-19 Ölüm ve İyileşme Vakaları")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "red", xlab = "Time (s)",
          ylab = "Counts")
    

        lines(seq(1:nrow(dataset())), dataset()[,"Toplam İyileşme"], lwd=2, col = "blue", xlab = "Time (s)",
              ylab = "Counts")


        legend("topleft", legend=c("Ölüm","İyileşme"),
               col=c("red", "blue"), lty=1)

    
    
  })

  ##### log(Toplam Vaka) Grafiği ####
  
  output$logPlotTotalCases <- renderPlot({
    
      
      xlimit = c(1, nrow(dataset()))
      ylimit = c(0,log(max(dataset()[,"Toplam Vaka"])))
      legendPosition = max(log(dataset()[nrow(dataset()),"Toplam Vaka"]))
      
      plot.new()
      plot(1, type="n", xlab="Gün", ylab="log(Toplam Vaka)", xlim=xlimit, ylim=ylimit, panel.first = grid(),
           main = "Toplam Resmi COVID-19 Vakaları (logaritmik)")
      
      lines(seq(1:nrow(dataset())), log(dataset()[,"Toplam Vaka"]), lwd=2, col = "blue", xlab = "Time (s)",
            ylab = "Counts")
      
      
      legend(1, legendPosition, legend=c("log(Vaka)"),
             col=c("blue"), lty=1)
      
    
    
    
  })
  
  ##### Test vs Vaka #### 
  
  output$testVsCasePlot <- renderPlot({
    
    
    data = dataset()
    
    time <- data$Time
    newTest <- data$`Yeni Test`
    newCaseRatio <- data$`Yeni Vaka Tespit Etme Oranı (%)`
    
    par(mar=c(5, 4, 4, 6) + 0.1)
    
    ylimit = max(newTest, na.rm = TRUE)
    plot(time, newTest, axes=FALSE, ylim=c(0,ylimit), xlab="", ylab="",  panel.first = grid(),
         type="l",col="blue", main="Günlük Yeni Test Sayısı ve Yeni Vaka Tespit Oranı", lwd = 2)
    axis(2, ylim=c(0,ylimit),col="blue",col.axis="blue", las=1)  ## las=1 makes horizontal labels
    mtext("Yeni Test Sayısı",col="blue",side=2,line=3)
    box()
    
    
    par(new=TRUE)
    
    ylimit2 = max(newCaseRatio, na.rm = TRUE)
    plot(time, newCaseRatio,  xlab="", ylab="", ylim=c(0,ylimit2), 
         axes=FALSE, type="l", col="red", panel.first = grid(), lwd=2)
    mtext("Yeni Vaka Tespit Etme Oranı (%)",side=4,col="red",line=2.5) 
    axis(4, ylim=c(0,ylimit2), col="red",col.axis="red",las=2)
    
    axis(1,pretty(range(time),length(time)))
    mtext("Gün",side=1,col="black",line=2.5)  
    
    
  })
  
  ##### Toplam Ölüm Grafiği #### 
  plotTotalDeats <- renderPlot({
    
    
    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(dataset()[,"Toplam Ölüm"]))
    legendPosition = max(dataset()[nrow(dataset()),"Toplam Ölüm"])
    
    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Ölüm", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam COVID-19 Ölümleri")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Ölüm"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")
    
    
    legend(1, legendPosition, legend=c("Vaka"),
           col=c("blue"), lty=1)
    
    
    
    
  })
  
  ##### Toplam Test Grafiği ####
  
  output$plotTotalTests <- renderPlot({
    
    
    
    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(dataset()[,"Toplam Test"], na.rm = TRUE))

    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Test", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam Resmi COVID-19 Test Sayısı")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Test"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")
    
    legend("topleft", legend=c("Test"),
           col=c("blue"), lty=1)
    

    
    
  })
  
  ##### Toplam Yoğun Bakım ve Entübe Hasta Sayısı ####
  
  output$plotTotalICU <- renderPlot({
    

    
    xlimit = c(1, nrow(dataset()))
    ylimit = c(1,max(max(dataset()[,"Toplam Entübe Hasta Sayısı"]), max(dataset()[,"Toplam Yoğun Bakım Hasta Sayısı"])))

    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Sayı", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam Resmi Yoğun Bakım ve Entübe Hasta Sayısı")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Yoğun Bakım Hasta Sayısı"], lwd=2, col = "red", xlab = "Time (s)",
          ylab = "Counts")
    
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Entübe Hasta Sayısı"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")
    
    
    legend("topleft", legend=c("Yoğun Bakım","Entübe"),
           col=c("red", "blue"), lty=1)
    
    
    
  })
  
  ##### Günlük Yeni Vakalar ####
  
  output$barPlotNewCases <- renderPlot({
    
    barplot(dataset()[,"Yeni Vaka"]~seq(1, nrow(dataset()),1), 
            xlab = "Gün", ylab="Günlük Yeni Vaka Sayısı" ,main = "Günlük Yeni Vakalar", panel.first = grid())
    
    
  })
    
    

  ##### Günlük Yeni Ölümler ####
  
  output$barPlotNewDeaths <- renderPlot({
    
    barplot(dataset()[,"Yeni Ölüm"]~seq(1, nrow(dataset()),1), 
            xlab = "Gün", ylab="Günlük Yeni Ölüm Sayısı" ,main = "Günlük Yeni Ölümler", panel.first = grid())
    
    
  })
  
  
  ##### Günlük Yeni İyileşmeler ####
  
  output$barPlotNewRecovered <- renderPlot({
    
    barplot(dataset()[,"Yeni İyileşme"]~seq(1, nrow(dataset()),1), 
            xlab = "Gün", ylab="Günlük Yeni İyileşme Sayısı" ,main = "Günlük Yeni İyileşmeler", panel.first = grid())
    
    
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
    

    dataComparedCountries = dataWorld()[[3]][dataWorld()[[3]]$Country %in% input$countries,]
    colnames(dataComparedCountries) = c("Ülke", "Toplam Vaka", "Toplam Ölüm", "Toplam İyileşen")
    
    
    allConfirmed = sum(dataWorld()[[3]]$MaxConfirmed, na.rm=TRUE)
    allDeaths = sum(dataWorld()[[3]]$MaxDeaths, na.rm=TRUE)
    allRecovered = sum(dataWorld()[[3]]$MaxRecovered, na.rm=TRUE)
    
    world = cbind.data.frame("Global", allConfirmed, allDeaths, allRecovered)
    colnames(world) = c("Ülke", "Toplam Vaka", "Toplam Ölüm", "Toplam İyileşen")
    
    dataComparedCountries = as.data.frame(rbind.data.frame(world, dataComparedCountries))
    dataComparedCountries = dataComparedCountries[order(dataComparedCountries[,"Toplam Vaka"], decreasing = TRUE),]
    
    
    datatable(dataComparedCountries, 
              extensions = c('Buttons','KeyTable', 'Responsive'), 
              rownames= FALSE,options = list(pageLength = 100, 
                                                                                                           info = FALSE,bFilter = FALSE, paging = FALSE, dom = 'Bfrtip',buttons = list(list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),             
                                                                                                                                                                                            text = 'İndir')), keys = TRUE
    ))
    
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
          
          breakPoints = c(input$firstCase,input$firstCase+input$firstCase, input$firstCase+input$firstCase*4,
          input$firstCase+input$firstCase*9,input$firstCase+input$firstCase*19,input$firstCase+input$firstCase*49
          ,input$firstCase+input$firstCase*99,input$firstCase+input$firstCase*199,input$firstCase+input$firstCase*499
          ,input$firstCase+input$firstCase*999)
          
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
                                                               limits = lim)
        
        
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
          
          df = splitCoordinates[[coord]][-9]
          df2 = df[complete.cases(df),]
          
          coordinateValues[coord,1] = ifelse(input$logTransform, exp(max(df2$y)),max(df2$y))
          coordinateValues[coord,2] = max(df2$x)
          
        }
        
        
        p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                      color="black")+ 
          annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                   color="black")+
          annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                   color="black")+
          annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                   color="black")
      }else{
        
        p
        
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
                          ,input$firstDeath+input$firstDeath*999)
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
                             limits = lim)
        
        
        
      
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
        
        df = splitCoordinates[[coord]][-9]
        df2 = df[complete.cases(df),]
        
        coordinateValues[coord,1] = ifelse(input$logTransform, exp(max(df2$y)),max(df2$y))
        coordinateValues[coord,2] = max(df2$x)
        
      }
      
      
      p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                    color="black")+ 
        annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                 color="black")+
        annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                 color="black")+
        annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                 color="black")
      
      
      }else{
        
        
        p
      }
      
    
      
    
    
    
  })
  
  #### Ülke iyileşme karşılaştırması ##### 
  output$compareRecovered <- renderPlot({
    
    
      
      compareData = comparedCountries()
      maxLimit = max(compareData$Recovered)
      splitCompareData = split(compareData, compareData$Country)
      
      for(counts in 1:length(unique(compareData$Country))){
        
        if(max(splitCompareData[[counts]]$Recovered) > input$firstRecover){
          
          tmp = splitCompareData[[counts]]
          tmp2 = dplyr::filter(tmp, tmp$Recovered >= input$firstRecover)
          indx = which(splitCompareData[[counts]]$Date == tmp2$Date[1])-1
          splitCompareData[[counts]] = splitCompareData[[counts]][indx:nrow(splitCompareData[[counts]]),]
          # splitCompareData[[counts]]$Recovered[[1]]  = input$firstRecover
          splitCompareData[[counts]]$logConfirmed[[1]] = log(input$firstRecover)
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
      if(input$firstRecover > 0){
        xlabel = paste0(input$firstRecover,". İyileşmeden Sonra Geçen Gün")
      }else{
        xlabel = "Gün"
      }
      
      # }
      
        yValues = seq(input$firstRecover,(max(compareData$Recovered)+max(compareData$Recovered)/8),
                      (max(compareData$Recovered)-input$firstRecover)/5)
        
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
        gtitle = ifelse(input$logTransform,"Ülkelerin Toplam Resmi İyileşme Sayıları (Log)", "Ülkelerin Toplam Resmi İyileşme Sayıları")
        
        if(input$logTransform){
          
          breakPoints = c(input$firstRecover,input$firstRecover+input$firstRecover, input$firstRecover+input$firstRecover*4,
                          input$firstRecover+input$firstRecover*9,input$firstRecover+input$firstRecover*19,input$firstRecover+input$firstRecover*49
                          ,input$firstRecover+input$firstRecover*99,input$firstRecover+input$firstRecover*199,input$firstRecover+input$firstRecover*499
                          ,input$firstRecover+input$firstRecover*999,input$firstRecover+input$firstRecover*1999
                        
                          )
          
          lim = c(min(compareData$Recover),maxLimit)
          
        }else{
          
          breakPoints = waiver()
          lim = c(input$firstRecover,maxLimit)
          
        }

       p =  ggplot(data = compareData, aes(x=Days, y=Recovered)) + geom_line(aes(colour=Country),size = 1) +
          geom_point(aes(colour=Country), size=2) +  xlab(xlabel) + ylab("Toplam İyileşme") + 
          scale_colour_discrete("Ülke")+ ggtitle(gtitle)+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom") +
          scale_y_continuous(trans = transform, breaks = breakPoints,
                             limits = c(input$firstRecover,maxLimit))
        
       
       if(input$trajectory){
       d = (input$firstRecover)*2^(1:max(compareData$Days))
       d = c(input$firstRecover, d[1:max(compareData$Days)])
       
       doublingEveryDay =  cbind.data.frame(Country = rep("2'ye katlanma (her gün)", length(d)), Confirmed = d, 
                                            Days = 0:(length(d)-1))
       
       
       d2 = (input$firstRecover)*1.4142137^(1:max(compareData$Days))
       d2 = c(input$firstRecover, d2[1:max(compareData$Days)])
       
       doublingEveryTwoDays =  cbind.data.frame(Country = rep("2'ye katlanma (2 günde bir)", length(d2)), Confirmed = d2, 
                                                Days = 0:(length(d2)-1))
       
       
       d3 = (input$firstRecover)*1.259921^(1:max(compareData$Days))
       d3 = c(input$firstRecover, d3[1:max(compareData$Days)])
       
       doublingEveryThreeDays =  cbind.data.frame(Country = rep("2'ye katlanma (3 günde bir)", length(d3)), Confirmed = d3, 
                                                  Days = 0:(length(d3)-1))
       
       
       d7 = (input$firstRecover)*1.10408955^(1:max(compareData$Days))
       d7 = c(input$firstRecover, d7[1:max(compareData$Days)])
       
       doublingEveryWeek =  cbind.data.frame(Country = rep("2'ye katlanma (haftada bir)", length(d7)), Confirmed = d7, 
                                             Days = 0:(length(d7)-1))
       
       
       
       doublingData = plyr::rbind.fill(doublingEveryDay, doublingEveryTwoDays,doublingEveryThreeDays, doublingEveryWeek)
       
       
       p2 = p + geom_line(data =doublingData, aes(y=Confirmed, fill=Country),size=1, linetype="dashed") 
       
       
       pg <- ggplot_build(p2)
       coordinates = pg$data[[3]]
       
       splitCoordinates = split(coordinates,coordinates$group)
       
       coordinateValues = matrix(NA,nrow=4,ncol=2)
       
       
       for(coord in 1:length(splitCoordinates)){
         
         df = splitCoordinates[[coord]][-9]
         df2 = df[complete.cases(df),]
         
         coordinateValues[coord,1] = ifelse(input$logTransform, exp(max(df2$y)),max(df2$y))
         coordinateValues[coord,2] = max(df2$x)
         
       }
       
       
       p2 + annotate(geom="text", size=3, x=coordinateValues[1,2], y=coordinateValues[1,1], label="Her gün 2 katı",
                     color="black")+ 
         annotate(geom="text", size=3, x=coordinateValues[2,2], y=coordinateValues[2,1], label="2 günde 2 katı",
                  color="black")+
         annotate(geom="text", size=3, x=coordinateValues[3,2], y=coordinateValues[3,1], label="3 günde 2 katı",
                  color="black")+
         annotate(geom="text", size=3, x=coordinateValues[4,2], y=coordinateValues[4,1], label="Haftada 2 katı",
                  color="black")
       
       
       }else{
         
         p
         
       }
      
      
      

  })
  
  #### Ülke test karşılaştırması ##### 
  output$testComparisonPlot <- renderPlot({
    
    ggplot(dataTest(), aes(x=Tests, y=Positive)) +
      geom_point() + 
      ggrepel::geom_text_repel(aes(label = Country))+
      xlab("Test Sayısı") + ylab("Vaka Sayısı")+
      scale_y_continuous(breaks = seq(0,175000,25000))+
      scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Test Sayıları ve Vaka Sayıları")+
      theme(text = element_text(size=14),legend.title=element_blank())
    
    
  })
  
  #### Ülke milyon nüfusta test karşılaştırması ##### 
  output$testComparisonPopAdjustedPlot <- renderPlot({
    
    ggplot(dataTest(), aes(x=Test_million_population, y=Positive)) +
      geom_point() + 
      ggrepel::geom_text_repel(aes(label = Country))+
      xlab("Test Sayısı/1 Milyon Nüfus") + ylab("Vaka Sayısı")+
      scale_y_continuous(breaks = seq(0,175000,25000))+
      scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin 1 Milyon Nüfusta Test Sayıları ve Vaka Sayıları")+
      theme(text = element_text(size=14),legend.title=element_blank())
    
    
    
  })
  
  #### Ülkelerin normalize edilmiş test/vaka karşılaştırması ##### 
  output$testComparisonPopTestAdjustedPlot <- renderPlot({
    
    ggplot(dataTest(), aes(x=Test_million_population, y=Positive_Thousand_Test)) +
      geom_point() + 
      ggrepel::geom_text_repel(aes(label = Country))+
      xlab("Test Sayısı/1 Milyon Nüfus") + ylab("Vaka Sayısı/Bin Test")+
      scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Test Sayısı ve Vaka Sayısı Karşılaştırması")+
      theme(text = element_text(size=14),legend.title=element_blank())
    
    
    
  })
  
  output$table1 <- renderText({
    
    "Bölüm 1. Türkiye'deki COVID-19 Vaka İstatistikleri"
  })
  
  output$table2 <- renderText({
    
    "Bölüm 2. Dünyadaki COVID-19 Vaka İstatistikleri"
  })
  
  output$table2 <- renderText({
    
    "Bölüm 2. Dünyadaki COVID-19 Vaka İstatistikleri"
  })
  
  
  
}