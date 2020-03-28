library(DT)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)

server <- function(input, output, session) {


  #### Veri yükleme ####
  
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
      pData$MaxConfirmed = max(pData$Confirmed)
      pData$MaxRecovered = max(pData$Recovered)
      pData$MaxDeaths = max(pData$Deaths)
      
      if(length(pData$Province.State) > length(unique(dataWorld$Date))){
        
        orderedData = setDT(pData)[order(Country.Region, Confirmed), .SD, .(Date)]
        orderedDataList = list()
        splitOrderedData = split(orderedData, orderedData$Date)
        
        for(j in 1:length(splitOrderedData)){
          
          orderedDataList[[j]] = cbind.data.frame(Province.State = NA,  
                                                  Country.Region = unique(splitOrderedData[[j]]$Country.Region),
                                                  Lat = unique(splitOrderedData[[j]]$Lat)[1],
                                                  Long = unique(splitOrderedData[[j]]$Long)[1],
                                                  Date = unique(splitOrderedData[[j]]$Date),
                                                  Confirmed = sum(splitOrderedData[[j]]$Confirmed),
                                                  Recovered = sum(splitOrderedData[[j]]$Recovered),
                                                  Deaths = sum(splitOrderedData[[j]]$Deaths),
                                                  MaxConfirmed = j,
                                                  MaxRecovered = j,
                                                  MaxDeaths = j,
                                                  Days = j
          )[1,]
          
        }
        
        tmpData = rbindlist(orderedDataList)
        tmpData$MaxConfirmed = max(tmpData$Confirmed)
        tmpData$MaxRecovered = max(tmpData$Recovered)
        tmpData$MaxDeaths = max(tmpData$Deaths)
        # if(input$firstCase){
          
          # tmpData = dplyr::filter(tmpData, tmpData$Confirmed >= input$firstCase)
          
        # }
        newWorldData[[i]] = tmpData
        
      }else{
        
        pData$Province.State = NA
        pData$Days = seq(1, length(unique(pData$Date)), 1)
        # if(input$firstCase){
          
          # pData = dplyr::filter(pData, pData$Confirmed >= input$firstCase)
          
        # }
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
  
  countries <-reactive({
    
    dataWorld()[[2]]
    
  })
    
  
  observe({
    
    if(input$compare){
      
      updateSelectizeInput(session, "countries", choices =  as.character(countries()), selected = c("Turkey", "Germany", "Italy", "Spain","France","Iran", "US", "Korea, South", "Japan", "Singapore"))
    }
    
  })
  
  dataset <- reactive({
    
    data <- read.table("www/data/covid_cases.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Tarih", "Toplam Vaka", "Yeni Vaka", "Toplam Ölüm", "Yeni Ölüm", "Toplam İyileşme", "Yeni İyileşme",  "Vaka Değişim Oranı (%)", "Toplam Test", "Yeni Test", "Yeni Vaka Tespit Etme Oranı (%)", "Time")
    return(data)
    
  })
  
  summaryData <- reactive({
    
    data <- read.table("www/data/summary.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Toplam Vaka", "Toplam Ölüm", "İyileşen Vaka", "Aktif Vaka", "Ölüm Oranı (%)", "Vaka Sayısı (Milyonda)")
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

    vaka = dataset()[,"Toplam Vaka"][8:nrow(dataset())]
    zaman = dataset()[,"Time"][8:nrow(dataset())]
    vaka_tespit_oran = dataset()[,"Yeni Vaka Tespit Etme Oranı (%)"][8:nrow(dataset())]
    
    expmodel <- lm(log(vaka)~ zaman+vaka_tespit_oran)
    
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
      
      xlimit = c(1, max(nrow(dataset()), max(times)))
      ylimit = c(1,max(max(predictions[,"fit"]), max(dataset()[,"Toplam Vaka"])))

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
    ylimit = c(1,max(dataset()[,"Toplam Test"]))

    plot.new()
    plot(1, type="n", xlab="Gün", ylab="Toplam Test", xlim=xlimit, ylim=ylimit, panel.first = grid(),
         main = "Toplam Resmi COVID-19 Test Sayısı")
    
    lines(seq(1:nrow(dataset())), dataset()[,"Toplam Test"], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")
    
    legend("topleft", legend=c("Test"),
           col=c("blue"), lty=1)
    

    
    
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
  
  output$compareConfirmed <- renderPlot({
    
    if(input$compare){
      
      compareData = comparedCountries()
      
            splitCompareData = split(compareData, compareData$Country)

            for(counts in 1:length(unique(compareData$Country))){

              if(max(splitCompareData[[counts]]$Confirmed) > input$firstCase){
              splitCompareData[[counts]] = dplyr::filter(splitCompareData[[counts]], splitCompareData[[counts]]$Confirmed >= input$firstCase)
              splitCompareData[[counts]]$Days = 1:nrow(data.frame(splitCompareData[[counts]]))
              }else{
                
                splitCompareData[[counts]] = NA
              }
              
            }

            na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
            compareData = rbindlist(na.omit.list(splitCompareData))      
            
            # }
            
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
        
      if(!input$logTransform){      
      population = FALSE
      if(!population){
        
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
        
        
        
        ggplot(data = compareData, aes(x=Days, y=Confirmed)) + geom_line(aes(colour=Country),size = 1) +
          xlab(xlabel) + ylab("Toplam Vaka") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Vaka Sayıları")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom") +
          scale_y_continuous(breaks = yValues, 
                             limits = c(input$firstCase,(max(compareData$Confirmed)+max(compareData$Confirmed)/8)))
        
        
        
      }else{
        
   
        
        ggplot(data = compareData, aes(x=Days, y=popAdjustedCase)) + geom_line(aes(colour=Country),size = 1) +
          xlab("Gün") + ylab("Toplam Vaka (Milyonda)") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Vaka Sayıları(Milyonda)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }
        
      }else{
        
        firstCase = input$firstCase
        if(input$firstCase < 1){firstCase = 0.5}
        
        yValues = seq(log(firstCase),(max(compareData$logConfirmed, na.rm = T)+max(compareData$logConfirmed, na.rm = T)/8),(max(compareData$logConfirmed, na.rm = T)-log(firstCase))/5)
        br = round(seq(min(yValues),max(yValues),2))
        yValues = round(exp(br),0)
        
        yValues2 = c(round.choose(yValues[[1]], roundTo = 10, 0),round.choose(yValues[-1]-yValues[[1]], roundTo = 10, 1))
        
        
        ggplot(data = compareData, aes(x=Days, y=logConfirmed)) + geom_line(aes(colour=Country),size = 1) +
          xlab(xlabel) + ylab("Toplam Vaka") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Vaka Sayıları (Logaritmik Ölçek)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom") + 
          scale_y_continuous(breaks = br,labels= yValues2)
        
      }
      
    
    }
    
    
  })
  
  output$compareDeaths <- renderPlot({
    
    if(input$compare){
      
      compareData = comparedCountries()
      
      splitCompareData = split(compareData, compareData$Country)
      
      for(counts in 1:length(unique(compareData$Country))){
        
        if(max(splitCompareData[[counts]]$Deaths) > input$firstDeath){
          splitCompareData[[counts]] = dplyr::filter(splitCompareData[[counts]], splitCompareData[[counts]]$Deaths >= input$firstDeath)
          splitCompareData[[counts]]$Days = 1:nrow(data.frame(splitCompareData[[counts]]))
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
      if(!input$logTransform){
      population = FALSE
      if(!population){
        


        
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
        
        
        
        ggplot(data = compareData, aes(x=Days, y=Deaths)) + geom_line(aes(colour=Country),size = 1) +
          xlab(xlabel) + ylab("Toplam Ölüm") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Ölüm Sayıları")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom") +
          scale_y_continuous(breaks = yValues, 
                             limits = c(input$firstDeath,(max(compareData$Deaths)+max(compareData$Deaths)/8)))
        
        
        
      }else{
        
        ggplot(data = compareData, aes(x=Days, y=popAdjustedCase)) + geom_line(aes(colour=Country),size = 1) +
          xlab("Gün") + ylab("Toplam Ölüm (Milyonda)") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Ölüm Sayıları(Milyonda)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }}else{
        
        
        firstDeath = input$firstDeath
        if(input$firstDeath < 1){firstDeath = 0.5}
        
        yValues = seq(log(firstDeath),(max(compareData$logDeaths, na.rm = T)+max(compareData$logDeaths, na.rm = T)/8),(max(compareData$logDeaths, na.rm = T)-log(firstDeath))/5)
        br = round(seq(min(yValues),max(yValues),2))
        yValues = round(exp(br),0)
        
        yValues2 = c(round.choose(yValues[[1]], roundTo = 10, 0),round.choose(yValues[-1]-yValues[[1]], roundTo = 10, 1))
        
        
        ggplot(data = compareData, aes(x=Days, y=logDeaths)) + geom_line(aes(colour=Country),size = 1) +
          xlab(xlabel) + ylab("Toplam Ölüm") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Ölüm Sayıları (Logaritmik Ölçek)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom") + 
          scale_y_continuous(breaks = br,labels= yValues2)
        
      }
      
      
    }
    
    
  })
  
  output$compareRecovered <- renderPlot({
    
    if(input$compare){
      
      compareData = comparedCountries()
      
      splitCompareData = split(compareData, compareData$Country)
      
      for(counts in 1:length(unique(compareData$Country))){
        
        if(max(splitCompareData[[counts]]$Recovered) > input$firstRecover){
          splitCompareData[[counts]] = dplyr::filter(splitCompareData[[counts]], splitCompareData[[counts]]$Recovered >= input$firstRecover)
          splitCompareData[[counts]]$Days = 1:nrow(data.frame(splitCompareData[[counts]]))
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
      if(!input$logTransform){
      population = FALSE
      if(!population){
        

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
        
        
        
        ggplot(data = compareData, aes(x=Days, y=Recovered)) + geom_line(aes(colour=Country),size = 1) +
          xlab(xlabel) + ylab("Toplam İyileşme") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi İyileşme Sayıları")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom") +
          scale_y_continuous(breaks = yValues, 
                             limits = c(input$firstRecover,(max(compareData$Recovered)+max(compareData$Recovered)/8)))
        
        
        
      }else{
        
        ggplot(data = compareData, aes(x=Days, y=popAdjustedCase)) + geom_line(aes(colour=Country),size = 1) +
          xlab("Gün") + ylab("Toplam İyileşme (Milyonda)") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi İyileşme Sayıları(Milyonda)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }}else{
        
        
        firstRecover = input$firstRecover
        if(input$firstRecover < 1){firstRecover = 0.5}
        
        yValues = seq(log(firstRecover),(max(compareData$logRecovered, na.rm = T)+max(compareData$logRecovered, na.rm = T)/8),(max(compareData$logRecovered, na.rm = T)-log(firstRecover))/5)
        br = round(seq(min(yValues),max(yValues),2))
        yValues = round(exp(br),0)
        
        yValues2 = c(round.choose(yValues[[1]], roundTo = 10, 0),round.choose(yValues[-1]-yValues[[1]], roundTo = 10, 1))
        
        
        
        ggplot(data = compareData, aes(x=Days, y=logRecovered)) + geom_line(aes(colour=Country),size = 1) +
          xlab(xlabel) + ylab("Toplam İyileşme") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi İyileşme Sayıları (Logaritmik Ölçek)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")  + 
          scale_y_continuous(breaks = br,labels= yValues2)
        
      }
      
      
    }
    
    
  })
  
  
  output$table1 <- renderText({
    
    "Bölüm 1. Türkiye'deki COVID-19 Vaka İstatistikleri"
  })
  
  output$table2 <- renderText({
    
    "Bölüm 2. Dünyadaki COVID-19 Vaka İstatistikleri"
  })
  
  
}