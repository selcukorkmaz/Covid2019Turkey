library(DT)
library(data.table)
library(ggplot2)
library(dplyr)

server <- function(input, output, session) {


  #### Veri yükleme ####
  

  dataWorld <-reactive({
    
    
    url = "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"
    
    dataWorld = read.csv(url)
    head(dataWorld)
    
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
                                                  Days = j
          )[1,]
          
        }
        
        tmpData = rbindlist(orderedDataList)
        tmpData$MaxConfirmed = max(tmpData$Confirmed)
        if(input$firstCase){
          
          tmpData = dplyr::filter(tmpData, tmpData$Confirmed>0)
          
        }
        newWorldData[[i]] = tmpData
        
      }else{
        
        pData$Province.State = NA
        pData$Days = seq(1, length(unique(pData$Date)), 1)
        if(input$firstCase){
          
          pData = dplyr::filter(pData, pData$Confirmed>0)
          
        }
        newWorldData[[i]] = pData
        
      }
      
      
    }
    
    newWorldDataFull = rbindlist(newWorldData, use.names = TRUE, fill = TRUE)[,-"Province.State"]
    colnames(newWorldDataFull)[2] = "Country"
    newWorldDataFull$Recovered[is.na(newWorldDataFull$Recovered)] <- 0
    
  if(input$population){  
    newWorldDataFull = dplyr::filter(newWorldDataFull, MaxConfirmed >= input$filter)
    
    
    population <- read.table("www/data/population.txt", sep="\t", header=TRUE, comment.char="#",
                             na.strings=".", stringsAsFactors=FALSE,
                             quote="", fill=FALSE)
    

    mergedData <- left_join(newWorldDataFull, population, by = c("Country" ))

    newWorldDataFull$popAdjustedCase = 1000000/(mergedData$Population/mergedData$Confirmed)
    newWorldDataFull$popAdjustedDeaths = 1000000/(mergedData$Population/mergedData$Deaths)
    newWorldDataFull$popAdjustedRecovered = 1000000/(mergedData$Population/mergedData$Recovered)
    
  }
    
    countries = as.character(newWorldDataFull$Country)
    
    data = list(newWorldDataFull, countries)
    
    return(data)
  })
  
  countries <-reactive({
    
    dataWorld()[[2]]
    
  })
    
  
  observe({
    
    if(input$compare){
      
      updateSelectizeInput(session, "countries", choices =  as.character(countries()), selected = countries()[which(countries() == "Turkey")][1])
    }
    
  })
  
  dataset <- reactive({
    
    data <- read.table("www/data/covid_cases.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Tarih", "Toplam Vaka", "Yeni Vaka", "Toplam Ölüm", "Yeni Ölüm","Değişim (%)", "Toplam Test", "Yeni Test", "Yeni Vaka Tespit Etme Oranı (%)", "Time")
    return(data)
    
  })
  
  summaryData <- reactive({
    
    data <- read.table("www/data/summary.txt", header = TRUE, sep = "\t")
    colnames(data) = c("Toplam Vaka", "Toplam Ölüm", "Aktif Vaka", "Ölüm Oranı (%)", "Vaka Sayısı (Milyonda)")
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
         main = "Türkiye'deki Toplam Resmi COVID-19 Vakaları")
    
    lines(seq(1:nrow(dataset()))[1:input$expTime], dataset()[,"Toplam Vaka"][1:input$expTime], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")

    
    legend(1, legendPosition, legend=c("Vaka"),
           col=c("blue"), lty=1)
    
    if(input$totalDeaths){
    
        lines(seq(1:nrow(dataset()))[1:input$expTime], dataset()[,"Toplam Ölüm"][1:input$expTime], lwd=2, col = "violet", xlab = "Time (s)",
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
           main = "Türkiye'deki Toplam Resmi COVID-19 Vakaları")
      
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
        
        lines(seq(1:nrow(dataset()))[1:input$expTime], dataset()[,"Toplam Ölüm"][1:input$expTime], lwd=2, col = "violet", xlab = "Time (s)",
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
           main = "Türkiye'deki Toplam Resmi COVID-19 Vakaları (logaritmik)")
      
      lines(seq(1:nrow(dataset()))[1:input$expTime], log(dataset()[,"Toplam Vaka"][1:input$expTime]), lwd=2, col = "blue", xlab = "Time (s)",
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
         type="l",col="blue", main="Yeni Test Sayısı ve Yeni Vaka Tespit Oranı", lwd = 2)
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
         main = "Türkiye'deki Toplam COVID-19 Ölümleri")
    
    lines(seq(1:nrow(dataset()))[1:input$expTime], dataset()[,"Toplam Ölüm"][1:input$expTime], lwd=2, col = "blue", xlab = "Time (s)",
          ylab = "Counts")
    
    
    legend(1, legendPosition, legend=c("Vaka"),
           col=c("blue"), lty=1)
    
    
    
    
  })
  
  
  ##### Günlük Yeni Vakalar ####
  
  output$barPlotNewCases <- renderPlot({
    
    barplot(dataset()[,"Yeni Vaka"][1:input$expTime]~seq(1, nrow(dataset()),1)[1:input$expTime], 
            xlab = "Gün", ylab="Günlük Yeni Vaka Sayısı" ,main = "Günlük Yeni Vakalar", panel.first = grid())
    
    
  })
    
    

  ##### Günlük Yeni Ölümler ####
  
  output$barPlotNewDeaths <- renderPlot({
    
    barplot(dataset()[,"Yeni Ölüm"][1:input$expTime]~seq(1, nrow(dataset()),1)[1:input$expTime], 
            xlab = "Gün", ylab="Günlük Yeni Ölüm Sayısı" ,main = "Günlük Yeni Ölümler", panel.first = grid())
    
    
  })
  
  
  ##### Ülke Karşılaştırma ####
  
  output$compareConfirmed <- renderPlot({
    
    if(input$compare){
      
      comparedCountries = input$countries
      
      indx = which(dataWorld()[[1]]$Country %in% comparedCountries)
      
      compareData = as.data.frame(dataWorld()[[1]][indx,])
      
      if(input$firstCase){
        
        comparedCountries = input$countries
            compareData$Country = as.character(compareData$Country)
            
            splitCompareData = split(compareData, compareData$Country)
            
            for(counts in 1:length(unique(compareData$Country))){
              
          
              
              splitCompareData[[counts]]$Days = 1:nrow(data.frame(splitCompareData[[counts]]))
              
            }
            
            compareData = rbindlist(splitCompareData)
      }
        
      if(!input$population){
        ggplot(data = compareData, aes(x=Days, y=Confirmed)) + geom_line(aes(colour=Country),size = 1) +
          xlab("Gün") + ylab("Toplam Vaka") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Vaka Sayıları")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }else{
        
        ggplot(data = compareData, aes(x=Days, y=popAdjustedCase)) + geom_line(aes(colour=Country),size = 1) +
          xlab("Gün") + ylab("Toplam Vaka (Milyonda)") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Vaka Sayıları(Milyonda)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }
      
    
    }
    
    
  })
  
  output$compareDeaths <- renderPlot({
    
    if(input$compare){
      comparedCountries = input$countries
      
      indx = which(dataWorld()[[1]]$Country %in% comparedCountries)
      
      compareData = as.data.frame(dataWorld()[[1]][indx,])
      
      if(input$firstCase){
        
        comparedCountries = input$countries
        compareData$Country = as.character(compareData$Country)
        
        splitCompareData = split(compareData, compareData$Country)
        
        for(counts in 1:length(unique(compareData$Country))){
          
            
            splitCompareData[[counts]]$Days = 1:nrow(data.frame(splitCompareData[[counts]]))
          
        }
        
        compareData = rbindlist(splitCompareData)
      }
      
      if(!input$population){
      
      ggplot(data = compareData, aes(x=Days, y=Deaths)) + geom_line(aes(colour=Country),size = 1) +
         xlab("Gün") + ylab("Toplam Ölüm")  + 
        scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Ölüm Vaka Sayıları")+
        theme(text = element_text(size=14),legend.title=element_blank())+ 
        theme(legend.position="bottom")
      
      }else{
        
        ggplot(data = compareData, aes(x=Days, y=popAdjustedDeaths)) + geom_line(aes(colour=Country),size = 1) +
          xlab("Gün") + ylab("Toplam Ölüm (Milyonda)")  + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi Ölüm Vaka Sayıları (Milyonda)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }
      
    }
    
    
  })
  
  output$compareRecovered <- renderPlot({
    
    if(input$compare){
      comparedCountries = input$countries
      
      indx = which(dataWorld()[[1]]$Country %in% comparedCountries)
      
      compareData = as.data.frame(dataWorld()[[1]][indx,])
      
      if(input$firstCase){
        
        comparedCountries = input$countries
        compareData$Country = as.character(compareData$Country)
        
        splitCompareData = split(compareData, compareData$Country)
        
        for(counts in 1:length(unique(compareData$Country))){
          

            splitCompareData[[counts]]$Days = 1:nrow(data.frame(splitCompareData[[counts]]))
          
        }
        
        compareData = rbindlist(splitCompareData)
      }
      
      if(!input$population){
        ggplot(data = compareData, aes(x=Days, y=Recovered)) + geom_line(aes(colour=Country),size = 1)+
          xlab("Gün") + ylab("Toplam İyileşen") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi İyileşen Vaka Sayıları")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
          
      }else{
        
        ggplot(data = compareData, aes(x=Days, y=popAdjustedRecovered)) + geom_line(aes(colour=Country),size = 1)+
          xlab("Gün") + ylab("Toplam İyileşen (Milyonda)") + 
          scale_colour_discrete("Ülke")+ ggtitle("Ülkelerin Toplam Resmi İyileşen Vaka Sayıları (Milyonda)")+
          theme(text = element_text(size=14),legend.title=element_blank())+ 
          theme(legend.position="bottom")
        
      }
      
    }
    
    
  })
  
  
  
}