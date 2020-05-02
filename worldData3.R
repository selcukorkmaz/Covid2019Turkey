worldData3 <- function(filter){
    # dataWorld = read.csv(url)
    
    confirmedURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
                    
    confirmed = read.csv(confirmedURL, header = TRUE, check.names = FALSE)
    
    confirmedData = confirmed %>%
      gather(Date, Confirmed, -c("Province/State", "Country/Region","Lat", "Long"))
    
    confirmedCanada = confirmedData[confirmedData$`Country/Region` == "Canada",]
    
    ##################################
    
    deathsURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    
    deaths = read.csv(deathsURL, header = TRUE, check.names = FALSE)
    
    deathsData = deaths %>%
      gather(Date, Deaths, -c("Province/State", "Country/Region","Lat", "Long"))
    
    deathsCanada = deathsData[deathsData$`Country/Region` == "Canada",]

    ##################################
    
    recoveredURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    
    recovered = read.csv(recoveredURL, header = TRUE, check.names = FALSE)
    head(recovered)
    
    recoveredData = recovered %>%
      gather(Date, Recovered, -c("Province/State", "Country/Region","Lat", "Long"))
    
    recoveredCanada = recoveredData[recoveredData$`Country/Region` == "Canada",]


    ##########################
    
    dataCanada = plyr::join_all(list(confirmedCanada,deathsCanada), by=c("Province/State","Country/Region",
                                                                         "Lat", "Long", "Date"), type='left')
    d = nrow(dataCanada)
    pData = dataCanada
    pData$Date = as.character(pData$Date)
    pData$MaxConfirmed = max(pData$Confirmed, na.rm = TRUE)
    pData$MaxDeaths = max(pData$Deaths, na.rm = TRUE)
    colnames(pData)[1:2] = c("Province.State", "Country.Region")
    
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
                                              MaxDeaths = j,
                                              Days = j
      )[1,]
      
    }
    
    tmpData = rbindlist(orderedDataList)
    tmpData$MaxConfirmed = max(tmpData$Confirmed, na.rm = TRUE)
    tmpData$MaxDeaths = max(tmpData$Deaths, na.rm = TRUE)
    tmpData$MaxRecovered = max(recoveredCanada$Recovered, na.rm = TRUE)
    tmpCanada=tmpData[order(as.Date(tmpData$Date, format="%m/%d/%Y")),]
    tmpCanada$Recovered = recoveredCanada$Recovered
    names(tmpCanada)[2] = "Country"
    
    tmpCanada = tmpCanada[,c("Country","Lat", "Long", "Date", "Confirmed", "Deaths", "Recovered", "MaxConfirmed",
                 "MaxRecovered", "MaxDeaths", "Days")]
    
    
    
    #############################################
    
    # dataWorld = plyr::join_all(list(confirmedData,deathsData,recoveredData), type='left')
    dataWorld = Reduce(function(x, y) merge(x, y, all=TRUE), list(confirmedData,deathsData,recoveredData))

    colnames(dataWorld)[1:2] = c("Province.State","Country.Region")
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
      pData$Date = as.character(pData$Date)
      pData$MaxConfirmed = max(pData$Confirmed, na.rm = TRUE)
      pData$MaxRecovered = max(pData$Recovered, na.rm = TRUE)
      pData$MaxDeaths = max(pData$Deaths, na.rm = TRUE)
      
      pData2 = pData[pData$Province.State =="",]
      
      if(nrow(pData2) > 1){
        
        pData = pData2
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
        tmpData=tmpData[order(as.Date(tmpData$Date, format="%m/%d/%Y")),]
  
        newWorldData[[i]] = tmpData
        
      }else{
        
        pData$Province.State = NA
        pData$Days = seq(1, length(unique(pData$Date)), 1)
        pData=pData[order(as.Date(pData$Date, format="%m/%d/%Y")),]
        

        
        newWorldData[[i]] = pData
        
      }
      
      
    }
    
    newWorldDataFull = rbindlist(newWorldData, use.names = TRUE, fill = TRUE)[,-"Province.State"]
    colnames(newWorldDataFull)[1] = "Country"
    newWorldDataFull$Recovered[is.na(newWorldDataFull$Recovered)] <- 0
    head(newWorldDataFull)
    

    newWorldDataFull = newWorldDataFull[-which(newWorldDataFull$Country == "Canada"),]
    
    newWorldDataFull = rbind.data.frame(newWorldDataFull, tmpCanada)
    

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
    
    colnames(newWorldDataFull)[1] = "Country"
    
    dataCountries = newWorldDataFull[,c("Country", "MaxConfirmed", "MaxDeaths", "MaxRecovered")]
    dataCountries = dataCountries[!duplicated(dataCountries), ]
    
    newWorldDataFull = dplyr::filter(newWorldDataFull, MaxConfirmed >= filter)
    
    countries = as.character(newWorldDataFull$Country)
    
    data = list(newWorldDataFull, countries, dataCountries)
    

    return(data)

}