worldData2 <- function(filter){
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
        
        
        if(c == "China"){
          
          earlyChina = cbind.data.frame(Province.State = c(NA,NA,NA,NA),
                                        Country.Region = c("China","China","China","China"),
                                        Lat = c(37.8099,37.8099,37.8099,37.8099),
                                        Long = c(101.0583,101.0583,101.0583,101.0583),
                                        Date = as.factor(c("1/18/20","1/19/20","1/20/20","1/21/20")),
                                        Confirmed = c(80, 216,235,386),
                                        Recovered = c(0,0,0,0),
                                        Deaths =  c(0,0,0,6),
                                        MaxConfirmed = c(max(tmpData$Confirmed, na.rm = TRUE),max(tmpData$Confirmed, na.rm = TRUE),max(tmpData$Confirmed, na.rm = TRUE),max(tmpData$Confirmed, na.rm = TRUE)),
                                        MaxRecovered = c(max(tmpData$Recovered, na.rm = TRUE),max(tmpData$Recovered, na.rm = TRUE),max(tmpData$Recovered, na.rm = TRUE),max(tmpData$Recovered, na.rm = TRUE)),
                                        MaxDeaths = c(max(tmpData$Deaths, na.rm = TRUE),max(tmpData$Deaths, na.rm = TRUE),max(tmpData$Deaths, na.rm = TRUE),max(tmpData$Deaths, na.rm = TRUE)),
                                        Days = c(1,1,1,1))
          
          tmpData = rbind.data.frame(earlyChina, tmpData)
          
        }
        
        
        newWorldData[[i]] = tmpData
        
      }else{
        
        pData$Province.State = NA
        pData$Days = seq(1, length(unique(pData$Date)), 1)
        pData=pData[order(as.Date(pData$Date, format="%m/%d/%Y")),]
        
        if(c == "Turkey"){
          
          
          insertRow <- function(existingDF, newrow, r) {
            existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
            existingDF[r,] <- newrow
            existingDF
          }
          
          missingTurkey = cbind.data.frame( Date = as.factor("3/21/20"),
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
          missingTurkey = missingTurkey[c("Province.State","Country.Region", "Lat","Long", "Date", "Confirmed", "Deaths", "Recovered", "MaxConfirmed",
                                          "MaxRecovered", "MaxDeaths")]
          pData = insertRow(pData, missingTurkey, r = nrow(pData[pData$Confirmed <= 1236,]))
          
          
          
        }
        
        newWorldData[[i]] = pData
        
      }
      
      
    }
    
    newWorldDataFull = rbindlist(newWorldData, use.names = TRUE, fill = TRUE)[,-"Province.State"]
    colnames(newWorldDataFull)[1] = "Country"
    newWorldDataFull$Recovered[is.na(newWorldDataFull$Recovered)] <- 0
    head(newWorldDataFull)
    

    newWorldDataFull = newWorldDataFull[-which(newWorldDataFull$Country == "Canada"),]
    
    newWorldDataFull = rbind.data.frame(newWorldDataFull, tmpCanada)
    
    splitData = split(newWorldDataFull,newWorldDataFull$Country)
    # splitDataList = list()
    
    for(counts in 1:length(splitData)){
      splitData[[counts]]$NewCases = NA
      splitData[[counts]]$NewDeaths = NA
      splitData[[counts]]$NewRecovered = NA
    
    for(k in 2:nrow(splitData[[counts]])){
      
      splitData[[counts]]$NewCases[k] = splitData[[counts]]$Confirmed[k]-splitData[[counts]]$Confirmed[k-1]
      splitData[[counts]]$NewDeaths[k] = splitData[[counts]]$Deaths[k]-splitData[[counts]]$Deaths[k-1]
      splitData[[counts]]$NewRecovered[k] = splitData[[counts]]$Recovered[k]-splitData[[counts]]$Recovered[k-1]
      
      
    }
      
    }

    newWorldDataFull = do.call(rbind.data.frame, splitData) 
    
    
    
    colnames(newWorldDataFull)[1] = "Country"
    
    dataCountries = newWorldDataFull[,c("Country", "MaxConfirmed", "MaxDeaths", "MaxRecovered", "NewCases", "NewDeaths", "NewRecovered")]
    
    
    
    splitData = split(dataCountries,dataCountries$Country)
    # splitDataList = list()
    
    for(counts in 1:length(splitData)){
      

        splitData[[counts]] = splitData[[counts]][nrow(splitData[[counts]]),]
    
      
    }
    
    dataCountries = do.call(rbind.data.frame, splitData) 
    
    dataCountries[dataCountries$NewRecovered<0,"NewRecovered"] = NA
    dataCountries[dataCountries$NewCases<0,"NewCases"] = NA
    dataCountries[dataCountries$NewDeaths<0,"NewDeaths"] = NA
    
    dataCountries = dataCountries[!duplicated(dataCountries), ]
    dataCountries$Country = as.character(dataCountries$Country)
    
    population = TRUE
    
    
    if(population){
      population <- read.table("www/data/population.txt", sep="\t", header=TRUE, comment.char="#",
                               na.strings=".", stringsAsFactors=FALSE,
                               quote="", fill=FALSE)
      
      population$Country = as.character(population$Country)
      
      population$Country[population$Country=="United States"] = "US"
      population$Country[population$Country=="Taiwan"] = "Taiwan*"
      population$Country[population$Country=="South Korea"] = "Korea, South"
      
      mergedData <- left_join(dataCountries, population, by = c("Country" ))
      mergedData=mergedData[complete.cases(mergedData),]
      
      mergedData$popAdjustedCase = round(1000000/(mergedData$Population/mergedData$MaxConfirmed),0)
      mergedData$popAdjustedDeaths = round(1000000/(mergedData$Population/mergedData$MaxDeaths),0)
      mergedData$popAdjustedRecovered = round(1000000/(mergedData$Population/mergedData$MaxRecovered),0)
      
    }
    
    mergedData = mergedData[c(1:7,19:21,9:13,15:18)]
    
    newWorldDataFull = dplyr::filter(newWorldDataFull, MaxConfirmed >= filter)
    
    countries = as.character(newWorldDataFull$Country)
    selectedCountries = dplyr::filter(dataCountries, dataCountries$MaxConfirmed>10000)
    countriesDisplayed = as.character(selectedCountries$Country)
    data = list(newWorldDataFull, countries, dataCountries, mergedData, countriesDisplayed)
    

    return(data)

}