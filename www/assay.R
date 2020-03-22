data =read.table("~/Documents/GitHub/Covid2019Turkey/www/data/covid_cases.txt", header = T,
                 sep = "\t")
data
colnames(data) = c("Tarih", "Toplam Vaka", "Yeni Vaka", "Toplam Ölüm", "Yeni Ölüm", "Time")

url = "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"

dataWorld = read.csv(url)
head(dataWorld)

conutries = data.frame(Country = unique(as.character(dataWorld$Country.Region)))

 dataWorld$Province.State = as.character(dataWorld$Province.State)
 dataWorld$Province.State = noquote(dataWorld$Province.State)
 head(dataWorld)
 
 

splitDataWorld = split(dataWorld, dataWorld$Country.Region)

countryNanmes = names(splitDataWorld)

for(i in 1:length(countryNanmes)){
  
  c = "US"#countryNanmes[i]
  d = nrow(splitDataWorld[[c]])
  pData = splitDataWorld[[c]]
  
  if(length(pData$Province.State) > length(unique(dataWorld$Date))){
    
    
    
  }
  
  splitDataWorld[[countryNanmes[i]]]$Days = seq(1, d, 1 )
  
  
  
}

dataWorldFull = data.table::rbindlist(splitDataWorld)

head(dataWorldFull)

comparedCountries = c("Turkey", "Germany")

indx = which(dataWorldFull$Country.Region %in% comparedCountries)

compareData = dataWorldFull[indx,]
head(compareData)



library(ggplot2)

ggplot(data = compareData, aes(x=Days, y=Confirmed)) + geom_line(aes(colour=Country.Region))

+ scale_x_date(date_labels = "%b/%Y")

plot.new()
plot(1, type="n", xlab="Gün", ylab="Toplam Vaka", xlim = c(1,120), ylim = c(1,1000), panel.first = grid(),
     main = "Koronavirüs Vakaları")

lines(compareData$Date, compareData$Confirmed, lwd=2, col = "blue", xlab = "Time (s)",
      ylab = "Counts")




y = data[,"Toplam Vaka"]
x = data[,"Time"]

expmodel <- lm(log(y) ~ x)
t=11
times <- seq(1,t, 1)

predictions = exp(predict.lm(expmodel,list(x = times), interval = "confidence"))


xlimit = c(1, max(nrow(data), max(times)))
ylimit = c(1,max(max(predictions), max(data[,"Toplam Vaka"])))
legendPosition = max(data[nrow(data),"Toplam Vaka"], max(predictions[,"fit"]))


plot.new()
plot(1, type="n", xlab="Gün", ylab="Toplam Vaka", xlim = c(1,10), ylim = c(1,200), panel.first = grid(),
     main = "Türkiye'deki Toplam Koronavirüs Vakaları")

lines(seq(1:nrow(data))[1:t], data[,"Toplam Vaka"][1:t], lwd=2, col = "blue", xlab = "Time (s)",
      ylab = "Counts")

lines(seq(1:nrow(data))[1:t], data[,"Toplam Ölüm"][1:t], lwd=2, col = "violet", xlab = "Time (s)",
      ylab = "Counts")



lines(times, predictions[,"fit"], lwd=2, col = "red", xlab = "Time (s)",
      ylab = "Counts")


legend(1, legendPosition, legend=c("Gözlenen", "Üstel model"),
       col=c("blue", "red"), lty=1)

lines(times, predictions[,"lwr"], lwd=2, col = "black", xlab = "Time (s)",
      ylab = "Counts")

lines(times, predictions[,"upr"], lwd=2, col = "black", xlab = "Time (s)",
      ylab = "Counts")



################
max(data[nrow(data),"Toplam Vaka"], max(predictions[,"fit"]))


preds = as.data.frame(formatC(predictions, digits = 0, format = "f"))
colnames(preds) = c("Kestirim","Alt limit", "Üst limit")
dim(data)
as.Date(data[nrow(data),"Tarih"])


data[(nrow(data)+1):nrow(preds),] <- NA



dim(preds)

cbind.data.frame(data[1:2], preds)

