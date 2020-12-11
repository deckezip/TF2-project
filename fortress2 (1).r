library(dplyr)
library(plyr)
library(zoo)
library(xts)
library(lubridate)
library(urca)
library(dygraphs)
library(quantmod)
library(ggplot2)
library(ggfortify)
library(forecast)
library(reshape2)
library(fGarch)
library(rugarch)
library(quantmod)

prices_new <- prices[,-1]
prices_new <- prices_new[colSums(prices_new) > 14300]

rate <- c(1:256)
for (i in 1:256) {
  rate[i] <- ((mean(prices_new[1400:1430,i])-mean(prices_new[1:30,i]))/mean(prices_new[1:30,i]))
}
rate_1<- c(1:256)
for (i in 1:256) {
  rate_1[i] <- ((median(prices_new[1400:1430,i])-median(prices_new[1:30,i]))/median(prices_new[1:30,i]))
}

plot(rate,1:256)
plot(rate_1,1:256)
plot(rate,rate_1)

df<-rbind(prices_new,rate_1)
for (i in 1:256) {if (rate_1[i] < 2) {df[,i]<-NA}}
df <- df[,colSums(is.na(df))<nrow(df)]
df1<-df
for (i in 1:12) {p[,i] = subset(sales, select=colnames(df1)[i])}

portfel<-c(1:12)
portfel_names<-colnames(df1)
portfel_names
portfel_prices<-df1[1430,]
portfel_prices
for (i in 1:12) {
  portfel[i]<-(mean(p[1:1430,i])*1.4125*df[1430,i])
}
portfel
sum(portfel)

portfel_old<-c(1:12)
for (i in 1:12) {
  portfel_old[i]<-(mean(p[1:1430,i])*1.4125*df[1065,i])
}
sum(portfel_old)


portfel_whole<-cbind(prices_new$Genuine.Grandmaster.*1,
                     prices_new$Strange.Professional.Killstreak.Original.*3,
                     prices_new$Strange.Festive.Huntsman.*1,
                     prices_new$Genuine.Robo.Sandvich.*1,
                     prices_new$Strange.Construction.PDA*6,
                     prices_new$Strange.Specialized.Killstreak.Original.*1,
                     prices_new$Strange.Frying.Pan.*2,
                     prices_new$Flip.Flops.*4,
                     prices_new$Unusual.Your.Worst.Nightmare.*1,
                     prices_new$Summer.Hat.*3,
                     prices_new$Strange.Invis.Watch.*7,
                     prices_new$Strange.Original.*4
                       )
portfel_time_series<-rowSums(portfel_whole)
portfel_total <- cbind(lapply(prices$X, as.character), portfel_whole, portfel_time_series)
colnames(portfel_total) <- c("Date", "Genuine Grandmaster", "Strange Professional Killstreak Original", "Strange Festive Huntsman", "Genuine Robo Sandvich", "Strange Construction PDA", "Strange Specialized Killstreak Original", "Strange Frying Pan", "Flip Flops", "Unusual Your Worst Nightmare", "Summer Hat", "Strange Invis Watch",  "Strange Original", "Total")
portfel_total
#Мы получили временной ряд стоимости 
###Строим модель анализа временного ряда
TS_Portfel <- cbind.data.frame(prices$X, portfel_time_series)
TS_Portfel
colnames(TS_Portfel) <- c("date", "value")
TS_Portfel$date <- as.character(TS_Portfel$date)
TS_Portfel$date <- as.Date(TS_Portfel$date, format = "%m-%d-%Y")
TS_Portfel_1 <- ts(TS_Portfel$value, frequency = 365, start = c(2017,1,1))
TS_Portfel_2 <- xts(x = TS_Portfel$value, order.by = TS_Portfel$date)

chartSeries(TS_Portfel_2, name = "Временной ряд стоимости портфеля в Steam")
dec <- decompose(TS_Portfel_1)
plot(TS_Portfel_1)

Plot_TS <- dygraph(TS_Portfel_2) %>%
  dyOptions( fillGraph=TRUE, colors = "red")
Plot_TS
?tsdisplay
tsdisplay(TS_Portfel_2, main = "Временной ряд стоимости портфеля в Steam")
###Декомпозиция временного ряда
dec <- decompose(TS_Portfel_2)
plot(dec)
###Динамики цен на отдельные товары

idate <- TS_Portfel$date
Items_TS <- cbind.data.frame(idate, prices_new$Genuine.Grandmaster.,
                                                  prices_new$Strange.Professional.Killstreak.Original.,
                                                  prices_new$Strange.Festive.Huntsman.,
                                                  prices_new$Genuine.Robo.Sandvich.,
                                                  prices_new$Strange.Construction.PDA,
                                                  prices_new$Strange.Specialized.Killstreak.Original.,
                                                  prices_new$Strange.Frying.Pan.,
                                                  prices_new$Flip.Flops.,
                                                  prices_new$Unusual.Your.Worst.Nightmare.,
                                                  prices_new$Summer.Hat.,
                                                  prices_new$Strange.Invis.Watch.,
                                                  prices_new$Strange.Original.
                                                  )
zitems <- read.zoo(Items_TS)
zitems
colnames(Items_TS) <- c("date", "Genuine Grandmaster", "Strange Professional Killstreak Original", "Strange Festive Huntsman", "Genuine Robo Sandvich", "Strange Construction PDA", "Strange Specialized Killstreak Original", "Strange Frying Pan", "Flip Flops", "Unusual Your Worst Nightmare", "Summer Hat", "Strange Invis Watch",  "Strange Original")
colnames(TS_Portfel_1) <- "Value"
autoplot(zitems, alpha = 0.7, facet = NULL, main = "Динамика цен на товары из портфеля")


###ARIMA
our_model <- auto.arima(TS_Portfel_1)
summary(our_model)
prognoz <- forecast(our_model, h=365, robust = TRUE, find.frequency = TRUE, allow.multiplicative.trend = TRUE, model = Arima)
prognoz
plot(prognoz, main = "Прогноз стоимости портфеля в Steam через год", ylab = "Стоимость портфеля в долларах США", xlab = "Период")

###Визуализация прогноза в виде датасета
prts <- fortify(prognoz, ts.connect = FALSE)
colnames(prts) <- c("Index", "Data", "Fitted", "Forecast", "L080", "Hi80", "Lo95", "Hi95")
prts <- subset(prts, select = -Index)

prts_1 <- cbind.data.frame(sq, prts)
colnames(prts_1) <- c("Date", "Data", "Fitted", "Forecast", "L080", "Hi80", "Lo95", "Hi95")
tail(prts_1)
zprts <- read.zoo(prts_1)

autoplot(zprts, alpha = 0.7, facet = NULL, main = "Прогноз стоимости портфеля в Steam (в долларах США)")
###GARCH
N<-1400
test<-TS_Portfel_1[(N+1):length(TS_Portfel_1)]
VaR<-rep(0,length(test))

for (i in (N+1):length(TS_Portfel_1) ){
  cat("\r", i-N, "of", length(TS_Portfel_1)-N)
  train<-TS_Portfel_1[(i-N):(i-1)]
  our_model_1<-garchFit(formula= ~arma(0,0)+garch(1,1), data=train, trace=FALSE)
  VaR[i-N]<-as.numeric(predict(our_model_1,1)[1]-1.96*predict(our_model_1,1)[3])
}
plot(test, type="l")
lines(VaR, col="red")
predict(our_model_1, data = train, 365, plot=TRUE, conf=0.01)
ugarchforecast(our_model_1,n.ahead=365)

# если бы мы составляли портфель в прошлом году
prices_past <- prices[1:1065,-1]
prices_past <- prices_past[colSums(prices_past) > 19650]
rate_past <- c(1:181)
for (i in 1:181) {
  rate_past[i] <- ((median(prices_past[1035:1065,i])-median(prices_past[1:30,i]))/median(prices_past[1:30,i]))
}
past<-rbind(prices_past,rate_past)
for (i in 1:181) {if (rate_past[i] < 0.75) {past[,i]<-NA}}
past <- past[,colSums(is.na(past))<nrow(past)]
p<-past[-1066,]
for (i in 1:8) {p[,i] = subset(sales, select=colnames(past)[i])}
p<-p[1:1065,]
portfel<-c(1:8)
portfel_names<-colnames(past)
portfel_names
portfel_prices<-past[1065,]
portfel_prices
for (i in 1:8) {portfel[i]<-(mean(p[1:1,i])*2.3*df[1065,i])}
portfel
sum(portfel)

portfel_past_year<-cbind(prices$Strange.Professional.Killstreak.Original*2,
                         prices$Strange.Festive.Huntsman.*11,
                         prices$Genuine.Robo.Sandvich.*10,
                         prices$Strange.Specialized.Killstreak.Original.*2,
                         prices$Strange.Frying.Pan.*5,
                         prices$Strange.Professional.Killstreak.Frying.Pan.*4,
                         prices$Strange.Specialized.Killstreak.Festive.Huntsman.*2,
                         prices$Strange.Original.*1,
                         prices$Strange.Professional.Killstreak.Conniver.s.Kunai.*4
)
portfel_past_year[1064,]
sum(portfel_past_year[1430,])
sum(portfel_past_year[1064,])
#portfel_past_year<-portfel_past_year[1064:1430,]
portfel_time_series_past_year<-rowSums(portfel_past_year)
portfel_total_past_year <- cbind(lapply(prices$X, as.character), portfel_past_year, portfel_time_series_past_year)
colnames(portfel_total_past_year) <- c("Date", "Strange Professional Killstreak Original", "Strange Festive Huntsman", 
                                       "Genuine Robo Sandvich", "Strange Specialized Killstreak Original", 
                                       "Strange Frying Pan", "Strange Professional Killstreak Frying Pan", 
                                       "Strange Specialized Killstreak Festive Huntsman", "Strange Original", "Total")
portfel_total_past_year
portfel_time_series_past_year

Sys.setlocale("LC_TIME", "C")
getSymbols(Symbols="AAPL", from ="2019-11-27", to = "2020-11-27")
head(AAPL)
2000/66.96*116.0300

z<-AAPL$AAPL.Close*2000/66.96
plot(z,main = "Временной ряд стоимости портфеля из акций Apple")
z<-AAPL$AAPL.Close
tsdisplay(z)
Plot_TS_past <- dygraph(portfel_time_series_past_year) %>%
  dyOptions( fillGraph=TRUE, colors = "red")
Plot_TS_past
tsdisplay(portfel_time_series_past_year)
chartSeries(portfel_time_series_past_year, name = "Временной ряд стоимости портфеля в Steam")

Plot_TS2 <- dygraph(portfel_time_series_past_year) %>%
  dyOptions( fillGraph=TRUE, colors = "red")
Plot_TS2
