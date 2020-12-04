library(dplyr)
library(plyr)
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
library(zoo)
library(xts)
library(lubridate)
library(urca)
install.packages("dygraphs")
library(dygraphs)
install.packages("quantmod")
library(quantmod)
library(ggplot2)
library(forecast)

TS_Portfel <- cbind.data.frame(prices$X, portfel_time_series)
TS_Portfel
colnames(TS_Portfel) <- c("date", "value")
TS_Portfel$date <- as.character(TS_Portfel$date)
TS_Portfel$date <- as.Date(TS_Portfel$date, format = "%m-%d-%Y")
TS_Portfel_1 <- xts(x = TS_Portfel$value, order.by = TS_Portfel$date)

?chartSeries
chartSeries(TS_Portfel_1, name = "Временной ряд стоимости портфеля в Steam")

Plot_TS <- dygraph(TS_Portfel_1) %>%
  dyOptions( fillGraph=TRUE, colors = "red")
Plot_TS
?tsdisplay
tsdisplay(TS_Portfel_1, main = "Временной ряд стоимости портфеля в Steam")
###ARIMA
our_model<-auto.arima(TS_Portfel_1)
summary(our_model)
prognoz<-forecast(our_model, h=365, robust = TRUE, find.frequency = TRUE, allow.multiplicative.trend = TRUE, model = Arima)
prognoz$Forecasts
autoplot(prognoz, main = "Прогноз стоимости портфеля в Steam через год", ylab = "Стоимость портфеля в долларах США", xlab = "Период")
###GARCH

library(fGarch)

library(rugarch)

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
