library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(ggcorrplot)
library(forecast)
library(fpp)

setwd("C:/Users/SaidBahçeci/Desktop")
data = read.csv("IE360_Spring22_HW2_data.csv")
data$Quarter = as.yearqtr(data$Quarter, format = "%Y_Q%q")

tsdata = ts(data$Unleaded.Gasoline.Sale..UGS.[1:28],freq=4,start=c(2000,1))
tsdata
ts.plot(tsdata,xlab = "Year", ylab = "Sale",main=" Unleaded Gasoline Sales")

acf(tsdata)

data$trend = (1:32)
data$seasonality = rep(1:4,8)
data$seasonality = as.factor(data$seasonality)
data$lagged = c(NA)
for(i in 1:28){
    data$lagged[i+1] = data$Unleaded.Gasoline.Sale..UGS.[i]
}

fit1 = lm(Unleaded.Gasoline.Sale..UGS. ~.-Quarter,  data = data[1:28,])
summary(fit1)

wo_na_data = data[!is.na(data$Unleaded.Gasoline.Sale..UGS. & data$lagged),]
corr = cor(wo_na_data[,unlist(lapply(wo_na_data, is.numeric))])
ggcorrplot(corr,
           hc.order = TRUE,
           type='lower',
           lab=TRUE,
           lab_size = 2.5)

fit2 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - GNP.Agriculture, data = data[1:28,])
summary(fit2)

fit3 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - GNP.Agriculture - Price.of.Diesel.Gasoline..PG. , data = data[1:28,])
summary(fit3)

fit4 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG. - GNP.Agriculture - GNP.Total, data = data[1:28,])
summary(fit4)

fit5 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG. - GNP.Agriculture - GNP.Total - RNUV, data = data[1:28,])
summary(fit5)

fit6 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG. - GNP.Agriculture - GNP.Total - RNUV - GNP.Commerce, data = data[1:28,])
summary(fit6)

fit7 <- lm(Unleaded.Gasoline.Sale..UGS. ~. - Quarter - Price.of.Diesel.Gasoline..PG. - GNP.Agriculture - GNP.Total - RNUV - GNP.Commerce - X..LPG.Vehicles..NLPG., data = data[1:28,])
summary(fit7)

checkresiduals(fit7)
plot(fit7)

predictions = c()
for(i in 1:4){
  predictions[i] = predict(fit7,newdata=data[i+28,])
  if(i+28 < 32){
    data$lagged[i+29] = predictions[i]
  }
}
predictions
