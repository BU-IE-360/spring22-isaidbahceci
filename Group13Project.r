require(readr)
require(tidyverse)
require(dplyr)
require(caTools)
require(rpart)
require(rpart.plot)
require(party)
require(rattle)
require(caret)
require(tree)
require(Metrics)
require(ROCR)
require(ISLR)
require(class)
require(data.table)
require(gridExtra)
require(readxl)
require(ggcorrplot)
require(forecast)
require(writexl)

weather <- fread("C://Users//SaidBahçeci//Documents//2022-06-02_weather.csv")
prod <- fread("C://Users//SaidBahçeci//Documents//2022-06-02_production.csv")

str(weather)
str(prod)

#transform data from long to wide
weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")


#adjust date format
weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
prod$date <- as.Date(prod$date, format = "%Y-%m-%d")


#merge weather and production
merged <- merge(weather, prod, by = c("date","hour"), all = T)
merged$hour <- as.factor(merged$hour)
show_na <-merged[is.na(merged$production) & !is.na(merged$CLOUD_LOW_LAYER_36.25_33), ]

# fill NA values with the mean of related last three days
for (i in 1:(nrow(merged) - 168)) {
  if(is.na(merged$production[i]))
    merged$production[i] = (merged$production[i-24] + merged$production[i-48] + merged$production[i-72])/3
}

#observe production = 0 hours
merged$date <- as.Date(merged$date)
for (i in c(0:5,20:23)){
  print(ggplot(merged[merged$hour==i,],aes(x=date,y=production)) +  geom_line()+
          xlab("Date") + ylab(i)+ ggtitle(paste("Hourly production amounts(2021 Feb - 2022 June), hour = ",i))+
          scale_x_date(date_breaks = '1 month', date_labels = '%Y %b', date_minor_breaks = '1 month') +
          geom_smooth(color='blue',linetype='solid',fill=NA)+theme(axis.text.x=element_text(angle=90,hjust=1,
                                                                                            vjust = 0.5)))
}

#decomposition
merged2_ts=ts(merged2$production[4000:6000],frequency=14)
plot(decompose(merged2_ts))

#assign capacity by observing production values of last 30 days
merged$capacity <- NA
for (i in (30*24):nrow(merged)) {
  for (j in 1:30) {
    merged$capacity[i] <- max(merged$production[seq(i, i - j*24, -24)][!is.na(merged$production[seq(i, i - j*24, -24)])])
  }
}

#since for loop take a while, write_xlsx function is used to gather arranged data
write_xlsx(merged, "merged_06_02.xlsx")

#read exported data
merged <- read_xlsx("C:\\Users\\SaidBahçeci\\Documents\\merged_06_02.xlsx")
merged2 <- merged

# lag 72 addition (last available data)
merged2$lag72 <- NA
for(i in 1:(nrow(merged2)-72)){
  merged2$lag72[i+72] <- merged2$production[i]
}

#substract first month of the data since it does not involve capacity variable
merged2 <- merged2[721:(nrow(merged2) - 20),]

#subtract the hours yield production = 0
merged2 <- merged2 %>% filter(hour != 0 & hour != 1 & hour != 2 & hour != 3
                              & hour != 4 & hour != 5 & hour != 20 & hour != 21 
                              & hour != 22  & hour != 23)

#add weekday and months variable
merged2$weekday <- strftime(merged2$date, format = "%A")
merged2$months <- months(merged2$date)

#factorization and date adjustment
merged2$hour <- as.factor(merged2$hour)
merged2$months <- as.factor(merged2$months)
merged2$weekday <- as.factor(merged2$weekday)
merged2$date <- as.Date(merged2$date) 

#assign trend for each day
merged2$trend <- 0
for (i in 1:round(nrow(merged2))) {
  merged2$trend[(i*14-13):(14*i)] <- i
}
merged2$trend <- as.numeric(merged2$trend)

#correlation plot
corr = cor(merged2[1:6370,c(3:41,44)])
ggcorrplot(corr,
           type='lower',
           lab=TRUE,
           tl.cex = 5,
           lab_size = 2.3,
           digits = 2)

#create model that includes all variables
model <- lm(production ~. - date , data = merged2[1:6370,])
summary(model)

checkresiduals(model)

#lag subtraction, since harder to predict extreme cases
#weekday subtraction, since it is unrelated
model2 <- lm(production ~. - date - weekday - lag72 , data = merged2[1:6370,])
summary(model2)
checkresiduals(model2)

#after observing lag plot and high varying variance, it is meaningful to separate the data with hours and treat them accordingly
h6_merged2 <- merged2 %>%
  filter(hour == 6)
h7_merged2 <- merged2 %>%
  filter(hour == 7)
h8_merged2 <- merged2 %>%
  filter(hour == 8)
h9_merged2 <- merged2 %>%
  filter(hour == 9)
h10_merged2 <- merged2 %>%
  filter(hour == 10)
h11_merged2 <- merged2 %>%
  filter(hour == 11)
h12_merged2 <- merged2 %>%
  filter(hour == 12)
h13_merged2 <- merged2 %>%
  filter(hour == 13)
h14_merged2 <- merged2 %>%
  filter(hour == 14)
h15_merged2 <- merged2 %>%
  filter(hour == 15)
h16_merged2 <- merged2 %>%
  filter(hour == 16)
h17_merged2 <- merged2 %>%
  filter(hour == 17)
h18_merged2 <- merged2 %>%
  filter(hour == 18)
h19_merged2 <- merged2 %>% 
  filter(hour == 19)

#subtraction of hour column
h6_merged2 <- h6_merged2[, -2]
h7_merged2 <- h7_merged2[, -2]
h8_merged2 <- h8_merged2[, -2]
h9_merged2 <- h9_merged2[, -2]
h10_merged2 <- h10_merged2[, -2]
h11_merged2 <- h11_merged2[, -2]
h12_merged2 <- h12_merged2[, -2]
h13_merged2 <- h13_merged2[, -2]
h14_merged2 <- h14_merged2[, -2]
h15_merged2 <- h15_merged2[, -2]
h16_merged2 <- h16_merged2[, -2]
h17_merged2 <- h17_merged2[, -2]
h18_merged2 <- h18_merged2[, -2]
h19_merged2 <- h19_merged2[, -2]

print(ggplot(h17_merged2[434:448,],aes(x=date,y=production)) +  geom_line()+
                  xlab("Date") + ylab(i)+ ggtitle(paste("Hourly production amounts(2022  - 2022 June), hour = 17"))+
                  scale_x_date(date_breaks = '1 month', date_labels = '%Y %b', date_minor_breaks = '1 month') +
                  geom_smooth(color='blue',linetype='solid',fill=NA)+theme(axis.text.x=element_text(angle=90,hjust=1,
                                                                                                    vjust = 0.5)))


#form model of each hour and observe the significance of variables and check residuals
#while forming model data from March 1 2022 is used to prevent effect of rough weather conditions and high variance
modelh6 <- lm(production ~ . - date - weekday - lag72,
              data = h6_merged2[364:nrow(h6_merged2 - 13),])
#summary(modelh6)
#checkresiduals(modelh6)

modelh7 <- lm(production ~ . - date - weekday - lag72,
              data = h7_merged2[364:nrow(h7_merged2 - 13),])
#summary(modelh7)
#checkresiduals(modelh7)

modelh8 <- lm(production ~ . - date - weekday - lag72,
              data = h8_merged2[364:nrow(h8_merged2 - 13),])
summary(modelh8)
checkresiduals(modelh8)

modelh9 <- lm(production ~ . - date - weekday - lag72,
              data = h9_merged2[364:nrow(h9_merged2 - 13),])
#summary(modelh9)
#checkresiduals(modelh9)

modelh10 <- lm(production ~ . - date - weekday - lag72,
               data = h10_merged2[364:nrow(h10_merged2 - 13),])
summary(modelh10)
checkresiduals(modelh10)

modelh11 <- lm(production ~ . - date - weekday - lag72,
               data = h11_merged2[364:nrow(h11_merged2 - 13),])
#summary(modelh11)
#checkresiduals(modelh11)

modelh12 <- lm(production ~ . - date - weekday - lag72,
               data = h12_merged2[364:nrow(h12_merged2 - 13),])
#summary(modelh12)
#checkresiduals(modelh12)

modelh13 <- lm(production ~ . - date - weekday - lag72,
               data = h13_merged2[364:nrow(h13_merged2 - 13),])
#summary(modelh13)
#checkresiduals(modelh13)

modelh14 <- lm(production ~ . - date - weekday - lag72,
               data = h14_merged2[364:nrow(h14_merged2 - 13),])
summary(modelh14)
checkresiduals(modelh14)

modelh15 <- lm(production ~ . - date - weekday - lag72,
               data = h15_merged2[364:nrow(h15_merged2 - 13),])
#summary(modelh15)
#checkresiduals(modelh15)

modelh16 <- lm(production ~ . - date - weekday - lag72,
               data = h16_merged2[364:nrow(h16_merged2 - 13),])
summary(modelh16)
checkresiduals(modelh16)

modelh17 <- lm(production ~ . - date - weekday - lag72,
               data = h17_merged2[364:nrow(h17_merged2 - 13),])
#summary(modelh17)
#checkresiduals(modelh17)

modelh18 <- lm(production ~ . - date - weekday - lag72,
               data = h18_merged2[364:nrow(h18_merged2 - 13),])
#summary(modelh18)
#checkresiduals(modelh18)

modelh19 <- lm(production ~ . - date - weekday - lag72,
               data = h19_merged2[364:nrow(h19_merged2 - 13),])
summary(modelh19)
checkresiduals(modelh19)

#prediction of the last linear model
pred1 <- c()
for (i in c(364 : 448)) {
  pred1[(i-363)*14 -13] = predict(modelh6,newdata=h6_merged2[i,])
  pred1[(i-363)*14 -12] = predict(modelh7,newdata=h7_merged2[i,])
  pred1[(i-363)*14 -11] = predict(modelh8,newdata=h8_merged2[i,])
  pred1[(i-363)*14 -10] = predict(modelh9,newdata=h9_merged2[i,])
  pred1[(i-363)*14 -9] = predict(modelh10,newdata=h10_merged2[i,])
  pred1[(i-363)*14 -8] = predict(modelh11,newdata=h11_merged2[i,])
  pred1[(i-363)*14 -7] = predict(modelh12,newdata=h12_merged2[i,])
  pred1[(i-363)*14 -6] = predict(modelh13,newdata=h13_merged2[i,])
  pred1[(i-363)*14 -5] = predict(modelh14,newdata=h14_merged2[i,])
  pred1[(i-363)*14 -4] = predict(modelh15,newdata=h15_merged2[i,])
  pred1[(i-363)*14 -3] = predict(modelh16,newdata=h16_merged2[i,])
  pred1[(i-363)*14 -2] = predict(modelh17,newdata=h17_merged2[i,])
  pred1[(i-363)*14 -1] = predict(modelh18,newdata=h18_merged2[i,])
  pred1[(i-363)*14] = predict(modelh19,newdata=h19_merged2[i,])
}

#assign 0 for negative values
pred1[pred1 < 0] = 0

#WMAPE calculation
actual = merged2$production[5083:6272]
wmape1 = sum(abs(pred1-actual))/sum(actual)

#sarima alternative
sarima_model=auto.arima(merged2_ts,seasonal=T,stepwise=T,approximation=T,trace=T)

#WMAPE calculation
wmape2 = sum(abs(sarima_model[["residuals"]]))/sum(actual)

#data frame of WMAPE's
data.frame("Method" = c("Linear Regression","SARIMA"), "WMAPE" = c(wmape1, wmape2))
