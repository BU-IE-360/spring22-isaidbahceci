library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(ggcorrplot)

#Read the data files
setwd("C:/Users/SaidBahçeci/Desktop/Assignment 1")
google_trends = read.csv("multiTimeline.csv")
evds_data = read_excel("EVDS.xlsx")

#Combining data taken from EVDS and Google Trends
the_data = cbind(evds_data, google_trends[,2])
colnames(the_data) = c("Date","Tourist_Number","Avg_Spending","Dolar_Rate","Google_Trends")
the_data = head(the_data, - 25)  
the_data$Date<-gsub("-","",as.character(the_data$Date))
the_data[,"Date"] = as.Date(as.yearmon(the_data[,"Date"],format = "%Y%m"), format = "%Y-%m-%d")

#Add "Year" column to use while drawing histograms
the_data$Year = factor(format(the_data$Date, "%Y"),
                     levels = c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'))

#Set NA values to zero
the_data[is.na(the_data[,"Avg_Spending"] )==T,"Avg_Spending"]=0

#Plots the line graph of USD/TRy Exchange Rates
ggplot(data = the_data,aes(x = Date,y = Dolar_Rate)) + geom_line(color ="black") +
  geom_smooth(fill = NA, color="green",linetype = "twodash", size = 0.5) +
  labs(title = "USD/TRY Exchange Rates vs. Time",
       x = "Time",
       y = "US Dollar Exchange Rate" ) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 8,by = 1),
                     minor_breaks = seq(from = 0,to = 9,by = .2))

#Plots the USD/TRY histograms
ggplot(data = the_data,aes(x = Dolar_Rate)) + geom_histogram(bins = 12, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Histograms of Daily US Dollar Exchange Rates for Years 2011-2018",
       x = "US Dollar Exchange Rate (TRY)",
       y = "Frequency") +
  theme(legend.position = "none")

#Plots the USD/TRY boxplots
ggplot(data = the_data,aes(x = Year,y = Dolar_Rate)) + geom_boxplot(aes(y = Dolar_Rate, fill=Year)) +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Boxplots of Daily US Dollar Exchange Rates for Years 2011-2018",
       x = "Years",
       y = "US Dollar Exchange Rate (TRY)") +
  scale_y_continuous(breaks = seq(from = 0,to = 8,by = 1),
                     minor_breaks = seq(from = 0,to = 9,by = .2))

#Plots the line graph of Tourist Numbers
ggplot(data = the_data,aes(x = Date,y = Tourist_Number)) + geom_line(color ="black") +
  geom_smooth(fill = NA, color="green",linetype = "twodash", size = 0.5) +
  labs(title = "Tourist Numbers vs. Time",
       x = "Time",
       y = "Tourist Number" ) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 10000000,by = 500000))

#Plots the Tourist Numbers histograms
ggplot(data = the_data,aes(x = Tourist_Number/1000000)) + geom_histogram(bins = 12, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Histograms of Tourist Numbers for Years 2011-2018",
       x = "Tourist Numbers (in millions)",
       y = "Frequency") +
  theme(legend.position = "none")

#Plots the Tourist Number boxplots
ggplot(data = the_data,aes(x = Year,y = Tourist_Number)) + geom_boxplot(aes(y = Tourist_Number, fill=Year)) +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Boxplots of Tourist Numbers for Years 2011-2018",
       x = "Years",
       y = "Tourist Number") +
  scale_y_continuous(breaks = seq(from = 0,to = 10000000,by = 500000))

#Plots the line graph of Average Spending per Tourist
ggplot(data = the_data,aes(x = Date,y = Avg_Spending)) + geom_line(color ="black") +
  geom_smooth(fill = NA, color="green",linetype = "twodash", size = 0.5) +
  labs(title = "Average Spending per Tourist vs. Time",
       x = "Time",
       y = "Average Spending" ) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 1500,by = 100))

#Plots the Average Spending per Tourist histograms
ggplot(data = the_data,aes(x = Avg_Spending)) + geom_histogram(bins = 12, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Histograms of Average Spending per Tourist for Years 2011-2018",
       x = "Avereage Spending",
       y = "Frequency") +
  theme(legend.position = "none")

#Plots the Tourist Number boxplots
ggplot(data = the_data,aes(x = Year,y = Avg_Spending)) + geom_boxplot(aes(y = Avg_Spending, fill=Year)) +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Boxplots of Average Spending (USD) for Years 2011-2018",
       x = "Years",
       y = "Average Spending Per Tourist") +
  scale_y_continuous(breaks = seq(from = 0,to = 1500,by = 100))

#Plots the line graph of search rate of "Turkey" in travel category of Google Trends
ggplot(data = the_data,aes(x = Date,y = Google_Trends)) + geom_line(color ="black") +
  geom_smooth(fill = NA, color="green",linetype = "twodash", size = 0.5) +
  labs(title = "Search Rate of Turkey in Travel Category vs. Time",
       x = "Time",
       y = "Search Rate of Turkey" ) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 10))

#Plots the search rate of "Turkey" in travel category of Google Trends histograms
ggplot(data = the_data,aes(x = Google_Trends)) + geom_histogram(bins = 12, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Histograms of Search Rate of Turkey in Travel Category for Years 2011-2018",
       x = "Search Rate of Turkey",
       y = "Frequency") +
  theme(legend.position = "none")

#Plots the search rate of "Turkey" in travel category of Google Trends boxplots
ggplot(data = the_data,aes(x = Year,y = Google_Trends)) + geom_boxplot(aes(y = Google_Trends, fill=Year)) +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Boxplots of Search Rate of Turkey in Travel Category for Years 2011-2018",
       x = "Years",
       y = "Search Rate of Turkey") +
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 10))

#Add smoothed tourist numbers as column to see a meaningful relationship between dolar rate and tourist numbers.
#By this way, we can avoid the effect of seasonality
lowess_values = lowess(the_data$Tourist_Number) 
names(lowess_values)[2] ="Smoothed Tourist Number"
the_data = cbind(the_data, lowess_values[2])
the_data

#Correlation 
correl_info = cor(the_data[c(2,3,4,5,7)])
ggcorrplot(correl_info, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("RColorBrewer")
install.packages("zoo")
install.packages('ggcorrplot')
