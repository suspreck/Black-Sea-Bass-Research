# Hailey Schmidt
# July 13 2021
# CMAST

# Create simple stacked bar graph showing total catch and release numbers of
# black sea bass in the South Atlantic from 1981-2020

library(Hmisc)
library(dplyr)

setwd("C:/Users/bookw/Desktop/CMAST_Research")

released <- read.csv("MRIP Graph/mrip_BLACK_SEA_BASS_released.csv")
totalCatch <- read.csv("MRIP Graph/mrip_BLACK_SEA_BASS_catch_series.csv")

data <- data.frame(Year = released$Year, Total_Catch = totalCatch$Total.Catch..A.B1.B2., Total_Released = released$Released.Alive..B2.)

data$Total_Catch = gsub(",", "", data$Total_Catch)
data$Total_Released = gsub(",", "", data$Total_Released)

data$Total_Catch = as.numeric(data$Total_Catch)
data$Total_Released = as.numeric(data$Total_Released)

data <- filter(data, Year != 2021)

data$percentage = data$Total_Released / data$Total_Catch * 100

barplot(height = data$percentage, names = data$Year, xlab="Year",ylab="Percentage (%)",
        ylim=c(0,100), space = 0, col="gray50")

barplot(height = data$Total_Catch / 1000000, names = data$Year, xlab="Year",
        ylab="Number (millions)",
        ylim=c(0,10), space = 0, col="gray80", axes = F)

barplot(height = data$Total_Released / 1000000, names = data$Year, xlab="Year",
        ylab="Number (millions)",
        ylim = c(0,10), space = 0, col="gray50", add = T, axes = F)

legend("top",inset=0.0, legend=c("Retained","Discarded"),fill=c("gray80","gray50") )
box()
axis(2, at = seq(0,10,1))

# get average percentage over last decade

data <- filter(data, Year > 2010)
summary(data$percentage)
