# Hailey Schmidt
# July 13 2021
# CMAST

# Create simple stacked bar graph showing total catch and release numbers of
# black sea bass in the South Atlantic from 1981-2020

setwd("C:/Users/bookw/Desktop/CMAST_Research")

released <- read.csv("MRIP Graph/mrip_BLACK_SEA_BASS_released.csv")
totalCatch <- read.csv("MRIP Graph/mrip_BLACK_SEA_BASS_catch_series.csv")

data <- data.frame(Year = released$Year, Total_Catch = totalCatch$Total.Catch..A.B1.B2., Total_Released = released$Released.Alive..B2.)

#data$Total_Catch = gsub(",", "", data$Total_Catch)
#data$Total_Released = gsub(",", "", data$Total_Released)

#data$Total_Catch = as.numeric(data$Total_Catch)
#data$Total_Released = as.numeric(data$Total_Released)


data$percentage = data$Total_Released / data$Total_Catch * 100

barplot(height = data$percentage, names = data$Year, xlab="Year",ylab="Percentage (%)",
        main="Percentage of Black Sea Bass Released after Catch, 1981 - 2020", 
        ylim=c(0,100), space = 0, col="gray50")

barplot(height = data$Total_Catch / 1000000, names = data$Year, xlab="Year",
        ylab="Catch (millions)", main="Total Black Sea Bass Catch, 1981 - 2020",
        ylim=c(0,10), col=rgb(0.5,0.81,0.71,1))

barplot(height = data$Total_Released / 1000000, names = data$Year, xlab="Year",
        ylab="Catch (millions)", main="Total Black Sea Bass Released, 1981 - 2020",
        ylim = c(0,10), col=rgb(0.17,0.4,0.4,1))


#just some fun stuff with colors and what not

barplot(height = data$Total_Catch / 1000000, names = data$Year, xlab="Year",
        col=rgb(0.5,0.81,0.71,1), ylab="Catch (millions)", 
        main="Total Black Sea Bass Catch, 1981 - 2020", ylim=c(0,10))

barplot(height = data$Total_Released / 1000000, names = data$Year, 
        col=rgb(0.17,0.4,0.4,1), xlab="Year", ylab="Catch (millions)", 
        main="Total Black Sea Bass Released, 1981 - 2020", ylim = c(0,10), add=T)

legend("topleft",inset=0.05, legend=c("Total Catch","Total Released"), col=c(rgb(0.5,0.81,0.71,1), 
                                                              rgb(0.17,0.4,0.4,1)), pt.cex=2, pch=15 )

