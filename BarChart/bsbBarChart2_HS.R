# Hailey Schmidt
# 6/2/2021
# CMAST
#/ This is an R script to create simple graphs showing the recapture rates of   /
#/ black sea bass using various methods (venting, descending device, control)   /
#/ The data set contains all records, but we will only be analyzing entries     /
#/ from winter/spring of 2021, with species Black Sea Bass, and tag Color blue. /


# set up library and open files

library(dplyr)
library(ggplot2)
bsbData <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/BarChart/CRP_Data_HS.csv", 
                    stringsAsFactors=TRUE)
#View(CRP_Data_HS)

# change Date from factor type to string type, then to date type to allow filtering
bsbData$Date <- as.character(bsbData$Date)
bsbData$Date <- as.Date(bsbData$Date, "%m/%d/%Y") #NOTE: 'Y' and not 'y' to specify 
                                                  # 4 digit vs 2 digit year

# filter data by only correct species, tag color, and date
bsbFiltered <- filter(bsbData, Species == "bsb", TagCol == "blue", 
                      Date > "2020-12-31")

# create data set with only method and recapture success (1 or 2)
data <- data.frame(treatment = bsbFiltered$TreatmentPrincessStacy, recap = bsbFiltered$Recap)

# create variables: total tagged for each method, total recaptured for each method

totals = table(data$treatment) 
total1 = as.integer(totals[1])
total2 = as.integer(totals[2])
total3 = as.integer(totals[3])
total4 = as.integer(totals[4])

totalTagged = total1 + total2 + total3 + total4

# create a while loop to count recaptured individuals
i = 1

recaps <- c(0,0,0,0)

totalRecap = 0

while(i <= totalTagged){
  
  currentTreatment = data$treatment[i]
  if(data$recap[i] == "2"){
    recaps[currentTreatment] = recaps[currentTreatment] + 1
    totalRecap = totalRecap + 1
  }
  i = i + 1
}

recap1 = recaps[1]
recap2 = recaps[2]
recap3 = recaps[3]
recap4 = recaps[4]

# graph time
# first, the basics: frequency of total tagged and frequency of total recaptured for each method

barplot(totals, xlab="Method", ylab="Count", main="Total Individuals Tagged by Method")
barplot(height = recaps, names = c(1, 2, 3, 4), xlab="Method", ylab="Count", 
        main="Total Individuals Recaptured by Method")

# next, graph showing percentage recaptured for each method
# first, make new data frame

percentData <- data.frame(Method = c(1,2,3,4), Total_Tagged = totals, Total_Recap = recaps)
percentData$Percent_Recap = percentData$Total_Recap / percentData$Total_Tagged.Freq * 100 

barplot(height = percentData$Percent_Recap, names = c(1,2,3,4), xlab="Method",
        ylab="Percentage (%)", main="Percentage Recaptured by Method")

# now time for more fun graphs
barplot(totals, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,200))

barplot(height = recaps, names = c(1, 2, 3, 4), xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset=0.01, legend=c("Total Caught","Total Recaptured"), col=c(rgb(0.9,0.9,0.8,1), 
                                                                             rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )
