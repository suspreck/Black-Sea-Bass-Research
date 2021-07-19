# set up library and open files

library(dplyr)
library(ggplot2)
CRP_Data_HS <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/BarChart/CRP_Data_HS.csv", stringsAsFactors=TRUE)
#View(CRP_Data_HS)

# next, filter by species (black sea bass) and tag color (blue)
bsbFiltered <- filter(CRP_Data_HS, Species == "bsb" & TagCol == "blue")
#View(bsbFiltered)

# filter by catches from ~100ft deep (100 +/- 3)
bsb100ft <- filter(bsbFiltered, DepthFt <= 103 & DepthFt >= 97)
#View(bsb100ft)

# now we have all records of blue tagged black sea bass from ~100 feet of depth
# next we will graph total catch by method from this dataset

method = bsb100ft$TreatmentPrincessStacy
methodFreq = table(method)
barplot(methodFreq, xlab = "Method", ylab = "Count", main = "Total Individuals Tagged by Method")

# create variables for total recapture by method, make second barplot w/ total number recaptured individuals per method

bsbRecapture <- filter(bsb100ft, RecapDate != "8/31/2021" & RecapDate != "12/31/2020")
method = bsbRecapture$TreatmentPrincessStacy
recapFreq = table(method)
barplot(recapFreq, xlab = "Method", ylab = "Count", main = "Total Individuals Recaptured by Method")

# ok lets redo this
# lets make a table w/ all records of black seabass caught ~100 ft with blue tags -> bsb100ft
# now we want to assign each method a category, total or recaptured, along with a quantity
# we should use the rbind() function to combine our two earlier datasets

totalData <- data.frame(methodFreq)
totalData$Status <- c(rep("Total", 4))
recapData <- data.frame(recapFreq)
recapData$Status <- c(rep("Recaptured", 4))
finaldata <- rbind(totalData, recapData)

# create a stacked bar plot using both frequencies

ggplot(finaldata, aes(fill=Status, y=Freq, x=method)) + 
  geom_bar(position="stack", stat="identity") + xlab("Method") + ylab("Frequency") + 
  ggtitle("Total Tagged and Recaptured Individuals by Method")

# now, make a bar plot displaying percentage of individuals recaptured per method

percentageData <- data.frame(methodFreq)
percentageData$Freq2 = recapFreq
percentageData$PercentRecap = percentageData$Freq2/percentageData$Freq * 100

barplot(height = percentageData$PercentRecap, names = percentageData$method, xlab="Method", 
        ylab="Percentage Recaptured", main="Percentage Black Sea Bass Recaptured by Method",
        ylim=c(0,60))
