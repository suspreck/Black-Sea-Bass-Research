# Hailey Schmidt
# 7/9/2021
# CMAST
# This script creates some basic graphs from the combined black sea bass dataset


# set up library and open files

library(dplyr)
library(ggplot2)
setwd("C:/Users/bookw/Desktop/CMAST_Research")
bsbData <- read.csv("Combined data file for bass project July 12 2021_HS.csv")


# change Date from factor type to date type to allow filtering
bsbData$Date <- as.Date(bsbData$Date, "%m/%d/%Y") #NOTE: 'Y' and not 'y' to specify 
# 4 digit vs 2 digit year

# change ExperimentNum to be factor instead of string
bsbData$ExperimentNum <- as.factor(bsbData$ExperimentNum)

# filter for experiments 1 and 2
exp1 <- filter(bsbData, ExperimentNum != "2")
exp2 <- filter(bsbData, ExperimentNum != "1")

# create variables: total tagged for each method, total recaptured for each method

exp1Totals = table(exp1$TreatmentType) 
exp1Total1 = as.integer(exp1Totals[1])
exp1Total2 = as.integer(exp1Totals[2])
exp1Total3 = as.integer(exp1Totals[3])

exp1TotalTagged = exp1Total1 + exp1Total2 + exp1Total3


exp1RecapsOnly = filter(exp1, Recap == 2)

exp1Recap = table(exp1RecapsOnly$TreatmentType)

exp1Recap1 = as.integer(exp1Recap[1])
exp1Recap2 = as.integer(exp1Recap[2])
exp1Recap3 = as.integer(exp1Recap[3])

exp1TotalRecap = exp1Recap1 + exp1Recap2 + exp1Recap3

# graph time
# first, the basics: stacked bar chart showing total numbers caught + recaptured
# for each method

barplot(exp1Totals, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,1200))

barplot(exp1Recap, xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# next, graph showing percentage recaptured for each method
# first, make new data frame

exp1Totals = as.data.frame(exp1Totals)
exp1Recap = as.data.frame(exp1Recap)
percentData <- data.frame(Method = c("Control","Recompress","VentResearcher"), 
                          Total_Tagged = exp1Totals$Freq, Total_Recap = exp1Recap$Freq)

percentData$Percent_Recap = percentData$Total_Recap / percentData$Total_Tagged * 100 

barplot(height = percentData$Percent_Recap, names = percentData$Method, xlab="Method",
        ylab="Percentage (%)", main="Overall Percentage Recaptured by Method", ylim = c(0,30),
        col=rgb(0.4,0.7,0.7,1))

# now do the same as above but for the 3 distinct depth strata

depth1 = filter(exp1, DepthStrata == 1)
depth2 = filter(exp1, DepthStrata == 2)
depth3 = filter(exp1, DepthStrata == 3)

depth1totals = as.data.frame(table(depth1$TreatmentType))
depth2totals = as.data.frame(table(depth2$TreatmentType))
depth3totals = as.data.frame(table(depth3$TreatmentType))

depth1 = filter(depth1, Recap == 2)
depth2 = filter(depth2, Recap == 2)
depth3 = filter(depth3, Recap == 2)

depth1recaps = as.data.frame(table(depth1$TreatmentType))
depth2recaps = as.data.frame(table(depth2$TreatmentType))
depth3recaps = as.data.frame(table(depth3$TreatmentType))

depth1 <- data.frame(Method = c("Control", "Recompress", "VentResearcher"),
                           Total_Tagged = depth1totals$Freq, Total_Recap = depth1recaps$Freq)

depth2 <- data.frame(Method = c("Control", "Recompress", "VentResearcher"),
                           Total_Tagged = depth2totals$Freq, Total_Recap = depth2recaps$Freq)

depth3 <- data.frame(Method = c("Control", "Recompress", "VentResearcher"),
                           Total_Tagged = depth3totals$Freq, Total_Recap = depth3recaps$Freq)

depth1$Percent = depth1$Total_Recap / depth1$Total_Tagged * 100
depth2$Percent = depth2$Total_Recap / depth2$Total_Tagged * 100
depth3$Percent = depth3$Total_Recap / depth3$Total_Tagged * 100

# create stacked bar graphs for each depth
# depth 1

barplot(height=depth1$Total_Tagged, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method
        75 - 85 feet",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,400))

barplot(height = depth1$Total_Recap, names = c("Control", "Recompress", "VentResearcher"),
        xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# depth 2

barplot(height = depth2$Total_Tagged, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method
        95 - 105 feet",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,120))

barplot(depth2$Total_Recap, names = c("Control", "Recompress", "VentResearcher"), 
        xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# depth 3

barplot(depth3$Total_Tagged, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method
        115 - 125 feet",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,600))

barplot(depth3$Total_Recap, names = c("Control", "Recompress", "VentResearcher"), 
        xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# now create graphs to show overall percentage of recaptures per method

barplot(height = depth1$Percent, names = c("Control", "Recompress", "VentResearcher"), xlab="Method",
        ylab="Percentage (%)", main="Percentage Recaptured by Method, 75 - 85 feet", ylim = c(0,50),
        col=rgb(0.4,0.7,0.7,1))

barplot(height = depth2$Percent, names = c("Control", "Recompress", "VentResearcher"), xlab="Method",
        ylab="Percentage (%)", main="Percentage Recaptured by Method, 95 - 105 feet", ylim = c(0,50),
        col=rgb(0.4,0.7,0.7,1))

barplot(height = depth3$Percent, names = c("Control", "Recompress", "VentResearcher"), xlab="Method",
        ylab="Percentage (%)", main="Percentage Recaptured by Method, 115 - 125 feet", ylim = c(0,50),
        col=rgb(0.4,0.7,0.7,1))

#######

# month_filters <- character(length(12))
# for (x in 1:12) {
#         month_filters[x] = filter(exp1, format(DateTagged, "%m") == format(x, digits=2))
#         monthly[x,] = c(x,nrow(month_filters[x]))
# }

# plot number of fish tagged by month
monthly <- data.frame(Month = 0, Total_Tagged = 0)

jan = filter(exp1, format(Date, "%m") == "01")
feb = filter(exp1, format(Date, "%m") == "02")
mar = filter(exp1, format(Date, "%m") == "03")
apr = filter(exp1, format(Date, "%m") == "04")
may = filter(exp1, format(Date, "%m") == "05")
jun = filter(exp1, format(Date, "%m") == "06")
jul = filter(exp1, format(Date, "%m") == "07")
aug = filter(exp1, format(Date, "%m") == "08")
sep = filter(exp1, format(Date, "%m") == "09")
oct = filter(exp1, format(Date, "%m") == "10")
nov = filter(exp1, format(Date, "%m") == "11")
dec = filter(exp1, format(Date, "%m") == "12")

# get frequencies by counting number of entries in each month
monthly[1,] = c(1,nrow(jan))
monthly[2,] = c(2,nrow(feb))
monthly[3,] = c(3,nrow(mar))
monthly[4,] = c(4,nrow(apr))
monthly[5,] = c(5,nrow(may))
monthly[6,] = c(6,nrow(jun))
monthly[7,] = c(7,nrow(jul))
monthly[8,] = c(8,nrow(aug))
monthly[9,] = c(9,nrow(sep))
monthly[10,] = c(10,nrow(oct))
monthly[11,] = c(11,nrow(nov))
monthly[12,] = c(12,nrow(dec))

monthly$Month_Name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                       "Sep", "Oct", "Nov", "Dec")

# graph
barplot(height = monthly$Total_Tagged, names = monthly$Month_Name, 
        main = "Total Black Sea Bass Tagged by Month, All Depths",
        xlab = "Month", ylab = "Total")

# create same graph but differentiate by depth strata
jan1 = filter(jan, DepthStrata == 1)
jan2 = filter(jan, DepthStrata == 2)
jan3 = filter(jan, DepthStrata == 3)

feb1 = filter(feb, DepthStrata == 1)
feb2 = filter(feb, DepthStrata == 2)
feb3 = filter(feb, DepthStrata == 3)

mar1 = filter(mar, DepthStrata == 1)
mar2 = filter(mar, DepthStrata == 2)
mar3 = filter(mar, DepthStrata == 3)

apr1 = filter(apr, DepthStrata == 1)
apr2 = filter(apr, DepthStrata == 2)
apr3 = filter(apr, DepthStrata == 3)

may1 = filter(may, DepthStrata == 1)
may2 = filter(may, DepthStrata == 2)
may3 = filter(may, DepthStrata == 3)

jun1 = filter(jun, DepthStrata == 1)
jun2 = filter(jun, DepthStrata == 2)
jun3 = filter(jun, DepthStrata == 3)

jul1 = filter(jul, DepthStrata == 1)
jul2 = filter(jul, DepthStrata == 2)
jul3 = filter(jul, DepthStrata == 3)

aug1 = filter(aug, DepthStrata == 1)
aug2 = filter(aug, DepthStrata == 2)
aug3 = filter(aug, DepthStrata == 3)

sep1 = filter(sep, DepthStrata == 1)
sep2 = filter(sep, DepthStrata == 2)
sep3 = filter(sep, DepthStrata == 3)

oct1 = filter(oct, DepthStrata == 1)
oct2 = filter(oct, DepthStrata == 2)
oct3 = filter(oct, DepthStrata == 3)

nov1 = filter(nov, DepthStrata == 1)
nov2 = filter(nov, DepthStrata == 2)
nov3 = filter(nov, DepthStrata == 3)

dec1 = filter(dec, DepthStrata == 1)
dec2 = filter(dec, DepthStrata == 2)
dec3 = filter(dec, DepthStrata == 3)

# create data frames
monthly1 <- data.frame(Month = 0, Total_Tagged = 0)
monthly2 <- data.frame(Month = 0, Total_Tagged = 0)
monthly3 <- data.frame(Month = 0, Total_Tagged = 0)

monthly1[1,] = c(1, nrow(jan1))
monthly1[2,] = c(2, nrow(feb1))
monthly1[3,] = c(3, nrow(mar1))
monthly1[4,] = c(4, nrow(apr1))
monthly1[5,] = c(5, nrow(may1))
monthly1[6,] = c(6, nrow(jun1))
monthly1[7,] = c(7, nrow(jul1))
monthly1[8,] = c(8, nrow(aug1))
monthly1[9,] = c(9, nrow(sep1))
monthly1[10,] = c(10, nrow(oct1))
monthly1[11,] = c(11, nrow(nov1))
monthly1[12,] = c(12, nrow(dec1))

monthly1$Month_Name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                        "Sep", "Oct", "Nov", "Dec")

monthly2[1,] = c(1, nrow(jan2))
monthly2[2,] = c(2, nrow(feb2))
monthly2[3,] = c(3, nrow(mar2))
monthly2[4,] = c(4, nrow(apr2))
monthly2[5,] = c(5, nrow(may2))
monthly2[6,] = c(6, nrow(jun2))
monthly2[7,] = c(7, nrow(jul2))
monthly2[8,] = c(8, nrow(aug2))
monthly2[9,] = c(9, nrow(sep2))
monthly2[10,] = c(10, nrow(oct2))
monthly2[11,] = c(11, nrow(nov2))
monthly2[12,] = c(12, nrow(dec2))

monthly2$Month_Name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                        "Sep", "Oct", "Nov", "Dec")

monthly3[1,] = c(1, nrow(jan3))
monthly3[2,] = c(2, nrow(feb3))
monthly3[3,] = c(3, nrow(mar3))
monthly3[4,] = c(4, nrow(apr3))
monthly3[5,] = c(5, nrow(may3))
monthly3[6,] = c(6, nrow(jun3))
monthly3[7,] = c(7, nrow(jul3))
monthly3[8,] = c(8, nrow(aug3))
monthly3[9,] = c(9, nrow(sep3))
monthly3[10,] = c(10, nrow(oct3))
monthly3[11,] = c(11, nrow(nov3))
monthly3[12,] = c(12, nrow(dec3))

monthly3$Month_Name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                        "Sep", "Oct", "Nov", "Dec")


# create graphs

colorpalette = c("slategray4", "slategray3", "slategray1")

barplot(height = monthly1$Total_Tagged, names = monthly1$Month_Name,
        main = "Total Black Sea Bass Tagged by Month, 75 - 85 feet",
        xlab = "Month", ylab = "Total")
barplot(height = monthly2$Total_Tagged, names = monthly2$Month_Name,
        main = "Total Black Sea Bass Tagged by Month, 95 - 105 feet",
        xlab = "Month", ylab = "Total", ylim = c(0, 200))
barplot(height = monthly3$Total_Tagged, names = monthly3$Month_Name,
        main = "Total Black Sea Bass Tagged by Month, 115 - 125 feet",
        xlab = "Month", ylab = "Total", ylim = c(0, 300))

# Create a stacked bar graph showing same data as above

test <- rbind(monthly1$Total_Tagged, monthly2$Total_Tagged,
              monthly3$Total_Tagged)
barplot(test, main = "Total Black Sea Bass Tagged by Month, by Depth",
        names = monthly$Month_Name, xlab = "Month", ylab = "Total", 
        col = colorpalette, ylim = c(0, 600))

legend("topright",inset = 0, legend=c("75 - 85 feet", "95 - 105 feet", "115 - 125 feet"), 
       col = colorpalette, pt.cex=2, pch=15 )

# create a side by side bar graph showing the same data as above

barplot(test, main = "Total Black Sea Bass Tagged by Month, by Depth",
        names = monthly$Month_Name, xlab = "Month", ylab = "Total", 
        col = colorpalette, beside = T)
legend("topright",inset = 0, legend=c("75 - 85 feet", "95 - 105 feet", "115 - 125 feet"), 
       col = colorpalette, pt.cex=2, pch=15)

# create graphs showing total catch and percentage recaptured for Experiment 2

exp2totals = table(exp2$TreatmentType)
exp2recapsOnly = filter(exp2, Recap == 2)
exp2recaps = table(exp2recapsOnly$TreatmentType)

exp2totals = as.data.frame(exp2totals)
exp2recaps = as.data.frame(exp2recaps)

barplot(exp2totals$Freq, names = exp2totals$Var1, 
        main = "Total Black Sea Bass Tagged and Recaptured, Experiment 2",
        xlab = "Method", ylab = "Total", col = "slategray3")
barplot(height = exp2recaps$Freq, col = "slategrey", add = T)
legend("topright", legend=c("Total Tagged", "Total Recaptured"), 
       col = c("slategray3", "slategrey"), pt.cex = 2, pch=15)


exp2Percents <- data.frame(Method = c("Control", "Recompress", "VentAngler", "VentResearcher"),
                           Total_Tagged = exp2totals$Freq, Total_Recap = exp2recaps$Freq)
exp2Percents$Percent = exp2Percents$Total_Recap / exp2Percents$Total_Tagged * 100

barplot(height = exp2Percents$Percent, names = c("Control", "Recompress", "VentAngler", "VentResearcher"),
        main = "Percentage Recaptured by Method, Experiment 2", xlab = "Method",
        ylab = "Total", col = "slategray4", ylim = c(0,30))

#########
# 3 panel graph showing number of fish tagged by treatment and depth
#########

# add columns for treatment types

monthly1$Control = 0
monthly1$Recompress = 0
monthly1$VentResearcher = 0

monthly2$Control = 0
monthly2$Recompress = 0
monthly2$VentResearcher = 0

monthly3$Control = 0
monthly3$Recompress = 0
monthly3$VentResearcher = 0

# fill in columns by filtering each month by depth for treatment type

# i = 1:12
# j = 4:6
# k = 1:3
# m = 1:3
# monthlyData = list(monthly1, monthly2, monthly3)
# months1 = list(jan1, feb1, mar1, apr1, may1, jun1, jul1, aug1, sep1, oct1, nov1, dec1)
# 
# for(val in i){
#         for(val2 in j){
#                 for(val3 in k){
#                         monthlyData[k][i,j] = nrow(filter(as.data.frame(months1[i]), TreatmentCodeJuly2021 == (j-3)))
#                 }
#         }      
#         
# }

# jan
monthly1[1,4] = nrow(filter(jan1, TreatmentCodeJuly2021 == 1))
monthly1[1,5] = nrow(filter(jan1, TreatmentCodeJuly2021 == 2))
monthly1[1,6] = nrow(filter(jan1, TreatmentCodeJuly2021 == 3))

monthly2[1,4] = nrow(filter(jan2, TreatmentCodeJuly2021 == 1))
monthly2[1,5] = nrow(filter(jan2, TreatmentCodeJuly2021 == 2))
monthly2[1,6] = nrow(filter(jan2, TreatmentCodeJuly2021 == 3))

monthly3[1,4] = nrow(filter(jan3, TreatmentCodeJuly2021 == 1))
monthly3[1,5] = nrow(filter(jan3, TreatmentCodeJuly2021 == 2))
monthly3[1,6] = nrow(filter(jan3, TreatmentCodeJuly2021 == 3))

# feb
monthly1[2,4] = nrow(filter(feb1, TreatmentCodeJuly2021 == 1))
monthly1[2,5] = nrow(filter(feb1, TreatmentCodeJuly2021 == 2))
monthly1[2,6] = nrow(filter(feb1, TreatmentCodeJuly2021 == 3))

monthly2[2,4] = nrow(filter(feb2, TreatmentCodeJuly2021 == 1))
monthly2[2,5] = nrow(filter(feb2, TreatmentCodeJuly2021 == 2))
monthly2[2,6] = nrow(filter(feb2, TreatmentCodeJuly2021 == 3))

monthly3[2,4] = nrow(filter(feb3, TreatmentCodeJuly2021 == 1))
monthly3[2,5] = nrow(filter(feb3, TreatmentCodeJuly2021 == 2))
monthly3[2,6] = nrow(filter(feb3, TreatmentCodeJuly2021 == 3))

# mar
monthly1[3,4] = nrow(filter(mar1, TreatmentCodeJuly2021 == 1))
monthly1[3,5] = nrow(filter(mar1, TreatmentCodeJuly2021 == 2))
monthly1[3,6] = nrow(filter(mar1, TreatmentCodeJuly2021 == 3))

monthly2[3,4] = nrow(filter(mar2, TreatmentCodeJuly2021 == 1))
monthly2[3,5] = nrow(filter(mar2, TreatmentCodeJuly2021 == 2))
monthly2[3,6] = nrow(filter(mar2, TreatmentCodeJuly2021 == 3))

monthly3[3,4] = nrow(filter(mar3, TreatmentCodeJuly2021 == 1))
monthly3[3,5] = nrow(filter(mar3, TreatmentCodeJuly2021 == 2))
monthly3[3,6] = nrow(filter(mar3, TreatmentCodeJuly2021 == 3))

#apr
monthly1[4,4] = nrow(filter(apr1, TreatmentCodeJuly2021 == 1))
monthly1[4,5] = nrow(filter(apr1, TreatmentCodeJuly2021 == 2))
monthly1[4,6] = nrow(filter(apr1, TreatmentCodeJuly2021 == 3))

monthly2[4,4] = nrow(filter(apr2, TreatmentCodeJuly2021 == 1))
monthly2[4,5] = nrow(filter(apr2, TreatmentCodeJuly2021 == 2))
monthly2[4,6] = nrow(filter(apr2, TreatmentCodeJuly2021 == 3))

monthly3[4,4] = nrow(filter(apr3, TreatmentCodeJuly2021 == 1))
monthly3[4,5] = nrow(filter(apr3, TreatmentCodeJuly2021 == 2))
monthly3[4,6] = nrow(filter(apr3, TreatmentCodeJuly2021 == 3))

#may
monthly1[5,4] = nrow(filter(may1, TreatmentCodeJuly2021 == 1))
monthly1[5,5] = nrow(filter(may1, TreatmentCodeJuly2021 == 2))
monthly1[5,6] = nrow(filter(may1, TreatmentCodeJuly2021 == 3))

monthly2[5,4] = nrow(filter(may2, TreatmentCodeJuly2021 == 1))
monthly2[5,5] = nrow(filter(may2, TreatmentCodeJuly2021 == 2))
monthly2[5,6] = nrow(filter(may2, TreatmentCodeJuly2021 == 3))

monthly3[5,4] = nrow(filter(may3, TreatmentCodeJuly2021 == 1))
monthly3[5,5] = nrow(filter(may3, TreatmentCodeJuly2021 == 2))
monthly3[5,6] = nrow(filter(may3, TreatmentCodeJuly2021 == 3))

# jun
monthly1[6,4] = nrow(filter(jun1, TreatmentCodeJuly2021 == 1))
monthly1[6,5] = nrow(filter(jun1, TreatmentCodeJuly2021 == 2))
monthly1[6,6] = nrow(filter(jun1, TreatmentCodeJuly2021 == 3))

monthly2[6,4] = nrow(filter(jun2, TreatmentCodeJuly2021 == 1))
monthly2[6,5] = nrow(filter(jun2, TreatmentCodeJuly2021 == 2))
monthly2[6,6] = nrow(filter(jun2, TreatmentCodeJuly2021 == 3))

monthly3[6,4] = nrow(filter(jun3, TreatmentCodeJuly2021 == 1))
monthly3[6,5] = nrow(filter(jun3, TreatmentCodeJuly2021 == 2))
monthly3[6,6] = nrow(filter(jun3, TreatmentCodeJuly2021 == 3))

# jul
monthly1[7,4] = nrow(filter(jul1, TreatmentCodeJuly2021 == 1))
monthly1[7,5] = nrow(filter(jul1, TreatmentCodeJuly2021 == 2))
monthly1[7,6] = nrow(filter(jul1, TreatmentCodeJuly2021 == 3))

monthly2[7,4] = nrow(filter(jul2, TreatmentCodeJuly2021 == 1))
monthly2[7,5] = nrow(filter(jul2, TreatmentCodeJuly2021 == 2))
monthly2[7,6] = nrow(filter(jul2, TreatmentCodeJuly2021 == 3))

monthly3[7,4] = nrow(filter(jul3, TreatmentCodeJuly2021 == 1))
monthly3[7,5] = nrow(filter(jul3, TreatmentCodeJuly2021 == 2))
monthly3[7,6] = nrow(filter(jul3, TreatmentCodeJuly2021 == 3))

# aug
monthly1[8,4] = nrow(filter(aug1, TreatmentCodeJuly2021 == 1))
monthly1[8,5] = nrow(filter(aug1, TreatmentCodeJuly2021 == 2))
monthly1[8,6] = nrow(filter(aug1, TreatmentCodeJuly2021 == 3))

monthly2[8,4] = nrow(filter(aug2, TreatmentCodeJuly2021 == 1))
monthly2[8,5] = nrow(filter(aug2, TreatmentCodeJuly2021 == 2))
monthly2[8,6] = nrow(filter(aug2, TreatmentCodeJuly2021 == 3))

monthly3[8,4] = nrow(filter(aug3, TreatmentCodeJuly2021 == 1))
monthly3[8,5] = nrow(filter(aug3, TreatmentCodeJuly2021 == 2))
monthly3[8,6] = nrow(filter(aug3, TreatmentCodeJuly2021 == 3))

# sep
monthly1[9,4] = nrow(filter(sep1, TreatmentCodeJuly2021 == 1))
monthly1[9,5] = nrow(filter(sep1, TreatmentCodeJuly2021 == 2))
monthly1[9,6] = nrow(filter(sep1, TreatmentCodeJuly2021 == 3))

monthly2[9,4] = nrow(filter(sep2, TreatmentCodeJuly2021 == 1))
monthly2[9,5] = nrow(filter(sep2, TreatmentCodeJuly2021 == 2))
monthly2[9,6] = nrow(filter(sep2, TreatmentCodeJuly2021 == 3))

monthly3[9,4] = nrow(filter(sep3, TreatmentCodeJuly2021 == 1))
monthly3[9,5] = nrow(filter(sep3, TreatmentCodeJuly2021 == 2))
monthly3[9,6] = nrow(filter(sep3, TreatmentCodeJuly2021 == 3))

# oct
monthly1[10,4] = nrow(filter(oct1, TreatmentCodeJuly2021 == 1))
monthly1[10,5] = nrow(filter(oct1, TreatmentCodeJuly2021 == 2))
monthly1[10,6] = nrow(filter(oct1, TreatmentCodeJuly2021 == 3))

monthly2[10,4] = nrow(filter(oct2, TreatmentCodeJuly2021 == 1))
monthly2[10,5] = nrow(filter(oct2, TreatmentCodeJuly2021 == 2))
monthly2[10,6] = nrow(filter(oct2, TreatmentCodeJuly2021 == 3))

monthly3[10,4] = nrow(filter(oct3, TreatmentCodeJuly2021 == 1))
monthly3[10,5] = nrow(filter(oct3, TreatmentCodeJuly2021 == 2))
monthly3[10,6] = nrow(filter(oct3, TreatmentCodeJuly2021 == 3))

# nov
monthly1[11,4] = nrow(filter(nov1, TreatmentCodeJuly2021 == 1))
monthly1[11,5] = nrow(filter(nov1, TreatmentCodeJuly2021 == 2))
monthly1[11,6] = nrow(filter(nov1, TreatmentCodeJuly2021 == 3))

monthly2[11,4] = nrow(filter(nov2, TreatmentCodeJuly2021 == 1))
monthly2[11,5] = nrow(filter(nov2, TreatmentCodeJuly2021 == 2))
monthly2[11,6] = nrow(filter(nov2, TreatmentCodeJuly2021 == 3))

monthly3[11,4] = nrow(filter(nov3, TreatmentCodeJuly2021 == 1))
monthly3[11,5] = nrow(filter(nov3, TreatmentCodeJuly2021 == 2))
monthly3[11,6] = nrow(filter(nov3, TreatmentCodeJuly2021 == 3))

# dec
monthly1[12,4] = nrow(filter(dec1, TreatmentCodeJuly2021 == 1))
monthly1[12,5] = nrow(filter(dec1, TreatmentCodeJuly2021 == 2))
monthly1[12,6] = nrow(filter(dec1, TreatmentCodeJuly2021 == 3))

monthly2[12,4] = nrow(filter(dec2, TreatmentCodeJuly2021 == 1))
monthly2[12,5] = nrow(filter(dec2, TreatmentCodeJuly2021 == 2))
monthly2[12,6] = nrow(filter(dec2, TreatmentCodeJuly2021 == 3))

monthly3[12,4] = nrow(filter(dec3, TreatmentCodeJuly2021 == 1))
monthly3[12,5] = nrow(filter(dec3, TreatmentCodeJuly2021 == 2))
monthly3[12,6] = nrow(filter(dec3, TreatmentCodeJuly2021 == 3))

shallow_treatments <- rbind(monthly1$Control, monthly1$Recompress, monthly1$VentResearcher)
mid_treatments <- rbind(monthly2$Control, monthly2$Recompress, monthly2$VentResearcher)
deep_treatments <- rbind(monthly3$Control, monthly3$Recompress, monthly3$VentResearcher)

par(mfrow=c(3,1))
barplot(shallow_treatments, names = monthly1$Month_Name, ylab = "Total",
        xlab = "Month", main = "23 - 26 m", beside = T)
barplot(mid_treatments, names = monthly1$Month_Name, ylab = "Total",
        xlab = "Month", main = "29 - 32 m", beside = T)
legend("topright", legend=c("Control", "Recompressed","Vent by Researcher"), 
       fill=c("gray20","gray50","gray80"))
barplot(deep_treatments, names = monthly1$Month_Name, ylab = "Total",
        xlab = "Month", main = "35 - 38 m", beside = T)


