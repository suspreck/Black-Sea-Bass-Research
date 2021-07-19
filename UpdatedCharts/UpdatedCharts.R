# Hailey Schmidt
# 6/16/2021
# CMAST
# This script creates some basic graphs from the combined black sea bass dataset


# set up library and open files

library(dplyr)
library(ggplot2)
setwd("C:/Users/bookw/Desktop/CMAST_Research")
bsbData <- read.csv("Combined file from both BSB venting descending studies updated June 16 2021_HS.csv")
bsbData <- read.csv("Combined BSB data_HS.csv")

# rename study number column because for some reason it is freaking it

names(bsbData)[names(bsbData) == "ï..Study_Num"] <- "Study_Num"

# change Date from factor type to string type, then to date type to allow filtering
bsbData$DateTagged <- as.character(bsbData$DateTagged)
bsbData$DateTagged <- as.Date(bsbData$DateTagged, "%m/%d/%Y") #NOTE: 'Y' and not 'y' to specify 
# 4 digit vs 2 digit year

# filter for experiments 1 and 2
exp1 <- filter(bsbData, TreatmentNumber != 4)
exp2 <- filter(bsbData, Study_Num == 2)

# create variables: total tagged for each method, total recaptured for each method

exp1Totals = table(exp1$Treatment) 
exp1Total1 = as.integer(exp1Totals[1])
exp1Total2 = as.integer(exp1Totals[2])
exp1Total3 = as.integer(exp1Totals[3])

exp1TotalTagged = exp1Total1 + exp1Total2 + exp1Total3


exp1RecapsOnly = filter(exp1, Recap == 2)

exp1Recap = table(exp1RecapsOnly$Treatment)

exp1Recap1 = as.integer(exp1Recap[1])
exp1Recap2 = as.integer(exp1Recap[2])
exp1Recap3 = as.integer(exp1Recap[3])

exp1TotalRecap = exp1Recap1 + exp1Recap2 + exp1Recap3

# graph time
# first, the basics: stacked bar chart showing total numbers caught + recaptured
# for each method

barplot(exp1Totals, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,1200))

barplot(exp1Recap, names = c(1, 2, 3, 4), xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# next, graph showing percentage recaptured for each method
# first, make new data frame

exp1Totals = as.data.frame(exp1Totals)
exp1Recap = as.data.frame(exp1Recap)
percentData <- data.frame(Method = c("Control","Recompress","ToolResearcher"), 
                          Total_Tagged = exp1Totals$Freq, Total_Recap = exp1Recap$Freq)

percentData$Percent_Recap = percentData$Total_Recap / percentData$Total_Tagged * 100 

barplot(height = percentData$Percent_Recap, names = percentData$Method, xlab="Method",
        ylab="Percentage (%)", main="Overall Percentage Recaptured by Method", ylim = c(0,30),
        col=rgb(0.4,0.7,0.7,1))

# now do the same as above but for the 3 distinct depth strata

depth1 = filter(exp1, Depth_Strata_Categorical == 1)
depth2 = filter(exp1, Depth_Strata_Categorical == 2)
depth3 = filter(exp1, Depth_Strata_Categorical == 3)

depth1totals = as.data.frame(table(depth1$Treatment))
depth2totals = as.data.frame(table(depth2$Treatment))
depth3totals = as.data.frame(table(depth3$Treatment))

depth1 = filter(depth1, Recap == 2)
depth2 = filter(depth2, Recap == 2)
depth3 = filter(depth3, Recap == 2)

depth1recaps = as.data.frame(table(depth1$Treatment))
depth2recaps = as.data.frame(table(depth2$Treatment))
depth3recaps = as.data.frame(table(depth3$Treatment))

depth1.frame <- data.frame(Method = c("Control", "Recompress", "ToolResearcher"),
                     Total_Tagged = depth1totals$Freq, Total_Recap = depth1recaps$Freq)

depth2.frame <- data.frame(Method = c("Control", "Recompress", "ToolResearcher"),
                     Total_Tagged = depth2totals$Freq, Total_Recap = depth2recaps$Freq)

depth3.frame <- data.frame(Method = c("Control", "Recompress", "ToolResearcher"),
                     Total_Tagged = depth3totals$Freq, Total_Recap = depth3recaps$Freq)

depth1$Percent = depth1$Total_Recap / depth1$Total_Tagged * 100
depth2$Percent = depth2$Total_Recap / depth2$Total_Tagged * 100
depth3$Percent = depth3$Total_Recap / depth3$Total_Tagged * 100

# create stacked bar graphs for each depth
# depth 1

barplot(depth1$Total_Tagged, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method
        75 - 85 feet",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,400))

barplot(height = depth1$Total_Recap, names = c("Control", "Recompress", "ToolResearcher"),
        xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# depth 2

barplot(height = depth2$Total_Tagged, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method
        95 - 105 feet",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,120))

barplot(depth2$Total_Recap, names = c("Control", "Recompress", "ToolResearcher"), 
        xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# depth 3

barplot(depth3$Total_Tagged, xlab="Method", ylab="Count", main="Total Black Sea Bass Caught and Recaptured per Method
        115 - 125 feet",
        col=rgb(0.9,0.9,0.8,1), ylim=c(0,600))

barplot(depth3$Total_Recap, names = c("Control", "Recompress", "ToolResearcher"), 
        xlab="Method", ylab="Count", 
        col=rgb(0.4,0.7,0.7,1), add=T)

legend("topright",inset = 0, legend=c("Total Caught","Total Recaptured"), 
       col=c(rgb(0.9,0.9,0.8,1), rgb(0.4,0.7,0.7,1)), pt.cex=2, pch=15 )

# now create graphs to show overall percentage of recaptures per method

barplot(height = depth1$Percent, names = c("Control", "Recompress", "ToolResearcher"), xlab="Method",
        ylab="Percentage (%)", main="Percentage Recaptured by Method, 75 - 85 feet", ylim = c(0,50),
        col=rgb(0.4,0.7,0.7,1))

barplot(height = depth2$Percent, names = c("Control", "Recompress", "ToolResearcher"), xlab="Method",
        ylab="Percentage (%)", main="Percentage Recaptured by Method, 95 - 105 feet", ylim = c(0,50),
        col=rgb(0.4,0.7,0.7,1))

barplot(height = depth3$Percent, names = c("Control", "Recompress", "ToolResearcher"), xlab="Method",
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

jan = filter(exp1, format(DateTagged, "%m") == "01")
feb = filter(exp1, format(DateTagged, "%m") == "02")
mar = filter(exp1, format(DateTagged, "%m") == "03")
apr = filter(exp1, format(DateTagged, "%m") == "04")
may = filter(exp1, format(DateTagged, "%m") == "05")
jun = filter(exp1, format(DateTagged, "%m") == "06")
jul = filter(exp1, format(DateTagged, "%m") == "07")
aug = filter(exp1, format(DateTagged, "%m") == "08")
sep = filter(exp1, format(DateTagged, "%m") == "09")
oct = filter(exp1, format(DateTagged, "%m") == "10")
nov = filter(exp1, format(DateTagged, "%m") == "11")
dec = filter(exp1, format(DateTagged, "%m") == "12")

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
jan1 = filter(jan, Depth_Strata_Categorical == 1)
jan2 = filter(jan, Depth_Strata_Categorical == 2)
jan3 = filter(jan, Depth_Strata_Categorical == 3)

feb1 = filter(feb, Depth_Strata_Categorical == 1)
feb2 = filter(feb, Depth_Strata_Categorical == 2)
feb3 = filter(feb, Depth_Strata_Categorical == 3)

mar1 = filter(mar, Depth_Strata_Categorical == 1)
mar2 = filter(mar, Depth_Strata_Categorical == 2)
mar3 = filter(mar, Depth_Strata_Categorical == 3)

apr1 = filter(apr, Depth_Strata_Categorical == 1)
apr2 = filter(apr, Depth_Strata_Categorical == 2)
apr3 = filter(apr, Depth_Strata_Categorical == 3)

may1 = filter(may, Depth_Strata_Categorical == 1)
may2 = filter(may, Depth_Strata_Categorical == 2)
may3 = filter(may, Depth_Strata_Categorical == 3)

jun1 = filter(jun, Depth_Strata_Categorical == 1)
jun2 = filter(jun, Depth_Strata_Categorical == 2)
jun3 = filter(jun, Depth_Strata_Categorical == 3)

jul1 = filter(jul, Depth_Strata_Categorical == 1)
jul2 = filter(jul, Depth_Strata_Categorical == 2)
jul3 = filter(jul, Depth_Strata_Categorical == 3)

aug1 = filter(aug, Depth_Strata_Categorical == 1)
aug2 = filter(aug, Depth_Strata_Categorical == 2)
aug3 = filter(aug, Depth_Strata_Categorical == 3)

sep1 = filter(sep, Depth_Strata_Categorical == 1)
sep2 = filter(sep, Depth_Strata_Categorical == 2)
sep3 = filter(sep, Depth_Strata_Categorical == 3)

oct1 = filter(oct, Depth_Strata_Categorical == 1)
oct2 = filter(oct, Depth_Strata_Categorical == 2)
oct3 = filter(oct, Depth_Strata_Categorical == 3)

nov1 = filter(nov, Depth_Strata_Categorical == 1)
nov2 = filter(nov, Depth_Strata_Categorical == 2)
nov3 = filter(nov, Depth_Strata_Categorical == 3)

dec1 = filter(dec, Depth_Strata_Categorical == 1)
dec2 = filter(dec, Depth_Strata_Categorical == 2)
dec3 = filter(dec, Depth_Strata_Categorical == 3)

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

exp2totals = table(exp2$Treatment)
exp2recapsOnly = filter(exp2, Recap == 2)
exp2recaps = table(exp2recapsOnly$Treatment)

exp2totals = as.data.frame(exp2totals)
exp2recaps = as.data.frame(exp2recaps)

barplot(height = exp2totals$Freq, names = c("Control", "Recompress", "ToolAngler", "ToolResearcher"), 
        main = "Total Black Sea Bass Tagged and Recaptured, Experiment 2",
        xlab = "Method", ylab = "Total", col = "slategray3")
barplot(height = exp2recaps$Freq, col = "slategrey", add = T)
legend("topright", legend=c("Total Tagged", "Total Recaptured"), 
       col = c("slategray3", "slategrey"), pt.cex = 2, pch=15)


exp2Percents <- data.frame(Method = c("Control", "Recompress", "ToolAngler", "ToolResearcher"),
                           Total_Tagged = exp2totals$Freq, Total_Recap = exp2recaps$Freq)
exp2Percents$Percent = exp2Percents$Total_Recap / exp2Percents$Total_Tagged * 100

barplot(height = exp2Percents$Percent, names = c("Control", "Recompress", "ToolAngler", "ToolResearcher"),
        main = "Percentage Recaptured by Method, Experiment 2", xlab = "Method",
        ylab = "Total", col = "slategray4", ylim = c(0,30))
