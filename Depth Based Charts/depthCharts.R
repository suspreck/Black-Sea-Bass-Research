# Hailey Schmidt
# June 2 2021
# CMAST
#   This script pulls information from two separate data sets to create one set
#   containing all records for tagged black sea bass. This is used to create
#   graphs showing the total and relative numbers of tagged vs. recaptured fish
#   for a variety of treatments.

newSet <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/Black Sea Bass/CRP Vent Descend Data Updated Jun 2 2021_HS.csv", stringsAsFactors=TRUE)
oldSet <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/BSB Decender Data through Mar 23 2018.csv")
library(dplyr)


# first, we must filter newSet to contain only black sea bass

newSet <- filter(newSet, newSet$Species == "bsb")

# we know that the old set was only black sea bass, so now both sets have the same species
# next, we want to filter out all entries that used treatment 4 for both sets
# (idk why tho)

newSet <- filter(newSet, is.na(newSet$TreatmentPrincessStacy) | newSet$TreatmentPrincessStacy != 4)
newSet <- filter(newSet, is.na(newSet$TreatmentSensationWhaler) | newSet$TreatmentSensationWhaler != 4)
oldSet <- filter(oldSet, oldSet$Treatment != 4)

# now, we should combine the Princess and Sensation methods into one factor

newSet$methodsCombined = ifelse(is.na(newSet$TreatmentPrincessStacy), newSet$TreatmentSensationWhaler,
                                newSet$TreatmentPrincessStacy)

# now, create two data sets which only contain Depth, Method, and Recapture Status
newData <- data.frame(Depth = newSet$DepthFt, Method = newSet$methodsCombined, Status = newSet$Recap)
oldData <- data.frame(Depth = oldSet$Depth.ft., Method = oldSet$Treatment)

# create a recapture variable for old data set: 1 of not recaptured, 2 if recaptured
# evaluate by seeing if recapture date column is empty ("")
oldData$Status = ifelse(oldSet$Tag.Return.Date == "", 1, 2)

# some of the old set entries don't have a depth, so we will get rid of those
oldData <- filter(oldData, !is.na(oldData$Depth))

# finally, we want to combine both of these into one combined data set
combinedData = rbind(newData, oldData)

#   we now have a data set containing all records of black sea bass tags along with
#   depth, recapture status, and method used.
# -----
# next: create separate data sets based on depth
# categories are: 75-85 feet, 95-105 feet, 115-125 feet.
# the range of the data set is 70 - 129 feet, so some entries will be left out
# there are also entries with a depth between these values
# maybe fix this later

depth75to85 <- filter(combinedData, combinedData$Depth <= 85 & combinedData$Depth >= 75)
depth95to105 <- filter(combinedData, combinedData$Depth <= 105 & combinedData$Depth >= 95)
depth115to125 <- filter(combinedData, combinedData$Depth <= 125 & combinedData$Depth >= 115)

# find total tagged individuals per method and total recaptured individuals per method per depth
# Create two way tables showing method and status for each depth

totalsShallow = table(depth75to85$Method, depth75to85$Status)
totalsMid = table(depth95to105$Method, depth95to105$Status)
totalsDeep = table(depth115to125$Method, depth115to125$Status)

# convert these tables into data frames for easier analysis
# Column Headings 1, 2 correspond to recapture status (1 = yes, 2 = no)
# Row Headings 1, 2, 3 correspond to method used
# TODO: change these to names

totalsShallow = as.data.frame.matrix(totalsShallow)
totalsMid = as.data.frame.matrix(totalsMid)
totalsDeep = as.data.frame.matrix(totalsDeep)


# now lets do some graphs
# these will be stacked bar graphs showing total # of fish tagged and total # recaptured

barplot(height=totalsShallow$'1'+totalsShallow$'2', names = c("1", "2", "3"),
        xlab="Method", ylab="Count", main="Total Catch and Recapture of Black Seabass, 75 - 85 feet",
        col=rgb(0.7, 0.8, 0.9, 1))
barplot(height=totalsShallow$'2', names = c("1", "2", "3"), col=rgb(0.3, 0.4, 0.5, 1),
        add=T)
legend("topright", legend=c("Total Caught","Total Recaptured"), col=c(rgb(0.7,0.8,0.9,1), 
                                                                                 rgb(0.3,0.4,0.5,1)), pt.cex=2, pch=15 )

barplot(height=totalsMid$'1'+totalsMid$'2', names = c("1", "2", "3"),
        xlab="Method", ylab="Count", main="Total Catch and Recapture of Black Seabass, 95 - 105 feet",
        col=rgb(0.7, 0.8, 0.9, 1))
barplot(height=totalsMid$'2', names = c("1", "2", "3"), col=rgb(0.3, 0.4, 0.5, 1),
        add=T)
legend("topright", legend=c("Total Caught","Total Recaptured"), col=c(rgb(0.7,0.8,0.9,1), 
                                                                      rgb(0.3,0.4,0.5,1)), pt.cex=2, pch=15 )

barplot(height=totalsDeep$'1'+totalsDeep$'2', names = c("1", "2", "3"),
        xlab="Method", ylab="Count", main="Total Catch and Recapture of Black Seabass, 115 - 125 feet",
        col=rgb(0.7, 0.8, 0.9, 1))
barplot(height=totalsDeep$'2', names = c("1", "2", "3"), col=rgb(0.3, 0.4, 0.5, 1),
        add=T)
legend("topright", legend=c("Total Caught","Total Recaptured"), col=c(rgb(0.7,0.8,0.9,1), 
                                                                      rgb(0.3,0.4,0.5,1)), pt.cex=2, pch=15 )

# calculate percentage of recaptured fish for each method for each depth
# just add a percentage column to each table

totalsShallow$Percentage = totalsShallow$'2' / (totalsShallow$'1' + totalsShallow$'2') * 100
totalsMid$Percentage = totalsMid$'2' / (totalsMid$'1' + totalsMid$'2') * 100
totalsDeep$Percentage = totalsDeep$'2' / (totalsDeep$'1' + totalsDeep$'2') * 100

# now graph percentages per method for each depth

barplot(height=totalsShallow$Percentage, names=c("1","2","3"), xlab="Method",
        ylab="Percentage Recaptured (%)", 
        main="Percentage of Tagged Black Seabass Recaptured per Method, 75 - 85 feet",
        ylim=c(0,50), col=rgb(0.3,0.4,0.5,1))

barplot(height=totalsMid$Percentage, names=c("1","2","3"), xlab="Method",
        ylab="Percentage Recaptured (%)", 
        main="Percentage of Tagged Black Seabass Recaptured per Method, 95 - 105 feet",
        ylim=c(0,50), col=rgb(0.3,0.4,0.5,1))

barplot(height=totalsDeep$Percentage, names=c("1","2","3"), xlab="Method",
        ylab="Percentage Recaptured (%)", 
        main="Percentage of Tagged Black Seabass Recaptured per Method, 115 - 125 feet", 
        ylim=c(0,50), col=rgb(0.3,0.4,0.5,1))

# just for fun: calculate total tagged and total recaptured across all methods
# and depths

#TODO: fix row names so they are less confusing

totals <- rbind.data.frame(totalsShallow, totalsMid, totalsDeep)

totalCatch = sum(totals$'1') + sum(totals$'2')
totalRecaptured = sum(totals$'2')

barplot(height=totalRecaptured / totalCatch * 100, ylab="Percentage(%)")
