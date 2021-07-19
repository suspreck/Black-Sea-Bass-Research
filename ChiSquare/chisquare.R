# Hailey Schmidt
# June 7, 2021
# CMAST
# This script performs Chi Square tests on black sea bass data.
# We are testing to see if there is a correlation between treatment
# and whether a fish swims or doesn't swim after release. 

# import combined data set
library(dplyr)
library(ggplot2)
combined <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/Combined file from both BSB venting descending studies updated June 2021_HS.csv")

# filter into two data sets: experiment 1 and experiment 2
# use Study_Num field
# also filter out recompression treatments - we are only looking at venting
exp1 <- filter(combined, TreatmentNumber != 2, TreatmentNumber != 4)
exp2 <- filter(combined, Study_Num==2, TreatmentNumber != 2)

# let's do the experiment 2 chi square test first since it only needs one
# make a two way table w/ treatment and swim status
experiment2_test <- table(exp2$TreatmentNumber, exp2$Swim)

# perform chi square test
chi1 <- chisq.test(experiment2_test)

# now lets do the data from experiment 1
# first we need to filter the data set into 3 depth strata
exp1shallow <- filter(exp1, Depth_Strata_Categorical == 1)
exp1mid <- filter(exp1, Depth_Strata_Categorical == 2)
exp1deep <- filter(exp1, Depth_Strata_Categorical == 3)

# make two way tables
experiment1_75to85_test <- table(exp1shallow$TreatmentNumber, exp1shallow$Swim)
experiment1_95to105_test <- table(exp1mid$TreatmentNumber, exp1mid$Swim)
experiment1_115to125_test <- table(exp1deep$TreatmentNumber, exp1deep$Swim)
#test5 <- table(exp1$TreatmentNumber, exp1$Swim)

# perform chi square tests
chi2 <- chisq.test(experiment1_75to85_test)
chi3 <- chisq.test(experiment1_95to105_test)
chi4 <- chisq.test(experiment1_115to125_test)
#chi5 <- chisq.test(test5)

# print expected p-values
chi1
chi2
chi3
chi4
#chi5

# redo labels on tables to make them easier to read

colnames(experiment2_test) = c("No Swim", "Swim")
rownames(experiment2_test)[rownames(experiment2_test) == "1"] = "Control"
rownames(experiment2_test)[rownames(experiment2_test) == "3"] = "VentResearcher"
rownames(experiment2_test)[rownames(experiment2_test) == "4"] = "VentAngler"

colnames(experiment1_75to85_test) = c("No Swim", "Swim")
rownames(experiment1_75to85_test)[rownames(experiment1_75to85_test) == "1"] = "Control"
rownames(experiment1_75to85_test)[rownames(experiment1_75to85_test) == "3"] = "VentResearcher"

colnames(experiment1_95to105_test) = c("No Swim", "Swim")
rownames(experiment1_95to105_test)[rownames(experiment1_95to105_test) == "1"] = "Control"
rownames(experiment1_95to105_test)[rownames(experiment1_95to105_test) == "3"] = "VentResearcher"

colnames(experiment1_115to125_test) = c("No Swim", "Swim")
rownames(experiment1_115to125_test)[rownames(experiment1_115to125_test) == "1"] = "Control"
rownames(experiment1_115to125_test)[rownames(experiment1_115to125_test) == "3"] = "VentResearcher"
