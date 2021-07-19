# Hailey Schmidt
# June 7, 2021
# CMAST
# This script performs Chi Square tests on black sea bass data.
# We are testing to see if there is a correlation between treatment
# and whether a fish swims or doesn't swim after release. 

# import combined data set
library(dplyr)
library(ggplot2)
setwd("C:/Users/bookw/Desktop/CMAST_Research")
bsbData <- read.csv("Combined data file for bass project July 12 2021_HS.csv")

# filter into two data sets: experiment 1 and experiment 2
# also filter out recompression treatments - we are only looking at venting
exp1 <- filter(bsbData, ExperimentNum != "2", TreatmentCodeJuly2021 != 2)
exp2 <- filter(bsbData, ExperimentNum != "1", TreatmentCodeJuly2021 != 2)
bsbData <- filter(bsbData, TreatmentCodeJuly2021 != 2)

# let's do the experiment 2 chi square test first since it only needs one
# make a two way table w/ treatment and swim status
experiment2_test <- table(exp2$TreatmentCodeJuly2021, exp2$Swim)

# perform chi square test
chi1 <- chisq.test(experiment2_test)

# now lets do the data from experiment 1
# first we need to filter the data set into 3 depth strata
exp1shallow <- filter(exp1, DepthStrata == 1)
exp1mid <- filter(exp1, DepthStrata == 2)
exp1deep <- filter(exp1, DepthStrata == 3)

# make two way tables
experiment1_75to85_test <- table(exp1shallow$TreatmentCodeJuly2021, exp1shallow$Swim)
experiment1_95to105_test <- table(exp1mid$TreatmentCodeJuly2021, exp1mid$Swim)
experiment1_115to125_test <- table(exp1deep$TreatmentCodeJuly2021, exp1deep$Swim)
#test5 <- table(exp1$TreatmentNumber, exp1$Swim)

# print tables, because sometimes that is useful to see
experiment1_75to85_test
experiment1_95to105_test
experiment1_115to125_test

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

# print tables, because sometimes that is useful to see
experiment1_75to85_test
experiment1_95to105_test
experiment1_115to125_test
experiment2_test


################
# do chi square tests to see if there is indenpendence between submergence
# success and recapture (i.e. survival)
################

# overall for both experiments
# make two-way table

recap_swim <- table(bsbData$Recap, bsbData$Swim)
colnames(recap_swim) = c("No Swim", "Swim")
rownames(recap_swim)[rownames(recap_swim) == "1"] = "Not recaptured"
rownames(recap_swim)[rownames(recap_swim) == "2"] = "Recaptured"
recap_swim

chi5 <- chisq.test(recap_swim)
chi5

# now for each depth strata

recap_shallow <- table(exp1shallow$Recap, exp1shallow$Swim)
colnames(recap_shallow) = c("No Swim", "Swim")
rownames(recap_shallow)[rownames(recap_shallow) == "1"] = "Not recaptured"
rownames(recap_shallow)[rownames(recap_shallow) == "2"] = "Recaptured"

recap_mid <- table(exp1mid$Recap, exp1mid$Swim)
colnames(recap_mid) = c("No Swim", "Swim")
rownames(recap_mid)[rownames(recap_mid) == "1"] = "Not recaptured"
rownames(recap_mid)[rownames(recap_mid) == "2"] = "Recaptured"

recap_deep <- table(exp1deep$Recap, exp1deep$Swim)
colnames(recap_deep) = c("No Swim", "Swim")
rownames(recap_deep)[rownames(recap_deep) == "1"] = "Not recaptured"
rownames(recap_deep)[rownames(recap_deep) == "2"] = "Recaptured"

recap_shallow
recap_mid
recap_deep

chi6 <- chisq.test(recap_shallow)
chi7 <- chisq.test(recap_mid)
chi8 <- chisq.test(recap_deep)

chi6
chi7
chi8

# finally, for experiment 2

recap_exp2 <- table(exp2$Recap, exp2$Swim)
colnames(recap_exp2) = c("No Swim", "Swim")
rownames(recap_exp2)[rownames(recap_exp2) == "1"] = "Not recaptured"
rownames(recap_exp2)[rownames(recap_exp2) == "2"] = "Recaptured"

recap_exp2

chi9 <- chisq.test(recap_exp2)
chi9
