# Hailey Schmidt
# 7/9/2021
# CMAST
# This script creates some basic plots and graphs to examine if there is any
# unexpected correlation between covariates


# set up library and open files

library(dplyr)
library(ggplot2)
setwd("C:/Users/bookw/Desktop/CMAST_Research")
bsbData <- read.csv("Combined data file for bass project July 12 2021_HS.csv")

# change ExperimentNum to be factor instead of string
bsbData$ExperimentNum <- as.factor(bsbData$ExperimentNum)

# filter for experiments 1 and 2
exp1 <- filter(bsbData, ExperimentNum != "2")
exp2 <- filter(bsbData, ExperimentNum != "1")

exp1$DepthStrata = as.factor(exp1$DepthStrata)

# Depth vs Fish Size
# one with categorical strata, one with continuous depth

plot(exp1$DepthStrata, exp1$TLmm)
plot(exp1$DepthFt, exp1$TLmm)

boxplot(TLmm~DepthStrata, data=exp1)

# Fish Size vs Temp
# one for surface temp, one for bottom temp

plot(exp1$SurfaceWaterTempC, exp1$TLmm)
plot(exp1$BottomWaterTempC, exp1$TLmm)

plot(exp1$TLmm, exp1$SurfaceWaterTempC)
plot(exp1$TLmm, exp1$BottomWaterTempC)

# Fish Size vs Gradient Temperature

plot(exp1$TempGradientC, exp1$TLmm)
plot(exp1$TLmm, exp1$TempGradientC)


# Temperature vs Depth
# one for surface temp, one for bottom temp

plot(exp1$DepthStrata, exp1$SurfaceWaterTempC)
plot(exp1$DepthFt, exp1$SurfaceWaterTempC)

plot(exp1$DepthStrata, exp1$BottomWaterTempC)
plot(exp1$DepthFt, exp1$BottomWaterTempC)

boxplot(SurfaceWaterTempC~DepthStrata, data=exp1)
boxplot(BottomWaterTempC~DepthStrata, data=exp1)

# Gradient Temperature vs Depth

plot(exp1$DepthStrata, exp1$TempGradientC)
plot(exp1$DepthFt, exp1$TempGradientC)

boxplot(TempGradientC~DepthStrata, data=exp1)


# Grouped box and whisker plot: Size vs depth+trt

boxplot(TLmm~DepthStrata+TreatmentType, data=exp1)

# Grouped box and whisker plot: Temp vs depth+trt
# Surface, bottom, and gradient

boxplot(SurfaceWaterTempC~DepthStrata+TreatmentType, data=exp1)
boxplot(BottomWaterTempC~DepthStrata+TreatmentType, data=exp1)
boxplot(TempGradientC~DepthStrata+TreatmentType, data=exp1)


#######
# Use one way ANOVA to compare Total Length and temperature across depths

TL_depth = aov(TLmm ~ DepthStrata, data = exp1)
summary(TL_depth)

temp_depth = aov(SurfaceWaterTempC ~ DepthStrata, data = exp1)
summary(temp_depth)

temp_size = aov(SurfaceWaterTempC ~ TLmm, data = exp1)
summary(temp_size)
