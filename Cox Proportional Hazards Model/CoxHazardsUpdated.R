# Hailey Schmidt
# June 9, 2021
# CMAST
# This script attempts to fit a Cox Proportional Hazards Model to data
#  of black sea bass.

# set libraries and import data set

library(coxme)
library(dplyr)
library(survival)
library(survminer)

setwd("C:/Users/bookw/Desktop/CMAST_Research")
bsbData <- read.csv("C:/Users/bookw/Desktop/CMAST_Research/Combined data file for bass project July 12 2021_HS.csv")
head(bsbData)
dim(bsbData)

# rename study number column

# names(combined)[names(combined) == "ï..Study_Num"] <- "Study_Num"

# filter data set 
# experiment 1: pulls from all combined data, but is looking only at control,
# descend, and vent by angler treatments.

# filter for experiments 1 and 2
exp1 <- filter(bsbData, ExperimentNum != "2")
#exp2 <- filter(combined, ExperimentNum != "1")

# some light housekeeping: convert things to factors, change column names

exp1$f.date = as.factor(exp1$TripNumForExperiment1)
exp1$trt = as.factor(exp1$TreatmentCodeJuly2021)
exp1$days = as.numeric(exp1$DaysAtLarge)
exp1$recap = exp1$Recap
exp1$depth = as.factor(exp1$DepthStrata)
exp1$surfaceTemp = as.numeric(exp1$SurfaceWaterTempC.Centered)
#exp1$bottomTemp = as.numeric(exp1$BottomWaterTempC_centered)
exp1$gradientTemp = as.numeric(exp1$TempGradientCenteredC)
exp1$TL = as.numeric(exp1$TL.Centered)

# quick test model: see if TL has a positive or negative effect on survival

testmod <- coxph(Surv(days, recap) ~ TL, data = exp1)
testmod.me <- coxme(Surv(days, recap) ~ TL + (1|f.date), data = exp1)

summary(testmod)
summary(testmod.me)

# create full models (with all possible covariates) and run ANOVA to see
# if any covariates are not significant

fullmod <- coxph(Surv(days, recap) ~ trt + depth + TL + gradientTemp, data = exp1)
fullmod.me <- coxme(Surv(days, recap) ~ trt + depth + TL + gradientTemp + (1|f.date),
                    data = exp1)

summary(fullmod)
summary(fullmod.me)

anova(fullmod)

# use step function to find covariates

test.set <- filter(exp1, !is.na(trt), !is.na(depth), !is.na(TL), !is.na(gradientTemp))
step.model = coxph(Surv(days, recap) ~ trt + depth + TL + gradientTemp + 
                     surfaceTemp + bottomTemp + trt*depth, data = test.set)

step.model2 = coxph(Surv(days, recap) ~ trt + depth + TL + gradientTemp + trt*depth, data = test.set)


step(step.model, direction = "both")
step(step.model2, direction = "both")


# mixed effect model w/ date tagged as random effect
# model 1: treatment only
# model 2: treatment and depth
# model 3: treatment interaction w/ depth
# model 4: treatment, depth, total length
# model 5: treatment, depth, TL, gradient temp
# model 6: treatment interaction w/ depth, TL, gradient temp
# model 7: depth, TL, gradient temp
# model 8: treatment interaction w/ depth, gradientTemp

me.mod1 <- coxme(Surv(days, recap) ~ trt + (1|f.date), data = exp1)
me.mod2 <- coxme(Surv(days, recap) ~ trt + depth + (1|f.date), data = exp1)
me.mod3 <- coxme(Surv(days, recap) ~ trt * depth + (1|f.date), data = exp1)
me.mod4 <- coxme(Surv(days, recap) ~ trt + depth + TL + (1|f.date), data = exp1)
 # me.mod5 <- coxme(Surv(days, recap) ~ trt + depth + TL + gradientTemp
 #                  + (1|f.date), data = exp1)

me.mod5 <- coxme(Surv(days, recap) ~ trt + depth + TL + surfaceTemp
                 + (1|f.date), data = exp1)

# me.mod6 <- coxme(Surv(days, recap) ~ trt * depth + TL + gradientTemp
#                  + (1|f.date), data = exp1)

me.mod6 <- coxme(Surv(days, recap) ~ trt * depth + TL + surfaceTemp
                 + (1|f.date), data = exp1)

# me.mod7 <- coxme(Surv(days, recap) ~ depth + TL + gradientTemp + (1|f.date),
#                  data = exp1)

me.mod7 <- coxme(Surv(days, recap) ~ depth + TL + surfaceTemp + (1|f.date),
                 data = exp1)

# me.mod8 <- coxme(Surv(days, recap) ~ trt * depth + gradientTemp + (1|f.date), data = exp1)

me.mod8 <- coxme(Surv(days, recap) ~ trt * depth + surfaceTemp + (1|f.date), data = exp1)

me.nullmodel <- coxme(Surv(days, recap) ~ 1 + (1|f.date), data = exp1)

# print summaries of mixed effect models

summary(me.mod1)
AIC(me.mod1)
summary(me.mod2)
AIC(me.mod2)
summary(me.mod3)
AIC(me.mod3)
summary(me.mod4)
AIC(me.mod4)
summary(me.mod5)
AIC(me.mod5)
summary(me.mod6)
AIC(me.mod6)
summary(me.mod7)
AIC(me.mod7)
summary(me.mod8)
AIC(me.mod8)
summary(me.nullmodel)
AIC(me.nullmodel)

# AIC values using AIC() command are different - should we be looking at those?

# fixed effect only models, same as above

mod1 <- coxph(Surv(days, recap) ~ trt, data = exp1)
mod2 <- coxph(Surv(days, recap) ~ trt + depth, data = exp1)
mod3 <- coxph(Surv(days, recap) ~ trt * depth, data = exp1)
mod4 <- coxph(Surv(days, recap) ~ trt + depth + TL, data = exp1)
mod5 <- coxph(Surv(days, recap) ~ trt + depth + TL + gradientTemp, data = exp1)
mod6 <- coxph(Surv(days, recap) ~ trt * depth + TL + gradientTemp, data = exp1)
mod7 <- coxph(Surv(days, recap) ~ depth + TL + gradientTemp, data = exp1)
mod8 <- coxph(Surv(days, recap) ~ trt * depth + gradientTemp, data = exp1)

nullmodel <- coxph(Surv(days, recap) ~ 1, data = exp1)

# print summary statistics and AIC values of fixed effect models

summary(mod1)
AIC(mod1)
summary(mod2)
AIC(mod2)
summary(mod3)
AIC(mod3)
summary(mod4)
AIC(mod4)
summary(mod5)
AIC(mod5)
summary(mod6)
AIC(mod6)
summary(mod7)
AIC(mod7)
summary(mod8)
AIC(mod8)
summary(nullmodel)
AIC(nullmodel)

#################################
#################################

# experiment 2: only from Study Number 2, looks at 4 treatments, ignores depth

exp2 <- filter(bsbData, ExperimentNum != "1")

# more housekeeping

exp2$f.date = as.factor(exp2$TripNumForExperiment2)
exp2$trt = as.factor(exp2$TreatmentCodeJuly2021)
exp2$days = as.numeric(exp2$DaysAtLarge)
exp2$recap = exp2$Recap
exp2$TL = as.numeric(exp2$TL.Centered)
exp2$gradientTemp = as.numeric(exp2$TempGradientCenteredC)
exp2$surfaceTemp = as.numeric(exp2$SurfaceWaterTempC.Centered)
#exp2$bottomTemp = as.numeric(exp2$BottomWaterTempC_centered)

# Mixed-effect models w/ date tagged as a random effect
# Model 1: treatment
# Model 2: treatment and total length
# Model 3: treatment and gradient temperature
# Model 4: treatment, total length, and gradient temperature


ex2.me.mod1 <- coxme(Surv(days, recap) ~ trt + (1|f.date), data = exp2)
ex2.me.mod2 <- coxme(Surv(days, recap) ~ trt + TL + (1|f.date), data = exp2)
#ex2.me.mod3 <- coxme(Surv(days, recap) ~ trt + gradientTemp + (1|f.date), 
#                     data = exp2)
ex2.me.mod3 <- coxme(Surv(days, recap) ~ trt + surfaceTemp + (1|f.date), 
                     data = exp2)

#ex2.me.mod4 <- coxme(Surv(days, recap) ~ trt + TL + gradientTemp + (1|f.date),
#                     data = exp2)
ex2.me.mod4 <- coxme(Surv(days, recap) ~ trt + TL + surfaceTemp + (1|f.date),
                     data = exp2)

ex2.me.nullmodel <- coxme(Surv(days, recap) ~ 1 + (1|f.date), data = exp2)

# print summaries of mixed-effect models

summary(ex2.me.mod1)
AIC(ex2.me.mod1)
summary(ex2.me.mod2)
AIC(ex2.me.mod2)
summary(ex2.me.mod3)
AIC(ex2.me.mod3)
summary(ex2.me.mod4)
AIC(ex2.me.mod4)
summary(ex2.me.nullmodel)
AIC(ex2.me.nullmodel)


# Fixed-effect models with same parameters as above

ex2.mod1 <- coxph(Surv(days, recap) ~ trt, data = exp2)
ex2.mod2 <- coxph(Surv(days, recap) ~ trt + TL, data = exp2)
ex2.mod3 <- coxph(Surv(days, recap) ~ trt + gradientTemp, data = exp2)
ex2.mod4 <- coxph(Surv(days, recap) ~ trt + TL + gradientTemp, data = exp2)
ex2.nullmodel <- coxph(Surv(days, recap) ~ 1, data = exp2)

# print summaries and AIC values of fixed-effect models

summary(ex2.mod1)
AIC(ex2.mod1)
summary(ex2.mod2)
AIC(ex2.mod2)
summary(ex2.mod3)
AIC(ex2.mod3)
summary(ex2.mod4)
AIC(ex2.mod4)
summary(ex2.nullmodel)
AIC(ex2.nullmodel)

# graphs?

ggadjustedcurves(mod2, variable = "trt", data=exp1, legend = "bottom", 
                 legend.title = "Treatment", ylim=c(0.6,1), xlim=c(0,400), 
                 ylab = 'Proportion not recaptured', xlab='Time (days)')


############
# run models for experiment 1 without depth strata 2
############

exp1 <- filter(exp1, DepthStrata != 2)
exp1$DepthStrata = as.factor(exp1$DepthStrata)

# mixed effect model w/ date tagged as random effect
# model 1: treatment only
# model 2: treatment and depth
# model 3: treatment interaction w/ depth
# model 4: treatment, depth, total length
# model 5: treatment, depth, TL, gradient temp
# model 6: treatment interaction w/ depth, TL, gradient temp
# model 7: depth, TL, gradient temp
# model 8: treatment interaction w/ depth, gradientTemp

me.mod1 <- coxme(Surv(days, recap) ~ trt + (1|f.date), data = exp1)
me.mod2 <- coxme(Surv(days, recap) ~ trt + depth + (1|f.date), data = exp1)
me.mod3 <- coxme(Surv(days, recap) ~ trt * depth + (1|f.date), data = exp1)
me.mod4 <- coxme(Surv(days, recap) ~ trt + depth + TL + (1|f.date), data = exp1)
# me.mod5 <- coxme(Surv(days, recap) ~ trt + depth + TL + gradientTemp
#                  + (1|f.date), data = exp1)

me.mod5 <- coxme(Surv(days, recap) ~ trt + depth + TL + surfaceTemp
                 + (1|f.date), data = exp1)

# me.mod6 <- coxme(Surv(days, recap) ~ trt * depth + TL + gradientTemp
#                  + (1|f.date), data = exp1)

me.mod6 <- coxme(Surv(days, recap) ~ trt * depth + TL + surfaceTemp
                 + (1|f.date), data = exp1)

# me.mod7 <- coxme(Surv(days, recap) ~ depth + TL + gradientTemp + (1|f.date),
#                  data = exp1)

me.mod7 <- coxme(Surv(days, recap) ~ depth + TL + surfaceTemp + (1|f.date),
                 data = exp1)

# me.mod8 <- coxme(Surv(days, recap) ~ trt * depth + gradientTemp + (1|f.date), data = exp1)

me.mod8 <- coxme(Surv(days, recap) ~ trt * depth + surfaceTemp + (1|f.date), data = exp1)

me.nullmodel <- coxme(Surv(days, recap) ~ 1 + (1|f.date), data = exp1)

# print summaries of mixed effect models

summary(me.mod1)
AIC(me.mod1)
summary(me.mod2)
AIC(me.mod2)
summary(me.mod3)
AIC(me.mod3)
summary(me.mod4)
AIC(me.mod4)
summary(me.mod5)
AIC(me.mod5)
summary(me.mod6)
AIC(me.mod6)
summary(me.mod7)
AIC(me.mod7)
summary(me.mod8)
AIC(me.mod8)
summary(me.nullmodel)
AIC(me.nullmodel)



################################
################################
# # some old code from just messing around 
# 
# # filter data set for experiment 1 and 2
# # for experiment 1, we are using all data but excluding treatment 4
# # (vent by angler)
# exp1 <- filter(combined, TreatmentNumber != 4)
# exp1$DaysAtLarge = as.integer(exp1$DaysAtLarge)
# 
# # first, run a univariate cox model using treatment number (just to see
# #  how this works)
# 
# result1 = coxph(Surv(DaysAtLarge, Recap) ~ TreatmentNumber, data = exp1)
# 
# # now, run a multivariate cox model using: TreatmentNumber, Depth Strata
# 
# result2 = coxph(Surv(DaysAtLarge, Recap) ~ TreatmentNumber + Depth_Strata_Categorical,
#                 data = exp1)
# 
# ggsurvplot(survfit(result1), data = result1)
# ggsurvplot(survfit(result2), data = result2)
# 
# # Experiment 2
# 
# exp2 <- filter(combined, Study_Num == 2, TreatmentNumber != 2)
# exp2$DaysAtLarge = as.integer(exp2$DaysAtLarge)
# 
# result3 = coxph(Surv(DaysAtLarge, Recap) ~ TreatmentNumber, data = exp2)
# ggsurvplot(survfit(result3), data = result3)
