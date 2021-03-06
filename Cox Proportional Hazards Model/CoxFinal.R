# Hailey Schmidt
# July 26, 2021
# CMAST
# This script fits the final Cox Proportional Hazards Model to our black sea
# bass data.

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

# names(combined)[names(combined) == "�..Study_Num"] <- "Study_Num"

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


# mixed effect model w/ date tagged as random effect

# model 4: treatment, depth, total length

me.mod <- coxme(Surv(days, recap) ~ trt + depth + TL + (1|f.date), data = exp1)

me.nullmodel <- coxme(Surv(days, recap) ~ 1 + (1|f.date), data = exp1)


summary(me.mod)
AIC(me.mod)
summary(me.nullmodel)
AIC(me.nullmodel)

# AIC values using AIC() command are different - should we be looking at those?

# fixed effect only models, same as above


mod <- coxph(Surv(days, recap) ~ trt + depth + TL, data = exp1)

nullmodel <- coxph(Surv(days, recap) ~ 1, data = exp1)

# print summary statistics and AIC values of fixed effect models

summary(mod)
AIC(mod)
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
# Model 2: treatment and total length



ex2.me.mod <- coxme(Surv(days, recap) ~ trt + TL + (1|f.date), data = exp2)

ex2.me.nullmodel <- coxme(Surv(days, recap) ~ 1 + (1|f.date), data = exp2)

# print summaries of mixed-effect models


summary(ex2.me.mod)
AIC(ex2.me.mod)
summary(ex2.me.nullmodel)
AIC(ex2.me.nullmodel)


# Fixed-effect models with same parameters as above

ex2.mod <- coxph(Surv(days, recap) ~ trt + TL, data = exp2)

ex2.nullmodel <- coxph(Surv(days, recap) ~ 1, data = exp2)

# print summaries and AIC values of fixed-effect models

summary(ex2.mod)
AIC(ex2.mod)
summary(ex2.nullmodel)
AIC(ex2.nullmodel)

# graphs?

ggadjustedcurves(mod, variable = "trt", data=exp1, legend = "bottom", 
                 legend.title = "Treatment", ylim=c(0.7,1), xlim=c(0,300), 
                 ylab = 'Proportion not recaptured', xlab='Time (days)',
                 palette=c("gray0","gray30","gray60"), size=1.25)

ggadjustedcurves(ex2.mod, variable = "trt", data=exp2, legend = "bottom", 
                 legend.title = "Treatment", ylim=c(0.7,1), xlim=c(0,85), 
                 ylab = 'Proportion not recaptured', xlab='Time (days)',
                 palette=c("gray0","gray30","gray60","gray80"), size=1)

#intervals are not calculated natively in the coxme package. 
#I hard-code it here so if the model changes you get them automatically by running this code. 

CIresults<-matrix(NA,length(me.mod$coefficients),4)
colnames(CIresults)<-c("Variable","Mean","2.5","97.5")
CIresults[,1]<-names(me.mod$coefficients)
CIresults[,2]<-exp(me.mod$coefficients)

covs<-sqrt(vcov(me.mod))

for (i in 1:length(me.mod$coefficients)){
  CIresults[i,3]<-exp(me.mod$coefficients[i]-(1.96*covs[i,i]))
  CIresults[i,4]<-exp(me.mod$coefficients[i]+(1.96*covs[i,i]))
}

CIresults = as.data.frame(CIresults)
CIresults$Mean = as.numeric(CIresults$Mean)
CIresults$`2.5` = as.numeric(CIresults$`2.5`)
CIresults$`97.5` = as.numeric(CIresults$`97.5`)
CIresults

CIresults2<-matrix(NA,length(ex2.me.mod$coefficients),4)
colnames(CIresults2)<-c("Variable","Mean","2.5","97.5")
CIresults2[,1]<-names(ex2.me.mod$coefficients)
CIresults2[,2]<-exp(ex2.me.mod$coefficients)

covs<-sqrt(vcov(ex2.me.mod))

for (i in 1:length(ex2.me.mod$coefficients)){
  CIresults2[i,3]<-exp(ex2.me.mod$coefficients[i]-(1.96*covs[i,i]))
  CIresults2[i,4]<-exp(ex2.me.mod$coefficients[i]+(1.96*covs[i,i]))
}

CIresults2 = as.data.frame(CIresults2)
CIresults2$Mean = as.numeric(CIresults2$Mean)
CIresults2$`2.5` = as.numeric(CIresults2$`2.5`)
CIresults2$`97.5` = as.numeric(CIresults2$`97.5`)
CIresults2

# graph for experiment 1 overall
########################################################## RR Graph Updated Jan 2017       
# install.packages('plotrix')
library('plotrix')       #package for std.error bars


par(mar=c(4,5,1,2))

VentingTool <- c(CIresults[2,3],CIresults[2,2],CIresults[2,4])      #2.5, median, 97.5 values, in that order
Descender <- c(CIresults[1,3],CIresults[1,2],CIresults[1,4])          #2.5, median, 97.5 values, in that order

x=c(1,2)
y=cbind(VentingTool[2], Descender[2])          

plot(x,y,xlab="Experimental treatment", xaxt='n',
     ylab="Mean relative survival (2.5/97.5 CI)",
     xlim=c(0.5,2.5), ylim=c(0, 2.1), cex=2, cex.lab=1.2, cex.axis=1.1, main="", cex.main=1.5,
     col=c("gray25", "gray50"), pch=19)
title("", line=-1, adj=0.5, cex.main=1)

axis(1, labels=c("Venting cannula", "Recompression device"), at=seq(1,2), cex.axis=1.1)

arrows(1, VentingTool[2], 1, c(VentingTool[1], VentingTool[3]), angle=90, length=1/8)
arrows(2, Descender[2], 2, c(Descender[1], Descender[3]), angle=90, length=1/8)

abline(h=1, col="red", lty=1)

# make 3 panel graph showing results at each depth strata
par(mfrow=c(1,3))
shallow <- filter(exp1, DepthStrata == 1)
mid <- filter(exp1, DepthStrata == 2)
deep <- filter(exp1, DepthStrata == 3)

# graph for experiment 1 shallow depth

shallow.me.mod <- coxme(Surv(days, recap) ~ trt + TL + (1|f.date), data = shallow)

shallowCI<-matrix(NA,length(shallow.me.mod$coefficients),4)
colnames(shallowCI)<-c("Variable","Mean","2.5","97.5")
shallowCI[,1]<-names(shallow.me.mod$coefficients)
shallowCI[,2]<-exp(shallow.me.mod$coefficients)

covs<-sqrt(vcov(shallow.me.mod))

for (i in 1:length(shallow.me.mod$coefficients)){
  shallowCI[i,3]<-exp(shallow.me.mod$coefficients[i]-(1.96*covs[i,i]))
  shallowCI[i,4]<-exp(shallow.me.mod$coefficients[i]+(1.96*covs[i,i]))
}

shallowCI = as.data.frame(shallowCI)
shallowCI$Mean = as.numeric(shallowCI$Mean)
shallowCI$`2.5` = as.numeric(shallowCI$`2.5`)
shallowCI$`97.5` = as.numeric(shallowCI$`97.5`)
shallowCI


VentingTool <- c(shallowCI[2,3],shallowCI[2,2],shallowCI[2,4])      #2.5, median, 97.5 values, in that order
Descender <- c(shallowCI[1,3],shallowCI[1,2],shallowCI[1,4])          #2.5, median, 97.5 values, in that order

x=c(1,2)
y=cbind(VentingTool[2], Descender[2])          

plot(x,y,xlab="Experimental treatment", xaxt='n',
     ylab="Mean relative survival (2.5/97.5 CI)",
     xlim=c(0.5,2.5), ylim=c(0, 2.1), cex=2, cex.lab=1.2, cex.axis=1.1, main="", cex.main=1.5,
     col=c("gray25", "gray50"), pch=19)
title("23-26 m", line=1, adj=0.5, cex.main=1)

axis(1, labels=c("Venting cannula", "Recompression device"), at=seq(1,2), cex.axis=1.1)

arrows(1, VentingTool[2], 1, c(VentingTool[1], VentingTool[3]), angle=90, length=1/8)
arrows(2, Descender[2], 2, c(Descender[1], Descender[3]), angle=90, length=1/8)

abline(h=1, col="red", lty=1)

# graph for experiment 1 medium depth

med.me.mod <- coxme(Surv(days, recap) ~ trt + TL + (1|f.date), data = mid)

midCI<-matrix(NA,length(med.me.mod$coefficients),4)
colnames(midCI)<-c("Variable","Mean","2.5","97.5")
midCI[,1]<-names(med.me.mod$coefficients)
midCI[,2]<-exp(med.me.mod$coefficients)

covs<-sqrt(vcov(med.me.mod))

for (i in 1:length(med.me.mod$coefficients)){
  midCI[i,3]<-exp(med.me.mod$coefficients[i]-(1.96*covs[i,i]))
  midCI[i,4]<-exp(med.me.mod$coefficients[i]+(1.96*covs[i,i]))
}

midCI = as.data.frame(midCI)
midCI$Mean = as.numeric(midCI$Mean)
midCI$`2.5` = as.numeric(midCI$`2.5`)
midCI$`97.5` = as.numeric(midCI$`97.5`)
midCI


VentingTool <- c(midCI[2,3],midCI[2,2],midCI[2,4])      #2.5, median, 97.5 values, in that order
Descender <- c(midCI[1,3],midCI[1,2],midCI[1,4])          #2.5, median, 97.5 values, in that order

x=c(1,2)
y=cbind(VentingTool[2], Descender[2])          

plot(x,y,xlab="Experimental treatment", xaxt='n',
     ylab="Mean relative survival (2.5/97.5 CI)",
     xlim=c(0.5,2.5), ylim=c(0, 2.1), cex=2, cex.lab=1.2, cex.axis=1.1, main="", cex.main=1.5,
     col=c("gray25", "gray50"), pch=19)
title("29-32 m", line=1, adj=0.5, cex.main=1)

axis(1, labels=c("Venting cannula", "Recompression device"), at=seq(1,2), cex.axis=1.1)

arrows(1, VentingTool[2], 1, c(VentingTool[1], VentingTool[3]), angle=90, length=1/8)
arrows(2, Descender[2], 2, c(Descender[1], Descender[3]), angle=90, length=1/8)

abline(h=1, col="red", lty=1)

# graph for experiment 1 deepest depth

deep.me.mod <- coxme(Surv(days, recap) ~ trt + TL + (1|f.date), data = deep)

deepCI<-matrix(NA,length(deep.me.mod$coefficients),4)
colnames(deepCI)<-c("Variable","Mean","2.5","97.5")
deepCI[,1]<-names(deep.me.mod$coefficients)
deepCI[,2]<-exp(deep.me.mod$coefficients)

covs<-sqrt(vcov(deep.me.mod))

for (i in 1:length(deep.me.mod$coefficients)){
  deepCI[i,3]<-exp(deep.me.mod$coefficients[i]-(1.96*covs[i,i]))
  deepCI[i,4]<-exp(deep.me.mod$coefficients[i]+(1.96*covs[i,i]))
}

deepCI = as.data.frame(deepCI)
deepCI$Mean = as.numeric(deepCI$Mean)
deepCI$`2.5` = as.numeric(deepCI$`2.5`)
deepCI$`97.5` = as.numeric(deepCI$`97.5`)
deepCI


VentingTool <- c(deepCI[2,3],deepCI[2,2],deepCI[2,4])      #2.5, median, 97.5 values, in that order
Descender <- c(deepCI[1,3],deepCI[1,2],deepCI[1,4])          #2.5, median, 97.5 values, in that order

x=c(1,2)
y=cbind(VentingTool[2], Descender[2])          

plot(x,y,xlab="Experimental treatment", xaxt='n',
     ylab="Mean relative survival (2.5/97.5 CI)",
     xlim=c(0.5,2.5), ylim=c(0, 2.1), cex=2, cex.lab=1.2, cex.axis=1.1, main="", cex.main=1.5,
     col=c("gray25", "gray50"), pch=19)
title("35-38 m", line=1, adj=0.5, cex.main=1)

axis(1, labels=c("Venting cannula", "Recompression device"), at=seq(1,2), cex.axis=1.1)

arrows(1, VentingTool[2], 1, c(VentingTool[1], VentingTool[3]), angle=90, length=1/8)
arrows(2, Descender[2], 2, c(Descender[1], Descender[3]), angle=90, length=1/8)

abline(h=1, col="red", lty=1)

# graph for experiment 2

VentResearcher <- c(CIresults2[2,3],CIresults2[2,2],CIresults2[2,4])      #2.5, median, 97.5 values, in that order
VentAngler <- c(CIresults2[3,3],CIresults2[3,2],CIresults2[3,4])      #2.5, median, 97.5 values, in that order
Descender <- c(CIresults2[1,3],CIresults2[1,2],CIresults2[1,4])          #2.5, median, 97.5 values, in that order

x=c(1,2,3)
y=cbind(VentResearcher[2], VentAngler[2], Descender[2])          

plot(x,y,xlab="Experimental treatment", xaxt='n',
     ylab="Mean relative survival (2.5/97.5 CI)",
     xlim=c(0.75,3.25), ylim=c(0, 2.1), cex=2, cex.lab=1.2, cex.axis=1.1, main="", cex.main=1.5,
     col=c("gray25", "gray50", "gray70"), pch=19)
title("", line=-1, adj=0.5, cex.main=1)

axis(1, labels=c("Vent by Researcher", "Vent by Angler", "Recompression device"), at=seq(1,3), cex.axis=1.1)

arrows(1, VentResearcher[2], 1, c(VentResearcher[1], VentResearcher[3]), angle=90, length=1/8)
arrows(2, VentAngler[2], 2, c(VentAngler[1], VentAngler[3]), angle=90, length=1/8)
arrows(3, Descender[2], 3, c(Descender[1], Descender[3]), angle=90, length=1/8)

abline(h=1, col="red", lty=1)
