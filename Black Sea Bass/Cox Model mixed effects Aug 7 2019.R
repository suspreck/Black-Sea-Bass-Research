rm(list = ls())
#getwd()
setwd("C:/Users/pjruders/Desktop")
#set.seed(1000000000)
#bass <- read.csv("bsb_data_5_30_19.csv", header=T)
bass = read.csv("Copy of BSB Decender Data through Apr 25 2019 FOR COX MODEL model fit Aug 3 2019 NO DATA FROM JUNE THRU SEPT.csv", header=T)
head(bass)
dim(bass)


#######Logistic model of TL on submergence here:
#bass2 = read.csv("Copy of BSB Decender Data through Apr 25 2019 FOR COX MODEL model fit Aug 3 2019 NO DATA FROM JUNE THRU SEPT Data for logistic model.csv", header=T)
#head(bass2)
#dim(bass2)
#bass2 = bass2[1:1315,]        #no descended fish for logistic
#dim(bass2)

# my.mod = glm(Swim ~ TL, family="binomial", data=bass2)
# summary(my.mod)
# ###################################################################
# #binomial glm to assess relationship between sumergence success and fish TL
# fit <- glm(swim ~ TL, data=bass, family=binomial())
# summary(fit) # display results
# plot(TL, swim)
# confint(fit) # 95% CI for the coefficients
# exp(coef(fit)) # exponentiated coefficients
# exp(confint(fit)) # 95% CI for exponentiated coefficients
# predict(fit, type="ponse") # predicted values
# res = residuals(fit, type="deviance") # residuals
# plot(res)

#control = bass[1:469,]
#tool = bass[470:928,]
#needle = bass[929:1384,]
#descend = bass[1385:1840,]
###############################################    Prep the data

#bass$month<-double(length=nrow(bass))
bass$day<-double(length=nrow(bass))
#bass$year<-double(length=nrow(bass))

#library(reshape)
#bass[,17:19]<-colsplit(bass$Date,"/",names=c("month","day","year"))

#ind.bad.months<-which(bass$month<10&bass$month>5)       #which rows are months 6, 7, 8, 9
#bass<-bass[-ind.bad.months,]

#bass$month=as.factor(bass$month)

bass$trt = as.factor(bass$Treatment)
bass$Centered.temp = bass$WaterTempC.Centered   #Centered
bass$Centered.TL = bass$TL.Centered                    #Centered
bass$day = bass$Days
bass$recap = bass$Recap
bass$NR=bass$NonRecap
bass$f.date<-as.factor(bass$Date)

####################################################subset command here
#recap1<-which(bass$Recap==1&bass$Treatment==1)
#length(recap1)

#recap2<-which(bass$Treatment==4&bass$Swim==2)
#length(recap2)

#my.range <- range(bass$Days,which(bass$Recap==1&bass$Treatment==4))
#my.range

#my.unique = unique(bass$Date)
#length(my.unique)
########################################################Cox model 
library("survival")
library("coxme")


#mixed effects model
me.mod.final<-coxme(Surv(day,recap) ~ trt+TL + (1|f.date), data=bass, iter.max=100)

summary(me.mod.final)
ranef(me.mod.final) #estimated effect of each trip (so-called blups)

#intervals are not calculated natively in the coxme package. 
#I hard-code it here so if the model changes you get them automatically by running this code. 

CIresults<-matrix(NA,length(me.mod.final$coefficients),4)
colnames(CIresults)<-c("Variable","Mean","2.5","97.5")
CIresults[,1]<-names(me.mod.final$coefficients)
CIresults[,2]<-exp(me.mod.final$coefficients)

covs<-sqrt(vcov(me.mod.final))

for (i in 1:length(me.mod.final$coefficients)){
  CIresults[i,3]<-exp(me.mod.final$coefficients[i]-(1.96*covs[i,i]))
  CIresults[i,4]<-exp(me.mod.final$coefficients[i]+(1.96*covs[i,i]))
}

CIresults



#other models and some evaluation

# me.mod1<-coxme(Surv(day,recap) ~ trt+TL+month+WaterTempC  + (1|f.date), data=bass, iter.max=100)
# me.mod2<-coxme(Surv(day,recap) ~ trt+TL+month  + (1|f.date), data=bass, iter.max=100)
# me.mod3<-coxme(Surv(day,recap) ~ trt+TL+WaterTempC  + (1|f.date), data=bass, iter.max=100)
# 
# me.mod5<-coxme(Surv(day,recap) ~ trt*TL+month*WaterTempC  + (1|f.date), data=bass, iter.max=100)
# me.mod6<-coxme(Surv(day,recap) ~ trt*TL+month  + (1|f.date), data=bass, iter.max=100)
# me.mod7<-coxme(Surv(day,recap) ~ trt*TL+WaterTempC  + (1|f.date), data=bass, iter.max=100)
# me.mod8<-coxme(Surv(day,recap) ~ trt*TL  + (1|f.date), data=bass, iter.max=100)
# 

# anova(me.mod1,me.mod2,me.mod3,me.mod4,me.mod5,me.mod6,me.mod7,me.mod8)
# AIC(me.mod1,me.mod2,me.mod3,me.mod4,me.mod5,me.mod6,me.mod7,me.mod8)




#older models without random effect

#mod1 = coxph(Surv(day, recap) ~ trt + Centered.temp + Centered.TL + month, data=bass, iter.max=100)   
#mod2 = coxph(Surv(day, recap) ~ trt + Centered.temp + Centered.TL, data=bass, iter.max=100)   
#mod3 = coxph(Surv(day, recap) ~ trt + month + Centered.TL, data=bass, iter.max=100)   
# mod4 = coxph(Surv(day, recap) ~ trt  , data=bass, iter.max=100)   
# test.bass<-bass[-which(bass$WaterTempC>27),]
# full.mod<-coxph(Surv(day,recap) ~ f.date+trt*TL + trt * month + trt*WaterTempC + TL * month + TL*WaterTempC + month*WaterTempC, data=bass, iter.max=100)
# 
# model_stepwise<-stepAIC(full.mod,direction="both")
# final.mod<-coxph(model_stepwise$formula,data=bass)
# summary(final.mod)
# anova(final.mod)
# 
# summary(mod4)
# anova(mod4)   #gives overall effects across factor levels
# #AIC(mod1,mod2,mod3,mod4)




 par(mfrow=c(2,2)) 
 par(mar=c(4,4,2,2))

Centered.temp = control$WaterTempC.Centered   #Centered
Centered.TL = control$TL.Centered                    #Centered
day = control$Days
recap = control$Recap
modControl = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=control)    
plot(survfit(modControl), ylim=c(.7,1), xlim=c(0,200), 
ylab='Proportion of tags not reported', main="A. Control")

Centered.temp = tool$WaterTempC.Centered   #Centered
Centered.TL = tool$TL.Centered                    #Centered
day = tool$Days
recap = tool$Recap
modTool = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=tool)    
plot(survfit(modTool), ylim=c(.7,1), xlim=c(0,200),main="B. Venting tool")

Centered.temp = needle$WaterTempC.Centered   #Centered
Centered.TL = needle$TL.Centered                    #Centered
day = needle$Days
recap = needle$Recap
modNeedle = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=needle)    
plot(survfit(modNeedle), ylim=c(.7,1), xlim=c(0,200),xlab='Days', 
ylab='Proportion of tags not reported', main="C. Venting needle")

Centered.temp = descend$WaterTempC.Centered   #Centered
Centered.TL = descend$TL.Centered                    #Centered
day = descend$Days
recap = descend$Recap
modDesc = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=descend)    
plot(survfit(modDesc), ylim=c(.7,1), xlim=c(0,200), xlab='Days',
main="D. Descender")

#likelihood ratio tests
anova(mod1)

plot(survfit(mod1), ylim=c(.8,1), xlim=c(0,200))


#Here, models specific to each treatment

swim = as.factor(control$Swim)
temp = control$WaterTempC
TL = control$TL
day = control$Days
recap = control$Recap
NR =control$NonRecap
Centered.temp = control$WaterTempC.Centered   #Centered
Centered.TL = control$TL.Centered                    #Centered
modControl = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=control)  #control fish only
summary(modControl)
anova(modControl)
par(mfrow=c(2,2))
plot(survfit(modControl), ylim=c(.7,1), xlim=c(0,500),ylab="Proportion not recaptured", main="A. Control")


swim = as.factor(tool$Swim)
temp = tool$WaterTempC
TL = tool$TL
day = tool$Days
recap = tool$Recap
NR = tool$NonRecap
Centered.temp = tool$WaterTempC.Centered   #Centered
Centered.TL = tool$TL.Centered                    #Centered
modTool = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=tool)  #control fish only
summary(modTool)
anova(modTool)
plot(survfit(modTool), ylim=c(.7,1), xlim=c(0,500), main="B. Venting tool")


swim = as.factor(needle$Swim)
temp = needle$WaterTempC
TL = needle$TL
day = needle$Days
recap = needle$Recap
NR = needle$NonRecap
Centered.temp = needle$WaterTempC.Centered   #Centered
Centered.TL = needle$TL.Centered                    #Centered
modNeedle = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=needle)  #control fish only
summary(modNeedle)
anova(modNeedle)
plot(survfit(modNeedle), ylim=c(.7,1), xlim=c(0,500),ylab="Proportion not recaptured", main="C. Venting needle", xlab="Day")


swim = as.factor(descend$Swim)
temp = descend$WaterTempC
TL = descend$TL
day = descend$Days
recap = descend$Recap
NR = descend$NonRecap
Centered.temp = descend$WaterTempC.Centered   #Centered
Centered.TL = descend$TL.Centered                    #Centered
modDescend = coxph(Surv(day, recap) ~ Centered.temp + Centered.TL, data=descend)  #control fish only
summary(modDescend)
anova(modDescend)
plot(survfit(modDescend), ylim=c(.7,1), xlim=c(0,500), xlab="Day",  main='D. Descender')



##########################################################
#satisfying proportionality constant
#install.packages("survminer")
library("survminer")
library("survival")
bass$trt = as.factor(bass$Treatment)
bass$swim = as.factor(bass$Swim)
bass$temp = bass$WaterTempC
bass$TL = bass$TL
bass$Centered.temp = bass$WaterTempC.Centered   #Centered
bass$Centered.TL = bass$TL.Centered                    #Centered
bass$day = bass$Days
bass$recap = bass$Recap
bass$NR=bass$NonRecap

mod4 = coxph(Surv(day, recap) ~ trt  + Centered.TL, data=bass) 

ggadjustedcurves(mod4, variable = "trt", data=bass, legend = "bottom", 
                 legend.title = "Treatment", ylim=c(0.7,1), xlim=c(0,500), 
               #  legend.labs=c("Control", "Venting tool", "Venting needle", "Descender"),
                 palette = c("black", "gray40", "gray60", "gray80"),
                 ylab = 'Proportion not recaptured', xlab='Time (days)')


anova(me.mod.final, mod4)



