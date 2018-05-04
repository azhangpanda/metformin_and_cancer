##descriptive statistics

#read in packages
require(readr)
require(survival)
library(survminer)

#set work directory
setwd("~/Macabi/data_generated/method9.3")

# read cleaned data
sample <- read_csv("~/Macabi/data_generated/method9.3/sample.csv")
sample$X1=NULL

#number of people older thn 80
length(which(sample$age>80))
length(which(sample$age>80))/dim(sample)[1]

median(sample$age)

plot(table(sample$cancerAge))

test=summarize(group_by(sample,age),N=n())
test$age[which.max(test$N)]
test$N[which.max(test$N)]/dim(sample)[1]
plot(test$N)


#table 1
#age
mean(sample$age)
sd(sample$age)

#gender
table(sample$SEX)
prop.table(table(sample$SEX))

#death
table(sample$deathYN)
prop.table(table(sample$deathYN))

#diabetes status
table(sample$diab)
prop.table(table(sample$diab))

#biological measurements
mean(sample$BMI,na.rm=T)
sd(sample$BMI,na.rm = T)
length(which(is.na(sample$BMI)))

mean(sample$HBA1C,na.rm=T)
sd(sample$HBA1C,na.rm = T)
length(which(is.na(sample$HBA1C)))

mean(sample$CREATININE,na.rm = T)
sd(sample$CREATININE,na.rm = T)
length(which(is.na(sample$CREATININE)))

table(sample$DISEASE_GROUPUP_CD)
prop.table(table(sample$DISEASE_GROUPUP_CD))
length(which(is.na(sample$DISEASE_GROUPUP_CD)))

#table 2
#age
aggregate(age~diab,sample,FUN=plyr::each(avg=mean,sd=sd))
summary(aov(sample$age~sample$diab))

#gender
table(sample$SEX,sample$diab)
prop.table(table(sample$SEX,sample$diab),2)
chisq.test(table(sample$SEX,sample$diab))

#death
table(sample$deathYN,sample$diab)
prop.table(table(sample$deathYN,sample$diab),2)
chisq.test(table(sample$deathYN,sample$diab))

#cancer diagnoses
table(sample$diab,sample$DISEASE_GROUPUP_CD)
prop.table(table(sample$diab,sample$DISEASE_GROUPUP_CD),1)
chisq.test(table(sample$diab,sample$DISEASE_GROUPUP_CD))

#biological measurements
aggregate(BMI~diab,sample,FUN=plyr::each(avg=mean,sd=sd),na.rm=T)
length(which(is.na(sample$BMI[which(sample$diab==0)])))
length(which(is.na(sample$BMI[which(sample$diab==1)])))
summary(aov(sample$BMI~sample$diab),na.rm=T)

aggregate(HBA1C~diab,sample,FUN=plyr::each(avg=mean,sd=sd),na.rm=T)
length(which(is.na(sample$HBA1C[which(sample$diab==0)])))
length(which(is.na(sample$HBA1C[which(sample$diab==1)])))
summary(aov(sample$HBA1C~sample$diab),na.rm=T)

aggregate(CREATININE~diab,sample,FUN=plyr::each(avg=mean,sd=sd),na.rm=T)
length(which(is.na(sample$CREATININE[which(sample$diab==0)])))
length(which(is.na(sample$CREATININE[which(sample$diab==1)])))
summary(aov(sample$CREATININE~sample$diab),na.rm=T)

table(sample$diab,sample$DISEASE_GROUPUP_CD)
prop.table(table(sample$diab,sample$DISEASE_GROUPUP_CD))
length(which(is.na(sample$diab,sample$DISEASE_GROUPUP_CD)))

#table 3 in file 12 

##################################################################################################

#survival models by diabetic medication status 
sample$dur=as.numeric(sample$end)-as.numeric(sample$dateCancerMod)
sample$SurvObj <- with(sample, Surv(dur, deathYN ))

#sample$SurvObj1 <- with(sample, Surv(as.numeric(sample$dateCancerMod)-min(as.numeric(sample$dateCancerMod)),
#                                             as.numeric(sample$end)-min(as.numeric(sample$dateCancerMod)), deathYN ))


fit1= survfit(SurvObj~diab,data=sample)
ggsurvplot(fit1,data=sample, risk.table = TRUE,
           legend.labs=c("non diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For All Cancer Patients")

#log rank test
survdiff(formula = sample$SurvObj~ as.numeric(sample$diab))
survdiff(formula = sample$SurvObj~ as.numeric(sample$diab)+strata(sample$DISEASE_GROUPUP_CD))

summary(coxph(SurvObj~diab,data=sample))
summary(coxph(SurvObj~diab+age+SEX,data=sample))



#cancer types
#2000: colon
#3000:lung
#4000: breast
#7000: female genital
#8000: male genital
#14000: Hematologic Neoplasm

#GI
fit= survfit(SurvObj~diab,
              data=sample[which(sample$DISEASE_GROUPUP_CD==2000),])
ggsurvplot(fit,data=sample[which(sample$DISEASE_GROUPUP_CD==2000),], risk.table = TRUE,
           legend.labs=c("non diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For GI Cancer Patients")

summary(coxph(SurvObj~diab, data=sample[which(sample$DISEASE_GROUPUP_CD==2000),]))
summary(coxph(SurvObj~diab+age+SEX,data=sample[which(sample$DISEASE_GROUPUP_CD==2000),]))

survdiff(formula = SurvObj~ diab,data=sample[which(sample$DISEASE_GROUPUP_CD==2000),])
#lung
fit= survfit(SurvObj~diab,
              data=sample[which(sample$DISEASE_GROUPUP_CD==3000),])
ggsurvplot(fit,data=sample[which(sample$DISEASE_GROUPUP_CD==3000),], risk.table = TRUE,
           legend.labs=c("nob diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For Lung Cancer Patients")

summary(coxph(SurvObj~diab, data=sample[which(sample$DISEASE_GROUPUP_CD==3000),]))
summary(coxph(SurvObj~diab+age+SEX,data=sample[which(sample$DISEASE_GROUPUP_CD==3000),]))
survdiff(formula = SurvObj~ diab,data=sample[which(sample$DISEASE_GROUPUP_CD==3000),])

#breast
fit= survfit(SurvObj~diab,
              data=sample[which(sample$DISEASE_GROUPUP_CD==4000),])
ggsurvplot(fit,data=sample[which(sample$DISEASE_GROUPUP_CD==4000),], risk.table = TRUE,
           legend.labs=c("non diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For Breast Cancer Patients")
summary(coxph(SurvObj~diab, data=sample[which(sample$DISEASE_GROUPUP_CD==4000),]))
summary(coxph(SurvObj~diab+age+SEX,data=sample[which(sample$DISEASE_GROUPUP_CD==4000),]))

survdiff(formula = SurvObj~ diab,data=sample[which(sample$DISEASE_GROUPUP_CD==4000),])



#female genital
fit= survfit(SurvObj~diab,
              data=sample[which(sample$DISEASE_GROUPUP_CD==7000),])
ggsurvplot(fit,data=sample[which(sample$DISEASE_GROUPUP_CD==7000),], risk.table = TRUE,
           legend.labs=c("non diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For Female Genital Cancer Patients")
summary(coxph(SurvObj~diab, data=sample[which(sample$DISEASE_GROUPUP_CD==7000),]))
summary(coxph(SurvObj~diab+age, data=sample[which(sample$DISEASE_GROUPUP_CD==7000),]))
survdiff(formula = SurvObj~ diab,data=sample[which(sample$DISEASE_GROUPUP_CD==7000),])

#male genital
fit= survfit(SurvObj~diab,
              data=sample[which(sample$DISEASE_GROUPUP_CD==8000),])
ggsurvplot(fit,data=sample[which(sample$DISEASE_GROUPUP_CD==8000),], risk.table = TRUE,
           legend.labs=c("non diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For Male Genital Cancer")
summary(coxph(SurvObj~diab, data=sample[which(sample$DISEASE_GROUPUP_CD==8000),]))
summary(coxph(SurvObj~diab+age,data=sample[which(sample$DISEASE_GROUPUP_CD==8000),]))
survdiff(formula = SurvObj~ diab,data=sample[which(sample$DISEASE_GROUPUP_CD==8000),])

#Hematologic Neoplasm
fit= survfit(SurvObj~diab,
              data=sample[which(sample$DISEASE_GROUPUP_CD==14000),])
ggsurvplot(fit,data=sample[which(sample$DISEASE_GROUPUP_CD==14000),], risk.table = TRUE,
           legend.labs=c("non diabetic","diabetic"),
           title="Survival Plot Comparing Diabetic and Non-Diabetic Patients For Hematologic Neoplasm Cancer Patients")
summary(coxph(SurvObj~age, data=sample[which(sample$DISEASE_GROUPUP_CD==14000),]))
summary(coxph(SurvObj~diab+age+SEX,data=sample[which(sample$DISEASE_GROUPUP_CD==14000),]))

survdiff(formula = SurvObj~ diab,data=sample[which(sample$DISEASE_GROUPUP_CD==14000),])
#==========================================================


test=sample
fit1= survfit(SurvObj~DISEASE_GROUPUP_CD,data=test[which(test$diab==1),])
ggsurvplot(fit1,data=test[which(test$diab==1),], risk.table = TRUE,
           legend.labs=c("GI","lung", "breast",'female genital','male genital', "hematologic neoplasm"),
           title="Survival Plot Comparing different cancer types for diabetic patients")
#survdiff(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=sample)
coxph(SurvObj~factor(DISEASE_GROUPUP_CD),data=test[which(test$diab==1),])
coxph(SurvObj~factor(DISEASE_GROUPUP_CD)+BIRTH_YR+SEX,data=test[which(test$diab==1),])


