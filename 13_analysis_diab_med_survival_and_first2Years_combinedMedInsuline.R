require(readr)
require(survival)
library(survminer)
require(coxphf)
require(lubridate)
library(dplyr)
setwd("~/Macabi/data_generated/method9.3")
#cleaned data
diabsample1 <- read_csv("~/Macabi/data_generated/method9.3/diab_med_cleaned.csv")
diabsample1=diabsample1[which(diabsample1$medEnd1-diabsample1$medDate1>=180),]

################################# add med comb ##############################################

#add med comb to data
diabsample1$diabMedComb=rep("NA", dim(diabsample1)[1])
diabsample1$diabMedComb[which(diabsample1$diabMed=="no treatment")]="no treatment"
diabsample1$diabMedComb[which(diabsample1$diabMed=="met")]="other"
diabsample1$diabMedComb[which(diabsample1$diabMed=="met_ins")]="other"
diabsample1$diabMedComb[which(diabsample1$diabMed=="met_ntl")]="other"
diabsample1$diabMedComb[which(diabsample1$diabMed=="three")]="insulin comb"
diabsample1$diabMedComb[which(diabsample1$diabMed=="ntl")]="other"
diabsample1$diabMedComb[which(diabsample1$diabMed=="ins")]="insulin comb"
diabsample1$diabMedComb[which(diabsample1$diabMed=="ins_ntl")]="insulin comb"



########################################################################################################

#med selection: two years within cancer diagnoses or people whose medication never changed
diabsample1$end1=diabsample1$dateCancerMod+365*2

diabsample1$before=rep("no",dim(diabsample1)[1])
diabsample1$before[which(diabsample1$medEnd-diabsample1$dateCancerMod<=365*2)]="yes"

diabsample2=diabsample1[which(diabsample1$before=="yes"|diabsample1$id.chg==T ),]
diabsample2$id.chg=NULL
diabsample2$id.chg2=c(F,diff(diabsample2$RANDOM_ID)!=0)

#people on multiple medication
mult_med_id=unique(diabsample2$RANDOM_ID[which(diabsample2$id.chg2==F)])

#people on one medication
sgl_med_id=unique(diabsample2$RANDOM_ID[which(diabsample2$id.chg2==T)])
sgl_med_id1=sgl_med_id[-which(sgl_med_id%in%mult_med_id)]

#add medication to people on one medication
sgl_med_med=diabsample2[which(diabsample2$RANDOM_ID%in% sgl_med_id1),c("RANDOM_ID","diabMedComb")]

#determine which medication is the longest for multiple medication people
mult_med=diabsample2[which(diabsample2$RANDOM_ID%in% mult_med_id),]
mult_med=mult_med[,c("RANDOM_ID","diabMedComb", "medDate","medEnd")]
mult_med$diff=as.numeric(mult_med$medEnd-mult_med$medDate)
mult_med$medDate=NULL
mult_med$medEnd=NULL

#group by to find duration on different meds
mult_med1=summarise(group_by(mult_med,RANDOM_ID, diabMedComb),N=sum(diff))
mult_med1$id.chg=c(F,diff(mult_med1$RANDOM_ID)!=0)

#after grouping by, there are still people on more than one med
two_meds=unique(mult_med1$RANDOM_ID[which(mult_med1$id.chg==F)])

#determined one med after group by 
one_meds0=unique(mult_med1$RANDOM_ID[which(mult_med1$id.chg==T)])
one_meds=one_meds0[-which(one_meds0%in%two_meds)]

#add med to one med people
one_meds_med=data.frame(mult_med1[which(mult_med1$RANDOM_ID%in% one_meds),c("RANDOM_ID","diabMedComb")])


#more than one med
two=data.frame(mult_med1[which(mult_med1$RANDOM_ID %in% two_meds),c("RANDOM_ID","diabMedComb","N")])
w <- reshape(two, 
             timevar = "diabMedComb",
             idvar = c("RANDOM_ID"),
             direction = "wide")
w[is.na(w)] <- 0
w$m=rep(NA, dim(w)[1])

for(i in 1:dim(w)[1]){
  w$m[i]=which.max(w[i,2:4])
}
meds=c("insulin comb","no treatment","other")
w$m=meds[w$m]

more_meds=w[,c("RANDOM_ID","m")]
colnames(more_meds)[2]="diabMedComb"

#allmed
allmeds=rbind(more_meds,one_meds_med,sgl_med_med)

#add cancer diagnose date, study end date, 
names(diabsample1)
vars=c("RANDOM_ID","dateCancerMod","end","SEX","BIRTH_YR","BMI","HBA1C","CREATININE","DISEASE_GROUPUP_CD")
diabsample3=unique(diabsample1[,vars])
diabsample4=merge(diabsample3,allmeds)

death=unique(data.frame(cbind(diabsample1$RANDOM_ID,diabsample1$deathYN)))
death0=summarize(group_by(death,X1),death=sum(X2))
colnames(death0)[1]="RANDOM_ID"
diabsample4=merge(diabsample4,death0)

###################### add age and diff ###############################
#age
diabsample4$age=round(as.numeric(diabsample4$end-diabsample4$BIRTH_YR)/365)

#diff
diabsample4$diff1=as.numeric(diabsample4$end-diabsample4$dateCancerMod)

########################################################################################################
################################### #####################################################
########################################################################################################
diabsample4$SurvObj <- with(diabsample4, Surv( diabsample4$diff1, diabsample4$death ))


fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"), data=diabsample4, type="kaplan-meier")
#summary(fit1)

ggsurvplot(fit1,data=diabsample4, risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for All Cancer Patients")


fit=coxph(SurvObj~relevel(factor(diabMedComb),ref="no treatment"), data=diabsample4)
summary(fit)
  

fit=coxph(SurvObj~relevel(factor(diabMedComb),ref="no treatment")+age+SEX+BMI+HBA1C+CREATININE,data=diabsample4)
summary(fit)



###############################################################################################
############################### sub group analyses ####################
###############################################################################################
#cancer types
#2000: colon
#3000:lung
#4000: breast
#7000: female genital
#8000: male genital
#14000: Hematologic Neoplasm

#GI
fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"),
              data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==2000),])
ggsurvplot(fit1,data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==2000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic GI Cancer Patients")

dat2000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==2000),]
dat2000$diabMedComb[which(dat2000$diabMedComb=="no treatment")]="A"

summary(coxph(formula=SurvObj~diabMedComb,dat2000))
summary(coxph(formula=SurvObj~diabMedComb+age+SEX+BMI+HBA1C+CREATININE,dat2000))

#lung
fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"),
              data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==3000),])
ggsurvplot(fit1,data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==3000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Lung Cancer Patients")

# dat3000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==3000),]
# dat3000$diabMed[which(dat3000$diabMed=="no treatment")]="A"
# coxphf(formula=SurvObj~diabMed,dat3000)
# dat3000b=dat3000[which(!is.na(dat3000$BMI)),]
# coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat3000b)

dat3000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==3000),]
dat3000$diabMedComb[which(dat3000$diabMedComb=="no treatment")]="A"
summary(coxph(formula=SurvObj~diabMedComb,dat3000))
#dat3000b=dat3000[which(!is.na(dat3000$BMI)),]
summary(coxph(formula=SurvObj~diabMedComb+age+SEX+BMI+HBA1C+CREATININE,dat3000))

#breast
fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"),
              data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==4000),])
ggsurvplot(fit1,data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==4000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Breast Cancer Patients")

# dat4000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==4000),]
# dat4000$diabMed[which(dat4000$diabMed=="no treatment")]="A"
# coxphf(formula=SurvObj~diabMed,dat4000)
# dat4000b=dat4000[which(!is.na(dat4000$BMI)),]
# coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat4000b)

dat4000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==4000),]
dat4000$diabMedComb[which(dat4000$diabMedComb=="no treatment")]="A"
summary(coxph(formula=SurvObj~diabMedComb,dat4000))
summary(coxph(formula=SurvObj~diabMedComb+age+SEX+BMI+HBA1C+CREATININE,dat4000))

#female genital
fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"),
              data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==7000),])
ggsurvplot(fit1,data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==7000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Female Genital Cancer Patients")
# dat7000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==7000),]
# dat7000$diabMed[which(dat7000$diabMed=="no treatment")]="A"
# coxphf(formula=SurvObj~diabMed,dat7000)
# dat7000b=dat7000[which(!is.na(dat7000$BMI)),]
# coxphf(formula=SurvObj~diabMed+age+BMI+HBA1C+CREATININE,dat7000b)

dat7000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==7000),]
dat7000$diabMedComb[which(dat7000$diabMedComb=="no treatment")]="A"
summary(coxph(formula=SurvObj~diabMedComb,dat7000))
summary(coxph(formula=SurvObj~diabMedComb+age+BMI+HBA1C+CREATININE,dat7000))

#male genital
fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"),
              data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==8000),])
ggsurvplot(fit1,data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==8000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Male Genital Cancer Patients")
# dat8000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==8000),]
# dat8000$diabMed[which(dat8000$diabMed=="no treatment")]="A"
# coxphf(formula=SurvObj~diabMed,dat8000)
# dat8000b=dat8000[which(!is.na(dat8000$BMI)),]
# coxphf(formula=SurvObj~diabMed+age+BMI+HBA1C+CREATININE,dat8000b)

dat8000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==8000),]
dat8000$diabMedComb[which(dat8000$diabMedComb=="no treatment")]="A"
summary(coxph(formula=SurvObj~diabMedComb,dat8000))
summary(coxph(formula=SurvObj~diabMedComb+age+BMI+HBA1C+CREATININE,dat8000))

#Hematologic Neoplasm
fit1= survfit(SurvObj~relevel(factor(diabMedComb),ref="no treatment"),
              data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==14000),])
ggsurvplot(fit1,data=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==14000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin comb","other"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Hematologic Neoplasm Cancer Patients")
# dat14000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==14000),]
# dat14000$diabMed[which(dat14000$diabMed=="no treatment")]="A"
# coxphf(formula=SurvObj~diabMed,dat14000)
# dat14000b=dat14000[which(!is.na(dat14000$BMI)),]
# coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat14000b)

dat14000=diabsample4[which(diabsample4$DISEASE_GROUPUP_CD==14000),]
dat14000$diabMedComb[which(dat14000$diabMedComb=="no treatment")]="A"
summary(coxph(formula=SurvObj~diabMedComb,dat14000))
summary(coxph(formula=SurvObj~diabMedComb+age+SEX+BMI+HBA1C+CREATININE,dat14000))
