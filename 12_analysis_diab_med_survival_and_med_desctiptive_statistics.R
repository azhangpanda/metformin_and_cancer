require(readr)
require(survival)
library(survminer)
require(coxphf)
require(lubridate)
setwd("~/Macabi/data_generated/method9.3")
#cleaned data
diabsample1 <- read_csv("~/Macabi/data_generated/method9.3/diab_med_cleaned.csv")
diabsample1=diabsample1[which(diabsample1$medEnd1-diabsample1$medDate1>=180),]

################################# add age base on birth year ##############################################

diabsample1$age=year(diabsample1$medEnd)-diabsample1$BIRTH_YR

################################# descriptive stats for treatment type #########################################


table(diabsample1$DISEASE_GROUPUP_CD)
prop.table(table(diabsample1$DISEASE_GROUPUP_CD))


table(diabsample1$diabMed)
prop.table(table(diabsample1$diabMed))
############################### survival models by diabetic medication status #######################################
 
diabsample1$SurvObj <- with(diabsample1, Surv( diabsample1$medDate1, diabsample1$medEnd1, deathYN ))


fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1, type="kaplan-meier")
summary(fit1)
  
plot(fit1)

ggsurvplot(fit1,data=diabsample1, risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
          title="Survival Plot Comparing the Effects of Different Diabetic Treatments for All Diabetic Cancer Patients")


fit=coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1)
summary(fit)
exp(confint(fit))  
fit=coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+age+SEX+BMI+HBA1C+CREATININE,data=diabsample1)
summary(fit)
exp(confint(fit))

#summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+CREATININE,data=diabsample1))

# diabsample1$diabMedN=diabsample1$diabMed
# diabsample1$diabMedN[which(diabsample1$diabMedN=="no treatment")]=0
# diabsample1$diabMedN[which(diabsample1$diabMedN=="ins")]=1
# diabsample1$diabMedN[which(diabsample1$diabMedN=="ins_ntl")]=2
# diabsample1$diabMedN[which(diabsample1$diabMedN=="met")]=3
# diabsample1$diabMedN[which(diabsample1$diabMedN=="met_ins")]=4
# diabsample1$diabMedN[which(diabsample1$diabMedN=="met_ntl")]=5
# diabsample1$diabMedN[which(diabsample1$diabMedN=="ntl")]=6
# diabsample1$diabMedN[which(diabsample1$diabMedN=="three")]=7
# diabsample1$diabMedN=as.numeric(diabsample1$diabMedN)
# 
# 
# survdiff(formula = SurvObj~ as.numeric(diabMedN), data=diabsample1)
# survdiff(formula = SurvObj~ event+strata(DISEASE_GROUPUP_CD), data=diabsample1)

#cancer types
#2000: colon
#3000:lung
#4000: breast
#7000: female genital
#8000: male genital
#14000: Hematologic Neoplasm

#GI
fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"),
              data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==2000),])
ggsurvplot(fit1,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==2000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic GI Cancer Patients")

dat2000=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==2000),]
dat2000$diabMed[which(dat2000$diabMed=="no treatment")]="A"
coxphf(formula=SurvObj~diabMed,dat2000)
dat2000b=dat2000[which(!is.na(dat2000$BMI)),]
coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat2000b)

# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==2000),])
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+HBA1C+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==2000),])
# summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==2000),]))
 
#lung
fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"),
              data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==3000),])
ggsurvplot(fit1,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==3000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Lung Cancer Patients")

dat3000=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==3000),]
dat3000$diabMed[which(dat3000$diabMed=="no treatment")]="A"
coxphf(formula=SurvObj~diabMed,dat3000)
dat3000b=dat3000[which(!is.na(dat3000$BMI)),]
coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat3000b)

# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==3000),])
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+HBA1C+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==3000),])
# summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==3000),]))
 
#breast
fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"),
              data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==4000),])
ggsurvplot(fit1,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==4000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Breast Cancer Patients")

dat4000=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==4000),]
dat4000$diabMed[which(dat4000$diabMed=="no treatment")]="A"
coxphf(formula=SurvObj~diabMed,dat4000)
dat4000b=dat4000[which(!is.na(dat4000$BMI)),]
coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat4000b)

# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==4000),])
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+HBA1C+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==4000),])
# summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==4000),]))

#female genital
fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"),
              data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==7000),])
ggsurvplot(fit1,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==7000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Female Genital Cancer Patients")
dat7000=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==7000),]
dat7000$diabMed[which(dat7000$diabMed=="no treatment")]="A"
coxphf(formula=SurvObj~diabMed,dat7000)
dat7000b=dat7000[which(!is.na(dat7000$BMI)),]
coxphf(formula=SurvObj~diabMed+age+BMI+HBA1C+CREATININE,dat7000b)
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1[which(diabsample$DISEASE_GROUPUP_CD==7000),])
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+BMI+HBA1C+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==7000),])
# summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+BMI+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==7000),]))
 
#male genital
fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"),
              data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==8000),])
ggsurvplot(fit1,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==8000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Male Genital Cancer Patients")
dat8000=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==8000),]
dat8000$diabMed[which(dat8000$diabMed=="no treatment")]="A"
coxphf(formula=SurvObj~diabMed,dat8000)
dat8000b=dat8000[which(!is.na(dat8000$BMI)),]
coxphf(formula=SurvObj~diabMed+age+BMI+HBA1C+CREATININE,dat8000b)
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==8000),])
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+BMI+HBA1C+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==8000),])
# summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+BMI+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==8000),]))
 
#Hematologic Neoplasm
fit1= survfit(SurvObj~relevel(factor(diabMed),ref="no treatment"),
              data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==14000),])
ggsurvplot(fit1,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==14000),], risk.table = TRUE,
           legend.labs=c("no treatment","insulin","insulin & neutral",
                         "metformin","metformin & insulin","metformin & neutral", "neutral",
                         "metformin & insulin & neutral"),
           title="Survival Plot Comparing the Effects of Different Diabetic Treatments for Diabetic Hematologic Neoplasm Cancer Patients")
dat14000=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==14000),]
dat14000$diabMed[which(dat14000$diabMed=="no treatment")]="A"
coxphf(formula=SurvObj~diabMed,dat14000)
dat14000b=dat14000[which(!is.na(dat14000$BMI)),]
coxphf(formula=SurvObj~diabMed+age+SEX+BMI+HBA1C+CREATININE,dat14000b)
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment"), data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==14000),])
# coxph(SurvObj~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+HBA1C+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==14000),])
# summary(lm(HBA1C~relevel(factor(diabMed),ref="no treatment")+BIRTH_YR+SEX+BMI+CREATININE,data=diabsample1[which(diabsample1$DISEASE_GROUPUP_CD==14000),]))
# 
# 
