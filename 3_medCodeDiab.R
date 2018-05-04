#take into consideration of date and year and identify diab medications with medications
#macabi data cleaning
require(dplyr)
require(readr)
library(reshape2)
require(lubridate)
require(dplyr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")

#read in medication data
med <- read_csv("~/Macabi/data_generated/method9.3/diabmedInit.csv")

#exclude the ones that have zero dosage
med=med[which(med$DDD!=0),]

#select relevand vars
med$medEnd=med$medDate+med$DDD
vars=c("RANDOM_ID","medDate", "DDD","medEnd","diabMedInd" )

#find unique entries
diabMed_time=med[vars]
diabMed_time1=unique(diabMed_time)
diabMed=diabMed_time1


#read in cleaned sample patient data
sample <- read_csv("~/Macabi/data_generated/method9.3/sample.csv")

#subset and only focus on diabetic patients
diabSample=sample[which(sample$diab==1),]

#subset to focus on first three years of cancer diagnosis and create death within three years var
vars=c( "RANDOM_ID","dateCancerMod", "end","deathYN" )
diabSample1=diabSample[vars]


#find the RANDOM_ID in prescription data that are also in patient sample data
diab_ppl=unique(diabMed$RANDOM_ID[which(diabMed$RANDOM_ID%in% unique(diabSample1$RANDOM_ID))])

#subset and only focus on the patients in prescription data that are also in sample
diabMed1=diabMed[which(diabMed$RANDOM_ID %in% diab_ppl),]

#order by randome ID and medication prescription data
diabMed1=diabMed1[order(diabMed1$RANDOM_ID,diabMed1$medDate),]

#merge med data with diab sample data to truncate the patients whose med before and after study period
diabMed2=merge(diabMed1,diabSample1,all=T,by="RANDOM_ID")

#truncate med before cancer diagnosis
diabMed3=diabMed2[which(diabMed2$medDate>=diabMed2$dateCancerMod-180),]

diabMed3$medDate=as.Date(diabMed3$medDate)
diabMed3$medEnd=as.Date(diabMed3$medEnd)

write.csv(diabMed3,"DiabmedWithInCancerMergedCleaned.csv")
