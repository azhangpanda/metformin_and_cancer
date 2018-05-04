#cleaning sample data with descriptive statistics 

require(readr)
library(readxl)
require(lubridate)
library(reshape2)
require(dplyr)

################################### cleaning data #########################

#set work directory and read in data
setwd("~/Macabi/data_generated/method9.3")

#patient data
patients_data <- read_csv("~/Macabi/Macabi_raw/patients_data.csv")

# #medication data
# med <- read_csv("~/Macabi/Macabi_raw/04 - Medication purchases.csv")
# 
# #medication code
# med_code <- read_excel("~/Macabi/Macabi_raw/04A - Medication codes description (mariela codes) (1).xls")

#clean up confounding values


#patients_data$ANTIDIABETIC_DRUGS[patients_data$ANTIDIABETIC_DRUGS=="NULL"]=NA

#if having diabetes or not
patients_data$diab=rep(1, dim(patients_data)[1])
patients_data$diab[which(patients_data$DATEDIAB=="NULL")]=0

#bmi has 5485 missing values
length(which(patients_data$BMI=="NULL"))
patients_data$BMI=as.numeric(patients_data$BMI)

#there are 11684 missing values for hba1c
length(which(patients_data$HBA1C=="NULL"))
patients_data$HBA1C=as.numeric(patients_data$HBA1C)

#clean up dates 
#date diagnosed with diabetes
diab_yr=substr(patients_data$DATEDIAB,7,10)
diab_m=substr(patients_data$DATEDIAB,4,5)
diab_d=substr(patients_data$DATEDIAB,1,2)
diab_date=as.Date(paste(diab_yr,diab_m,diab_d, sep="-"))
patients_data$dateDiabMod=diab_date


#age and birth year
patients_data$BIRTH_YR=as.numeric(patients_data$BIRTH_YR)
patients_data$age=as.numeric(substr(patients_data$DATECANCER,7,10))-patients_data$BIRTH_YR

# create new variable to add in (potential) study end date to death date
# patients_data$dateEnd=patients_data$DATEDEATH
# patients_data$dateEnd[which(patients_data$dateEnd=="NULL")]="01/03/2013"
# end_yr=substr(patients_data$dateEnd,7,10)
# end_m=substr(patients_data$dateEnd,4,5)
# end_d=substr(patients_data$dateEnd,1,2)
# end_date=as.Date(paste(end_yr,end_m,end_d, sep="-"))
# patients_data$dateEnd=end_date

#death date
patients_data$DATEDEATH[which(patients_data$DATEDEATH=="NULL")]=NA
patients_data$dateDeathMod= dmy(patients_data$DATEDEATH)

#cancer diagnosed date
cancer_yr=substr(patients_data$DATECANCER,7,10)
cancer_m=substr(patients_data$DATECANCER,4,5)
cancer_d=substr(patients_data$DATECANCER,1,2)
cancer_date=ymd(paste(cancer_yr,cancer_m,cancer_d))
patients_data$dateCancerMod=cancer_date

#create var for  end
patients_data$end=rep(max(patients_data$dateDeathMod,na.rm=T),dim(patients_data)[1])

#substitute end date with death date if the person is dead
patients_data$end[which(!is.na(patients_data$dateDeathMod))]=patients_data$dateDeathMod[which(!is.na(patients_data$dateDeathMod))]


patients_data$deathYN=rep(0,dim(patients_data)[1])
patients_data$deathYN[which(!is.na(patients_data$dateDeathMod))]=1

#patients_data1=patients_data[order(patients_data$RANDOM_ID),]

#creating time var and status var
#days from cancer diagnosis to death
patients_data$TimeCancerToEnd=as.numeric(patients_data$end-patients_data$dateCancerMod)


#eliminate people who do not qualify and data in result section
#eliminating people age less than 18 years old when diagnosis with cancer
#age diagnosed with cancer
patients_data$cancerYr=as.numeric(substr(patients_data$DATECANCER,7,10))
patients_data$cancerAge=patients_data$cancerYr-patients_data$BIRTH_YR
cancerAgeElm=which(patients_data$cancerAge<18)
length(cancerAgeElm)

#eleminating people whose death occured within 180 days after cancer diagnosis
daystoDeathelm=which(patients_data$TimeCancerToEnd<180)
length(daystoDeathelm)

elm=unique(c(cancerAgeElm,daystoDeathelm))
sample=patients_data[-elm,]
#only keep people who are with in three years of diagnosis

dim(sample)
length(which(sample$deathYN==1))


write.csv(sample,"sample.csv")

