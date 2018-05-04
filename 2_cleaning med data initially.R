#cleanning diab data initial (identify med code in prescription)
require(readr)
library(readxl)
require(lubridate)
library(reshape2)
require(dplyr)
#set work directory and read in data
setwd("~/Macabi/data_generated/method9.3")

#medicatino data
med <- read_csv("~/Macabi/Macabi_raw/04 - Medication purchases.csv")

#medication code
med_code <- read_excel("~/Macabi/Macabi_raw/04A - Medication codes description (mariela codes) (1).xls")

#cleanning med and med code data
#clean med date 
med$medDate=ymd(med$Date)
med$Date=NULL


#diab med 
colnames(med_code)[10]="diabMed"

#find the largo of different diab med
insulin_largo=unique(med_code$LARGO[med_code$diabMed=="insulin group"])
metformin_largo=unique(med_code$LARGO[med_code$diabMed=="metformin"])
neutral_largo=unique(med_code$LARGO[med_code$diabMed=="neutral"])


#add diab med type and aspirin med into med data
med$diabMedInd=rep("non_diab",dim(med)[1])
med$diabMedInd[which( med$LARGO %in% insulin_largo)]="insulin"
med$diabMedInd[which( med$LARGO %in% metformin_largo)]="metformin"
med$diabMedInd[which( med$LARGO %in% neutral_largo)]="neutral"
vars=c("RANDOM_ID","DDD","medDate","diabMedInd" )
diabMed=med[vars]
write.csv(diabMed,"diabmedInit.csv")
#write.csv(sample,"sample.csv")

