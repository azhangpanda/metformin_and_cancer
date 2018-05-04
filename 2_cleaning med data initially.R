#cleanning diab and aspirin data initial 
require(readr)
library(readxl)
require(lubridate)
library(reshape2)
require(dplyr)
#set work directory and read in data
setwd("~/Macabi/data_generated/method9.3")

#sample data
#sample <- read_csv("~/Macabi/data_generated/method9.2/sample.csv")
#patients_data <- read_csv("~/Macabi/Macabi_raw/patients_data.csv")

#medicatino data
med <- read_csv("~/Macabi/Macabi_raw/04 - Medication purchases.csv")

#medication code
med_code <- read_excel("~/Macabi/Macabi_raw/04A - Medication codes description (mariela codes) (1).xls")

#cleanning med and med code data
#clean med date 
med$medDate=ymd(med$Date)
med$Date=NULL

# #find all aspirin
# aspirin_where_1=which(med_code$Treatment_Group=="ASA")
# aspirin_where_2=grep("aspirin", tolower(med_code$Ingridients))
# aspirin_where=unique(c(aspirin_where_1, aspirin_where_2))
# 
# #put aspirin into one column
# med_code$aspirinInd=rep(0, dim(med_code)[1])
# med_code$aspirinInd[aspirin_where]=1
# 
# #find the largo of aspirin med
# aspirin_largo=unique(med_code$LARGO[med_code$aspirinInd==1])
# 
# med$aspirinInd=rep("non aspirin",dim(med)[1])
# med$aspirinInd[which( med$LARGO %in% aspirin_largo)]="aspirin"
# 
# aspirin=med[which(med$aspirinInd=="aspirin"),]
# vars=c("RANDOM_ID","DDD","medDate" )
# aspirin1=aspirin[vars]
# write.csv(aspirin1,"aspirinInit.csv")

# #merge asp med with patients data by random id
# varAsp=c("RANDOM_ID","aspirinInd")
# med_asp=unique(med[,varAsp])
# med_asp1=dcast(med_asp, RANDOM_ID~aspirinInd)
# med_asp1$aspirin[is.na(med_asp1$aspirin)]="nonAsp"
# med_asp1$`non aspirin`=NULL
# med_asp2=med_asp1[which(med_asp1$aspirin=="aspirin"),]
# asp_ppl=unique(med_asp2$RANDOM_ID)
# sample$asprin=rep(0,dim(sample)[1])
# sample$asprin[which(sample$RANDOM_ID %in% asp_ppl )]=1



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

