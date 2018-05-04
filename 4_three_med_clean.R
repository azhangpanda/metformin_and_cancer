#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")

#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")


#create duplicate dataset of medication prescription data. 
#the goal is to see who and on which period is on three medications at the same time
three=diabMed1
three$X1=NULL

#order by ID, then medication prescription data and medicatio indicator
three=three[order(three$RANDOM_ID,three$medDate,three$diabMedInd),]

#creat ID change indicator
three$id.chg= c(F,diff(three$RANDOM_ID)!=0)

#create medication alternating (at the same time) indicator. 
# are they still alternating (taking them at the same time) or are they switching to taking two or one med only
three$med.three=rep(F,dim(three)[1])
for(i in 3:dim(three)[1]){
  if( three$diabMedInd[i]!=three$diabMedInd[i-2] 
      & three$diabMedInd[i]!=three$diabMedInd[i-1]
      & three$diabMedInd[i-2]!=three$diabMedInd[i-1]){
    three$med.three[i]=T
    three$med.three[i-1]=T
    three$med.three[i-2]=T
  }
}

#find the gap bewteen prescription ends and new prescription pick up
three$gap=rep(0,dim(three)[1])
for(i in 2:dim(three)[1]){
  three$gap[i-1]=three$medDate[i]-three$medEnd[i-1]
}

#if the gap is less than 2 weeks
three$cont=rep(F,dim(three)[1])
three$cont[1]=T
for(i in 2:dim(three)[1]){
  if (three$gap[i]<=14){
    three$cont[i]=T
  }
}

#identify the ones that are not having continuation nor name change 
three$alt=rep("not on three meds",dim(three)[1])
three$alt[which(three$id.chg==F& three$med.three==T & three$cont==T)]="on three meds"

#create boundry indicators that shows the rows right before and after 
#the time a when a patient that is switching from or to on three meds together or other forms of med

#0: on three meds
#1: not on three meds
#2: right before or after switching to or from three meds and other meds
#3: the first date that has a switch from other meds to on three meds

three$chg=rep(1,dim(three)[1])
before=which(three$alt=="on three meds")-1
after=which(three$alt=="on three meds")+1
three$chg[before]=2
three$chg[after]=2
three$chg[which(three$alt=="on three meds")]=0

#subset and only focus on 2 and 0 in order to create category 3
three1=three[which(three$chg!=1),]

#create category 3: the time when one switch from other meds to three meds
chg3=which(three1$chg==2)+1
chg4=chg3[-length(chg3)]
three1$chg[chg4]=3

chg2=which(three1$chg==2)-1
three1$chg[chg2]=3

three1$chg[which(three1$alt=="not on three meds")]=2

#eliminate the 0s (eliminate the period that are continuing on three meds)
three2=three1[which(three1$chg!=0),]

#compute the length one is on three meds (including the margin label ones)
three2$diff=rep(NA,dim(three2)[1])
for (i in 1:dim(three2)[1]-1){
  if (three2$chg[i]==3 && three2$chg[i+1]==3){
    three2$diff[i]=difftime(three2$medEnd[i+1],three2$medDate[i])
  }
}

three3=three2[which(!is.na(three2$diff)),]
three4=three3[which(three3$diff>=180),]

vars=c("RANDOM_ID","medDate","diff")
three5=three4[vars]
three5$medEnd=three5$medDate+three5$diff
three5$diff=NULL
write.csv(three5,"three.csv")



