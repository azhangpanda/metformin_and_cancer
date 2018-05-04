#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")


#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")

#subset to only focus on met and ins
met_ins=diabMed1[which(diabMed1$diabMedInd=="metformin" |diabMed1$diabMedInd=="insulin"),] 

#exclude three
#read in the three med data
three=read_csv("~/Macabi/data_generated/method9.3/three.csv")
three$X1=NULL

include_fun=function(dat_med,dat_exclude){
  include=rep("included",dim(dat_med)[1])
  for (i in 1: dim(dat_med)[1]){
    for (j in 1: dim(dat_exclude)[1]){
      if(dat_med$RANDOM_ID[i]==dat_exclude$RANDOM_ID[j]){
        if (dat_med$medDate[i]>=dat_exclude$medDate[j] &
            dat_med$medEnd[i]<=dat_exclude$medEnd[j]){
          include[i]="not included" 
        }
      }
    }
  }
  return(include)
}


met_ins$include=include_fun(met_ins,three)
met_ins1=met_ins[which(met_ins$include=="included"),]
met_ins1$include=NULL

#order by ID, date and medication
met_ins3=met_ins1[order(met_ins1$RANDOM_ID,met_ins1$medDate,met_ins1$diabMedInd),]

#find ID change
met_ins3$id.chg= c(F,diff(met_ins3$RANDOM_ID)!=0)

#find medication change
met_ins3$med.met_ins=rep(F,dim(met_ins3)[1])
for(i in 2:dim(met_ins3)[1]){
  if( met_ins3$diabMedInd[i]!=met_ins3$diabMedInd[i-1]){
    met_ins3$med.met_ins[i]=T
    met_ins3$med.met_ins[i-1]=T
  }
}

#find the gap bewteen prescription ends and new prescription pick up
met_ins3$gap=rep(0,dim(met_ins3)[1])
for(i in 2:dim(met_ins3)[1]){
  met_ins3$gap[i-1]=met_ins3$medDate[i]-met_ins3$medEnd[i-1]
}

#if the gap is less than two weeks
met_ins3$cont=rep(F,dim(met_ins3)[1])
met_ins3$cont[1]=T
for(i in 2:dim(met_ins3)[1]){
  if (met_ins3$gap[i]<=14){
    met_ins3$cont[i]=T
  }
}

#identify the ones that are not having continuation nor name change 
met_ins3$alt=rep("not on met or ins",dim(met_ins3)[1])
met_ins3$alt[which(met_ins3$id.chg==F& met_ins3$med.met_ins==T & met_ins3$cont==T)]="on met and ins"

#create boundry indicators that shows the rows right before and after 
#the time a when a patient that is switching from or to on three meds together or other forms of med

#0: on three meds
#1: not on met and ins
#2: right before or after switching to or from on met and ins and other meds
#3: the first date that has a switch from other meds to on to met and ins
met_ins3$chg=rep(1,dim(met_ins3)[1])
before=which(met_ins3$alt=="on met and ins")-1
after=which(met_ins3$alt=="on met and ins")+1
after=after[-length(after)]
met_ins3$chg[before]=2
met_ins3$chg[after]=2
met_ins3$chg[which(met_ins3$alt=="on met and ins")]=0


#subset and only focus on 2 and 0 in order to create category 3
met_ins4=met_ins3[which(met_ins3$chg!=1),]

#create category 3: the time when one switch from other meds to three meds
chg3=which(met_ins4$chg==2)+1
#chg4=chg3[-length(chg3)]
met_ins4$chg[chg3]=3

chg2=which(met_ins4$chg==2)-1
met_ins4$chg[chg2]=3

met_ins4$chg[which(met_ins4$alt=="not on met or ins")]=2

#eliminate the 0s (eliminate the period that are continuing on three meds)
met_ins5=met_ins4[which(met_ins4$chg!=0),]

#compute the difference of days on medication
met_ins5$diff=rep(NA,dim(met_ins5)[1])
for (i in 1:dim(met_ins5)[1]-1){
  if (met_ins5$chg[i]==3 && met_ins5$chg[i+1]==3){
  met_ins5$diff[i]=difftime(met_ins5$medEnd[i+1],met_ins5$medDate[i])
  }
}

met_ins6=met_ins5[which(!is.na(met_ins5$diff)),]
met_ins7=met_ins6[which(met_ins6$diff>=180),]

vars=c("RANDOM_ID","medDate","diff")
met_ins8=met_ins7[vars]
met_ins8$medEnd=met_ins8$medDate+met_ins8$diff
met_ins8$diff=NULL
write.csv(met_ins8,"met_ins.csv")


