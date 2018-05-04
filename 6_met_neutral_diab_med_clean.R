#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")


#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")

#subset to only focus on met and neutral
met_ntl=diabMed1[which(diabMed1$diabMedInd=="metformin" |diabMed1$diabMedInd=="neutral"),] 

#exclude three and met_ins

#read in the  met_ins data
met_ins=read_csv("~/Macabi/data_generated/method9.3/met_ins.csv")
met_ins$X1=NULL

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

met_ntl$include=include_fun(met_ntl,three)
met_ntl1=met_ntl[which(met_ntl$include!="not included"),]
met_ntl1$include=NULL


met_ntl1$include=include_fun(met_ntl1,met_ins)
met_ntl2=met_ntl1[which(met_ntl1$include!="not included"),]
met_ntl2$include=NULL


#order by ID, date and medication
met_ntl6=met_ntl2[order(met_ntl2$RANDOM_ID,met_ntl2$medDate,met_ntl2$diabMedInd),]

#find ID change
met_ntl6$id.chg= c(F,diff(met_ntl6$RANDOM_ID)!=0)

#find medication change
met_ntl6$med.met_ntl=rep(F,dim(met_ntl6)[1])
for(i in 2:dim(met_ntl6)[1]){
  if( met_ntl6$diabMedInd[i]!=met_ntl6$diabMedInd[i-1]){
    met_ntl6$med.met_ntl[i]=T
    met_ntl6$med.met_ntl[i-1]=T
  }
}

#find the gap bewteen prescription ends and new prescription pick up
met_ntl6$gap=rep(0,dim(met_ntl6)[1])
for(i in 2:dim(met_ntl6)[1]){
  met_ntl6$gap[i-1]=met_ntl6$medDate[i]-met_ntl6$medEnd[i-1]
}

#if the gap is less than half of DDD than it is counted toward continuous
met_ntl6$cont=rep(F,dim(met_ntl6)[1])
met_ntl6$cont[1]=T
for(i in 2:dim(met_ntl6)[1]){
  if (met_ntl6$gap[i]<=14){
    met_ntl6$cont[i]=T
  }
}

#identify the ones that are not having continuation nor name change 
met_ntl6$alt=rep("not on met or ntl",dim(met_ntl6)[1])
met_ntl6$alt[which(met_ntl6$id.chg==F& met_ntl6$med.met_ntl==T & met_ntl6$cont==T)]="on met and ntl"

#create boundry indicators that shows the rows right before and after
#the time a when a patient that is switching from or to on three meds together or other forms of med

#0: on met and ntl
#1: not on met and ntl
#2: right before or after switching to or from ins and met and other meds
#3: the first date that has a switch from other meds to on met and ntl
met_ntl6$chg=rep(1,dim(met_ntl6)[1])
before=which(met_ntl6$alt=="on met and ntl")-1
after=which(met_ntl6$alt=="on met and ntl")+1
#after=after[-length(after)]
met_ntl6$chg[before]=2
met_ntl6$chg[after]=2
met_ntl6$chg[which(met_ntl6$alt=="on met and ntl")]=0


#subset and only focus on 2 and 0 in order to create category 3
met_ntl7=met_ntl6[which(met_ntl6$chg!=1),]

#create category 3: the time when one switch from other meds to three meds
chg3=which(met_ntl7$chg==2)+1
chg4=chg3[-length(chg3)]
met_ntl7$chg[chg4]=3

chg2=which(met_ntl7$chg==2)-1
met_ntl7$chg[chg2]=3

met_ntl7$chg[1]=3
met_ntl7$chg[which(met_ntl7$alt=="not on met or ntl")]=2

#eliminate the 0s (eliminate the period that are continuing on three meds)
met_ntl8=met_ntl7[which(met_ntl7$chg!=0),]

#compute the difference of days on medication
met_ntl8$diff=rep(NA,dim(met_ntl8)[1])
for (i in 1:dim(met_ntl8)[1]-1){
  if(met_ntl8$chg[i]==3 && met_ntl8$chg[i+1]==3){
    met_ntl8$diff[i]=difftime(met_ntl8$medEnd[i+1],met_ntl8$medDate[i])
  }
}

met_ntl8=met_ntl8[which(!is.na(met_ntl8$diff)),]
met_ntl8=met_ntl8[which(met_ntl8$diff>=180),]

vars=c("RANDOM_ID","medDate","diff")
met_ntl9=met_ntl8[vars]
met_ntl9$medEnd=met_ntl9$medDate+met_ntl9$diff
met_ntl9$diff=NULL
write.csv(met_ntl9,"met_ntl.csv")
