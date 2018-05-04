#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")


#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")

#subset to only focus on met and neutral
ins_ntl=diabMed1[which(diabMed1$diabMedInd=="insulin" |diabMed1$diabMedInd=="neutral"),] 

#exclude three and met_ins

#read in the  met_ins data
met_ins=read_csv("~/Macabi/data_generated/method9.3/met_ins.csv")
met_ins$X1=NULL

#read in the  met_ntl data
met_ntl=read_csv("~/Macabi/data_generated/method9.3/met_ntl.csv")
met_ntl$X1=NULL

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

ins_ntl$include=include_fun(ins_ntl,three)
ins_ntl1=ins_ntl[which(ins_ntl$include=="included"),]
ins_ntl1$include=NULL

ins_ntl1$include=include_fun(ins_ntl1,met_ins)
ins_ntl2=ins_ntl1[which(ins_ntl1$include=="included"),]
ins_ntl2$include=NULL

ins_ntl2$include=include_fun(ins_ntl2,met_ntl)
ins_ntl3=ins_ntl2[which(ins_ntl2$include=="included"),]
ins_ntl3$include=NULL



#order by ID, date and medication
ins_ntl6=ins_ntl3[order(ins_ntl3$RANDOM_ID,ins_ntl3$medDate,ins_ntl3$diabMedInd),]

#find ID change
ins_ntl6$id.chg= c(F,diff(ins_ntl6$RANDOM_ID)!=0)

#find medication change
ins_ntl6$med.ins_ntl=rep(F,dim(ins_ntl6)[1])
for(i in 2:dim(ins_ntl6)[1]){
  if( ins_ntl6$diabMedInd[i]!=ins_ntl6$diabMedInd[i-1]){
    ins_ntl6$med.ins_ntl[i]=T
    ins_ntl6$med.ins_ntl[i-1]=T
  }
}

#find the gap bewteen prescription ends and new prescription pick up
ins_ntl6$gap=rep(0,dim(ins_ntl6)[1])
for(i in 2:dim(ins_ntl6)[1]){
  ins_ntl6$gap[i-1]=ins_ntl6$medDate[i]-ins_ntl6$medEnd[i-1]
}

#if the gap is less than two weeks
ins_ntl6$cont=rep(F,dim(ins_ntl6)[1])
ins_ntl6$cont[which(ins_ntl6$gap<=14)]=T
ins_ntl6$cont[1]=T
# for(i in 2:dim(ins_ntl6)[1]){
#   if (ins_ntl6$gap[i]<=14){
#     ins_ntl6$cont[i]=T
#   }
# }

#identify the ones that are not having continuation nor name change 
ins_ntl6$alt=rep("not on ins and ntl",dim(ins_ntl6)[1])
ins_ntl6$alt[which(ins_ntl6$id.chg==F& ins_ntl6$med.ins_ntl==T & ins_ntl6$cont==T)]="on ins and ntl"

#create boundry indicators that shows the rows right before and after
#the time a when a patient that is switching from or to on three meds together or other forms of med

#0: on ins and ntl
#1: not on ins and ntl
#2: right before or after switching to or from ins and ntl and other meds
#3: the first date that has a switch from other meds to on met and ntl
ins_ntl6$chg=rep(1,dim(ins_ntl6)[1])
before=which(ins_ntl6$alt=="on ins and ntl")-1
after=which(ins_ntl6$alt=="on ins and ntl")+1
#after=after[-length(after)]
ins_ntl6$chg[before]=2
ins_ntl6$chg[after]=2
ins_ntl6$chg[which(ins_ntl6$alt=="on ins and ntl")]=0


#subset and only focus on 2 and 0 in order to create category 3
ins_ntl7=ins_ntl6[which(ins_ntl6$chg!=1),]

#create category 3: the time when one switch from other meds to three meds
chg3=which(ins_ntl7$chg==2)+1
chg4=chg3[-length(chg3)]
ins_ntl7$chg[chg4]=3

chg2=which(ins_ntl7$chg==2)-1
ins_ntl7$chg[chg2]=3

ins_ntl7$chg[1]=3
ins_ntl7$chg[which(ins_ntl7$alt=="not on ins and ntl")]=2

#eliminate the 0s (eliminate the period that are continuing on three meds)
ins_ntl8=ins_ntl7[which(ins_ntl7$chg!=0),]

#compute the difference of days on medication
ins_ntl8$diff=rep(NA,dim(ins_ntl8)[1])
for (i in 1:dim(ins_ntl8)[1]-1){
  if(ins_ntl8$chg[i]==3 && ins_ntl8$chg[i+1]==3){
  ins_ntl8$diff[i]=difftime(ins_ntl8$medEnd[i+1],ins_ntl8$medDate[i])
  }
}

ins_ntl8=ins_ntl8[which(!is.na(ins_ntl8$diff)),]
ins_ntl8=ins_ntl8[which(ins_ntl8$diff>=180),]

vars=c("RANDOM_ID","medDate","diff")
ins_ntl9=ins_ntl8[vars]
ins_ntl9$medEnd=ins_ntl9$medDate+ins_ntl9$diff
ins_ntl9$diff=NULL
write.csv(ins_ntl9,"ins_ntl.csv")
