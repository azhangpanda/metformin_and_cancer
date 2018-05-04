#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")


#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")

#subset to only focus on met and neutral
ins=diabMed1[which(diabMed1$diabMedInd=="insulin" ),] 

#exclude three and met_ins

#read in the  met_ins data
met_ins=read_csv("~/Macabi/data_generated/method9.3/met_ins.csv")
met_ins$X1=NULL

#read in the  ins_ntl data
ins_ntl=read_csv("~/Macabi/data_generated/method9.3/ins_ntl.csv")
ins_ntl$X1=NULL

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

ins$include=include_fun(ins,three)
ins1=ins[which(ins$include=="included"),]
ins1$include=NULL

ins1$include=include_fun(ins1,met_ins)
ins2=ins1[which(ins1$include=="included"),]
ins2$include=NULL

ins2$include=include_fun(ins2,ins_ntl)
ins3=ins2[which(ins2$include=="included"),]
ins3$include=NULL



#sort
ins9=ins3[order(ins3$RANDOM_ID,ins3$medDate),]

#find ID change 
ins9$id.chg= c(F,diff(ins9$RANDOM_ID)!=0)

ins9$gap=rep(0,dim(ins9)[1])
for(i in 2:dim(ins9)[1]){
  ins9$gap[i-1]=ins9$medDate[i]-ins9$medEnd[i-1]
}
#if the gap is less than 2 weeks
ins9$cont=rep(F,dim(ins9)[1])
ins9$cont[which(ins9$gap<=14)]=T
#ins9$cont[1]=T
# for(i in 2:dim(ins9)[1]){
#   if (ins9$gap[i]<=14){
#     ins9$cont[i]=T
#   }
# }

#identify the ones that are not having continuation nor name change
ins9$alt=rep("pause",dim(ins9)[1])
ins9$alt[which(ins9$id.chg==F & ins9$cont==T)]="on ins"

#0: on insulin
#1: not on insulin
#2: right before or after switching to or from on met to pause

ins9$chg=rep(1,dim(ins9)[1])
ins9$chg[which(ins9$alt=="pause")]=0
before=which(ins9$alt=="pause")-1
after=which(ins9$alt=="pause")+1
after=after[-length(after)]
ins9$chg[before]=2
ins9$chg[after]=2
ins9$chg[which(ins9$alt=="pause")]=0
# ins9$chg[which(ins9$alt=="on ins")]=1

#eliminate the continuous ones
ins10=ins9[which(ins9$chg!=1),]
ins10$diff=rep(NA,dim(ins10)[1])
for (i in 1:(dim(ins10)[1]-1)){
  if(ins10$chg[i]==2 && ins10$chg[i+1]==2){
    ins10$diff[i]=difftime(ins10$medEnd[i+1],ins10$medDate[i])
  }
}
ins11=ins10[which(!is.na(ins10$diff)),]
ins12=ins11[which(ins11$diff>=180),]


vars=c("RANDOM_ID","medDate","diff")
ins13=ins12[vars]
ins13$medEnd=ins13$medDate+ins13$diff
ins13$diff=NULL

write.csv(ins13,"ins.csv")