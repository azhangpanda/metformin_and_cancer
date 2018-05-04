#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")


#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")

#subset to only focus on met and neutral
ntl=diabMed1[which(diabMed1$diabMedInd=="neutral" ),] 

#exclude three and met_ins

#read in the  ins_ntl data
ins_ntl=read_csv("~/Macabi/data_generated/method9.3/ins_ntl.csv")
ins_ntl$X1=NULL

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


ntl$include=include_fun(ntl,three)
ntl1=ntl[which(ntl$include=="included"),]
ntl1$include=NULL


ntl1$include=include_fun(ntl1,met_ntl)
ntl2=ntl1[which(ntl1$include=="included"),]
ntl2$include=NULL

ntl2$include=include_fun(ntl2,ins_ntl)
ntl3=ntl2[which(ntl2$include=="included"),]
ntl3$include=NULL


#sort
ntl6=ntl3[order(ntl3$RANDOM_ID,ntl3$medDate),]

#find ID change 
ntl6$id.chg= c(F,diff(ntl6$RANDOM_ID)!=0)

ntl6$gap=rep(0,dim(ntl6)[1])
for(i in 2:dim(ntl6)[1]){
  ntl6$gap[i-1]=ntl6$medDate[i]-ntl6$medEnd[i-1]
}
#if the gap is less than two weeks
ntl6$cont=rep(F,dim(ntl6)[1])
ntl6$cont[1]=T
for(i in 2:dim(ntl6)[1]){
  if (ntl6$gap[i]<=14){
    ntl6$cont[i]=T
  }
}

#identify the ones that are not having continuation nor name change
ntl6$alt=rep("pause",dim(ntl6)[1])
ntl6$alt[which(ntl6$id.chg==F & ntl6$cont==T)]="on ntl"

#0: on neutral
#1: not on neutral
#2: right before or after switching to or from on met to pause

ntl6$chg=rep(1,dim(ntl6)[1])
ntl6$chg[which(ntl6$alt=="pause")]=0
before=which(ntl6$alt=="pause")-1
after=which(ntl6$alt=="pause")+1
after=after[-length(after)]
ntl6$chg[before]=2
ntl6$chg[after]=2
ntl6$chg[which(ntl6$alt=="pause")]=0
#ntl6$chg[1]=2

#eliminate the continuous ones
ntl7=ntl6[which(ntl6$chg!=1),]
ntl7$diff=rep(0,dim(ntl7)[1])
for (i in 1:(dim(ntl7)[1]-1)){
  if(ntl7$chg[i]==2 && ntl7$chg[i+1]==2){
    ntl7$diff[i]=difftime(ntl7$medEnd[i+1],ntl7$medDate[i])
  }
}
ntl8=ntl7[which(!is.na(ntl7$diff)),]
ntl9=ntl8[which(ntl8$diff>=180),]

vars=c("RANDOM_ID","medDate","diff")
ntl10=ntl9[vars]
ntl10$medEnd=ntl10$medDate+ntl10$diff
ntl10$diff=NULL

write.csv(ntl10,"ntl.csv")
