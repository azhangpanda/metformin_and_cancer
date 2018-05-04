#read in package
require(readr)

#set work directory to the generated data folder
setwd("~/Macabi/data_generated/method9.3")


#read in cleaned cancer patient diab med data
diabMed1 <- read_csv("~/Macabi/data_generated/method9.3/DiabmedWithInCancerMergedCleaned.csv")


#subset to only focus on met and neutral
met=diabMed1[which(diabMed1$diabMedInd=="metformin" ),] 

#read in the  met_ins data
met_ins=read_csv("~/Macabi/data_generated/method9.3/met_ins.csv")
met_ins$X1=NULL

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

met$include=include_fun(met,three)
met1=met[which(met$include=="included"),]
met1$include=NULL

met1$include=include_fun(met1,met_ins)
met2=met1[which(met1$include=="included"),]
met2$include=NULL

met2$include=include_fun(met2,met_ntl)
met3=met2[which(met2$include=="included"),]
met3$include=NULL



#order by ID, date and medication
met6=met3[order(met3$RANDOM_ID,met3$medDate,met3$diabMedInd),]



#find ID change 
met6$id.chg= c(F,diff(met6$RANDOM_ID)!=0)

met6$gap=rep(0,dim(met6)[1])
for(i in 2:dim(met6)[1]){
  met6$gap[i-1]=met6$medDate[i]-met6$medEnd[i-1]
}
#if the gap is less than 2 weeks
met6$cont=rep(F,dim(met6)[1])
met6$cont[which(met6$gap<=14)]=T
met6$cont[1]=T
# for(i in 2:dim(met6)[1]){
#   if (met6$gap[i]<=14){
#     met6$cont[i]=T
#   }
# }

#identify the ones that are not having continuation nor name change
met6$alt=rep("pause",dim(met6)[1])
met6$alt[which(met6$id.chg==F & met6$cont==T)]="on met"

#0: on metformin
#1: not on met 
#2: right before or after switching to or from on met and other meds

met6$chg=rep(1,dim(met6)[1])
met6$chg[which(met6$alt=="pause")]=0
before=which(met6$alt=="pause")-1
after=which(met6$alt=="pause")+1
after=after[-length(after)]
met6$chg[before]=2
met6$chg[after]=2
met6$chg[1]=2
met6$chg[which(met6$alt=="pause")]=0

# ins9$chg[which(ins9$alt=="on ins")]=1
#eliminate the continuous ones
met7=met6[which(met6$chg!=1),]

met7$diff=rep(NA,dim(met7)[1])
for (i in 1: (dim(met7)[1]-1)){
  if (met7$chg[i]==2 && met7$chg[i+1]==2){
    met7$diff[i]=difftime(met7$medEnd[i+1],met7$medDate[i])
  }
}

met8=met7[which(!is.na(met7$diff)),]
met9=met8[which(met8$diff>=180),]




vars=c("RANDOM_ID","medDate","diff")
met10=met9[vars]
met10$medEnd=met10$medDate+met10$diff
met10$diff=NULL

write.csv(met10,"met.csv")
