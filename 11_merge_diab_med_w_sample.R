require(readr)

setwd("~/Macabi/data_generated/method9.3")
#patient data
sample <- read_csv("~/Macabi/data_generated/method9.3/sample.csv")
sample$X1=NULL

#subset to focus on first three years of cancer diagnosis and create death within three years var
diabSample=sample[which(sample$diab==1),]
vars=c( "RANDOM_ID","dateCancerMod","dateDeathMod",
        "end","deathYN" ,  
        "BIRTH_YR","SEX",
        "BMI", "DISEASE_GROUPUP_CD","HBA1C",
        "CREATININE" )
diabSample1=diabSample[vars]

three <- read_csv("~/Macabi/data_generated/method9.3/three.csv")
three$X1=NULL
three$diabMed="three"

met_ins <- read_csv("~/Macabi/data_generated/method9.3/met_ins.csv")
met_ins$X1=NULL
met_ins$diabMed="met_ins"

met_ntl <- read_csv("~/Macabi/data_generated/method9.3/met_ntl.csv")
met_ntl$X1=NULL
met_ntl$diabMed="met_ntl"

ins_ntl <- read_csv("~/Macabi/data_generated/method9.3/ins_ntl.csv")
ins_ntl$X1=NULL
ins_ntl$diabMed="ins_ntl"

met <- read_csv("~/Macabi/data_generated/method9.3/met.csv")
met$X1=NULL
met$diabMed="met"

ins <- read_csv("~/Macabi/data_generated/method9.3/ins.csv")
ins$X1=NULL
ins$diabMed="ins"

ntl <- read_csv("~/Macabi/data_generated/method9.3/ntl.csv")
ntl$X1=NULL
ntl$diabMed="ntl"

med=rbind(three,met_ins)
med=rbind(med,met_ntl)
med=rbind(med,ins_ntl)
med=rbind(med,met)
med=rbind(med,ins)
med=rbind(med,ntl)

med1=med[order(med$RANDOM_ID,med$medDate),]


#merge sample with medication prescription data
med2=merge(diabSample1, med1,all=T,by="RANDOM_ID")

#check diff
med2$diff=as.numeric(med2$medEnd-med2$medDate)
which(med2$diff<180)
med2$diff=NULL
#before any trimming, everyone has been on a treatment for longer than 180 days

#add no treatment into patients that don't have medication prescription data
med2$medDate[which(is.na(med2$medDate))]=med2$dateCancerMod[which(is.na(med2$medDate))]
med2$medEnd[which(is.na(med2$medEnd))]=med2$end[which(is.na(med2$medEnd))]
med2$diabMed[which(is.na(med2$diabMed))]="no treatment through out"


med3=med2[order(med2$RANDOM_ID,med2$medDate,med2$medEnd),]


#adding no treatment in gap
med3$id.chg= c(T,diff(med3$RANDOM_ID)!=0)

med3$gap=rep(F,dim(med3)[1])

a=c()
for (i in 2:dim(med3)[1]){
  if(med3$medDate[i]-med3$medEnd[i-1]>=180 ){
    a=c(a,i)
  }
}

a=a-1
med3$gap[a]=T

med3$add=rep(F,dim(med3)[1])
a=which(med3$gap==T)
b=which(med3$id.chg==F)-1
c=intersect(a,b)
med3$add[c]=T

med4=med3[which(med3$add==T),]
med4$diabMed="nn"

med5=rbind(med3,med4)
med5=med5[order(med5$RANDOM_ID,med5$medDate),]

med5$medDate[which(med5$diabMed=="nn")]=med5$medEnd[which(med5$diabMed=="nn")-1]+1
med5$medEnd[which(med5$diabMed=="nn")]=med5$medDate[which(med5$diabMed=="nn")+1]-1

med5$add=NULL
med5$gap=NULL
med5$id.chg=NULL
med5$diabMed[which(med5$diabMed=="nn")]="no treatment add gap"  

# #add no treatment before med start and cancer diagnosed
# med5$before=rep(F,dim(med5)[1])
# med5$before[which(med5$medDate-med5$dateCancerMod>=180)]=T
# 
# med5$id.chg= c(T,diff(med5$RANDOM_ID)!=0)
# 
# med5$add=rep(F,dim(med5)[1])
# med5$add[which(med5$before==T & med5$id.chg==T )]=T
# 
# med6=med5[which(med5$add==T),]
# med6$diabMed="a"
# 
# med7=rbind(med5,med6)
# med7=med7[order(med7$RANDOM_ID,med7$medDate,med7$diabMed),]
# med7$medDate[which(med7$diabMed=="a")]=med7$dateCancerMod[which(med7$diabMed=="a")]
# med7$medEnd[which(med7$diabMed=="a")]=med7$medDate[which(med7$diabMed=="a")+1]-1
# med7$diabMed[which(med7$diabMed=="a")]="no treatment add before"
# 
# med7$before=NULL
# med7$add=NULL
# med7$id.chg=NULL


#change med start to match with cancer diagnosis and death/study end date
med5$medDate[which(med5$medDate<=med5$dateCancerMod)]=med5$dateCancerMod[which(med5$medDate<=med5$dateCancerMod)]
med5$diff=as.numeric(med5$medEnd-med5$medDate)


#change last med as death
#find id change 
med5$id.chg= c(diff(med5$RANDOM_ID)!=0,T)
med5$deathYN[which(med5$deathYN==1 & med5$id.chg==F)]=0

med5$medEnd[which(med5$medEnd>=med5$end)]=med5$end[which(med5$medEnd>=med5$end)]

med11=med5
med11$diabMed[which(med11$diabMed=="no treatment add gap")]="no treatment"
#med11$diabMed[which(med11$diabMed=="no treatment add before")]="no treatment"
med11$diabMed[which(med11$diabMed=="no treatment through out")]="no treatment"
med11$diff=NULL


med12=med11[which(med11$medEnd>med11$medDate),]


med12$medDate1=as.numeric(med12$medDate)-min(as.numeric(med12$medDate))
med12$medEnd1=as.numeric(med12$medEnd)-min(as.numeric(med12$medDate))
med12$diff1=med12$medEnd1-med12$medDate1

#write to csv
write.csv(med12,"diab_med_cleaned.csv")

