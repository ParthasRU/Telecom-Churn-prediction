#setting working directory and loading reqd libraries
setwd("C:\\Jig19073\\Capstone_Final")
library(dplyr)
library(dataQualityR)
library(rpart)
library(car)
library(ROCR)

#reading dataset
tel<-read.csv("telecomfinal.csv",stringsAsFactors = F)
dim(tel)
names(tel)

#Data quality report
checkDataQuality(tel,out.file.num = "numeric.csv",out.file.cat = "character.csv")
numeric<-read.csv("numeric.csv")
character<-read.csv("character.csv")

#removing variables with more than 15% missing values
ind_num<-which(numeric$missing.percent>15)
numeric$X[ind_num]
#income,retdays and numbcars have more than 15% missing values.
#income and retdays are important variables for prediction
#so removing only 'numbcars' from the dataset
tel<-select(tel,-c(numbcars))

ind_char<-which(character$n.miss.percent>15)
character$X[ind_char]
#dwlltype,dwllsize,mailordr,occu1,
#wrkwoman,solflag,proptype,mailresp,cartype,children,div_type have more than 15% missing values
tel<-select(tel,-c(dwlltype,dwllsize,mailordr,occu1,
                   wrkwoman,solflag,proptype,mailresp,cartype,children,div_type))

dim(tel)

summary(tel)


#Variables profiling

#Continuous variables

#mou_Mean 

##decile analysis - no trend in churn rate

tel%>%mutate(dec=ntile(mou_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$min<-unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$max<-unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))
dat1

##imputing missing values with the mean of decile with closest churn rate
tel$mou_Mean<-ifelse(is.na(tel$mou_Mean),unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%group_by(dec)%>%summarise(mean(mou_Mean)))[[2]][1],tel$mou_Mean)

tel%>%mutate(dec=ntile(mou_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$min<-unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$max<-unclass(tel%>%mutate(dec=ntile(mou_Mean,10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))
dat1

#totmrc_Mean

##decile analysis - no trend in churn rate
tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$min<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$max<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
dat2

##imputing missing values with the mean of decile with closest churn rate
tel$totmrc_Mean<-ifelse(is.na(tel$totmrc_Mean),unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%group_by(dec)%>%summarise(mean(totmrc_Mean)))[[2]][5],tel$totmrc_Mean)

tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$min<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$max<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
dat2

#rev_Range

##decile analysis - no trend in churn rate
tel%>%mutate(dec=ntile(rev_Range,8))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(tel%>%mutate(dec=ntile(rev_Range,8))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$min<-unclass(tel%>%mutate(dec=ntile(rev_Range,8))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$max<-unclass(tel%>%mutate(dec=ntile(rev_Range,8))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))
dat3

##imputing missing values with the mean of decile with closest churn rate
tel$rev_Range<-ifelse(is.na(tel$rev_Range),unclass(tel%>%mutate(dec=ntile(rev_Range,10))%>%group_by(dec)%>%summarise(mean(rev_Range)))[[2]][6],tel$rev_Range)

tel%>%mutate(dec=ntile(rev_Range,8))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(tel%>%mutate(dec=ntile(rev_Range,8))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$min<-unclass(tel%>%mutate(dec=ntile(rev_Range,8))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$max<-unclass(tel%>%mutate(dec=ntile(rev_Range,8))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))
dat3

#mou_Range

##decile analysis - no trend in churn rate
tel%>%mutate(dec=ntile(mou_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$min<-unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$max<-unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))
dat4

##imputing missing values with the mean of decile with closest churn rate
tel$mou_Range<-ifelse(is.na(tel$mou_Range),unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%group_by(dec)%>%summarise(mean(mou_Range)))[[2]][7],tel$mou_Range)

tel%>%mutate(dec=ntile(mou_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$min<-unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$max<-unclass(tel%>%mutate(dec=ntile(mou_Range,10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))
dat4

#change_mou

##decile analysis - no trend in churn rate
tel%>%mutate(dec=ntile(change_mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$min<-unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$max<-unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))
dat5

##imputing missing values with the mean of decile with closest churn rate
tel$change_mou<-ifelse(is.na(tel$change_mou),unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%group_by(dec)%>%summarise(mean(change_mou)))[[2]][5],tel$change_mou)

tel%>%mutate(dec=ntile(change_mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$min<-unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$max<-unclass(tel%>%mutate(dec=ntile(change_mou,10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))
dat5


#drop_blk_Mean

##decile analysis - no trend in churn rate
tel%>%mutate(dec=ntile(drop_blk_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(tel%>%mutate(dec=ntile(drop_blk_Mean,10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$min<-unclass(tel%>%mutate(dec=ntile(drop_blk_Mean,10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$max<-unclass(tel%>%mutate(dec=ntile(drop_blk_Mean,10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6))
dat6

#drop_vce_Range

##decile analysis - no trend
tel%>%mutate(dec=ntile(drop_vce_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(tel%>%mutate(dec=ntile(drop_vce_Range,10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$min<-unclass(tel%>%mutate(dec=ntile(drop_vce_Range,10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$max<-unclass(tel%>%mutate(dec=ntile(drop_vce_Range,10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7))
dat7

#owylis_vce_Range

##decile analysis - 
tel%>%mutate(dec=ntile(owylis_vce_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(tel%>%mutate(dec=ntile(owylis_vce_Range,10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-round((dat8$n/dat8$N),2)
dat8$min<-unclass(tel%>%mutate(dec=ntile(owylis_vce_Range,10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$max<-unclass(tel%>%mutate(dec=ntile(owylis_vce_Range,10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))
dat8


#mou_opkv_Range

##decile analysis - no trend
tel%>%mutate(dec=ntile(mou_opkv_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(tel%>%mutate(dec=ntile(mou_opkv_Range,10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$min<-unclass(tel%>%mutate(dec=ntile(mou_opkv_Range,10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$max<-unclass(tel%>%mutate(dec=ntile(mou_opkv_Range,10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))
dat9

#months

##decile analysis - no trend
tel%>%mutate(dec=ntile(months,10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(tel%>%mutate(dec=ntile(months,10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$min<-unclass(tel%>%mutate(dec=ntile(months,10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$max<-unclass(tel%>%mutate(dec=ntile(months,10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))
dat10

#totcalls

tel%>%mutate(dec=ntile(totcalls,10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(tel%>%mutate(dec=ntile(totcalls,10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$min<-unclass(tel%>%mutate(dec=ntile(totcalls,10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$max<-unclass(tel%>%mutate(dec=ntile(totcalls,10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))
dat11

#eqpdays

##missing value imputation
tel$eqpdays<-ifelse(is.na(tel$eqpdays)==TRUE,mean(tel$eqpdays,na.rm = T),tel$eqpdays)

##decile analysis - no trend
tel%>%mutate(dec=ntile(eqpdays,10))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(tel%>%mutate(dec=ntile(eqpdays,10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$min<-unclass(tel%>%mutate(dec=ntile(eqpdays,10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat12$max<-unclass(tel%>%mutate(dec=ntile(eqpdays,10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))
dat12

#custcare_Mean

##decile analysis - no trend - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(custcare_Mean,3))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$N<-unclass(tel%>%mutate(dec=ntile(custcare_Mean,3))%>%count(dec)%>%unname())[[2]]
dat13$churn_perc<-dat13$n/dat13$N
dat13$min<-unclass(tel%>%mutate(dec=ntile(custcare_Mean,3))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
dat13$max<-unclass(tel%>%mutate(dec=ntile(custcare_Mean,3))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
dat13$varname<-rep("custcare_Mean",nrow(dat13))
dat13


#callwait_Mean

##decile analysis - no trend
tel%>%mutate(dec=ntile(callwait_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(tel%>%mutate(dec=ntile(callwait_Mean,4))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$min<-unclass(tel%>%mutate(dec=ntile(callwait_Mean,4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat14$max<-unclass(tel%>%mutate(dec=ntile(callwait_Mean,4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))
dat14


#iwylis_vce_Mean

##decile analysis - no trend
tel%>%mutate(dec=ntile(iwylis_vce_Mean,6))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(tel%>%mutate(dec=ntile(iwylis_vce_Mean,6))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$min<-unclass(tel%>%mutate(dec=ntile(iwylis_vce_Mean,6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat15$max<-unclass(tel%>%mutate(dec=ntile(iwylis_vce_Mean,6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))
dat15

#callwait_Range

##decile analysis - no trend - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(callwait_Range,3))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$N<-unclass(tel%>%mutate(dec=ntile(callwait_Range,3))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$min<-unclass(tel%>%mutate(dec=ntile(callwait_Range,3))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
dat16$max<-unclass(tel%>%mutate(dec=ntile(callwait_Range,3))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
dat16$varname<-rep("callwait_Range",nrow(dat16))
dat16


#ccrndmou_Range

##decile analysis - no trend - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(ccrndmou_Range,3))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$N<-unclass(tel%>%mutate(dec=ntile(ccrndmou_Range,3))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$min<-unclass(tel%>%mutate(dec=ntile(ccrndmou_Range,3))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
dat17$max<-unclass(tel%>%mutate(dec=ntile(ccrndmou_Range,3))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))
dat17


#adjqty

##decile analysis - no trend
tel%>%mutate(dec=ntile(adjqty,10))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(tel%>%mutate(dec=ntile(adjqty,10))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$min<-unclass(tel%>%mutate(dec=ntile(adjqty,10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat18$max<-unclass(tel%>%mutate(dec=ntile(adjqty,10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))
dat18


#ovrrev_Mean

##decile analysis - no trend
tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$min<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$max<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))
dat19

## imputing missing values
tel$ovrrev_Mean<-ifelse(is.na(tel$ovrrev_Mean),unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,10))%>%group_by(dec)%>%summarise(mean(ovrrev_Mean)))[[2]][1],tel$ovrrev_Mean)

tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$min<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat19$max<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))
dat19


#rev_Mean

##decile analysis - no trend
tel%>%mutate(dec=ntile(rev_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$min<-unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$max<-unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))
dat20

##imputing misssing values
tel$rev_Mean<-ifelse(is.na(tel$rev_Mean),unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%group_by(dec)%>%summarise(mean(rev_Mean)))[[2]][1],tel$rev_Mean)

tel%>%mutate(dec=ntile(rev_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$min<-unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat20$max<-unclass(tel%>%mutate(dec=ntile(rev_Mean,10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))
dat20


#ovrmou_Mean

##decile analysis - No trend
tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$min<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$max<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))
dat21

##imputing misssing values
tel$ovrmou_Mean<-ifelse(is.na(tel$ovrmou_Mean),unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%group_by(dec)%>%summarise(mean(ovrmou_Mean)))[[2]][1],tel$ovrmou_Mean)

tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$min<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat21$max<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))
dat21


#comp_vce_Mean

##decile analysis - No trend
tel%>%mutate(dec=ntile(comp_vce_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(tel%>%mutate(dec=ntile(comp_vce_Mean,10))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$min<-unclass(tel%>%mutate(dec=ntile(comp_vce_Mean,10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat22$max<-unclass(tel%>%mutate(dec=ntile(comp_vce_Mean,10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))
dat22


#plcd_vce_Mean

##decile analysis - No trend
tel%>%mutate(dec=ntile(plcd_vce_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(tel%>%mutate(dec=ntile(plcd_vce_Mean,10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-round((dat23$n/dat23$N),2)
dat23$min<-unclass(tel%>%mutate(dec=ntile(plcd_vce_Mean,10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat23$max<-unclass(tel%>%mutate(dec=ntile(plcd_vce_Mean,10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))
dat23


#avg3mou

##decile analysis - No trend
tel%>%mutate(dec=ntile(avg3mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(tel%>%mutate(dec=ntile(avg3mou,10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-round((dat24$n/dat24$N),2)
dat24$min<-unclass(tel%>%mutate(dec=ntile(avg3mou,10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat24$max<-unclass(tel%>%mutate(dec=ntile(avg3mou,10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))
dat24


#avgmou

##decile analysis - No trend
tel%>%mutate(dec=ntile(avgmou,10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(tel%>%mutate(dec=ntile(avgmou,10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$min<-unclass(tel%>%mutate(dec=ntile(avgmou,10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat25$max<-unclass(tel%>%mutate(dec=ntile(avgmou,10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))
dat25


#avg3qty

##decile analysis - No trend
tel%>%mutate(dec=ntile(avg3qty,10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(tel%>%mutate(dec=ntile(avg3qty,10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$min<-unclass(tel%>%mutate(dec=ntile(avg3qty,10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat26$max<-unclass(tel%>%mutate(dec=ntile(avg3qty,10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))
dat26


#avgqty

##decile analysis - No trend
tel%>%mutate(dec=ntile(avgqty,10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(tel%>%mutate(dec=ntile(avgqty,10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$min<-unclass(tel%>%mutate(dec=ntile(avgqty,10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat27$max<-unclass(tel%>%mutate(dec=ntile(avgqty,10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))
dat27


#avg6mou

##decile analysis - No trend
tel%>%mutate(dec=ntile(avg6mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$min<-unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$max<-unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))
dat28

##imputing misssing values
tel$avg6mou<-ifelse(is.na(tel$avg6mou),unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%group_by(dec)%>%summarise(mean(avg6mou)))[[2]][10],tel$avg6mou)

tel%>%mutate(dec=ntile(avg6mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$min<-unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat28$max<-unclass(tel%>%mutate(dec=ntile(avg6mou,10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))
dat28


#avg6qty

##decile analysis - No trend
tel%>%mutate(dec=ntile(avg6qty,10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$min<-unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$max<-unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))
dat29

##imputing misssing values
tel$avg6qty<-ifelse(is.na(tel$avg6qty),unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%group_by(dec)%>%summarise(mean(avg6qty)))[[2]][7],tel$avg6qty)

tel%>%mutate(dec=ntile(avg6qty,10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$min<-unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat29$max<-unclass(tel%>%mutate(dec=ntile(avg6qty,10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))
dat29


#opk_dat_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(opk_dat_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(tel%>%mutate(dec=ntile(opk_dat_Mean,2))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$min<-unclass(tel%>%mutate(dec=ntile(opk_dat_Mean,2))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
dat30$max<-unclass(tel%>%mutate(dec=ntile(opk_dat_Mean,2))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
dat30$varname<-rep("opk_dat_Mean",nrow(dat30))
dat30


#roam_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(roam_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$N<-unclass(tel%>%mutate(dec=ntile(roam_Mean,2))%>%count(dec)%>%unname())[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$min<-unclass(tel%>%mutate(dec=ntile(roam_Mean,2))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
dat31$max<-unclass(tel%>%mutate(dec=ntile(roam_Mean,2))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
dat31$varname<-rep("roam_Mean",nrow(dat31))
dat31


#recv_sms_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(recv_sms_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$N<-unclass(tel%>%mutate(dec=ntile(recv_sms_Mean,2))%>%count(dec)%>%unname())[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$min<-unclass(tel%>%mutate(dec=ntile(recv_sms_Mean,2))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
dat32$max<-unclass(tel%>%mutate(dec=ntile(recv_sms_Mean,2))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
dat32$varname<-rep("recv_sms_Mean",nrow(dat32))
dat32


#blck_dat_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(blck_dat_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$N<-unclass(tel%>%mutate(dec=ntile(blck_dat_Mean,2))%>%count(dec)%>%unname())[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$min<-unclass(tel%>%mutate(dec=ntile(blck_dat_Mean,2))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat33$max<-unclass(tel%>%mutate(dec=ntile(blck_dat_Mean,2))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat33$varname<-rep("blck_dat_Mean",nrow(dat33))
dat33


#mou_pead_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(mou_pead_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$N<-unclass(tel%>%mutate(dec=ntile(mou_pead_Mean,2))%>%count(dec)%>%unname())[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$min<-unclass(tel%>%mutate(dec=ntile(mou_pead_Mean,2))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
dat34$max<-unclass(tel%>%mutate(dec=ntile(mou_pead_Mean,2))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
dat34$varname<-rep("mou_pead_Mean",nrow(dat34))
dat34


#da_Mean

##decile analysis - No trend
tel%>%mutate(dec=ntile(da_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$N<-unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$min<-unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat35$max<-unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat35$varname<-rep("da_Mean",nrow(dat35))
dat35

##imputing misssing values
tel$da_Mean<-ifelse(is.na(tel$da_Mean),unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%group_by(dec)%>%summarise(mean(da_Mean)))[[2]][1],tel$da_Mean)

tel%>%mutate(dec=ntile(da_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$N<-unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$min<-unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat35$max<-unclass(tel%>%mutate(dec=ntile(da_Mean,4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat35$varname<-rep("da_Mean",nrow(dat35))
dat35


#da_Range

##decile analysis - No trend
tel%>%mutate(dec=ntile(da_Range,4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$N<-unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%count(dec)%>%unname())[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$min<-unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat36$max<-unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat36$varname<-rep("da_Range",nrow(dat36))
dat36

##imputing misssing values
tel$da_Range<-ifelse(is.na(tel$da_Range),unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%group_by(dec)%>%summarise(mean(da_Range)))[[2]][1],tel$da_Range)

tel%>%mutate(dec=ntile(da_Range,4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$N<-unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%count(dec)%>%unname())[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$min<-unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat36$max<-unclass(tel%>%mutate(dec=ntile(da_Range,4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat36$varname<-rep("da_Range",nrow(dat36))
dat36


#datovr_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(datovr_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$N<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%count(dec)%>%unname())[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$min<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat37$max<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
dat37$varname<-rep("datovr_Mean",nrow(dat37))
dat37

tel$datovr_Mean<-ifelse(is.na(tel$datovr_Mean),unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%group_by(dec)%>%summarise(mean(datovr_Mean)))[[2]][1],tel$datovr_Mean)

tel%>%mutate(dec=ntile(datovr_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$N<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%count(dec)%>%unname())[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$min<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat37$max<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,2))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
dat37$varname<-rep("datovr_Mean",nrow(dat37))
dat37


#datovr_Range

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(datovr_Range,2))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$N<-unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$min<-unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat38$max<-unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
dat38$varname<-rep("datovr_Range",nrow(dat38))
dat38

tel$datovr_Range<-ifelse(is.na(tel$datovr_Range),unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%group_by(dec)%>%summarise(mean(datovr_Range)))[[2]][1],tel$datovr_Range)

tel%>%mutate(dec=ntile(datovr_Range,2))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$N<-unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$min<-unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat38$max<-unclass(tel%>%mutate(dec=ntile(datovr_Range,2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
dat38$varname<-rep("datovr_Range",nrow(dat38))
dat38


#drop_dat_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(drop_dat_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$N<-unclass(tel%>%mutate(dec=ntile(drop_dat_Mean,2))%>%count(dec)%>%unname())[[2]]
dat39$churn_perc<-dat39$n/dat39$N
dat39$min<-unclass(tel%>%mutate(dec=ntile(drop_dat_Mean,2))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
dat39$max<-unclass(tel%>%mutate(dec=ntile(drop_dat_Mean,2))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
dat39$varname<-rep("drop_dat_Mean",nrow(dat39))
dat39


#drop_vce_Mean

##decile analysis - No trend
tel%>%mutate(dec=ntile(drop_vce_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$N<-unclass(tel%>%mutate(dec=ntile(drop_vce_Mean,10))%>%count(dec)%>%unname())[[2]]
dat40$churn_perc<-dat40$n/dat40$N
dat40$min<-unclass(tel%>%mutate(dec=ntile(drop_vce_Mean,10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat40$max<-unclass(tel%>%mutate(dec=ntile(drop_vce_Mean,10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat40$varname<-rep("drop_vce_Mean",nrow(dat40))
dat40


#adjmou

##decile analysis - No trend
tel%>%mutate(dec=ntile(adjmou,10))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$N<-unclass(tel%>%mutate(dec=ntile(adjmou,10))%>%count(dec)%>%unname())[[2]]
dat41$churn_perc<-round((dat41$n/dat41$N),2)
dat41$min<-unclass(tel%>%mutate(dec=ntile(adjmou,10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat41$max<-unclass(tel%>%mutate(dec=ntile(adjmou,10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat41$varname<-rep("adjmou",nrow(dat41))
dat41


#totrev

##decile analysis - No trend
tel%>%mutate(dec=ntile(totrev,10))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$N<-unclass(tel%>%mutate(dec=ntile(totrev,10))%>%count(dec)%>%unname())[[2]]
dat42$churn_perc<-dat42$n/dat42$N
dat42$min<-unclass(tel%>%mutate(dec=ntile(totrev,10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat42$max<-unclass(tel%>%mutate(dec=ntile(totrev,10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat42$varname<-rep("totrev",nrow(dat42))
dat42


#adjrev

##decile analysis - No trend
tel%>%mutate(dec=ntile(adjrev,10))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(tel%>%mutate(dec=ntile(adjrev,10))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$min<-unclass(tel%>%mutate(dec=ntile(adjrev,10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat43$max<-unclass(tel%>%mutate(dec=ntile(adjrev,10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat43$varname<-rep("adjrev",nrow(dat43))
dat43


#avgrev

##decile analysis - No trend
tel%>%mutate(dec=ntile(avgrev,10))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(tel%>%mutate(dec=ntile(avgrev,10))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$min<-unclass(tel%>%mutate(dec=ntile(avgrev,10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat44$max<-unclass(tel%>%mutate(dec=ntile(avgrev,10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat44$varname<-rep("avgrev",nrow(dat44))
dat44


#comp_dat_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(comp_dat_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(tel%>%mutate(dec=ntile(comp_dat_Mean,2))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$min<-unclass(tel%>%mutate(dec=ntile(comp_dat_Mean,2))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]
dat45$max<-unclass(tel%>%mutate(dec=ntile(comp_dat_Mean,2))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]
dat45$varname<-rep("comp_dat_Mean",nrow(dat45))
dat45


#plcd_dat_Mean

##decile analysis - less than 4 deciles - Omit Variable
tel%>%mutate(dec=ntile(plcd_dat_Mean,2))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$N<-unclass(tel%>%mutate(dec=ntile(plcd_dat_Mean,2))%>%count(dec)%>%unname())[[2]]
dat46$churn_perc<-dat46$n/dat46$N
dat46$min<-unclass(tel%>%mutate(dec=ntile(plcd_dat_Mean,2))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean)))[[2]]
dat46$max<-unclass(tel%>%mutate(dec=ntile(plcd_dat_Mean,2))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean)))[[2]]
dat46$varname<-rep("plcd_dat_Mean",nrow(dat46))
dat46

dim(tel)

##creating derived variables and deleting variables with corelation

#comp_perc - percentage of completed calls

tel$tot_comp_Mean<-tel$comp_dat_Mean+tel$comp_vce_Mean
tel$tot_comp_calls<-tel$tot_comp_Mean*4

tel$tot_plcd_Mean<-tel$plcd_dat_Mean+tel$plcd_vce_Mean
tel$tot_plcd_calls<-tel$tot_plcd_Mean*4

tel$comp_perc<-tel$tot_comp_calls/tel$tot_plcd_calls

#ovrrev_perc - percentage of overage revenue

tel$ovrrev<-tel$ovrrev_Mean*4
tel$ovrrev_perc<-tel$ovrrev/tel$totrev

v<-c("tot_comp_Mean","comp_dat_Mean","comp_vce_Mean","tot_comp_calls",
     "tot_plcd_Mean","plcd_dat_Mean","plcd_vce_Mean","tot_plcd_calls",
     "ovrrev_Mean","ovrrev")
i<-which(names(tel)%in%v)
tel<-tel[,-(i)]

summary(tel$comp_perc)
indcompperc<-which(is.na(tel$comp_perc))
tel<-tel[-indcompperc,]
dim(tel)


## deleting variables with less than 4 deciles

names(tel)

tel<-tel[,-c(14,17,18,43,47:50,56:58)]


dim(tel)

dat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,
           dat11,dat12,dat14,dat15,dat18,dat19,dat20,dat21,dat22,
           dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat35,dat36,
           dat40,dat41,dat42,dat43,dat44)

write.csv(dat,"variables profiling - Continuous vars.csv")

#Categorical variables

#actvsubs

summary(tel$actvsubs)

tel$actvsubs<-ifelse(tel$actvsubs<=2,"actvsubs1",
                     ifelse(tel$actvsubs>2&tel$actvsubs<=4,"actvsubs2",
                            ifelse(tel$actvsubs>4,"actvsubs3",tel$actvsubs)))

tel%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datc1
datc1$N<-unclass(tel%>%filter(actvsubs%in%datc1$levels)%>%count(actvsubs))[[2]]
datc1$churnPerc<-round((datc1$n/datc1$N),2)
datc1$varName<-rep("actvsubs",nrow(datc1))
datc1

tel$actvsubs<-as.factor(tel$actvsubs)
summary(tel$actvsubs)

#age1

summary(tel$age1)

tel$age1<-ifelse(is.na(tel$age1)==T,"Missing",
                 ifelse(tel$age1==0,"Default",
                        ifelse(tel$age1<=30,"Young",
                               ifelse(tel$age1>30&tel$age1<=50,"Mid-Age","old"))))

tel%>%count(churn,levels=age1)%>%filter(churn==1)->datc2
datc2$N<-unclass(tel%>%filter(age1%in%datc2$levels)%>%count(age1))[[2]]
datc2$churnPerc<-datc2$n/datc2$N
datc2$varName<-rep("age1",nrow(datc2))
datc2

tel$age1<-ifelse(tel$age1=="Default","Young",tel$age1)
tel$age1<-ifelse(is.na(tel$age1),"old",tel$age1)

tel%>%count(churn,levels=age1)%>%filter(churn==1)->datc2
datc2$N<-unclass(tel%>%filter(age1%in%datc2$levels)%>%count(age1))[[2]]
datc2$churnPerc<-datc2$n/datc2$N
datc2$varName<-rep("age1",nrow(datc2))
datc2

tel$age1<-as.factor(tel$age1)
summary(tel$age1)

#age2

summary(tel$age2)

tel$age2<-ifelse(is.na(tel$age2)==T,"Missing",
                 ifelse(tel$age2==0,"Default",
                        ifelse(tel$age2<=30,"Young",
                               ifelse(tel$age2>30&tel$age2<=50,"Mid-Age","old"))))

tel%>%count(churn,levels=age2)%>%filter(churn==1)->datc3
datc3$N<-unclass(tel%>%filter(age2%in%datc3$levels)%>%count(age2))[[2]]
datc3$churnPerc<-datc3$n/datc3$N
datc3$varName<-rep("age2",nrow(datc3))
datc3

tel$age2<-as.factor(tel$age2)
summary(tel$age2)

#area

summary(tel$area)
head(tel$area)
ind_area<-which(is.na(tel$area))
tel<-tel[-ind_area,]

tel%>%count(churn,levels=area)%>%filter(churn==1)->datc4
datc4$N<-unclass(tel%>%filter(area%in%datc4$levels)%>%count(area))[[2]]
datc4$churnPerc<-round((datc4$n/datc4$N),2)
datc4$varName<-rep("area",nrow(datc4))
datc4

##Reducing levels

data<-data.frame(text=tel$area,target=tel$churn)
mod<-rpart(target~text,data = data,method = "class")
unique(mod$where)

ind_area1<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.20)%>%select(levels))[[1]])
tel$area[ind_area1]<-"area1"
ind_area2<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.21)%>%select(levels))[[1]])
tel$area[ind_area2]<-"area2"
ind_area3<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.22)%>%select(levels))[[1]])
tel$area[ind_area3]<-"area3"
ind_area4<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.23)%>%select(levels))[[1]])
tel$area[ind_area4]<-"area4"
ind_area5<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.24)%>%select(levels))[[1]])
tel$area[ind_area5]<-"area5"
ind_area6<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.25)%>%select(levels))[[1]])
tel$area[ind_area6]<-"area6"
ind_area7<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.27)%>%select(levels))[[1]])
tel$area[ind_area7]<-"area7"
ind_area8<-which(tel$area%in%unclass(datc4%>%filter(churnPerc==0.29)%>%select(levels))[[1]])
tel$area[ind_area8]<-"area8"

tel%>%count(churn,levels=area)%>%filter(churn==1)->datc4
datc4$N<-unclass(tel%>%filter(area%in%datc4$levels)%>%count(area))[[2]]
datc4$churnPerc<-datc4$n/datc4$N
datc4$varName<-rep("area",nrow(datc4))
datc4

tel$area<-as.factor(tel$area)
summary(tel$area)

#asl_flag

tel%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datc5
datc5$N<-unclass(tel%>%filter(asl_flag%in%datc5$levels)%>%count(asl_flag))[[2]]
datc5$churnPerc<-datc5$n/datc5$N
datc5$varName<-rep("asl_flag",nrow(datc5))
datc5

tel$asl_flag<-as.factor(tel$asl_flag)
summary(tel$asl_flag)

#car_buy

summary(tel$car_buy)

ind_carbuy<-which(is.na(tel$car_buy))
tel<-tel[-ind_carbuy,]

tel%>%count(churn,levels=car_buy)%>%filter(churn==1)->datc6
datc6$N<-unclass(tel%>%filter(car_buy%in%datc6$levels)%>%count(car_buy))[[2]]
datc6$churnPerc<-datc6$n/datc6$N
datc6$varName<-rep("car_buy",nrow(datc6))
datc6

tel$car_buy<-as.factor(tel$car_buy)
summary(tel$car_buy)

#crclscod

tel%>%count(churn,levels=crclscod)%>%filter(churn==1)->datc7
datc7$N<-unclass(tel%>%filter(crclscod%in%datc7$levels)%>%count(crclscod))[[2]]
datc7$churnPerc<-round((datc7$n/datc7$N),1)
datc7$varName<-rep("crclscod",nrow(datc7))
datc7

##Reducing levels

data1<-data.frame(text=tel$crclscod,target=tel$churn)
mod1<-rpart(target~text,data = data1,method = "class")
unique(mod1$where)

ind_crclscod1<-which(tel$crclscod%in%unclass(datc7%>%filter(churnPerc==0.1)%>%select(levels))[[1]])
tel$crclscod[ind_crclscod1]<-"crclscod1"
ind_crclscod2<-which(tel$crclscod%in%unclass(datc7%>%filter(churnPerc==0.2)%>%select(levels))[[1]])
tel$crclscod[ind_crclscod2]<-"crclscod2"
ind_crclscod3<-which(tel$crclscod%in%unclass(datc7%>%filter(churnPerc==0.3)%>%select(levels))[[1]])
tel$crclscod[ind_crclscod3]<-"crclscod3"
ind_crclscod4<-which(tel$crclscod%in%unclass(datc7%>%filter(churnPerc==0.4)%>%select(levels))[[1]])
tel$crclscod[ind_crclscod4]<-"crclscod4"
ind_crclscod5<-which(tel$crclscod%in%unclass(datc7%>%filter(churnPerc==0.5)%>%select(levels))[[1]])
tel$crclscod[ind_crclscod5]<-"crclscod5"
ind_crclscod<-which(tel$crclscod!="crclscod1"&tel$crclscod!="crclscod2"&
                      tel$crclscod!="crclscod3"&tel$crclscod!="crclscod4"&
                      tel$crclscod!="crclscod5")
tel$crclscod[ind_crclscod]<-"crclscod6"

tel%>%count(churn,levels=crclscod)%>%filter(churn==1)->datc7
datc7$N<-unclass(tel%>%filter(crclscod%in%datc7$levels)%>%count(crclscod))[[2]]
datc7$churnPerc<-datc7$n/datc7$N
datc7$varName<-rep("crclscod",nrow(datc7))
datc7

tel$crclscod<-as.factor(tel$crclscod)

#csa

summary(tel$csa)

tel%>%count(churn,levels=csa)%>%filter(churn==1)->datc8
datc8$N<-unclass(tel%>%filter(csa%in%datc8$levels)%>%count(csa))[[2]]
datc8$churnPerc<-round((datc8$n/datc8$N),1)
datc8$varName<-rep("csa",nrow(datc8))
datc8

## reducing levels

unique(datc8$churnPerc)

ind_csa1<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.0)%>%select(levels))[[1]])
tel$csa[ind_csa1]<-"csa1"
ind_csa2<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.1)%>%select(levels))[[1]])
tel$csa[ind_csa2]<-"csa2"
ind_csa3<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.2)%>%select(levels))[[1]])
tel$csa[ind_csa3]<-"csa3"
ind_csa4<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.3)%>%select(levels))[[1]])
tel$csa[ind_csa4]<-"csa4"
ind_csa5<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.4)%>%select(levels))[[1]])
tel$csa[ind_csa5]<-"csa5"
ind_csa6<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.5)%>%select(levels))[[1]])
tel$csa[ind_csa6]<-"csa6"
ind_csa7<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.6)%>%select(levels))[[1]])
tel$csa[ind_csa7]<-"csa7"
ind_csa8<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.7)%>%select(levels))[[1]])
tel$csa[ind_csa8]<-"csa8"
ind_csa9<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==0.8)%>%select(levels))[[1]])
tel$csa[ind_csa9]<-"csa9"
ind_csa10<-which(tel$csa%in%unclass(datc8%>%filter(churnPerc==1.0)%>%select(levels))[[1]])
tel$csa[ind_csa10]<-"csa10"
tel$csa<-ifelse(tel$csa!="csa1"&tel$csa!="csa2"&tel$csa!="csa3"&tel$csa!="csa4"&
                  tel$csa!="csa5"&tel$csa!="csa6"&tel$csa!="csa7"&tel$csa!="csa8"&
                  tel$csa!="csa9"&tel$csa!="csa10","csa11",tel$csa)


tel%>%count(churn,levels=csa)%>%filter(churn==1)->datc8
datc8$N<-unclass(tel%>%filter(csa%in%datc8$levels)%>%count(csa))[[2]]
datc8$churnPerc<-datc8$n/datc8$N
datc8$varName<-rep("csa",nrow(datc8))
datc8

tel$csa<-as.factor(tel$csa)

#ethnic

tel%>%count(churn,levels=ethnic)%>%filter(churn==1)->datc9
datc9$N<-unclass(tel%>%filter(ethnic%in%datc9$levels)%>%count(ethnic))[[2]]
datc9$churnPerc<-round((datc9$n/datc9$N),1)
datc9$varName<-rep("ethnic",nrow(datc9))
datc9

ind_ethnic1<-which(tel$ethnic%in%unclass(datc9%>%filter(churnPerc==0.1)%>%select(levels))[[1]])
tel$ethnic[ind_ethnic1]<-"ethnic1"
ind_ethnic2<-which(tel$ethnic%in%unclass(datc9%>%filter(churnPerc==0.2)%>%select(levels))[[1]])
tel$ethnic[ind_ethnic2]<-"ethnic2"
ind_ethnic3<-which(tel$ethnic%in%unclass(datc9%>%filter(churnPerc==0.3)%>%select(levels))[[1]])
tel$ethnic[ind_ethnic3]<-"ethnic3"

tel%>%count(churn,levels=ethnic)%>%filter(churn==1)->datc9
datc9$N<-unclass(tel%>%filter(ethnic%in%datc9$levels)%>%count(ethnic))[[2]]
datc9$churnPerc<-round((datc9$n/datc9$N),1)
datc9$varName<-rep("ethnic",nrow(datc9))
datc9

tel$ethnic<-as.factor(tel$ethnic)

#forgntvl

tel%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datc10
datc10$N<-unclass(tel%>%filter(forgntvl%in%datc10$levels)%>%count(forgntvl))[[2]]
datc10$churnPerc<-datc10$n/datc10$N
datc10$varName<-rep("forgntvl",nrow(datc10))
datc10

summary(tel$forgntvl)
tel$forgntvl<-as.factor(tel$forgntvl)

#hnd_price

tel%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datc11
datc11$N<-unclass(tel%>%filter(hnd_price%in%datc11$levels)%>%count(hnd_price))[[2]]
datc11$churnPerc<-datc11$n/datc11$N
datc11$varName<-rep("hnd_price",nrow(datc11))
datc11

ind_hndprice<-which(is.na(tel$hnd_price))
tel<-tel[-ind_hndprice,]

##Reducing levels

data2<-data.frame(text=tel$hnd_price,target=tel$churn)
mod2<-rpart(target~text,data = data2,method = "class")
unique(mod2$where)


tel$hnd_price<-ifelse(tel$hnd_price<=80,"low price",
                      ifelse(tel$hnd_price>80&tel$hnd_price<=200,"medium price",
                             ifelse(tel$hnd_price>200,"high price",tel$hnd_price)))



tel%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datc11
datc11$N<-unclass(tel%>%filter(hnd_price%in%datc11$levels)%>%count(hnd_price))[[2]]
datc11$churnPerc<-datc11$n/datc11$N
datc11$varName<-rep("hnd_price",nrow(datc11))
datc11

tel$hnd_price<-as.factor(tel$hnd_price)

#hnd_webcap
summary(tel$hnd_webcap)

tel%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datc12
datc12$N<-unclass(tel%>%filter(hnd_webcap%in%datc12$levels)%>%count(hnd_webcap))[[2]]
datc12$churnPerc<-datc12$n/datc12$N
datc12$varName<-rep("hnd_webcap",nrow(datc12))
datc12

tel$hnd_webcap<-ifelse(is.na(tel$hnd_webcap),"WC",tel$hnd_webcap)

tel%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datc12
datc12$N<-unclass(tel%>%filter(hnd_webcap%in%datc12$levels)%>%count(hnd_webcap))[[2]]
datc12$churnPerc<-datc12$n/datc12$N
datc12$varName<-rep("hnd_webcap",nrow(datc12))
datc12

tel$hnd_webcap<-as.factor(tel$hnd_webcap)


#marital

tel%>%count(churn,levels=marital)%>%filter(churn==1)->datc13
datc13$N<-unclass(tel%>%filter(marital%in%datc13$levels)%>%count(marital))[[2]]
datc13$churnPerc<-datc13$n/datc13$N
datc13$varName<-rep("marital",nrow(datc13))
datc13

tel$marital<-as.factor(tel$marital)

#models

tel%>%count(churn,levels=models)%>%filter(churn==1)->datc14
datc14$N<-unclass(tel%>%filter(models%in%datc14$levels)%>%count(models))[[2]]
datc14$churnPerc<-round((datc14$n/datc14$N),1)
datc14$varName<-rep("models",nrow(datc14))
datc14

ind_models1<-which(tel$models%in%unclass(datc14%>%filter(churnPerc==0.1)%>%select(levels))[[1]])
tel$models[ind_models1]<-"models1"
ind_models2<-which(tel$models%in%unclass(datc14%>%filter(churnPerc==0.2)%>%select(levels))[[1]])
tel$models[ind_models2]<-"models2"
ind_models3<-which(tel$models%in%unclass(datc14%>%filter(churnPerc==0.3)%>%select(levels))[[1]])
tel$models[ind_models3]<-"models3"
ind_models4<-which(tel$models%in%unclass(datc14%>%filter(churnPerc==1.0)%>%select(levels))[[1]])
tel$models[ind_models4]<-"models4"
tel$models<-ifelse(tel$models==11|tel$models==15|tel$models==16|is.na(tel$models),"models5",tel$models)


tel%>%count(churn,levels=models)%>%filter(churn==1)->datc14
datc14$N<-unclass(tel%>%filter(models%in%datc14$levels)%>%count(models))[[2]]
datc14$churnPerc<-datc14$n/datc14$N
datc14$varName<-rep("models",nrow(datc14))
datc14

tel$models<-as.factor(tel$models)

#mtrcycle

tel%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datc15
datc15$N<-unclass(tel%>%filter(mtrcycle%in%datc15$levels)%>%count(mtrcycle))[[2]]
datc15$churnPerc<-datc15$n/datc15$N
datc15$varName<-rep("mtrcycle",nrow(datc15))
datc15

tel$mtrcycle<-as.factor(tel$mtrcycle)

#prizm_social_one

tel%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datc16
datc16$N<-unclass(tel%>%filter(prizm_social_one%in%datc16$levels)%>%count(prizm_social_one))[[2]]
datc16$churnPerc<-datc16$n/datc16$N
datc16$varName<-rep("prizm_social_one",nrow(datc16))
datc16

tel$prizm_social_one<-ifelse(is.na(tel$prizm_social_one),"T",tel$prizm_social_one)

tel%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datc16
datc16$N<-unclass(tel%>%filter(prizm_social_one%in%datc16$levels)%>%count(prizm_social_one))[[2]]
datc16$churnPerc<-datc16$n/datc16$N
datc16$varName<-rep("prizm_social_one",nrow(datc16))
datc16

tel$prizm_social_one<-as.factor(tel$prizm_social_one)

#refurb_new

tel%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datc17
datc17$N<-unclass(tel%>%filter(refurb_new%in%datc17$levels)%>%count(refurb_new))[[2]]
datc17$churnPerc<-datc17$n/datc17$N
datc17$varName<-rep("refurb_new",nrow(datc17))
datc17

tel$refurb_new<-as.factor(tel$refurb_new)

#truck

tel%>%count(churn,levels=truck)%>%filter(churn==1)->datc18
datc18$N<-unclass(tel%>%filter(truck%in%datc18$levels)%>%count(truck))[[2]]
datc18$churnPerc<-datc18$n/datc18$N
datc18$varName<-rep("truck",nrow(datc18))
datc18

tel$truck<-as.factor(tel$truck)

#uniqsubs

summary(tel$uniqsubs)

unique(tel$uniqsubs)

tel%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datc19
datc19$N<-unclass(tel%>%filter(uniqsubs%in%datc19$levels)%>%count(uniqsubs))[[2]]
datc19$churnPerc<-round((datc19$n/datc19$N),1)
datc19$varName<-rep("uniqsubs",nrow(datc19))
datc19


tel$uniqsubs<-ifelse(tel$uniqsubs<=4,"less than 4",
                     ifelse(tel$uniqsubs>4&tel$uniqsubs<=8,"4 to 8",
                            ifelse(tel$uniqsubs>8,"more than 8",tel$uniqsubs)))


tel%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datc19
datc19$N<-unclass(tel%>%filter(uniqsubs%in%datc19$levels)%>%count(uniqsubs))[[2]]
datc19$churnPerc<-datc19$n/datc19$N
datc19$varName<-rep("uniqsubs",nrow(datc19))
datc19

#income

summary(tel$income)
unique(tel$income)

tel%>%count(churn,levels=income)%>%filter(churn==1)->datc20
datc20$N<-unclass(tel%>%filter(income%in%datc20$levels)%>%count(income))[[2]]
datc20$churnPerc<-round((datc20$n/datc20$N),2)
datc20$varName<-rep("income",nrow(datc20))
datc20

tel$income<-ifelse(is.na(tel$income),7,tel$income)

tel$income<-ifelse(tel$income<=3,"less than 30000",
                   ifelse(tel$income>3&tel$income<=6,"30000 to 75000",
                          ifelse(tel$income>6,"more than 75000",tel$income)))

ind_income1<-which(tel$income%in%unclass(datc20%>%filter(churnPerc==0.21)%>%select(levels))[[1]])
tel$income[ind_income1]<-"income1"
ind_income2<-which(tel$income%in%unclass(datc20%>%filter(churnPerc==0.22)%>%select(levels))[[1]])
tel$income[ind_income2]<-"income2"
ind_income3<-which(tel$income%in%unclass(datc20%>%filter(churnPerc==0.23)%>%select(levels))[[1]])
tel$income[ind_income3]<-"income3"
ind_income4<-which(tel$income%in%unclass(datc20%>%filter(churnPerc==0.24)%>%select(levels))[[1]])
tel$income[ind_income4]<-"income4"
ind_income5<-which(tel$income%in%unclass(datc20%>%filter(churnPerc==0.25)%>%select(levels))[[1]])
tel$income[ind_income5]<-"income5"

tel%>%count(churn,levels=income)%>%filter(churn==1)->datc20
datc20$N<-unclass(tel%>%filter(income%in%datc20$levels)%>%count(income))[[2]]
datc20$churnPerc<-datc20$n/datc20$N
datc20$varName<-rep("income",nrow(datc20))
datc20

tel$income<-as.factor(tel$income)

#retdays

summary(tel$retdays)

tel$retdays<-ifelse(is.na(tel$retdays),0,1)
tel$retdays<-as.factor(tel$retdays)

datc<-rbind(datc1,datc2,datc3,datc4,datc5,datc6,datc7,datc8,datc9,datc10,
            datc11,datc12,datc13,datc14,datc15,datc16,datc17,datc18,datc19,datc20)

write.csv(datc,"Variables profiling - Categorical vars.csv")

dim(tel)
names(tel)

##Outlier Treatment

list<-names(tel)
list<-list[-c(12,25:45)]

par(mfrow=c(7,5))
par(mar=c(1,1,1,1))


for (i in 1:length(list))
{ 
  x<-boxplot(tel[,list[i]],main=list[i])
  out<-x$out
  indbox<-which(tel[,list[i]]%in%x$out)
  tel[indbox,list[i]]<-mean(tel[,list[i]],na.rm = T)
  rm(x)
  rm(out)
}

for (i in 1:length(list))
{ 
  x<-boxplot(tel[,list[i]],main=list[i])
}

dev.off()

#model building - We are using logistic regression model

set.seed(123)
indmod<-sample(nrow(tel),nrow(tel)*0.80)
train<-tel[indmod,]
test<-tel[-indmod,]
dim(train)
dim(test)


model<-glm(churn~.,data = train,family = "binomial")
summary(model)

# stepwise regression - taking long time for each step


# Creating dummy variables for significant categorical variable levels

names(tel)
tel$incomemorethan75k<-ifelse(tel$income=="more than 75000",1,0)
tel<-tel[,-12]

names(tel)
tel$crcls2<-ifelse(tel$crclscod=="crclscod2",1,0)
tel$crcls3<-ifelse(tel$crclscod=="crclscod3",1,0)
tel<-tel[,-24]

names(tel)
tel$asl_flagY<-ifelse(tel$asl_flag=="Y",1,0)
tel<-tel[,-24]

names(tel)
tel$prizm_social_oneR<-ifelse(tel$prizm_social_one=="R",1,0)
tel<-tel[,-24]

names(tel)
tel$area5<-ifelse(tel$area=="area5",1,0)
tel$area6<-ifelse(tel$area=="area6",1,0)
tel$area7<-ifelse(tel$area=="area7",1,0)
tel$area8<-ifelse(tel$area=="area8",1,0)
tel<-tel[,-24]

names(tel)
tel$refurb_newR<-ifelse(tel$refurb_new=="R",1,0)
tel<-tel[,-24]

names(tel)
tel$hnd_webcapWCMB<-ifelse(tel$hnd_webcap=="WCMB",1,0)
tel<-tel[,-24]

names(tel)
tel$ethnic2<-ifelse(tel$ethnic=="ethnic2",1,0)
tel$ethnic3<-ifelse(tel$ethnic=="ethnic3",1,0)
tel<-tel[,-25]

names(tel)
tel$age1Y<-ifelse(tel$age1=="Young",1,0)
tel<-tel[,-25]

names(tel)
tel$age2Y<-ifelse(tel$age2=="Young",1,0)
tel<-tel[,-25]

names(tel)
tel$actvsubs3<-ifelse(tel$actvsubs=="actvsubs3",1,0)
tel<-tel[,-27]

names(tel)
tel$uniqsubslessthan4<-ifelse(tel$uniqsubs=="less than 4",1,0)
tel<-tel[,-27]

names(tel)
tel$retdays1<-ifelse(tel$retdays==1,1,0)
tel<-tel[,-29]

names(tel)
tel$truck1<-ifelse(tel$truck==1,1,0)
tel<-tel[,-29]

set.seed(123)
indmod<-sample(nrow(tel),nrow(tel)*0.80)
train<-tel[indmod,]
test<-tel[-indmod,]
dim(train)
dim(test)

model1<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+    
              drop_vce_Range+mou_opkv_Range+months+incomemorethan75k+eqpdays+
              iwylis_vce_Mean+rev_Mean+avgmou+avgqty+avg6mou+crcls2+crcls3+asl_flagY+
              prizm_social_oneR+area5+area6+area7+area8+refurb_newR+
              hnd_webcapWCMB+ethnic2+ethnic3+age1Y+age2Y+actvsubs3+
              uniqsubslessthan4+retdays1+truck1+adjmou+totrev+comp_perc+ovrrev_perc,
            data = train,family = "binomial")
summary(model1)

model2<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+    
              drop_vce_Range+mou_opkv_Range+months+incomemorethan75k+eqpdays+
              iwylis_vce_Mean+avgmou+avg6mou+crcls2+crcls3+asl_flagY+
              prizm_social_oneR+area5+area6+area7+area8+refurb_newR+
              hnd_webcapWCMB+ethnic2+ethnic3+age1Y+age2Y+
              uniqsubslessthan4+retdays1+adjmou+totrev+comp_perc+ovrrev_perc,
            data = train,family = "binomial")
summary(model2)

model3<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+    
              drop_vce_Range+mou_opkv_Range+months+incomemorethan75k+eqpdays+
              iwylis_vce_Mean+avgmou+avg6mou+crcls2+crcls3+asl_flagY+
              prizm_social_oneR+area5+area6+area7+area8+refurb_newR+
              hnd_webcapWCMB+ethnic2+ethnic3+age1Y+age2Y+
              retdays1+adjmou+totrev+comp_perc+ovrrev_perc,
            data = train,family = "binomial")
summary(model3)

#All variables are significant
# Checking for multicollinearity

vif(model3)

#Removing Vars with vif>5 and reiterating

model4<-glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+    
              drop_vce_Range+mou_opkv_Range+months+incomemorethan75k+eqpdays+
              iwylis_vce_Mean+avgmou+asl_flagY+
              prizm_social_oneR+area5+area6+area7+area8+refurb_newR+
              hnd_webcapWCMB+age1Y+age2Y+
              retdays1+adjmou+totrev+comp_perc+ovrrev_perc,
            data = train,family = "binomial")
summary(model4)

#checking for multicollinearity

vif(model4)

# for model 4 all the variables are significant and there is no multicollinearity 
#between the variables. So finalising model 4.


# Confusion matrix and accuracy

prob<- predict(model4,type = "response", newdata = test)
head(prob)

predicted<-ifelse(prob>=0.5,1,0)

confmat<-table(predictedvalue=predicted,actualvalue=test$churn)

accuracy<-sum(diag(confmat))/nrow(test)
accuracy

#accuracy of the model at 0.5 predicted prob is 0.7659325


#ROC curve and auc


pred<-prediction(prob,test$churn)
roc<-performance(pred,"tpr","fpr")

plot(roc)
abline(0,1)

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

# AUC is 0.6247114. Model performs well.

#Topline questions

##1 top 5 factors contributing churn

top5<-data.frame(coefficients=sort(coefficients(model4),decreasing = T))
top5factors<-head(data.frame(factors=rownames(top5),top5),5)
row.names(top5factors)<-NULL
top5factors

#      factors coefficients
#1 ovrrev_perc    1.8417164
#2    retdays1    0.7386728
#3       area8    0.3016989
#4       area7    0.2515459
#5       age1Y    0.1910822

##2 Validation of survey findings. a) Whether "cost and billing" and "network and service quality" are
##important factors influencing churn behaviour. b) Are data usage connectivity issues turning out to be
##costly? In other words, is it leading to churn? 

#a)i) cost & billing

# Factors representing 'cost and billing' in the model are
#variable     :       Description                     : coefficient
#avgmou       : Average minutes of usage              : 0.000369
#adjmou       : Billing adjusted total minutes of use : 0.000016 
#mou_Mean     : Mean monthly minutes of usage         : -0.000560
#rev_Range    : Range of Revenue                      : 0.002126
#totrev       : Total revenue                         : 0.000123
#totmrc_Mean  : total monthly recurring charge        : -0.005615


#The coefficients of thes variables show that they have either a very little or 
# negative impact on the Probability of churn.
#Therefore, 'Cost and Billing" is not an important factor influencing Churn.

#a)ii) network and service quality

# Factors representing 'network and service quality' in the model are
#variable       :       Description                 : coefficient
#drop_vce_Range : Range of dropped voice calls      : 0.015471
#comp_perc      : Percentage of completed calls     : -0.669706

#The coefficients of these variable show that, 
#When the percentage of completed calls increases by one unit, 
# the probability of churn decreases 0.669706 times
#When the range of the dropped voice call increases by one unit,
# the probability of churn increases 0.015471 times

#Therefore we conclude that 'network and service quality" are important factors
#influencing churn behaviour.

#b) data usage connectivity issues

#Factors representing data usage in the dataset are:

#blck_dat_Maean
#blck_dat_Range
#comp_dat_Mean
#comp_dat_Range
#drop_dat_Mean
#drop_dat_Range
#mou_pead_Mean
#mou_pead_Range
#opk_dat_Mean
#opk_dat_Range
#plcd_dat_Mean
#plcd_dat_Range

#Analysing the data quality report of these variables, we find that the 
#data usage of the customers is low. Low usage leads to high churn rate.
#Therefore we conclude that data usage connectivity issues are leading to churn.

#3. Would you recommend rate plan migration as a proactive retention strategy?

# Variable ovrrev_perc(percent of overage revenue) is topmost factor driving 
#churn behaviour. when percentage of overage revenue increases by one unit, the
#probability of churn increases 1.8417164 times.

#This means that many customers are churning, as they are paying more due to 
#mou or data overage. Therefore, rate plan migration can be recommended 
#as a proactive retention strategy


#4. What would be your recommendation on how to use this churn model for 
#prioritisation of customers for a proactive retention campaigns in the future?

# Using this churn model we can selct the customers with the highest churn rate 
#and target them for proactive retention campaigns like rate plan migration,family bundling plan,etc.,

test$prob<-predict(model4,type = "response",newdata = test)
quantile(test$prob,p=(1:10)/10)

#top 20% of the probbilities lie between 0.2961918 and 0.7104288.

target_customers<-test[test$prob>0.2961918&test$prob<=0.7104288&test$churn==1,"Customer_ID"]

target_customers<-as.data.frame(target_customers)

write.csv(target_customers,"target customers.csv")

#5. What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
#concern and therefore, Mobicom would like to save their high revenue customers besides managing
#churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
#prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
#is the primary objective and revenue saves is the secondary objective.

#For finding the target segments  for proactive retention campaigns, we divide the 
#revenue levels as low,medium and High and the churn probabilities as Low,Medium and High.

quantile(test$totrev,p=(1:10)/10)
test$revenue_levels<-ifelse(test$totrev<586.44,"Low revenue",
                            ifelse(test$totrev>586.44&test$totrev<=972.20,"Medium Revenue","High Revenue"))
quantile(test$prob,p=(1:10)/10)

test$Churn_levels<-ifelse(test$prob<0.1895227,"Low churn",ifelse(test$prob>0.1895227&
                                                                   test$prob<0.2456440,"Medium churn","High churn"))

CustomerSegments<-table(test$revenue_levels,test$Churn_levels)
CustomerSegments

#                 High churn Low churn Medium churn
#High Revenue         2148      1421         1208
#Low revenue          1075      1212         1295
#Medium Revenue       1553       950         1079

#From this customers segments table, our target customers are those with
# High revenue and High churn
# High revenue and Medium churn
# Medium revenue and High churn

#Retrieving Customer id's of the target customers from the dataset

highrev_target1<-test[test$revenue_levels=="High Revenue"&test$Churn_levels=="High churn","Customer_ID"]
highrev_target1<-as.data.frame(highrev_target1)
highrev_target2<-test[test$revenue_levels=="High Revenue"&test$Churn_levels=="Medium churn","Customer_ID"]
highrev_target2<-as.data.frame(highrev_target2)
highrev_target3<-test[test$revenue_levels=="Medium Revenue"&test$Churn_levels=="High churn","Customer_ID"]
highrev_target3<-as.data.frame(highrev_target3)


HighrevenueTarget1<-merge(data.frame(highrev_target1,row.names = NULL),data.frame(highrev_target2,row.names = NULL),by=0,all = T)

HighrevenueTarget<-merge(data.frame(HighrevenueTarget1,row.names = NULL),data.frame(highrev_target3,row.names = NULL),by=0,all = T)

head(HighrevenueTarget)
HighrevenueTarget$Row.names<-NULL
HighrevenueTarget$Row.names<-NULL
names(HighrevenueTarget)<-c("Hi.Rev_Hi.Churn","Hi.Rev_Med.Churn","Med.Rev_Hi.Churn")

write.csv(HighrevenueTarget,"High Revenue Target Customers.csv")

