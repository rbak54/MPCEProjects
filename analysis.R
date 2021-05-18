col_names<-read.csv("../Data/data.csv",header=FALSE)
data<-read.csv("../Data/data.csv", skip = 4)
colnames(data)<-col_names[2,]
data<-data[1:799,1:5]

data_mod<-data
data_mod$monothermal_screen<-NA
selW<-which(data_mod$Order=="W")
selC<-which(data_mod$Order=="C")
data_mod$sum_lefts<-data_mod$WL+data_mod$CL
data_mod$sum_rights<-data_mod$WR+data_mod$CR
data_mod$sum_warms<-data_mod$WL+data_mod$WR
data_mod$sum_cools<-data_mod$CL+data_mod$CR
data_mod$diff_warms<-data_mod$WR-data_mod$WL
data_mod$diff_cools<-data_mod$CR-data_mod$CL
data_mod$sum_all<-data_mod$CL+data_mod$CR+data_mod$WL+data_mod$WR
data_mod$canal_paresis<-((data_mod$sum_rights-data_mod$sum_lefts)/data_mod$sum_all)*100
data_mod$WRandCL<-data_mod$WR+data_mod$CL
data_mod$WLandCR<-data_mod$WL+data_mod$CR
data_mod$directional_proponderance<-((data_mod$WRandCL-data_mod$WLandCR)/data_mod$sum_all)*100
data_mod[selW,"monothermal_screen"]<-(data_mod[selW,"diff_warms"]/data_mod[selW,"sum_all"])*100
data_mod[selC,"monothermal_screen"]<-(data_mod[selC,"diff_cools"]/data_mod[selC,"sum_all"])*100
