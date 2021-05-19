#import packages
require(ggplot2)

require(pROC)
col_names<-read.csv("../Data/data.csv",header=FALSE)
data_raw<-read.csv("../Data/data.csv", skip = 4)
colnames(data_raw)<-col_names[2,]
data_raw[data_raw == 0] <- NA
data_raw<-data_raw[1:799,1:5]

data_mod<-data_raw
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
data_mod[selW,"monothermal_screen"]<-(data_mod[selW,"diff_warms"]/data_mod[selW,"sum_warms"])*100
data_mod[selC,"monothermal_screen"]<-(data_mod[selC,"diff_cools"]/data_mod[selC,"sum_cools"])*100
data_mod$CP_significance<-abs(data_mod$canal_paresis)>20
data_mod$DP_significance<-abs(data_mod$directional_proponderance)>20
data_mod$overall_significance<-data_mod$CP_significance | data_mod$DP_significance
data_mod$rating<-NA
data_mod$mono_significance<-NA
get_spec_sens<-function(data_mod,significance){
  data_mod$mono_significance<-abs(data_mod$monothermal_screen)>significance
  data_mod[which(data_mod$overall_significance&data_mod$mono_significance),"rating"]<-"TP"
  data_mod[which(!data_mod$overall_significance&data_mod$mono_significance),"rating"]<-"FP"
  data_mod[which(data_mod$overall_significance&!data_mod$mono_significance),"rating"]<-"FN"
  data_mod[which(!data_mod$overall_significance&!data_mod$mono_significance),"rating"]<-"TN"
  sensitivity<-sum(data_mod$rating=="TP",na.rm = T)/(sum(data_mod$rating=="TP",na.rm = T)+sum(data_mod$rating=="FN",na.rm = T))
  specificity<-sum(data_mod$rating=="TN",na.rm = T)/(sum(data_mod$rating=="TN",na.rm = T)+sum(data_mod$rating=="FP",na.rm = T))
  return(c(sensitivity,specificity))
}

min_sig=0
max_sig=100
sig_range=seq(from=min_sig,to=max_sig,length.out=100)
sig_range=round(sig_range, 0)
sens_spec_dataset<-data.frame(matrix(nrow=(length(sig_range)),ncol=3))
sens_spec_dataset[,1]<-sig_range
colnames(sens_spec_dataset)=c("significance","sensitivity","specificity")
for (i in 1:length(sig_range)){
  res<-get_spec_sens(data_mod,sig_range[i])
  sens_spec_dataset[i,2]<-res[1]
  sens_spec_dataset[i,3]<-res[2]
}
sens_spec_dataset$false_pos_rate<- 1 - sens_spec_dataset$specificity
g<-ggplot(data=sens_spec_dataset,aes(false_pos_rate,sensitivity))+
  geom_text(label=sens_spec_dataset$significance,nudge_x = 0.05, nudge_y = 0.05, check_overlap = T)+
  geom_point()+theme_bw()+ylab("True Positive Rate (specificity)")+xlab("False Positive Rate (1-sensitivity)")
g

data_mod$overall_significance<-data_mod$overall_significance*1
roc(response=data_mod$overall_significance,predictor=data_mod$monothermal_screen,na.rm=TRUE)

#I think this plot tells us that using the value of the monothermic screening is a bad idea!!!
roc_obj<-roc(data_mod$overall_significance,abs(data_mod$monothermal_screen),na.rm=TRUE)
auc(roc_obj)
plot(roc_obj)
