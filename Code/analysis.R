#set working directory to current location

#i#mport packages
require(ggplot2)
require(pROC)


#function
#function which given a significance level, calculates specificity and sensitivity
get_spec_sens<-function(data_mod,significance){
  #boolean- is absolute value above significance value
  data_mod$mono_significance<-abs(data_mod$monothermal_screen)>=significance
  
  # for the rows of data_mod where overall significance is true AND mono significance is true - set rating to TP
  data_mod[which(data_mod$overall_significance&data_mod$mono_significance),"rating"]<-"TP"
  # for the rows of data_mod where overall significance is NOT true (!) AND mono significance is true - set rating to FP
  data_mod[which(!data_mod$overall_significance&data_mod$mono_significance),"rating"]<-"FP"
  # for the rows of data_mod where overall significance is true AND mono significance is NOT true - set rating to FN
  data_mod[which(data_mod$overall_significance&!data_mod$mono_significance),"rating"]<-"FN"
  # for the rows of data_mod where overall significance is NOT true (!) AND mono significance is NOT true - set rating to TN
  data_mod[which(!data_mod$overall_significance&!data_mod$mono_significance),"rating"]<-"TN"
  #calculate sensitivity  and specificty from our ratings column
  sensitivity<-sum(data_mod$rating=="TP",na.rm = T)/(sum(data_mod$rating=="TP",na.rm = T)+sum(data_mod$rating=="FN",na.rm = T))
  specificity<-sum(data_mod$rating=="TN",na.rm = T)/(sum(data_mod$rating=="TN",na.rm = T)+sum(data_mod$rating=="FP",na.rm = T))
  #return sensitivity and specificity
  #we don't return the dataset because we don't need the modified dataset anymore
  return(c(sensitivity,specificity))
}
##read in data

#done in a very messy way, because I didn't manually edit the excel doc and the formatting was weird
#read in dataframe to use column names
col_names<-read.csv("../Data/data.csv",header=FALSE)
# read in dataframe without the first four lines-just the data
data_raw<-read.csv("../Data/data.csv", skip =3)
#set colnames for dataset using col_names dataframe
colnames(data_raw)<-col_names[2,]
#replace 0 with NA
#data_raw[data_raw == 0] <- NA
#remove empty cells
data_raw<-data_raw[1:800,1:5]

#data_mod is the dataset we're adding to
data_mod<-data_raw


likelihood_ratio_dataset<-function(data_mod){
#make son columns with helpful additions done to make the formulae less messy!
data_mod$sum_lefts<-data_mod$WL+data_mod$CL
data_mod$sum_rights<-data_mod$WR+data_mod$CR
data_mod$sum_warms<-data_mod$WL+data_mod$WR
data_mod$sum_cools<-data_mod$CL+data_mod$CR
data_mod$diff_warms<-data_mod$WR-data_mod$WL
data_mod$diff_cools<-data_mod$CR-data_mod$CL
data_mod$sum_all<-data_mod$CL+data_mod$CR+data_mod$WL+data_mod$WR
data_mod$WRandCL<-data_mod$WR+data_mod$CL
data_mod$WLandCR<-data_mod$WL+data_mod$CR

#canal paresis
data_mod$canal_paresis<-((data_mod$sum_rights-data_mod$sum_lefts)/data_mod$sum_all)*100

#directional proponderance
data_mod$directional_proponderance<-((data_mod$WRandCL-data_mod$WLandCR)/data_mod$sum_all)*100


##monothermal screen
#make column for monothermal screen
data_mod$monothermal_screen<-NA

#which function outputs the row numbers in data_mod where the order is W or C respectively 
selW<-which(data_mod$Order=="W")
selC<-which(data_mod$Order=="C")

#monothermal for warm first using above row selection
data_mod[selW,"monothermal_screen"]<-(data_mod[selW,"diff_warms"]/data_mod[selW,"sum_warms"])*100

#monothermal for cool first using above row selection
data_mod[selC,"monothermal_screen"]<-(data_mod[selC,"diff_cools"]/data_mod[selC,"sum_cools"])*100

#Boolean variable- tels us whether the statement is true or false. so below asks is the absolute value of canal paresis >20
#is canal paresis significant
data_mod$CP_significance<-abs(data_mod$canal_paresis)>=20
#is directional proponderance significant
data_mod$DP_significance<-abs(data_mod$directional_proponderance)>=20
# is canal paresis and/or directional proponderance significant.
data_mod$overall_significance<-data_mod$CP_significance | data_mod$DP_significance

#make column which tells us whether TP,FN,TN, FP
data_mod$rating<-NA
#make empty column for whether the monothermal screening result is positive or negative
data_mod$mono_significance<-NA




#set min and max significances 
min_sig=0
max_sig=100

#sequence to iterate over
sig_range=seq(from=min_sig,to=max_sig,length.out=100)
#round to the nearest whole number to make the labels nicer!
sig_range=round(sig_range, 0)

#make new dataset to match the number of values we're using
sens_spec_dataset<-data.frame(matrix(nrow=(length(sig_range)),ncol=3))
colnames(sens_spec_dataset)=c("significance","sensitivity","specificity")

#first column in dataset is the significance value
sens_spec_dataset[,1]<-sig_range

#loop through length of sig_range
for (i in 1:length(sig_range)){
  #call function and set output to res
  res<-get_spec_sens(data_mod,sig_range[i])
  #store sensitivity in dataset for current significance value
  sens_spec_dataset[i,2]<-res[1]
  #store specificity in dataset
  sens_spec_dataset[i,3]<-res[2]
}

#false positive rate is 1 - specificity
sens_spec_dataset$false_pos_rate<- 1 - sens_spec_dataset$specificity

#make ROC plot (ggplot)
g<-ggplot(data=sens_spec_dataset,aes(x=false_pos_rate,y=sensitivity))+
  geom_text(label=sens_spec_dataset$significance,nudge_x = 0.05, nudge_y = 0.05, check_overlap = T)+
  geom_point()+theme_bw()+ylab("True Positive Rate (sensitivity)")+xlab("False Positive Rate (1-specificity)")
g<-g+geom_hline(yintercept = 0.9)
g
#pROC package prefers true and false to be 1s and 0s, multiplying by one does this
data_mod$overall_significance<-data_mod$overall_significance*1
#use roc function from the overall significance and the results of the screen
roc_obj<-roc(data_mod$overall_significance,abs(data_mod$monothermal_screen),na.rm=TRUE)
#get AUC- EXCELLENT
auc(roc_obj)
#plot ROC
plot(roc_obj)




#TEST 20
significance=20
#boolean- is absolute value above significance value
data_mod$mono_significance<-abs(data_mod$monothermal_screen)>=significance

# for the rows of data_mod where overall significance is true AND mono significance is true - set rating to TP
data_mod[which(data_mod$overall_significance&data_mod$mono_significance),"rating"]<-"TP"
# for the rows of data_mod where overall significance is NOT true (!) AND mono significance is true - set rating to FP
data_mod[which(!data_mod$overall_significance&data_mod$mono_significance),"rating"]<-"FP"
# for the rows of data_mod where overall significance is true AND mono significance is NOT true - set rating to FN
data_mod[which(data_mod$overall_significance&!data_mod$mono_significance),"rating"]<-"FN"
# for the rows of data_mod where overall significance is NOT true (!) AND mono significance is NOT true - set rating to TN
data_mod[which(!data_mod$overall_significance&!data_mod$mono_significance),"rating"]<-"TN"
#calculate sensitivity  and specificity from our ratings column
sensitivity<-sum(data_mod$rating=="TP",na.rm = T)/(sum(data_mod$rating=="TP",na.rm = T)+sum(data_mod$rating=="FN",na.rm = T))
specificity<-sum(data_mod$rating=="TN",na.rm = T)/(sum(data_mod$rating=="TN",na.rm = T)+sum(data_mod$rating=="FP",na.rm = T))
#return sensitivity and specificity
sum(data_mod$rating=="FN",na.rm = T)
sum(data_mod$rating=="TN",na.rm = T)
sum(data_mod$rating=="FP",na.rm = T)
sum(data_mod$rating=="TP",na.rm = T)


a<-glm(overall_significance~abs(monothermal_screen),family=binomial,data=data_mod)
summary(a)
x=60
p=(exp(a$coefficients[1]+a$coefficients[2]*x))/(1+exp(a$coefficients[1]+a$coefficients[2]*x))
p
exp(1*a$coefficients[2])


data_mod_short<-data.frame(monothermal_screen=data_mod$monothermal_screen)
predicted_prob<-predict(a,data_mod_short,type="response")
pred_cut<-ifelse(predicted_prob>0.5,1,0)

classDF<-data.frame(response=data_mod$overall_significance,predicted=pred_cut)
xtabs(~predicted+response,data=classDF)

return(list(sens_spec_dataset,g,summary(a)))
#how to get from predicted_prob to  cut off monothermal screen cut off
}

res_a=likelihood_ratio_dataset(data_mod)
res_a[[2]]
res_a[[3]]


data_mod_c<-data_raw[which(data_raw$Order=="C"),]
res_c=likelihood_ratio_dataset(data_mod_c)
res_c[[2]]
res_c[[3]]


data_mod_w<-data_raw[which(data_raw$Order=="W"),]
res_w=likelihood_ratio_dataset(data_mod_w)
res_w[[2]]
res_w[[3]]
#warm seems to have a stronger effect, it also has a lower AIC (better) 
#REALLY NEED TO VALIDATE THIS MODEL