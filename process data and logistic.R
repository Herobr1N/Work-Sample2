library(readr)
Hospitals_Train <- read_csv("D:/BUDT758T/Project Files/Hospitals_Train.csv")
View(Hospitals_Train)

#change the type of some variable
Hospitals_Train$RETURN=ifelse(Hospitals_Train$RETURN=='Yes',1,0)
Hospitals_Train$MONTH_ARR=as.factor(Hospitals_Train$MONTH_ARR)
Hospitals_Train$WEEKDAY_ARR=as.factor(Hospitals_Train$WEEKDAY_ARR)
Hospitals_Train$HOUR_ARHOUR_ARR=as.factor(Hospitals_Train$WEEKDAY_ARR)
Hospitals_Train$CHARGES=as.numeric(as.character(Hospitals_Train$CHARGES))
Hospitals_Train$DC_RESULT=as.factor(Hospitals_Train$DC_RESULT)
Hospitals_Train$ACUITY_ARR=as.factor(Hospitals_Train$ACUITY_ARR)
Hospitals_Train$ED_RESULT=as.factor(Hospitals_Train$ED_RESULT)
Hospitals_Train$DIAGNOSIS=as.factor(Hospitals_Train$DIAGNOSIS)
Hospitals_Train$FINANCIAL_CLASS=as.factor(Hospitals_Train$FINANCIAL_CLASS)
Hospitals_Train$RACE=as.factor(Hospitals_Train$RACE)
Hospitals_Train$HOSPITAL=as.factor(Hospitals_Train$HOSPITAL)

#Count the number of the null in each factor
check_null=sapply(Hospitals_Train, function(x) paste(round(((sum(is.na(x))-5500)/(length(x)-5500))*100,2),'%'))
check_null

#How manu  0 in CONSULT_ORDER--2553
length(which(Hospitals_Train$CONSULT_ORDER==0))
#How many 0 in CONSULT_CHARGE--224
length(which(Hospitals_Train$CONSULT_CHARGE==0))

#Test the colinearity
library("Hmisc")
test.corr=Hospitals_Train[,c('AGE','MONTH_ARR','HOUR_ARR','WEEKDAY_ARR','MONTH_DEP','WEEKDAY_DEP','HOUR_ARR', 'SAME_DAY')]
res2 <- rcorr(as.matrix(test.corr))
res2

#delete the 4 columns which have a lot of na
hospital_data<-subset(Hospitals_Train,select = -c(ADMIT_RESULT,CONSULT_IN_ED,RISK,SEVERITY))
hospital_data=na.omit(hospital_data)
dim(hospital_data)

#Partition the data set into a training data set and a validation data set
set.seed(9090)
num_obs=nrow(hospital_data)
valid_obs = sample(num_obs, 0.33*num_obs)
hospital_valid <- hospital_data[valid_obs,-1]
hospital_train <- hospital_data[-valid_obs,-1]

#Calculate the baseline of validation data set
base_table=table(hospital_valid$RETURN)
length(hospital_valid$RETURN[hospital_valid$RETURN==0])/length(hospital_valid$RETURN)

#Train the logistic model with the variables we chose
hospital_log=glm(RETURN ~ HOSPITAL+GENDER + AGE + RACE + FINANCIAL_CLASS + WEEKDAY_ARR + HOUR_ARR + MONTH_ARR + SAME_DAY + ED_RESULT + ACUITY_ARR + DC_RESULT+DIAGNOSIS+DIAG_DETAILS + CHARGES,data=hospital_train ,family="binomial")

#Make predictions on the validation data set
log_preds=predict(hospital_log,newdata = hospital_valid,type='response')

#Calculate the TNR,TPR and Accuracy measurements on the logistic model with different cutoffs
cutoffs_log=c(0.01,0.05,0.1,0.2,0.3,0.4,0.45,0.5,0.6,0.7,0.8,0.9,0.95,0.99)
log_acc=rep(0,14)
log_TPR=rep(0,14)
log_TNR=rep(0,14)
for (i in 1:14){    

#Train the LDA model with the variables we chose
library(MASS)
lda_model = lda(RETURN ~ HOSPITAL+GENDER + AGE + RACE + FINANCIAL_CLASS + WEEKDAY_ARR + HOUR_ARR + MONTH_ARR + SAME_DAY + ED_RESULT + ACUITY_ARR + DC_RESULT+DIAGNOSIS+DIAG_DETAILS + CHARGES,data=hospital_train)

#Make predictions on the validation data set
lda_preds=predict(lda_model,newdata = hospital_valid)

#Calculate the TNR,TPR and Accuracy measurements on the LDA with different cutoffs
cutoffs_lda=c(0.01,0.05,0.1,0.2,0.3,0.4,0.45,0.5,0.6,0.7,0.8,0.9,0.95)
lda_acc=rep(0,13)
lda_TPR=rep(0,13)
lda_TNR=rep(0,13)
for (i in 1:13){
  lda_class=ifelse(lda_preds$posterior[,2]>cutoffs[i],1,0)
  lda_confuse_test=table(hospital_test$RETURN,lda_class)
  lda_TPR[i]=(lda_confuse_test[2,2]/sum(lda_confuse_test[2,]))
  lda_TNR[i]=(lda_confuse_test[1,1]/sum(lda_confuse_test[1,]))
  lda_acc[i]=(lda_confuse_test[1,1]+lda_confuse_test[2,2])/sum(lda_confuse_test)
}

#Compare the LDA ROC & logistic ROC
plot(1-log_TNR,log_TPR,type = 'l',col='red', main='Compare Logistic Model ROC with LDA Model ROC')
lines(1-lda_TNR,lda_TPR,type = 'l',col='blue')
legend(0.5,0.3,legend = c('Logistic','LDA'),lty=c(1,1),col=c("blue","red"))

#kNN Model
hos_train.X = hospital_train[,c('AGE', 'SAME_DAY', 'CONSULT_ORDER', 'DIAG_DETAILS', 'CHARGES')]
hos_valid.X = hospital_valid[,c('AGE', 'SAME_DAY', 'CONSULT_ORDER', 'DIAG_DETAILS', 'CHARGES')]
hos_train.Y = hospital_train$RETURN
hos_valid.Y = hospital_valid$RETURN

#Try different k value and calculate the accuracy
library(class)
k_value=c(5,15,25,35,45)
KNN_acc=rep(0,5)

for (i in 1:5){
  knn = knn(hos_train.X, hos_valid.X, hos_train.Y, k=k_value[i])
  table = table(hos_valid.Y, knn)
  KNN_acc[i]= (table[1,1]+table[2,2])/sum(table)
}
KNN_acc

#QDA model(Eliminate all the variables that will generate the 'Rank deficiency' error)
qda_model = qda(RETURN ~ HOSPITAL+GENDER + AGE + FINANCIAL_CLASS + WEEKDAY_ARR + HOUR_ARR + MONTH_ARR + SAME_DAY + DIAGNOSIS + CHARGES,data=hospital_rest)
qda_preds=predict(qda_model,newdata = hospital_test)
qda_confuse_test=table(hospital_test$RETURN,qda_preds$class)
qda_acc=(qda_confuse_test[1,1]+qda_confuse_test[2,2])/sum(qda_confuse_test)