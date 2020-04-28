#import the training data set 
library(readr)
df<- read_csv("D:/BUDT758T/Project Files/Hospitals_Train.csv")

#
df$ACUITY_ARR[is.na(df$ACUITY_ARR)]<-'Unknown'
df$ED_RESULT[is.na(df$ED_RESULT)]<-'Unknown'
df$RACE[is.na(df$RACE)]<-'Unknown'
df$RISK[is.na(df$RISK)]<-'Unknown'
df$SEVERITY[is.na(df$SEVERITY)]<-'Unknown'

#change the type of some variable
df$RETURN=ifelse(df$RETURN=='Yes',1,0)
df$MONTH_ARR=as.factor(df$MONTH_ARR)
df$WEEKDAY_ARR=as.factor(df$WEEKDAY_ARR)
df$HOUR_ARR=as.factor(df$HOUR_ARR)
df$CHARGES=as.numeric(as.character(df$CHARGES))
df$CHARGES[is.na(df$CHARGES)]<-mean(as.numeric(df[!is.na(df$CHARGES),][['CHARGES']]))
df$DC_RESULT=as.factor(df$DC_RESULT)
df$ACUITY_ARR=as.factor(df$ACUITY_ARR)
df$ED_RESULT=as.factor(df$ED_RESULT)
df$DIAGNOSIS=as.factor(df$DIAGNOSIS)
df$FINANCIAL_CLASS=as.factor(df$FINANCIAL_CLASS)
df$RACE=as.factor(df$RACE)
df$HOSPITAL=as.factor(df$HOSPITAL)
df$GENDER=as.factor(df$GENDER)
df$ADMIT_RESULT=as.factor(df$ADMIT_RESULT)
df$RISK=as.factor(df$RISK)
df$SEVERITY=as.factor(df$SEVERITY)
df$CONSULT_IN_ED[is.na(df$CONSULT_IN_ED)]<-0
df$DIAG_DETAILS=as.numeric(df$DIAG_DETAILS)

sapply(df, function(x) sum(is.na(x)))


#check the structure the df
str(df)
library(rockchalk)
#reduce the levels in ED_RESULTS and DC_RESULTS
df$DC_RESULT = combineLevels(df$DC_RESULT,levs=c(2,4,5,7,8,9,10,12,16,28,30,31,32,33,34,35,36),newLabel = c("further treatment"))
df$DC_RESULT = combineLevels(df$DC_RESULT,levs = c(2,14,15,16,17,18),newLabel = c("left"))
df$DC_RESULT = combineLevels(df$DC_RESULT,levs = c(7,13),newLabel = c("other"))
df$DC_RESULT = combineLevels(df$DC_RESULT,levs = c(1,7,8,9,10),newLabel = ("dischage"))
df$DC_RESULT = combineLevels(df$DC_RESULT,levs = c(1,2,3,4,5,6),newLabel = c("no treatment"))
#reduce the levels in ED_RESULTS and DC_RESULTS
df$ED_RESULT=combineLevels(df$ED_RESULT,levs=c(1,2,3,16),newLabel = c("further treatment"))
df$ED_RESULT=combineLevels(df$ED_RESULT,levs=c(1,6,7,8),newLabel = c("leaving without completing treatment"))
df$ED_RESULT=combineLevels(df$ED_RESULT,levs=c(4,5),newLabel = c("left without permission"))
df$ED_RESULT=combineLevels(df$ED_RESULT,levs=c(6,5),newLabel = c("L&D"))
df$ED_RESULT=combineLevels(df$ED_RESULT,levs=c(1,3),newLabel = c("Discharged"))
#combine the 
df$ACUITY_ARR=combineLevels(df$ACUITY_ARR,levs=c(5,6),newLabel = c("5-Non-Urgent"))
df$ACUITY_ARR=combineLevels(df$ACUITY_ARR,levs=c(5),newLabel = c("Unknown"))

levels(df$DC_RESULT)
levels(df$ED_RESULT)
#seperate the data set by the ADMIT_RESULTS
df_ip <- df[!is.na(df$ADMIT_RESULT),]
df_op <-df[is.na(df$ADMIT_RESULT),]


#check the existence null values in the df_ip and df_op
sapply(df_ip, function(x) sum(is.na(x)))
sapply(df_op, function(x) sum(is.na(x)))
#elininated "ADMIT_RESULT", "RISK", "SEVERITY", "WEEKDAY_DEP", "HOUR_DEP", "MONTH_DEP", "ETHNICITY" from df_op
df_ip<-subset(df_ip,select = -c(WEEKDAY_DEP, HOUR_DEP, MONTH_DEP, ETHNICITY))
#elininated "ADMIT_RESULT", "RISK", "SEVERITY", "WEEKDAY_DEP", "HOUR_DEP", "MONTH_DEP", "ETHNICITY" from df_op
df_op<-subset(df_op,select = -c(WEEKDAY_DEP, HOUR_DEP, MONTH_DEP, ETHNICITY,ADMIT_RESULT, RISK, SEVERITY))
#check the structure of df_ip and df_op
str(df_ip)
str(df_op)
#create functions for train and validation set splitting
#seeds=908765
#perc=0.8
#gen_train<-function(df_whole){
#  set.seed(seeds)
#  ip_num=nrow(df_whole)
#  obs <-sample(ip_num,perc*ip_num)
#  trainset_alias <-df_whole[obs,]
#  return(trainset_alias)
#}

#gen_val<-function(df_whole){
#  set.seed(seeds)
#  ip_num=nrow(df_whole)
#  obs <-sample(ip_num,perc*ip_num)
#  valset_alias <-df_whole[-obs,]
#  return(valset_alias)
#}


#Remove all the nulls in the df_ip and df_op
ip_nonull <- na.omit(df_ip)
op_nonull <- na.omit(df_op)

###After having the testing data, we no longer need the validation data set!
#generate the train and validation sets in df_ip
#ip_nonull_train <-gen_train(ip_nonull)
#ip_nonull_val<-gen_val(ip_nonull)
#generate the train and validation sets in df_ip
#op_nonull_train <-gen_train(op_nonull)
#op_nonull_val<-gen_val(op_nonull)
#generate a sparse matrix on ip_nonull and op_nonull
library(Matrix)
ip_sparse <- sparse.model.matrix(RETURN ~ ., data = ip_nonull[,-1])
op_sparse <- sparse.model.matrix(RETURN ~ ., data = op_nonull[,-1])
#ip_sparse_train <- sparse.model.matrix(RETURN ~ ., data = ip_nonull_train[,-1])
#op_sparse_train <- sparse.model.matrix(RETURN ~ ., data = op_nonull_train[,-1])
#ip_sparse_val <- sparse.model.matrix(RETURN ~ ., data = ip_nonull_val[,-1])
#op_sparse_val <- sparse.model.matrix(RETURN ~ ., data = op_nonull_val[,-1])

###deal with the testing data set
test <- read_csv("D:/BUDT758T/Project Files/Hospitals_Test.csv")
#change the type of some variable

test$RETURN=ifelse(test$RETURN=='Yes',1,0)
test$MONTH_ARR=as.factor(test$MONTH_ARR)
test$WEEKDAY_ARR=as.factor(test$WEEKDAY_ARR)
test$HOUR_ARR=as.factor(test$HOUR_ARR)
test$CHARGES=as.numeric(as.character(test$CHARGES))
test$CHARGES[is.na(test$CHARGES)]<-mean(as.numeric(test[!is.na(test$CHARGES),][['CHARGES']]))
test$DC_RESULT=as.factor(test$DC_RESULT)
test$ACUITY_ARR[is.na(test$ACUITY_ARR)]<-'Unknown'
test$ACUITY_ARR=as.factor(test$ACUITY_ARR)
test$ED_RESULT[is.na(test$ED_RESULT)]<-'Unknown'
test$ED_RESULT=as.factor(test$ED_RESULT)
test$DIAGNOSIS=as.factor(test$DIAGNOSIS)
test$FINANCIAL_CLASS=as.factor(test$FINANCIAL_CLASS)
test$RACE[is.na(test$RACE)]<-'Unknown'
test$RACE=as.factor(test$RACE)
test$HOSPITAL=as.factor(test$HOSPITAL)
test$GENDER=as.factor(test$GENDER)
test$ADMIT_RESULT=as.factor(test$ADMIT_RESULT)
test$RISK[is.na(test$RISK)]<-'Unknown'
test$SEVERITY[is.na(test$SEVERITY)]<-'Unknown'
test$RISK=as.factor(test$RISK)
test$SEVERITY=as.factor(test$SEVERITY)
test$CONSULT_IN_ED[is.na(test$CONSULT_IN_ED)]<-0
test$DIAG_DETAILS=as.numeric(test$DIAG_DETAILS)


sapply(test, function(x) sum(is.na(x)))


#check the structure the test
str(test)
library(rockchalk)
#reduce the levels in ED_RESULTS and DC_RESULTS
test$DC_RESULT = combineLevels(test$DC_RESULT,levs=c(1,3,4,5,6,7,9,10,11,12,27,28,29,30,31,32,33),newLabel = c("further treatment"))
test$DC_RESULT = combineLevels(test$DC_RESULT,levs = c(1,11,12,13,14,15),newLabel = c("left"))
test$DC_RESULT = combineLevels(test$DC_RESULT,levs = c(5,10),newLabel = c("other"))
test$DC_RESULT = combineLevels(test$DC_RESULT,levs = c(5,6,7,8),newLabel = ("dischage"))
test$DC_RESULT = combineLevels(test$DC_RESULT,levs = c(1,2,3,4),newLabel = c("no treatment"))
#reduce the levels in ED_RESULTS and DC_RESULTS
test$ED_RESULT=combineLevels(test$ED_RESULT,levs=c(1,2,15),newLabel = c("further treatment"))
test$ED_RESULT=combineLevels(test$ED_RESULT,levs=c(1,6,7,8),newLabel = c("leaving without completing treatment"))
test$ED_RESULT=combineLevels(test$ED_RESULT,levs=c(4,5),newLabel = c("left without permission"))
test$ED_RESULT=combineLevels(test$ED_RESULT,levs=c(5,6),newLabel = c("L&D"))
test$ED_RESULT=combineLevels(test$ED_RESULT,levs=c(1,3),newLabel = c("Discharged"))

levels(test$DC_RESULT)
levels(test$ED_RESULT)
#seperate the data set by the ADMIT_RESULTS
test_ip <- test[!is.na(test$ADMIT_RESULT),]
test_op <-test[is.na(test$ADMIT_RESULT),]

#elininated "ADMIT_RESULT", "RISK", "SEVERITY", "WEEKDAY_DEP", "HOUR_DEP", "MONTH_DEP", "ETHNICITY" from test_op
test_ip<-subset(test_ip,select = -c(WEEKDAY_DEP, HOUR_DEP, MONTH_DEP, ETHNICITY))
#elininated "ADMIT_RESULT", "RISK", "SEVERITY", "WEEKDAY_DEP", "HOUR_DEP", "MONTH_DEP", "ETHNICITY" from test_op
test_op<-subset(test_op,select = -c(WEEKDAY_DEP, HOUR_DEP, MONTH_DEP, ETHNICITY,ADMIT_RESULT, RISK, SEVERITY))

test_op$DC_RESULT[is.na(test_op$DC_RESULT)]<-'other'


library(Matrix)
testip_sparse <- sparse.model.matrix(RETURN ~ ., data = test_ip[,-1])
testop_sparse <- sparse.model.matrix(RETURN ~ ., data = test_op[,-1])

test
sapply(test_ip,function(x) sum(is.na(x)))
sapply(test_op,function(x) sum(is.na(x)))



#classification regression
reg_ip_all <- glm(RETURN~.,data=ip_nonull[,-1], family="binomial")
reg_op_all <-glm(RETURN~.,data=op_nonull[,-1], family="binomial")
reg_ip_null <- glm(RETURN~1,data=ip_nonull[,-1], family="binomial")
reg_op_null <-glm(RETURN~1,data=op_nonull[,-1], family="binomial")
#stepwise
ip_both = step(reg_ip_null, scope=list(upper=reg_ip_all), direction="both", trace=1)
summary(ip_both)
op_both = step(reg_op_null, scope=list(upper=reg_op_all), direction="both", trace=0)
summary(op_both)
#predict the ip train and test set with classification regresssion
reg_pred_ip <- predict(ip_both,newdata=test_ip[,-1],type="response")
reg_pred_op <- predict(op_both,newdata=test_op[,-1],type="response")

#find the best cutoff on ip
cutoffs=c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.45,0.5,0.53,0.55,0.6,0.63)
reg_ip_acc=rep(0,13)
reg_ip_TPR=rep(0,13)
reg_ip_TNR=rep(0,13)
for (i in 1:13){
  reg_class_ip=ifelse(reg_pred_ip>cutoffs[i],1,0)
  reg_confuse_test=table(test_ip$RETURN,reg_class_ip)
  reg_ip_TPR[i]=(reg_confuse_test[2,2]/sum(reg_confuse_test[2,]))
  reg_ip_TNR[i]=(reg_confuse_test[1,1]/sum(reg_confuse_test[1,]))
  reg_ip_acc[i]=(reg_confuse_test[1,1]+reg_confuse_test[2,2])/sum(reg_confuse_test)
}
print(paste('For ip ,stepwise model at best cutoff=',toString(cutoffs[which.max(reg_ip_acc)]),', the acc =',toString(max(reg_ip_acc))))


#find the best cutoff on op
cutoffs=c(0.03,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
reg_op_acc=rep(0,12)
reg_op_TPR=rep(0,12)
reg_op_TNR=rep(0,12)
for (i in 1:13){
  reg_class_op=ifelse(reg_pred_op>cutoffs[i],1,0)
  reg_confuse_test_op=table(test_op$RETURN,reg_class_op)
  reg_op_TPR[i]=(reg_confuse_test_op[2,2]/sum(reg_confuse_test_op[2,]))
  reg_op_TNR[i]=(reg_confuse_test_op[1,1]/sum(reg_confuse_test_op[1,]))
  reg_op_acc[i]=(reg_confuse_test_op[1,1]+reg_confuse_test_op[2,2])/sum(reg_confuse_test_op)
}
print(paste('For op, stepwise model at best cutoff=',toString(cutoffs[which.max(reg_op_acc)]),', the acc =',toString(max(reg_op_acc))))

#combine the training and testing data together
all_ip=rbind(ip_sparse,testip_sparse)
all_op=rbind(op_sparse,testop_sparse)
ip_pca = prcomp(all_ip)
summary(ip_pca)
plot(ip_pca)
#combine the training and testing data
all_op=rbind(op_sparse,testop_sparse)
all_op=rbind(op_sparse,testop_sparse)
op_pca = prcomp(all_op)
summary(op_pca)
plot(op_pca)
#we only choose pc1
dfip_pca=prcomp(ip_sparse)
dfop_pca=prcomp(op_sparse)
dfip_pca1=dfip_pca$x[,1]
dfop_pca1=dfop_pca$x[,1]

#train the model with pc1
ip_pca1_model=glm(ip_nonull$RETURN~dfip_pca1,family='binomial')
op_pca1_model=glm(op_nonull$RETURN~dfop_pca1,family='binomial')
#compare the pca model and the logistic one
summary(ip_pca1_model)
summary(op_pca1_model)
summary(ip_both)
summary(op_both)

###xgBoost
library('xgboost')
traindataip <- ip_sparse
label_ip=ip_nonull$RETURN
bst_ip<-xgboost(data=as.matrix(traindataip),label=label_ip,max.depth=7,eta=4,nrounds=130,objective = "binary:logistic")
ip_pred_xgb<-predict(bst_ip,newdata = as.matrix(testip_sparse),type='response')
#plot the importance matrix of ip
ip_importance_matrix <- xgb.importance(colnames(as.matrix(traindataip)), model = bst_ip)
xgb.plot.importance(ip_importance_matrix)
#plot the importance matrix of op
op_importance_matrix <- xgb.importance(colnames(as.matrix(traindataop)), model = bst_op)
xgb.plot.importance(op_importance_matrix)
#choose a best cutoff on ip data
cutoffs_xgb_ip=c(0.01,0.09,0.1,0.15,0.2,0.3,0.4,0.45,0.5,0.6,0.7,0.8,0.9)
xgb_ip_acc=rep(0,13)
xgb_ip_TPR=rep(0,13)
xgb_ip_TNR=rep(0,13)
for (i in 1:13){
  xgb_class=ifelse(ip_pred_xgb>cutoffs_xgb_ip[i],1,0)
  xgb_confuse_test=table(test_ip$RETURN,xgb_class)
  xgb_ip_TPR[i]=(xgb_confuse_test[2,2]/sum(xgb_confuse_test[2,]))
  xgb_ip_TNR[i]=(xgb_confuse_test[1,1]/sum(xgb_confuse_test[1,]))
  xgb_ip_acc[i]=(xgb_confuse_test[1,1]+xgb_confuse_test[2,2])/sum(xgb_confuse_test)
}
print(paste('For ip data, XGboost model at cutoff=',toString(cutoffs_xgb_ip[which.max(xgb_ip_acc)],),'will have a highest acc',',which is =',toString(max(xgb_ip_acc))))
#do the same on the op data
traindataop <- op_sparse
label_op=op_nonull$RETURN
bst_op<-xgboost(data=as.matrix(traindataop),label=label_op,max.depth=7,eta=4,nround=521,objective = "binary:logistic")
op_pred_xgb<-predict(bst_op,newdata = as.matrix(testop_sparse),type='response')
cutoffs_xgb_op=c(0.01,0.09,0.1,0.15,0.2,0.3,0.4,0.5,0.65,0.7,0.8,0.9,0.95)
xgb_op_acc=rep(0,13)
xgb_op_TPR=rep(0,13)
xgb_op_TNR=rep(0,13)
for (i in 1:13){
  xgb_class=ifelse(op_pred_xgb>cutoffs_xgb_op[i],1,0)
  xgb_confuse_test=table(test_op$RETURN,xgb_class)
  xgb_op_TPR[i]=(xgb_confuse_test[2,2]/sum(xgb_confuse_test[2,]))
  xgb_op_TNR[i]=(xgb_confuse_test[1,1]/sum(xgb_confuse_test[1,]))
  xgb_op_acc[i]=(xgb_confuse_test[1,1]+xgb_confuse_test[2,2])/sum(xgb_confuse_test)
}

print(paste('For op data, XGboost model at cutoff=',toString(cutoffs_xgb_ip[which.max(xgb_op_acc)],),
            'will have a highest acc',',which is =',toString(max(xgb_op_acc))))

###randomForest
rf_ip=randomForest(as.factor(RETURN)~.,data=ip_nonull[,-1],ntree=500,mtry=4,importance=TRUE)
ip_pred_rf=predict(rf_ip,newdata=test_ip[,-1],type="prob")
ip_probs_rf=ip_pred_rf[,2]
cutoffs_rf_ip=c(0.03,0.09,0.1,0.15,0.2,0.3,0.4,0.5,0.6)
rf_ip_acc=rep(0,9)
rf_ip_TPR=rep(0,9)
rf_ip_TNR=rep(0,9)
for (i in 1:13){
  rf_class=ifelse(ip_probs_rf>cutoffs_rf_ip[i],1,0)
  rf_confuse_test=table(test_ip$RETURN,rf_class)
  rf_ip_TPR[i]=(rf_confuse_test[2,2]/sum(rf_confuse_test[2,]))
  rf_ip_TNR[i]=(rf_confuse_test[1,1]/sum(rf_confuse_test[1,]))
  rf_ip_acc[i]=(rf_confuse_test[1,1]+rf_confuse_test[2,2])/sum(rf_confuse_test)
}
print(paste('For ip data, rf model at cutoff=',toString(cutoffs_rf_ip[which.max(rf_ip_acc)],),
            'will have a highest acc',',which is =',toString(max(rf_ip_acc))))


rf_op=randomForest(as.factor(RETURN)~.,data=op_nonull[,-1],ntree=500,mtry=4,importance=TRUE)
op_pred_rf=predict(rf_op,newdata=test_op[,-1],type="prob")
op_probs_rf=op_pred_rf[,2]
cutoffs_rf_op=c(0.03,0.09,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95)
rf_op_acc=rep(0,13)
rf_op_TPR=rep(0,13)
rf_op_TNR=rep(0,13)
for (i in 1:13){
  rf_class=ifelse(op_probs_rf>cutoffs_rf_op[i],1,0)
  rf_confuse_test=table(test_op$RETURN,rf_class)
  rf_op_TPR[i]=(rf_confuse_test[2,2]/sum(rf_confuse_test[2,]))
  rf_op_TNR[i]=(rf_confuse_test[1,1]/sum(rf_confuse_test[1,]))
  rf_op_acc[i]=(rf_confuse_test[1,1]+rf_confuse_test[2,2])/sum(rf_confuse_test)
}
print(paste('For op data, rf model at cutoff=',toString(cutoffs_rf_op[which.max(rf_op_acc)],),
            'will have a highest acc',',which is =',toString(max(rf_op_acc))))
importance(rf_ip)
varImpPlot(rf_ip)
importance(rf_op)
varImpPlot(rf_op)
