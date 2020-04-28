#IP best depth=3 nrounds=50
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
