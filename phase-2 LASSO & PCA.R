

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
print(paste('For ip ,stepwise model at best cutoff=',toString(cutoffs_reg_ip[which.max(reg_ip_acc)]),', the acc =',toString(max(reg_ip_acc))))
#best:when cutoff=0.5, acc=84.30962%


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
print(paste('For op, stepwise model at best cutoff=',toString(cutoffs_reg_op[which.max(reg_op_acc)]),', the acc =',toString(max(reg_op_acc))))
#best:when cutoff=0.45, acc=76.121035%



#combine the training and testing data together
all_ip=rbind(ip_sparse,testip_sparse)
all_op=rbind(op_sparse,testop_sparse)
ip_pca = prcomp(all_ip)
summary(ip_pca)
plot(ip_pca)
#
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

#
ip_pca1_model=glm(ip_nonull$RETURN~dfip_pca1,family='binomial')
op_pca1_model=glm(op_nonull$RETURN~dfop_pca1,family='binomial')

#
testip_pca=prcomp(testip_sparse)
testop_pca=prcomp(testop_sparse)
#
testip_pca1=testip_pca$x[,1]
testop_pca1=testop_pca$x[,1] 

summary(ip_both)
summary(ip_pca1_model)
summary(op_both)
summary(op_pca1_model)


