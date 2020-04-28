test <- read_csv("D:/BUDT758T/Project Files/Hospitals_Test_X.csv")
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






test_ip$step<-ifelse(test_ip$step>0.5,1,0)
test_op$step<-ifelse(test_op$step>0.45,1,0)
test_ip[test_ip['DC_RESULT']=='others',]$step<-0

test_new <- rbind(test_op[,c(1,21)],test_ip[,c(1,24)])
test_new$step=ifelse(test_new[,2]==1,"Yes","No")
write.csv(test_new,'test_newstep3.csv')
