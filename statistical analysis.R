library(readr)
Hospitals_Train <- read_csv("D:/BUDT758T/Project Files/Hospitals_Train.csv")
View(Hospitals_Train)
Hospitals_Train$GENDER=ifelse(Hospitals_Train$GENDER='Female',1,0)
Hospitals_Train$CHARGES=as.numeric(as.character(Hospitals_Train$CHARGES))

#Count the number of the null in each factor
check_null=sapply(Hospitals_Train, function(x) sum(is.na(x)))
#Show the null percentage of each factor
sprintf("%1.2f%%", 100*(check_null-5500)/(nrow(Hospitals_Train)-5500))
#how manu  1 in CONSULT_ORDER--2553
length(which(Hospitals_Train$CONSULT_ORDER==1))
#How many 1 in CONSULT_CHARGE--224
length(which(Hospitals_Train$CONSULT_CHARGE==1))
#hour arriving &hour departure
cor(as.numeric(hospital_data$HOUR_ARR),as.numeric(hospital_data$HOUR_DEP))


#delete the 4 columns which have a lot of na
hospital_data<-subset(Hospitals_Train,select = -c(ADMIT_RESULT,CONSULT_IN_ED,RISK,SEVERITY))
hospital_data=na.omit(hospital_data)
dim(hospital_data)
View(hospital_data)
for (i in 1:nrow(hospital_data)){
 check_sameday[i]=ifelse((abs(hospital_data$WEEKDAY_DEP[i]-hospital_data$WEEKDAY_ARR[i])+abs(hospital_data$MONTH_DEP[i]-hospital_data$MONTH_ARR[i]))==0,1,0)
}
table(check_sameday,hospital_data$SAME_DAY)
sum(as.numeric(abs(hospital_data$check_sameday-hospital_data$SAME_DAY)),na.rm = TRUE)

#change the nature of some variable
hospital_data$RETURN=ifelse(hospital_data$RETURN=='Yes',1,0)
hospital_data$
  
#test and eliminate the multi colinearity
library("Hmisc")
res2 <- rcorr(as.matrix(test.corr))
res2

#logistic--backwards
set.seed(9090)
num_obs=nrow(hospital_data)
test_obs = sample(num_obs, 0.1*num_obs)
hospital_test <- hospital_data[test_obs,-1]
hospital_rest <- hospital_test[-test_obs,-1]
movies_all <- glm(RETURN~., data=hospital_test)

