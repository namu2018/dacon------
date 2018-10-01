plot(hos_test$OC, hos_test$revenue2)
hos <- hos[!is.na(hos_test$revenue1),]

boxplot(hos_test$salescost1)
quantile(hos_test$revenue2, na.rm=T)

hos_test1 <- NAProcess(hos_test, sel=c(8:55), 'mean')
names(hos_test1)
hos_test1[18,8]
tr_a

tr_a <- tr_a[!is.na(tr_a$revenue2),]

tr$employee1 <- as.integer(tr$employee1)
tr$employee2 <- as.integer(tr$employee2)
test$employee1 <- as.integer(test$employee1)
test$employee2 <- as.integer(test$employee2)
tr_a$employee1 <- as.integer(tr_a$employee1)
tr_a$employee2 <- as.integer(tr_a$employee2)
tr_a$OC <- as.integer(tr_a$OC)
tr_a$sido <- as.integer(tr_a$sido)
tr_a$instkind <- as.integer(tr_a$instkind)
tr_a$ownerChange <- as.integer(tr_a$ownerChange)
tr_a$instkind[is.na(tr_a$instkind)]<-5
str(tr_a)
tr_a$instkind
str(tr_a)

#####lm함수
LM <- lm(bedCount~., data=tr_a)
summary(LM)
selLm <- step(LM, direction='both')
LM <- lm(~ bedCount + instkind + interest1 + salescost2 + interest2 + 
           employee2, data=tr_a)
plot(LM)

###randomForest함수  
rf_emp1<- randomForest(employee1~salary2,data=tr_a)
pval <- predict(rf_emp, newdata=tr_a)
write.csv(pval, file="result_em2.csv", row.names=FALSE)
pval
imp <-importance(rf_emp)
imp
options("scipen"=100)
hos$employee1[is.na(hos$employee1)]<-predict(rf_emp1, hos[is.na(hos$employee1),])

rf_bed<- randomForest(ownerChange~.,data=tr)
imp <-importance(rf_bed)
imp
options("scipen"=100)


which(is.na(hos[[2]]))
names(hos)
