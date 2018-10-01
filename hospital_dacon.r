##데이터 불러오기
getwd()
setwd("C:/Users/ktm/Documents/choi_dontouch/data/hospital")
tr_a <- read.csv("train.csv" )
tr_test <- read.csv("test.csv")
table()
hos <- hos[!is.na(hos$revenue1),]
hos$employee1 <- as.integer(hos$employee1)
hos$employee2 <- as.integer(hos$employee2)
hos_test$employee1 <- as.integer(hos_test$employee1)
hos_test$employee2 <- as.integer(hos_test$employee2)
##데이터 가공
#hos <- na.omit(hos)
#bedCount와 employee1,2의 연관도 있는 컬럼을 찾아내서 결측치 처리

rf_emp<- randomForest(employee2~salary2,data=tr_a)
pval <- predict(rf_emp, newdata=tr_a)
write.csv(pval, file="result_em2.csv", row.names=FALSE)
pval
imp <-importance(rf_emp)
imp


rf_bed<- randomForest(ownerChange~.,data=tr)
imp <-importance(rf_bed)
imp

##boxplot을 이용하여 이상치를 제거후 open과 close간의 차이가 많이나는 변수를 선택

par(mfrow=c(1,5))
a<- hos[is.na(hos$bedCount),]
hos1 <- sqldf('select * from hos where revenue1 < 25000000000')
plot(hos1$OC,hos1$revenue1)
plot(hos$OC,hos$sga1)
plot(hos$OC, hos$interest1)# 이자지출이 많다
plot(hos$OC, hos$ctax1)
plot(hos$OC, hos$liquidAsset2)###유동자산이 많다
plot(hos$OC, hos$quickAsset1)##차이 매우 많음
#plot(hos$OC, hos$receivableS1)
plot(hos$OC, hos$inventoryAsset1)##차이많음
plot(hos$OC, hos$nonCAsset1)#
plot(hos$OC, hos$OnonCAsset2)
#plot(hos$OC, hos$surplus2)
plot(hos$OC, hos$longLoan1)###1,2차이
plot(hos$OC, hos$shortLoan2)###1,2차이남
plot(hos$OC, hos$longLoan2)###차이
plot(hos$OC, hos$shortLoan1)##차이
plot(hos$OC, hos$NCLiabilities1)###차이
plot(hos$OC, hos$debt1)##차이남
plot(hos$OC, hos$debt2)
plot(hos1$OC, hos$noe2)
#plot(hos$OC, hos$salescost1)
plot(hos$OC, as.integer(hos$employee2))###1,2차이 많이남
plot(hos$OC, as.integer(hos$sido))##
plot(hos$OC, as.integer(hos$instkind))

library(Amelia)
missmap(hos)
hos[is.na(hos$sido), ]
sapply(hos, function(x) sum(is.na(hos) ))

sum(is.na(hos$employee2))
###모델 만들기
rf_model <- randomForest(OC~liquidAsset1+liquidAsset2+quickAsset1+quickAsset2+debt2+employee1+debt2
                         +longLoan2+longLoan1+NCLiabilities2+NCLiabilities1+employee2, data=hos)

pval<- predict(rf_model, newdata=hos_test)
pval
sb$OC <- pval
sb[sb$OC=='close']
write.csv(pval, file="result_R.csv", row.names=FALSE)
sqldf('select * from sb where OC="close" ')  
sb
dim(hos_test)
rf_model <- randomForest(OC~employee2, data=hos)
pval<- predict(rf_model, newdata=hos_test)
sb$OC <- pval
sb
str(hos)
