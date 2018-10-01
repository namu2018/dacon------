getwd()
setwd("C:/choi/bigdata/data/hospital")
hos <- read.csv("train.csv")
hos_test<- read.csv("test.csv")
sb<- read.csv("submission_sample.csv")
is.na(hos$bedCount)

hos <- hos[!is.na(hos$revenue1),]
hos <- hos[is.na(hos$instkind)]
hos <- hos[hos$instkind!="dental_clinic",]

hos$employee1 <- as.integer(hos$employee1)
hos$employee2 <- as.integer(hos$employee2)
hos_test$employee1 <- as.integer(hos_test$employee1)
hos_test$employee2 <- as.integer(hos_test$employee2)

hos
str(hos1)
names(hos)
hos1_07 <- hos1[,c(1:31,56,58)]
hos1_08 <- hos1[,c(1:7,32:55,57,58)]
names(hos1_07)
str(hos1_07)
hos1_07$ownerChange <- as.integer(hos1_07$ownerChange)

hos07_cor <- cor(hos1_07)
corrplot(hos07_cor)
corrplot(cor(hos1_07), method="color", addCoef.col="black")
hos1_07

a <- hos[,c(1:8,16,32,40,56,57,58)]
a$totalrev1 <- hos1$revenue1- hos1$salescost1
a$totalrev2 <- hos1$revenue2 - hos1$salescost2
a$no_pro1 <- a$totalrev1 - hos1$sga1
a$no_pro2 <- a$totalrev2 - hos1$sga2
a$dif_re <- hos1$revenue1 - hos1$revenue2
a$dif_pr <- hos1$profit1 - hos1$profit2
a$dif_emp <- as.integer(hos1$employee1) - as.integer(hos1$employee2)

par(mfrow=c(3,3))
plot(a$OC, a$revenue1)
plot(a$OC, a$sgg)
plot(a$OC, a$bedCount)
plot(a$OC, a$revenue2)
plot(a$OC, a$profit1)
plot(a$OC, as.integer(a$employee1))
plot(a$revenue1, a$revenue2)
plot(a$revenue1, a$profit1)
plot(a$OC, a$totalrev1)
plot(a$OC, a$no_pro2)
plot(a$revenue1, a$no_pro1)
plot(as.integer(a$OC), a$dif_emp)
plot(as.integer(a$OC), a$dif_re)
plot(a$dif_pr)

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
str(hos)
library(sqldf)
str(hos)
sqldf('select * from hos where instkind="dental_clinic" ')  
names(hos)
library(randomForest)
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
rf_model <- randomForest(OC~surplus2, data=hos)
pval<- predict(rf_model, newdata=hos_test)
sb$OC <- pval
sb