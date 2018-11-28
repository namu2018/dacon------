rm(list=ls())
library(randomForest)
library(dplyr)

setwd("C:/Users/ktm/Documents/choi_dontouch/data/hospital")
setwd("C:/Kayoung/1. Big data 3/0. Data/Dacon_2")
setwd("C:/choi/bigdata/data")



## Data 불러오기 
hos_test <- read.csv("test.csv", na.string=c("", NA, "-"))
hos <- read.csv("train.csv",  na.string=c("", NA, "-"))
sub <- read.csv("submission_sample.csv")




##### 1. Pre-Processing for Data Cleaning #####

# test set의 employee1, employee2의 쉼표 삭제 
hos_test$employee1 <- gsub(",", "", hos_test$employee1)
hos_test$employee2 <- gsub(",", "", hos_test$employee2)


## train dataset과 test dataset의 sido와 instkind의 levels 맞추기 
SIDO = c("busan", "choongbuk", "choongnam", "daegu", "daejeon",
         "gangwon", "gwangju", "gyeongbuk", "gyeonggi", "gyeongnam",
         "incheon", "jeonbuk", "jeonnam", "sejong", "seoul",
         "ulsan", "jeju")

# train dataset과 test dataset의 sido lecels 맞추기 
hos$sido = factor(hos$sido, levels=SIDO)
hos_test$sido = factor(hos_test$sido, levels=SIDO)

levels(hos$sido)
levels(hos_test$sido)

## train dataset과 test dataset의 instkind의 levels 맞추기 
INSTKIND = c("", "clinic", "dental_clinic", "general_hospital", "hospital" ,
             "nursing_hospital", "traditional_clinic", "traditional_hospital")

hos$instkind = factor(hos$instkind, levels=INSTKIND)
hos_test$instkind = factor(hos_test$instkind, levels=INSTKIND)

levels(hos$instkind)
levels(hos_test$instkind)



# 구조 맞춰주기 
str(hos)
str(hos_test)

hos_test$employee1 <- as.integer(hos_test$employee1)
hos_test$employee2 <- as.integer(hos_test$employee2)
hos_test$OC <- as.factor(hos_test$OC)







##### 결측치 처리 과정 
hos <- hos[!is.na(hos$salary1), ]
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

loc <- which(is.na(hos$ownerChange))
hos[loc,"ownerChange"] <- getmode(hos$ownerChange)

loc <- which(is.na(hos_test$ownerChange))
hos_test[loc,"ownerChange"] <- getmode(hos_test$ownerChange)


loc <- which(is.na(hos$instkind))
hos[loc,"instkind"] <- getmode(hos$instkind)

loc <- which(is.na(hos_test$instkind))
hos_test[loc,"instkind"] <- getmode(hos_test$instkind)

### 파생변수 추가 
hos$bedCount0 = ifelse(is.na(hos$bedCount) ,1,0)
hos_test$bedCount0 <- ifelse(is.na(hos_test$bedCount) ,1,0)
hos$employee10 <- ifelse(is.na(hos$employee1) ,1,0)
hos_test$employee10 <- ifelse(is.na(hos_test$employee1) ,1,0)
hos$employee20 <- ifelse(is.na(hos$employee2) ,1,0)
hos_test$employee20 <- ifelse(is.na(hos_test$employee2) ,1,0)
hos$revenue0 <- hos$revenue1 - hos$revenue2
hos_test$revenue0 <- hos_test$revenue1 - hos_test$revenue2


for (i in (3:57) ){
  type = class(hos[[i]])
  if  (type=='numeric'){
    loc = which(is.na(hos[[i]]))
    hos[loc,i] <- 0 
   }else if (type=='integer'){
    loc = which(is.na(hos[[i]]))
    hos[loc,i] <- 0
   } 
}



for (i in 3:57){
  type = class(hos_test[[i]])
  if  (type=='numeric'){
    loc = which(is.na(hos_test[[i]]))
    hos_test[loc,i] <- 0 
   } else if  (type=='integer'){
    loc = which(is.na(hos_test[[i]]))
    hos_test[loc,i] <- 0 
   } 
}




sub <- read.csv("submission_sample.csv")
#### 2. Feature Engineering and Variable Selection & 3. Model Selection and Regularization
## 다양한 plot 탐색을 통해 변수 선택하여 randomForest analysis 실행

library(randomForest)
set.seed(10)
rf <- randomForest(OC~interest1+longLoan1+debt2+NCLiabilities1+employee1+employee2, hos)
pre0 <- predict(rf, hos_test)
sub <- cbind(sub, pre0)
sub$pre0 <- ifelse(sub$pre0==' close',0,1)

sub$OC <- ifelse(sub$pre0 ==1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
# [1]   6  30 293 424 429 431---------------------------------------------------------------- (6개)

rf <- randomForest(OC~bedCount0, hos)
pre1<- predict(rf, hos_test)
sub <- cbind(sub, pre1)
sub$pre1 <- ifelse(sub$pre1==' close',0,1)

sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
# [1]   6  24  30  64 123 229 258 293 341 424 425 429 430 431 -------------------------------- (추가)24, 64, 123, 229, 258, 293, 341, 430 (14개)

rf <- randomForest(OC~netAsset2 + employee10, hos)
pre2<- predict(rf, hos_test)
sub <- cbind(sub, pre2)
sub$pre2 <- ifelse(sub$pre2==' close',0,1)

sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1 & sub$pre2 ==1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
# [1]   6  24  30  64 123 229 258 293 341 424 425 429 430 431 -------------------------------- 그대로 (14개)


rf <- randomForest(OC~interest1 + employee1, hos)
pre3<- predict(rf, hos_test)
sub <- cbind(sub, pre3)
sub$pre3 <- ifelse(sub$pre3==' close',0,1)

sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1 & sub$pre2 ==1 & sub$pre3 == 1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
# [1]   6  24  30  64  79 123 229 258 293 310 341 424 425 429 430 431  ----------------------- 그대로 (14개)



### 로지스틱 선형회귀 분석 실시(답으로 추정되는 저 위의 병원 외 한 개 병원씩 추가 탐색)
glm <- glm(data = hos, OC ~ openDate + employee10, family = binomial)
pre4 <- predict(glm, hos_test)
sub <- cbind(sub, pre4)
sub$pre4 <- ifelse(sub$pre4 < 0.35 ,0,1)

sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1 & sub$pre2 ==1 & sub$pre3 == 1 & sub$pre4 ==1 , 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
# [1]   6  24  30  64 123 229 258 293 310 341 424 425 429 430 431 ---------------------------- 310 추가 (15개)




glm <- glm(data = hos, OC ~ revenue1 + receivableS1, family = binomial)
pre5 <- predict(glm, hos_test)
sub <- cbind(sub, pre5)
sub$pre5 <- ifelse(sub$pre5 < 1 ,0,1)
sub
sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1 & sub$pre2 ==1 & sub$pre3 == 1 & sub$pre4 ==1 & sub$pre5 == 1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
# [1]   6  24  30  64  79 123 229 258 293 310 341 424 425 429 430 431 ------------------------ 79 추가(16개)


glm <- glm(data = hos, OC ~ revenue1 + profit2, family = binomial)
pre6 <- predict(glm, hos_test)
sub <- cbind(sub, pre6)
sub$pre6 <- ifelse(sub$pre6 < 1 ,0,1)

sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1 & sub$pre2 ==1 & sub$pre3 == 1 & sub$pre4 ==1 & sub$pre5 == 1 & sub$pre6==1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
#  [1]   5   6  24  30  64  79 123 229 258 293 310 341 424 425 429 430 431 ------------------- 5번 추가(17개)

glm <- glm(data = hos, OC ~	bedCount, family = binomial)
pre7 <- predict(glm, hos_test)
sub = cbind(sub, pre7)
sub$pre7 <- ifelse(sub$pre7 < 2.86 ,0,1)

sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 ==1 & sub$pre2 ==1 & sub$pre3 == 1 & sub$pre4 ==1 & sub$pre5 == 1 & sub$pre6==1 & sub$pre7==1, 1, 0)
table(sub$OC); sub[sub$OC==0, "inst_id"] 
#  [1]   5   6  24  30  46  64  79 123 229 258 293 310 341 424 425 429 430 431 --------------- 46 추가(46을 제외하고 모든 id가 close이므로 close 가능성 높은 모델 추정)




##### 4. Optimization Processing
library(dplyr)
library(xgboost)
setwd("C:/choi/bigdata/data")
options("scipen"=100)

a_test <- read.csv("test.csv", na.string=c("", NA))
a <- read.csv("train.csv",  na.string=c("", NA))
sub1 <- read.csv("submission_sample.csv")
sub2 <- read.csv("result.csv")

colN = c("busan", "choongbuk", "choongnam", "daegu", "daejeon",
         "gangwon", "gwangju", "gyeongbuk", "gyeonggi", "gyeongnam",
         "incheon", "jeonbuk", "jeonnam", "sejong", "seoul",
         "ulsan", "jeju")

a$sido = factor(a$sido, levels=colN)
a_test$sido = factor(a_test$sido, levels=colN)

levels(a$sido)
levels(a_test$sido)


colN = c("", "clinic", "dental_clinic", "general_hospital", "hospital" ,
         "nursing_hospital", "traditional_clinic", "traditional_hospital")


a$instkind = factor(a$instkind, levels=colN)
a_test$instkind = factor(a_test$instkind, levels=colN)

levels(a$instkind)
levels(a_test$instkind)

hos <- hos[!is.na(hos$salary1), ]



a$OC <- as.integer(a$OC)
a$sido <- as.integer(a$sido)
a$instkind <- as.integer(a$instkind)
a$ownerChange <- as.integer(a$ownerChange)
a_test$OC <- as.integer(a_test$OC)
a_test$sido <- as.integer(a_test$sido)
a_test$instkind <- as.integer(a_test$instkind)
a_test$ownerChange <- as.integer(a_test$ownerChange)
a$redif <- a$revenue1 - a$revenue2
a_test$redif <- a_test$revenue1 - a_test$revenue2
a$bedCount0 = ifelse(is.na(a$bedCount) ,1,0)
a_test$bedCount0 <- ifelse(is.na(a_test$bedCount) ,1,0)
a$employee10 <- ifelse(is.na(a$employee1) ,1,0)
a_test$employee10 <- ifelse(is.na(a_test$employee1) ,1,0)
a$employee20 <- ifelse(is.na(a$employee2) ,1,0)
a_test$employee20 <- ifelse(is.na(a_test$employee2) ,1,0)

a_test$OC <- sub2$OC
b_test <- a_test
hospital <- rbind(a, a_test)

#########################################################데이터 전처리 마침

X_train = hospital %>% select(redif,salescost1,ctax1, profit1,  interest1,inventoryAsset1, bedCount0,
                              employee10, employee20, noe1,profit2,
                              longLoan2,debt2,NCLiabilities1,employee1,employee2
) %>% as.matrix()
y_train = hospital$OC
dtrain = xgb.DMatrix(X_train, label = y_train)
model = xgboost(data = dtrain, nround =1000,objective="reg:linear", eval_metric="rmse", max_depth =25, eta = 0.05)
xgb.importance(feature_names = colnames(X_train), model) %>% xgb.plot.importance()

X_test = b_test %>% select(redif,salescost1,ctax1, profit1,  interest1,inventoryAsset1, bedCount0,
                           employee10, employee20, noe1,profit2,
                           longLoan2,debt2,NCLiabilities1,employee1,employee2
) %>% as.matrix()
y_test = b_test$OC
preds = predict(model, X_test)
sub1$OC <- preds
head(sub[order(sub1$OC),],18) ##==> 0.001까지
