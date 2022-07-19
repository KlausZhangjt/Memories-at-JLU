##
getwd()  ##查看工作目录
setwd("C:/Users/HP/Desktop") ##设置工作目录位置
rm(list = ls()) ##清除内存中所有的变量
set.seed(777) ##设定种子

##数据读入
cardata=read.csv("freMTPL2freq.csv",stringsAsFactors = TRUE)

##添加是否发生理赔变量
cardata<-within(cardata,{
  Claim<-NA
  Claim[ClaimNb > 0]<-1
  Claim[ClaimNb == 0]<-0})

cardata$Claim<-as.factor(cardata$Claim)
cardata

##划分训练集和测试集，其中70%为训练集，30%为测试集
set.seed(777)
train_index <- sample(nrow(cardata),0.7*nrow(cardata))
traindata <-cardata[train_index,]
testdata <- cardata[-train_index,]


##决策树
## install.packages("rpart")
## install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

##双采样
traindata_balanced_both <- ovun.sample(Claim~Exposure + Area + VehPower + DrivAge + VehBrand + VehGas + Density + Region, data = traindata, method = "both",p=0.5,N=67856,seed=1)$data
table(traindata_balanced_both$Claim)

fit_both <- rpart(Claim~Exposure + Area + VehPower + DrivAge + VehBrand + VehGas + Density + Region,data=traindata_balanced_both,method="class",control=rpart.control(minsplit = 40, minbucket = 40, cp = 0.001, maxdepth = 10))
plotcp(fit_both)
rpart.plot(fit_both)

prp(fit_both,type=5,extra=2,digits=3)


fit_both$cptable
(cp_opt <-  fit_both$cptable[which(fit_both$cptable[,"nsplit"]==3),"CP"])
cp_opt

fit_both_best <- prune(fit_both, cp = cp_opt)
op <- par(no.readonly = TRUE)
par(mar=c(1,1,1,1))
plot(fit_both_best,uniform=TRUE,margin=0.1)
text(fit_both_best,cex=1.5)
par(op)

tree.pred_both <- predict(fit_both_best,testdata,type="class")

(table <- table(tree.pred_both,testdata$clm,dnn=c("预测值","真实值")))
(accuracy <- sum(diag(table))/sum(table))
(sensitivity <- table[2,2]/(table[1,2]+table[2,2]))

roc.curve(testdata$clm, tree.pred_under, plotit = T)






set.seed(777) 
fit <- rpart(Claim ~ Exposure + Area + VehPower + DrivAge + VehBrand + VehGas + Density + Region, data=traindata, method="class",split='gini',control=rpart.control(minsplit = 40, minbucket = 40, cp = 0.0000001, maxdepth = 10))
fit

fit <- rpart(Claim~Exposure + Area + VehPower + DrivAge + VehBrand + VehGas + Density + Region, data=traindata, method="class")
rpart.plot(fit)
plot(fit)


tree.pred <- predict(fit,testdata,type="class")

(table <- table(tree.pred,testdata$clm,dnn=c("预测值","真实值")))
(accuracy <- sum(diag(table))/sum(table))
(sensitivity <- table[2,2]/(table[1,2]+table[2,2]))

#install.packages("ROSE")
library(ROSE)

#roc.curve(testdata$clm, tree.pred, plotit = T)

table(traindata$clm)







################双采样#####################
traindata_balanced_both <- ovun.sample(clm~veh_value+veh_body+veh_age+gender+area+agecat, data = traindata, method = "both",p=0.5,N=67856,seed=1)$data
table(traindata_balanced_both$clm)

fit_both <- rpart(clm~veh_value+veh_body+veh_age+gender+area+agecat,data=traindata_balanced_both,method="class",control=rpart.control(minsplit = 40, minbucket = 40, cp = 0.001, maxdepth = 10))
plotcp(fit_both)

fit_both$cptable
(cp_opt <-  fit_both$cptable[which(fit_both$cptable[,"nsplit"]==3),"CP"])
cp_opt

fit_both_best <- prune(fit_both, cp = cp_opt)
op <- par(no.readonly = TRUE)
par(mar=c(1,1,1,1))
plot(fit_both_best,uniform=TRUE,margin=0.1)
text(fit_both_best,cex=1.5)
par(op)

tree.pred_both <- predict(fit_both_best,testdata,type="class")

(table <- table(tree.pred_both,testdata$clm,dnn=c("预测值","真实值")))
(accuracy <- sum(diag(table))/sum(table))
(sensitivity <- table[2,2]/(table[1,2]+table[2,2]))

roc.curve(testdata$clm, tree.pred_under, plotit = T)

################人工数据合成法(SMOTE)#####################
traindata_balanced_rose <- ROSE(clm~veh_value+veh_body+veh_age+gender+area+agecat, data = traindata, N=100000, seed=1)$data
table(traindata_balanced_rose$clm)

fit_rose <- rpart(clm~veh_value+veh_body+veh_age+gender+area+agecat,data=traindata_balanced_rose,method="class",control=rpart.control(minsplit = 40, minbucket = 40, cp = 0.001, maxdepth = 10))
plotcp(fit_rose)

fit_rose$cptable
(cp_opt <-  fit_rose$cptable[which(fit_rose$cptable[,"nsplit"]==4),"CP"])
cp_opt

fit_rose_best<- prune(fit_rose, cp = cp_opt)
op <- par(no.readonly = TRUE)
par(mar=c(1,1,1,1))
plot(fit_rose_best,uniform=TRUE,margin=0.1)
text(fit_rose_best,cex=1.5)
par(op)

tree.pred_rose <- predict(fit_rose_best,testdata,type="class")

(table <- table(tree.pred_rose,testdata$clm,dnn=c("预测值","真实值")))
(accuracy <- sum(diag(table))/sum(table))
(sensitivity <- table[2,2]/(table[1,2]+table[2,2]))

roc.curve(testdata$clm, tree.pred_rose, plotit = T)

################信息熵分裂准则#####################
#fit <- rpart(clm~veh_value+veh_body+veh_age+gender+area+agecat,data=traindata,method="class",control=rpart.control(minsplit = 40, minbucket = 40, cp = 0.001, maxdepth = 10),parms=list(split="information"))
