d <- read.csv('https://github.com/iwanami-datascience/vol3/raw/master/kato%26hoshino/q_data_x.csv')
head(d)
mean(d$gamesecond[d$cm_dummy==1])
mean(d$gamesecond[d$cm_dummy==0])

model <- glm(cm_dummy ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney + area_kanto +area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7  + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, d, family=binomial)
ps <- model$fitted.values
head(ps)
head(d$cm_dummy)

install.packages("rms")
library(rms)
model_lrm <- lrm(cm_dummy ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney + area_kanto +area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7  + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, d)
model_lrm
summary(glm(as.factor(d$gamedummy)~d$cm_dummy+ps, family=binomial))

ivec1 <- d$cm_dummy # Treated group
ivec0 <- rep(1, nrow(d))-ivec1 # Untreated group
ivec <- cbind(ivec1,ivec0)
iestp1 <- (ivec1/ps)
iestp0 <- (ivec0/(1-ps))
iestp <- iestp1+iestp0
ipwe_gs <- lm(d$gamesecond ~ ivec+0, weights=iestp)
summary(ipwe_gs)

iestp1_ATT <- ivec1
iestp0_ATT <- ivec0*ps/(1-ps)
iestp_ATT <- iestp1_ATT+iestp0_ATT
ipwe_ATT_gs = lm(d$gamesecond ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gs)

d2 <- subset(d, d$cm_dummy==0 | d$child_dummy==0)
ivec1_ltd <- d2$cm_dummy
ivec0_ltd <- rep(1, nrow(d2))-ivec1_ltd
model2 <- glm(cm_dummy ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney  + area_kanto +area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7  + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, family=binomial, data = d2)
ps2 <- model2$fitted

ivec_ltd <- cbind(ivec1_ltd,ivec0_ltd)
iestp1_ltd <- ivec1_ltd
iestp0_ltd <- ivec0_ltd*ps2/(1-ps2)
iestp_ltd <- iestp1_ltd+iestp0_ltd
ipwe_ltd_gs <- lm(d2$gamesecond ~ ivec_ltd+0, weights=iestp_ltd)
summary(ipwe_ltd_gs)

ivec1 <- d$cm_dummy # Treated group
ivec0 <- rep(1, nrow(d))-ivec1 # Untreated group
ivec <- cbind(ivec1, ivec0)

#SVM
# install.packages("e1071")
library(e1071)
model.svm <- svm(as.factor(cm_dummy) ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney + area_kanto + area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7 + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, d, probability = TRUE)
ps.svm.source <- predict(model.svm, newdata=d[,-1], probability=TRUE)
ps.svm <- attr(ps.svm.source, "probabilities")

# ROCカーブからAUC（即ちc統計量）を求める
install.packages("ROCR")
library(ROCR)

roc.svm <- cbind(ps.svm[,2],d$cm_dummy)
roc.pred.svm <- prediction(roc.svm[,1],roc.svm[,2])
auc.svm.tmp <- performance(roc.pred.svm, "auc")
as.numeric(auc.svm.tmp@y.values)
# [1] 0.9035095

# ATE

iestp1.svm <- (ivec1/ps.svm[,2])
iestp0.svm <- (ivec0/ps.svm[,1])
iestp.svm <- iestp1.svm+iestp0.svm
ipwe_gs.svm <- lm(d$gamesecond ~ ivec+0, weights=iestp.svm)
summary(ipwe_gs.svm)

# ATT

iestp1_ATT.svm <- ivec1
iestp0_ATT.svm <- ivec0*ps.svm[,2]/ps.svm[,1]
iestp_ATT.svm <- iestp1_ATT.svm+iestp0_ATT.svm
ipwe_ATT_gs.svm <- lm(d$gamesecond ~ ivec+0, weights=iestp_ATT.svm)
summary(ipwe_ATT_gs.svm)

# Randam Forest
install.packages("randomForest")
library(randomForest)
library(e1071)
model.rf <- randomForest(as.factor(cm_dummy) ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney + area_kanto + area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7 + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, d)
ps.rf <- predict(model.rf, newdata=d[,-1], type='prob')

# ROCカーブからAUC（即ちc統計量）を求める
roc.rf <- cbind(ps.rf[,2],d$cm_dummy)
roc.pred.rf <- prediction(roc.rf[,1],roc.rf[,2])
auc.rf.tmp <- performance(roc.pred.rf, "auc")
as.numeric(auc.rf.tmp@y.values)
# [1] 0.9967271

# ATE

iestp1.rf <- (ivec1/ps.rf[,2])
iestp0.rf <- (ivec0/ps.rf[,1])
iestp.rf <- iestp1.rf+iestp0.rf
ipwe_gs.rf <- lm(d$gamesecond ~ ivec+0, weights=iestp.rf)
summary(ipwe_gs.rf)

# ATT

iestp1_ATT.rf <- ivec1
iestp0_ATT.rf <- ivec0*ps.rf[,2]/ps.rf[,1]
iestp_ATT.rf <- iestp1_ATT.rf+iestp0_ATT.rf
ipwe_ATT_gs.rf <- lm(d$gamesecond ~ ivec+0, weights=iestp_ATT.rf)
summary(ipwe_ATT_gs.rf)


#Xgboost
install.packages("xgboost")
library(xgboost)
install.packages("Matrix")
library(Matrix)
dtrain <- d[,c(1,3,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,25,33)]
d.mx <- sparse.model.matrix(cm_dummy~., data=dtrain)
ddtrain <- xgb.DMatrix(d.mx, label=d$cm_dummy)
model.gdbt <- xgb.train(params=list(objective="binary:logistic",eta=0.5), data=ddtrain, nrounds=20)
pred.gdbt <- predict(model.gdbt, newdata=ddtrain)
ps.gdbt <- cbind(1-pred.gdbt, pred.gdbt)

# ROCカーブからAUC（即ちc統計量）を求める
roc.gdbt <- cbind(ps.gdbt[,2],d$cm_dummy)
roc.pred.gdbt <- prediction(roc.gdbt[,1],roc.gdbt[,2])
auc.gdbt.tmp <- performance(roc.pred.gdbt, "auc")
as.numeric(auc.gdbt.tmp@y.values)
# [1] 0.9926357

# ATE

iestp1.gdbt <- (ivec1/ps.gdbt[,2])
iestp0.gdbt <- (ivec0/ps.gdbt[,1])
iestp.gdbt <- iestp1.gdbt+iestp0.gdbt
ipwe_gs.gdbt <- lm(d$gamesecond ~ ivec+0, weights=iestp.gdbt)
summary(ipwe_gs.gdbt)

# ATT

iestp1_ATT.gdbt <- ivec1
iestp0_ATT.gdbt <- ivec0*ps.gdbt[,2]/ps.gdbt[,1]
iestp_ATT.gdbt <- iestp1_ATT.gdbt+iestp0_ATT.gdbt
ipwe_ATT_gs.gdbt <- lm(d$gamesecond ~ ivec+0, weights=iestp_ATT.gdbt)
summary(ipwe_ATT_gs.gdbt)

fit <- glm(cm_dummy~., data=dtrain, family=binomial)
ps <- fit$fitted.values # オリジナルの傾向スコア
par(mfrow=c(2,2))
hist(ps,breaks=50, main='Original PS: Logistic')
hist(ps.svm,breaks=50, main='SVM')
hist(ps.rf,breaks=50, main='Random Forest')
hist(ps.gdbt,breaks=50, main='Xgboost')
