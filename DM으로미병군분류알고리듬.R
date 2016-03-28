##건강??? 미병1, 미병2 ???machine leaning ??행. ?ѱ??̱?????
install.packages("RODBC")
install.packages("FSelector")
install.packages("party")
install.packages("mvtnorm")
install.packages("modeltools")
install.packages("zoo")
install.packages("sandwich")
install.packages("strucchange")
install.packages("coin")
install.packages("plyr")
install.packages("dplyr")

sessionInfo()
install.packages("rJava")
install.packages("e1071")

library(RODBC)
library(MatchIt)
library(rJava)
library(FSelector)
library(mvtnorm)
library(party)
library(plyr)
library(dplyr)
library(stats)
library(randomForest)
library(caret)

library(nnet)
library(e1071)

conn<-odbcConnect('mibyeong',uid='sa',pwd='leo0515')

Sys.getlocale()
Sys.setlocale("LC_COLLATE","en_US.UTF-8")

mb_tmp=sqlQuery(conn,"select * from 미병데이터final_선택지표4_체질값변경KS15")
mb_tmp=sqlQuery(conn,"select * from 미병데이터final_선택지표3")
head(mb_tmp)


mb_tmp
a<-xtabs(~성별+미병분류fi,data=mb_tmp)

str(mb_tmp)
colnames(mb_tmp)
str(mb_tmp1)
colnames(mb_tmp1)

a<-subset(mb_tmp,select=-미병분류fi)
colnames(a)
mb_tmp1<-upSample(a,mb_tmp$미병분류fi)

colnames(mb_tmp1)
colnames(mb_tmp1)[67]<-"미병분류fi"
summary(mb_tmp1)

write.csv(mb_tmp1,"c:/upsampleing.csv")


#전체
mb_tmp1<-mb_tmp[,c(1:21,63:66)]

#계측
subset()
mb_tmp1<-mb_tmp[,c(1:21,63:66)]

write.csv(filter(mb_tmp1, 미병분류fi == "건강군" | 미병분류fi == "미병2" ),"c:/계측만.csv")

#인바디
mb_tmp1<-mb_tmp[,c(22:38,63:66)]


#???
mb_tmp1<-mb_tmp[,c(41:47,63:66)]

#??동????량
mb_tmp1<-mb_tmp[,c(48:62,63:66)]


#???????량
mb_tmp1<-mb_tmp[,c(41:62,63:66)]


#메트리아
mb_tmp1<-mb_tmp[,c("avg_STEP_COUNT","avg_PHYSICAL_ACTIVITY","avg_SEDENTARY","avg_M_V","avg_DISTANCE","avg_METS","MI_day1","MI_day8" )]

mb_tmp1$MI_day1<-factor(mb_tmp1$MI_day1)
mb_tmp1$MI_day8<-factor(mb_tmp1$MI_day8)


summary(mb_tmp1)
names(mb_tmp1)
str(mb_tmp1)


mb_tmp1<-filter(mb_tmp1, 미병분류fi == "건강군" | 미병분류fi == "미병2" )

mb_tmp1$체질진단fi<-factor(mb_tmp1$체질진단fi, levels=1:3, labels=c("소음인","소양인","태양인"))

mb_tmp1$성별<-factor(mb_tmp1$성별)
mb_tmp1$미병분류fi<-factor(mb_tmp1$미병분류fi)



summary(mb_tmp1)
#mb_tmp1??기까?? ...


#
names(mb_tmp)
#??체??서 ??번 거르??? 


#계측??료???.미병분류??수, 체질진단 ??함

#미병 1 ??애???


mb_tmp2<-mb_tmp1


#mb_tmp2<-select(mb_tmp2, -??력징후,-체표??도,-??력,-??각,-체형측정)

#mb_tmp2<-centralImputation(mb_tmp2)
#traindata?? test ??이???? ??눈??
install.packages("caret")
require(caret)
names(mb_tmp2)
str(mb_tmp2)

mb_tmp2<-mb_tmp2[complete.cases(mb_tmp2),]

summary(mb_tmp2)
summary(data_체질3)

mb_tmp2<-select(mb_tmp2, -미병점수fi)


colnames(mb_tmp2)
colnames(mb_tmp2)[7]<-"미병분류fi"

summary(mb_tmp2)

data_all<-mb_tmp2
data_??별??-mb_tmp2[mb_tmp2$??별=="1",]
data_??별??-mb_tmp2[mb_tmp2$??별=="2",]

data_체질1<-mb_tmp2[mb_tmp2$체질진단fi=="??음??",]
data_체질2<-mb_tmp2[mb_tmp2$체질진단fi=="??음??",]
data_체질3<-mb_tmp2[mb_tmp2$체질진단fi=="??양??",]


colnames(data_all)
summary(data_all)
colnames(mb_tmp2)


require(party)
ctree
myformula<-미병분류fi~.
rlt<-mifunc(미병분류fi~.,data_all)
write.csv(rlt,"c:/rlt전체.csv")

rlt<-mifunc(미병분류fi~.,data_??별??
write.csv(rlt,"c:/rlt??별??csv")
rlt<-mifunc(미병분류fi~.,data_??별??
write.csv(rlt,"c:/rlt??별??csv")
rlt<-mifunc(미병분류fi~.,data_체질1)
write.csv(rlt,"c:/rlt체질1.csv")
rlt<-mifunc(미병분류fi~.,data_체질2)
write.csv(rlt,"c:/rlt체질2.csv")
rlt<-mifunc(미병분류fi~.,data_체질3)
write.csv(rlt,"c:/rlt체질3.csv")


######
ltmp<-data_all


inTrain=createDataPartition(ltmp$미병분류fi, p=0.7, list=FALSE)
traindata=ltmp[inTrain,]
testdata=ltmp[-inTrain,]


colnames(traindata)


##classification / decision tree -> random forest
rf<-randomForest(myformula,data=traindata,ntree=50,proximity=TRUE)

colnames(traindata)
cforest(myformula, data=traindata, controls=cforest_control(mtry=2, mincriterion=0))
#train??로 분류 ??인
rf.predicted_train <-predict(rf,newdata=traindata)
rf.trainingAccuracy<-sum(rf.predicted_train==traindata$미병분류fi)/NROW(traindata)

#test set??로 ??인.

rf.predicted_test_prob <-1-unlist(predict(rf,newdata=testdata,type="prob"),use.names=F)[seq(1,nrow(testdata))]


rf.predicted_test <-predict(rf, newdata=testdata, type="class")
rf.testAccuracy<-sum(rf.predicted_test==testdata$미병분류fi)/NROW(testdata)


confusionMatrix(rf.predicted_test,testdata$미병분류fi)


pred.rf<-prediction(rf.predicted_test_prob,testdata$미병분류fi)
#ROC를 그리기위해 performance 함수를사용
pred.rfpm<-performance(pred.rf,"tpr","fpr")
plot(pred.rfpm, type='s', ylab=pred.rfpm@y.name, xlab=pred.rfpm@x.name, col='grey50', lwd=1)



##
require(rpart)
rf<-rpart(myformula,data=traindata)
str(testdata)
set.seed(137)

probs<-runif(100)
labels<-as.factor(ifelse(probs<.5&runif(100)<.4,"A","B"))
install.packages("ROCR")
library(ROCR)

pred<-prediction(probs,labels)

plot(performance(pred,"tpr","fpr"))
############################################
##classification / decision tree -> ctree
############################################
ct<-ctree(myformula,data=traindata,controls = ctree_control(maxdepth = 10))

#train??로 분류 ??인
ct.predicted_train <-predict(ct,newdata=traindata)
ct.trainingAccuracy<-sum(ct.predicted_train==traindata$미병분류fi)/NROW(traindata)

#test set??로 ??인.
ct.predicted_test <-predict(ct,newdata=testdata)
ct.testAccuracy<-sum(ct.predicted_test==testdata$미병분류fi)/NROW(testdata)


######




# 중선??????분석
names(mb_tmp2)
mb_tmp2<-mb_tmp1
mb_tmp2<-select(mb_tmp2, -미병분류fi)
#계측??료 , 체질진단 ??함
mb_tmp2<-mb_tmp1[,c(1:42,231,297:327,329,330)]

#??바??만..미병분류fi () , 체질진단 ??함
mb_tmp2<-mb_tmp1[,c(231:271,297:327,329,330)]

#??바?? 계측??료
mb_tmp2<-mb_tmp1[,c(1:42,231:271,297:327,329,330)]


mb_tmp2$체질진단fi<-factor(mb_tmp2$체질진단fi)
names(mb_tmp2)

summary(mb_tmp2)
summary(mb_tmp2)


mb_tmp2<-mb_tmp2[complete.cases(mb_tmp2),]

data_all<-mb_tmp2
data_??별??-mb_tmp2[mb_tmp2$??별==1,]
data_??별??-mb_tmp2[mb_tmp2$??별==2,]

data_체질1<-mb_tmp2[mb_tmp2$체질진단fi=="??음??",]
data_체질2<-mb_tmp2[mb_tmp2$체질진단fi=="??음??",]
data_체질3<-mb_tmp2[mb_tmp2$체질진단fi=="??양??",]
data_체질3<-mb_tmp2[mb_tmp2$체질진단fi=="??양??" & mb_tmp2$??별==1,]
data_체질3<-mb_tmp2[mb_tmp2$체질진단fi=="??양??" & mb_tmp2$??별==2,]


summary(mb_tmp2)

colnames(mb_tmp2)
names(data_??별??
summary(data_체질3)

str(data_all[,-c(47,48,50)])

ml<-lm(미병??수fi~.,data=data_all[,-c(47,48,50)])
ml<-lm(미병??수fi~.,data=data_??별??,-c(47,48,50)])
ml<-lm(미병??수fi~.,data=data_??별??,-c(20,22)])
ml<-lm(미병??수fi~.,data=data_체질1[,-c(47,48,50)])
ml<-lm(미병??수fi~.,data=data_체질2[,-c(47,48,50)])
ml<-lm(미병??수fi~.,data=data_체질3[,-c(47,48,50)])
ml<-lm(미병??수fi~.,data=data_체질3[,-c(20,22)])

#ml<-lm(미병??수fi~.,data=select(data_체질3,-체질진단fi,-체표_??마1 ,-??축기혈??,-aa_근육_조절??,-??력징후,-체표??도,-??력,-??별,-(??각:BMI)))

summary(ml)

ml2<-step(ml,direction="both")

summary(ml2)


View(a)
as.data.frame(a)
attributes(ml2)

attributes(ml2$anova)

class(ml2$anova)

anova(ml2)
aov(ml2)

summary(ltmp)

mifunc<-function(myformula,ltmp){


pf<-data.frame(i=c(NA),randomForest=c(NA),cTree=c(NA),MNL=c(NA),SVM=c(NA),NN=(NA))

for(i in 1:100)
{
  
inTrain=createDataPartition(ltmp$미병분류fi, p=0.7, list=FALSE)
traindata=ltmp[inTrain,]
testdata=ltmp[-inTrain,]



##classification / decision tree -> random forest
rf<-randomForest(myformula,data=traindata,ntree=50,proximity=TRUE)

#train??로 분류 ??인
rf.predicted_train <-predict(rf,newdata=traindata)
rf.trainingAccuracy<-sum(rf.predicted_train==traindata$미병분류fi)/NROW(traindata)

#test set??로 ??인.
rf.predicted_test <-predict(rf,newdata=testdata)
rf.testAccuracy<-sum(rf.predicted_test==testdata$미병분류fi)/NROW(testdata)


############################################
##classification / decision tree -> ctree
############################################
ct<-ctree(myformula,data=traindata,controls = ctree_control(maxdepth = 10))


#train??로 분류 ??인
ct.predicted_train <-predict(ct,newdata=traindata)
ct.trainingAccuracy<-sum(ct.predicted_train==traindata$미병분류fi)/NROW(traindata)

#test set??로 ??인.
ct.predicted_test <-predict(ct,newdata=testdata)
ct.testAccuracy<-sum(ct.predicted_test==testdata$미병분류fi)/NROW(testdata)



############################################
##multinomial logistic regression
############################################
mnl<-multinom(myformula , data=traindata)

#train??로 분류 ??인
mnl.predicted_train <-predict(mnl,newdata=traindata)
mnl.trainingAccuracy<-sum(mnl.predicted_train==traindata$미병분류fi)/NROW(traindata)

#test set??로 ??인.
mnl.predicted_test <-predict(mnl,newdata=testdata)
mnl.testAccuracy<-sum(mnl.predicted_test==testdata$미병분류fi)/NROW(testdata)


############################################
##support vector machine
############################################

sm<-svm(myformula, data = traindata, cost = 100, gamma = 2)
sm<-svm(myformula, data = traindata, cost=4, gamma=0.0625, probability = TRUE)

#train??로 분류 ??인
sm.predicted_train <-predict(sm,newdata=traindata)
sm.trainingAccuracy<-sum(sm.predicted_train==traindata$미병분류fi)/NROW(traindata)

#test set??로 ??인.
sm.predicted_test <-predict(sm,newdata=testdata)
sm.testAccuracy<-sum(sm.predicted_test==testdata$미병분류fi)/NROW(testdata)

sm.predicted_test <-predict(sm,newdata=testdata, type="class")
sm.predicted_test_prob <-predict(sm,newdata=testdata,type="prob", probability = TRUE)

confusionMatrix(sm.predicted_test,testdata$미병분류fi)


# svm
x.svm.prob.rocr <- prediction(attr(sm.predicted_test_prob, "probabilities")[,2], testdata$미병분류fi)
x.svm.perf <- performance(x.svm.prob.rocr, "tpr","fpr")

performance(pred.rf,"auc")
performance(x.svm.prob.rocr,"auc")
performance(x.ann.prob.rocr,"auc")



plot(pred.rfpm, type='s', ylab=pred.rfpm@y.name, xlab=pred.rfpm@x.name, col='grey50', lwd=1)
plot(x.svm.perf, type='l',col=2, lwd=1, add=TRUE)
plot(x.ann.perf, col=3, add=TRUE)

legend('bottomright', c("CART", "SVM","ANN"), col=c('grey50',2,3), lwd=c(2,1), lty=1:2, cex=.9, bty='n')

############################################
##neural network
############################################


nn<-nnet(myformula, data=traindata, size=5)

#test set??로 ??인.
nn.predicted_test <-predict(nn,newdata=testdata,type="class")
nn.testAccuracy<-sum(nn.predicted_test==testdata$미병분류fi)/NROW(testdata)



pf<-rbind(pf,c(i,rf.testAccuracy,ct.testAccuracy,mnl.testAccuracy,sm.testAccuracy,nn.testAccuracy))

confusionMatrix(nn.predicted_test,testdata$미병분류fi)


ann.predicted_test_prob <-predict(nn,newdata=testdata,type="raw")

# svm
x.ann.prob.rocr <- prediction(ann.predicted_test_prob, testdata$미병분류fi)
x.ann.perf <- performance(x.ann.prob.rocr, "tpr","fpr")
plot(x.ann.perf, col=2, add=TRUE)


}

CART<-summary(pf$cTree,na.rm=TRUE)
randomForest<-summary(pf$randomForest,na.rm=TRUE)
MNL<-summary(pf$MNL,na.rm=TRUE)
SVM<-summary(pf$SVM,na.rm=TRUE)
NN<-summary(pf$NN,na.rm=TRUE)

rslt<-rbind(CART,randomForest,MNL,SVM,NN)

return(rslt)

}


#end of func











View(rslt)








# ctree???계산 ??는 ???CART)
# ctree??missing value??민감?? 그래??imputation ??업????행??고 진행??야??
names(traindata)
traindata<-mb_tmp1
mibyeong_ctree<-ctree(myformula,data=traindata,controls = ctree_control(maxdepth = 10))
attributes(mibyeong_ctree)
table(predict(mibyeong_ctree),traindata$미병fg)
x<-table(predict(mibyeong_ctree),traindata$미병fg)
sum(diag(x))/sum(x) # ??확????인
plot(mibyeong_ctree) # ??는 plot(chegil_ctree,type="simple")

#random forest???.
install.packages("randomForest")
install.packages("DMwR")
library("randomForest")
library(DMwR)


names(mb_tmp1)
traindata<-mb_tmp1
traindata<-centralImputation(traindata)
traindata<-traindata[,c(1:336)]
rf<-randomForest(myformula,data=traindata,ntree=100,proximity=TRUE)
names(traindata)

rf
plot(rf)
importance(rf)
varImpPlot(rf)
names(mb_tmp)



##rpart ??용.

install.packages("rpart")
install.packages("caret")

library("rpart")
library("caret")
m<-rpart(myformula,data=traindata)
attributes(m) #결과????성 보기
m$variable.importance

class(vm)
attributes(vm)
vm2<-data.frame(vm)
sqldf("select * from vm2")
vm<-varImp(m) #변??중요??????
plot(m,compress=TRUE,margin=.2)
text(m,cex=1.5)
attributes(m)
m$method


install.packages("rpart.plot")
library(rpart.plot)

prp(m,type=4,extra=6,digits=2)
summary(m)


#train set???test set??로 ??눠????기

ind <- sample(2,nrow(mb_tmp1),replace=T,prob=c(0.7,0.3)) 
traindata <- mb_tmp1[ind==1,]
testdata <- mb_tmp1[ind==2,]
myformula<-미병fg~.  
names(traindata)
mb_ctree <- ctree(myformula,data=traindata)
table(predict(mb_ctree),traindata$미병fg)
plot(mb_ctree)
plot(mb_ctree,type="simple")
testpred <- predict(mb_ctree, newdata=testdata)
table(testpred,testdata$미병fg)
 




#미병??수 ??측 multiple linear regression
#미병그룹??거
#계측??보???
names(mb_tmp1)
#계측??보?? combined value???
mb_tmp2<-mb_tmp1[,c(2:32,352:382,384,385)]
#계측??보???
mb_tmp2<-mb_tmp1[,c(2:32,384,385)]
#계측??보???combined ?????
mb_tmp2<-mb_tmp1[,c(352:382,384,385)]

#계측??보,combined 기기??료 hz ??함
mb_tmp2<-mb_tmp1[,c(2:6,8:42,233:235,271:327,329,330)]


#계측??보,combined 기기??료 (미병??수,미병분류,??문가체질,체질진단ks15 ??어???)
mb_tmp2<-mb_tmp1[,c(2:6,8:42,231,233:235,297:327,328,329,330,331)]


names(mb_tmp2)
library(DMwR)
mb_tmp2<-centralImputation(mb_tmp2)

#ind <- sample(2,nrow(mb_tmp2),replace=T,prob=c(0.8,0.2)) 
#traindata <- mb_tmp2[ind==1,]
#testdata <- mb_tmp2[ind==2,]

head(mb_tmp2)
traindata<-mb_tmp2

summary(mb_tmp2)

xtabs(~mb_tmp2$체질진단KS15)
xtabs(~mb_tmp2[mb_tmp2$체질진단KS15)

traindata<-mb_tmp2[mb_tmp2$체질진단fi=='1' ,]
traindata<-mb_tmp2[mb_tmp2$체질진단fi=='2' ,]
traindata<-mb_tmp2[mb_tmp2$체질진단fi=='3' ,]

traindata<-mb_tmp2[mb_tmp2$체질진단KS15=='1' ,]
traindata<-mb_tmp2[mb_tmp2$체질진단KS15=='2' ,]
traindata<-mb_tmp2[mb_tmp2$체질진단KS15=='3' ,]

summary(mb_tmp2)

xtabs(~mb_tmp2$??별+mb_tmp2$체질진단fi)
xtabs(~mb_tmp2$??별)

#결측?????인
mb_tmp2[is.na(mb_tmp2$체질진단fi),]
#결측?????인
mb_tmp2[!complete.cases(mb_tmp2),]

mb_tmp2$미병분류fi<-factor(mb_tmp2$미병분류fi)
mb_tmp2$체질진단fi<-factor(mb_tmp2$체질진단fi)
mb_tmp2$체질진단KS15<-factor(mb_tmp2$체질진단KS15)

traindata<-subset(mb_tmp2,??별=='1' & 미병분류fi!='미병1')
traindata<-subset(mb_tmp2,??별=='2' & 미병분류fi!='미병1')




traindata<-subset(mb_tmp2,??별=='1' & 체질진단fi=='1')
traindata<-subset(mb_tmp2,??별=='1' & 체질진단fi=='2')
traindata<-subset(mb_tmp2,??별=='1' & 체질진단fi=='3')
traindata<-subset(mb_tmp2,??별=='2' & 체질진단fi=='1')
traindata<-subset(mb_tmp2,??별=='2' & 체질진단fi=='2')
traindata<-subset(mb_tmp2,??별=='2' & 체질진단fi=='3')

traindata<-subset(mb_tmp2,??별=='1' & 체질진단KS15=='1')
traindata<-subset(mb_tmp2,??별=='1' & 체질진단KS15=='2')
traindata<-subset(mb_tmp2,??별=='1' & 체질진단KS15=='3')
traindata<-subset(mb_tmp2,??별=='2' & 체질진단KS15=='1')
traindata<-subset(mb_tmp2,??별=='2' & 체질진단KS15=='2')
traindata<-subset(mb_tmp2,??별=='2' & 체질진단KS15=='3')





names(traindata)

#--multiple linear regression ????릴?? 체질진단, 미병군을 빼????
traindata<-traindata[,-c(41,76,78,79)]
                     
summary(traindata)
NROW(traindata)

traindata<-centralImputation(traindata)

#traindata<-mb_tmp1
#210개??????후로는 ??러가 ??네? ??인 ??요.
#traindata2<-traindata[,c(1:303,383)]

traindata <-rbind(traindata,traindata)
ml<-lm(미병??수fi~.,data=traindata)
summary(ml)

at

ml2<-step(ml,direction="both")
summary(ml2)

attributes(ml2)

attributes(ml2$anova)

class(ml2$anova)

anova(ml2)
plot(ml2)

fitted(ml2)[1:10]
residuals(ml2)[1:10]
fitted(ml2)[1:10]+residuals(ml2)[1:10]
traindata$미병??수fi[1:10]


anova(ml2)
traindata[1:4,c("미병??수")]

summary(ml)

#multinomial logistic regression ????한 ???

names(mb_tmp1)
NROW(mb_tmp1)

#미병분류fi ??함.
mb_tmp2<-mb_tmp1[,c(2:6,8:42,233:235,271:327,328,329,330,331)]
names(mb_tmp2)

traindata<-mb_tmp2

#traindata<-mb_tmp2[mb_tmp2$미병분류fi=='건강??? | mb_tmp2$미병분류fi=='미병2',]
head(traindata)
summary(traindata)
str(traindata$미병분류fi)
traindata$미병분류fi<-factor(traindata$미병분류fi)

mb_tmp3<-mb_tmp2[mb_tmp2$체질진단fi=='1' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단fi=='2' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단fi=='3' ,]

mb_tmp3<-mb_tmp2[mb_tmp2$체질진단KS15=='1' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단KS15=='2' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단KS15=='3' ,]
mb_tmp3<-mb_tmp2


mb_tmp3<-subset(mb_tmp2,미병??수fi==0 |  미병분류fi=='미병2')
xtabs(~mb_tmp3$미병분류fi)
mb_tmp3<-mb_tmp3[mb_tmp3$체질진단fi=='1' ,]
xtabs(~mb_tmp3$미병분류fi)
mb_tmp3<-mb_tmp3[mb_tmp3$체질진단fi=='2' ,]
mb_tmp3<-mb_tmp3[mb_tmp3$체질진단fi=='3' ,]

summary(mb_tmp3)
names(mb_tmp3)
mb_tmp3<-mb_tmp3[,-c(102:105)]

head(mb_tmp3)
#m<-glm(미병분류fi~. , data=traindata, family="binomial")
library(nnet)

pf<-data.frame()
for(i in 1:100)
{

  mb_tmp3$미병분류fi<-factor(mb_tmp3$미병분류fi)
  ind <- sample(2,nrow(mb_tmp3),replace=T,prob=c(0.7,0.3)) 
  traindata <- mb_tmp3[ind==1,]
  testdata <- mb_tmp3[ind==2,]
  traindata<-centralImputation(traindata)
  testdata<-centralImputation(testdata)
  
  m<-multinom(미병분류fi~. , data=traindata)
  #summary(m)
  
  #train??로 분류 ??인
  predicted_train <-predict(m,newdata=traindata)
  trainingAccuracy<-sum(predicted_train==traindata$미병분류fi)/NROW(traindata)
  
  #test data set??로 ??인
  predicted_test <-predict(m,newdata=testdata)
  testAccuracy<-sum(predicted_test==testdata$미병분류fi)/NROW(testdata)
  pf<-rbind(pf,c(i,trainingAccuracy,testAccuracy))

}

pf
summary(pf)

xtabs(~ predicted_train+traindata$미병분류fi)
xtabs(~ predicted_test+testdata$미병분류fi)
#install.packages("tigerstats")
#require(tigerstats)

#rowPerc(a)
#colPerc(a)

xtabs(~seat+extra_life,data=m111survey)
ftable(m111survey)
head(m111survey)

#체질별로 BMI 분포 ??인

names(traindata)
traindata$체질진단fi<-factor(traindata$체질진단fi)



boxplot(y ~ x, col = "lightgray")
boxplot(traindata$aa__BMI~traindata$체질진단fi, col = "lightgray")
text(1:5, rep(min(traindata$aa__BMI), 5), paste("n=", tapply(traindata$aa__BMI,traindata$체질진단fi, length)))

boxplot(traindata$체질진단fi~traindata$aa__BMI, col = "lightgray")

boxplot(traindata$aa__BMI,traindata$체질진단fi)
str(mb_tmp2$aa__BMI)

xtabs(~mb_tmp2$체질진단fi+mb_tmp2$미병분류fi)
str(mb_tmp2$체질진단fi)
xtabs(~mb_tmp2$체질진단fi)

names(mb_tmp)
tmp<-mb_tmp[mb_tmp$Visit=='D1',c("aa__BMI","체질진단fi")]


aov(tmp$aa__BMI~factor(tmp$체질진단fi))
summary(aov(traindata$aa__BMI~factor(traindata$체질진단fi)))

TukeyHSD(aov(traindata$aa__BMI~factor(traindata$체질진단fi)))


;require(sqldf)
sqldf("select avg(aa__BMI), 체질진단fi from traindata group by 체질진단fi ")
sqldf("select avg(aa_BMI) from mb_tmp2 ")

#??경망으???미병?????측
library(nnet)

#계측??보,combined 기기??료 (미병??수,미병분류,??문가체질,체질진단ks15 ??어???)
mb_tmp2<-mb_tmp1[,c(2:6,8:42,231,233:235,297:327,328,329,330,331)]

names(mb_tmp2)
library(DMwR)
mb_tmp2<-centralImputation(mb_tmp2)

mb_tmp3<-mb_tmp2[mb_tmp2$체질진단fi=='1' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단fi=='2' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단fi=='3' ,]

mb_tmp3<-mb_tmp2[mb_tmp2$체질진단KS15=='1' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단KS15=='2' ,]
mb_tmp3<-mb_tmp2[mb_tmp2$체질진단KS15=='3' ,]


mb_tmp3<-subset(mb_tmp2,미병??수fi==0 |  미병분류fi=='미병2')

mb_tmp3<-mb_tmp2

install.packages("svm")
install.packages("ksvm")
install.packages("e1071")
require(ksvm)
require(e1071)

sm<-svm(미병분류fi~., data = traindata, cost = 50, gamma = 1)

predicted_train<-predict(sm, data =traindata)
trainingAccuracy<-sum(predicted_train==traindata$미병분류fi)/NROW(traindata)


attributes(sm)
summary(sm)
table(pred = predicted_train, true = traindata)

testAccuracy<-sum(sm$fitted==traindata$미병분류fi)/NROW(traindata)

sm$fitted
traindata<-traindata[,-c(12:37)]
str(traindata)
names(traindata)

mb_tmp3<-mb_tmp3[,-c(77,78,79)]
names(mb_tmp3)

kk<-tune(svm,미병분류fi~.,data=traindata,gamma=2^(-1:1),cost=2^(2:4))

attributes(kk)
summary(kk)
kk$performances
kk$best.performance
kk$best.model
pf<-data.frame()

for(i in 1:100)
{
mb_tmp3$미병분류fi<-factor(mb_tmp3$미병분류fi)
ind <- sample(2,nrow(mb_tmp3),replace=T,prob=c(0.7,0.3)) 
traindata <- mb_tmp3[ind==1,]
testdata <- mb_tmp3[ind==2,]
traindata<-centralImputation(traindata)
testdata<-centralImputation(testdata)

m<-nnet(미병분류fi~., data=traindata, size=10)
m<-ksvm(미병분류fi~.,data=traindata)
#summary(m)

#train??로 분류 ??인
predicted_train <-predict(m,newdata=traindata,type="class")
trainingAccuracy<-sum(predicted_train==traindata$미병분류fi)/NROW(traindata)
#confusionMatrix(predict(m,newdata=traindata,type="class"),traindata$미병분류fi)
#confusionMatrix(predict(m,newdata=testdata,type="class"),testdata$미병분류fi)

#train??로 분류 ??인

#test data set??로 ??인
predicted_test <-predict(m,newdata=testdata,type="class")
testAccuracy<-sum(predicted_test==testdata$미병분류fi)/NROW(testdata)
pf<-rbind(pf,c(i,trainingAccuracy,testAccuracy))
}
summary(pf)

#test data set??로 ??인
predicted_test <-predict(m,newdata=testdata,type="class")
testAccuracy<-sum(predicted_test==testdata$미병분류fi)/NROW(testdata)



confusionMatrix(predict(m,newdata=testdata,type="class"),testdata$미병분류fi)


xtabs(~ predicted_train+traindata$미병분류fi)
xtabs(~ testdata$미병분류fi+predicted_test)

xtabs(~ testdata$미병분류fi+predicted_test)

pf<-rbind(pf,c(i,trainingAccuracy,testAccuracy))



m<-nnet(미병분류fi~.,data=traindata,size=3)



m
summary(m)
mf<-predict(m,newdata=iris)
mf<-predict(m,newdata=iris,type="class")
str(iris)

xtabs(~mf+iris$iris)
sum(mf==iris$Species)/NROW(iris)

library(caret)
library(e1071)
install.packages("e1071")
confusionMatrix(predict(m,newdata=iris,type="class"),iris$Species)

summary(iris)
summary(Species~.,data=iris)
library(Hmisc)
install.packages("Hmisc")
