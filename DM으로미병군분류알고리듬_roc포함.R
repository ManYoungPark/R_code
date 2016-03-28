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
library(rpart)

library(nnet)
library(e1071)


conn<-odbcConnect('mibyeong',uid='sa',pwd='leo0515')

conn<-odbcConnect('mibyeong_gacheon',uid='sa',pwd='leo0515')

Sys.getlocale()
Sys.setlocale("LC_COLLATE","en_US.UTF-8")

mb_tmp=sqlQuery(conn,"select * from 미병데이터final_선택지표3")


mb_tmp1<-filter(mb_tmp, 미병분류fi == "건강군" | 미병분류fi == "미병2" )
mb_tmp1<-mb_tmp1[complete.cases(mb_tmp1),]

summary(mb_tmp1)

mb_tmp1$성별<-factor(mb_tmp1$성별)
mb_tmp1$미병분류fi<-factor(mb_tmp1$미병분류fi)
mb_tmp1$체질진단fi<-factor(mb_tmp1$체질진단fi, levels=1:3, labels=c("소음인","소양인","태양인"))

mb_tmp1[mb_tmp1!=mb_tmp1]


summary(mb_tmp1)
colnames(mb_tmp1)

sqlSave(conn,mb_tmp1,"upsampling_before")

#a<-xtabs(~성별+미병분류fi,data=mb_tmp)
set.seed(1535)


mb_tmp2up<-upSample(subset(mb_tmp1,select=-미병분류fi),mb_tmp1$미병분류fi)

mb_tmp2down_tmp<-mb_tmp2down
colnames(mb_tmp2down_tmp)
colnames(mb_tmp1)

# 여러번 돌릴때 fuction으로 만들어 놓고 돌리면 됨.
#downsampling 할때 최고의 성능을 보이는 sampling 데이터 찾을때 수행. 

# identical(mb_tmp2down,mb_tmp2down_tmp)
# 
# #데이터 파일을 저장 
# 
#컬럼명을 넣어준다.
# colnames(mb_tmp2up)[75]<-"미병분류fi"
# 
# #write.csv(mb_tmp0,"c:/upsampleing.csv")
# sqlSave(conn,mb_tmp2up,"upsampling_after")
#sqlSave(conn,mb_tmp2down,"downsampling_after2")
# 
#save(mb_tmp2up,mb_tmp2down,datasetMax,datasetMax2,file="mibyeongUpDownsampling.RData")
# 
# names(mb_tmp2down)
# 
# mb_tmp2up1<-select(mb_tmp2up,-c(Apen:esi),-c(체질진단fi,미병점수fi,대상자_등록번호))
# mb_tmp0<-mb_tmp2up1

mb_tmp2down1<-select(mb_tmp2down,-c(Apen:esi),-c(체질진단fi,미병점수fi,대상자_등록번호))
mb_tmp0<-mb_tmp2down1

#이건 임시... 위에서 select를 안했을때
#mb_tmp0<-mb_tmp2down




# 
# ##분산이 0에 가까운 컬럼들 확인
 nearZeroVar(mb_tmp0, saveMetrics = TRUE)
# install.packages("mlbench")
# library(mlbench)
# library(FSelector)
# 
# 
 v<-linear.correlation(수축기혈압~. , data=subset(mb_tmp0,select = -c(미병분류fi,성별)))
 a<-cutoff.k(v,10)
 a<-findCorrelation(cor(subset(mb_tmp0,select = -c(미병분류fi,성별))))


# subset(mb_tmp0,select = -c(미병분류fi,성별))
# mb_tmp1<-subset(mb_tmp0,select=-c(이완기혈압,  곡골체중비,  a0_체중_,    이마체중비  ,a_단백질,    aa_체세포량, a_세포내액,  aa_골격근량, a_근육량,    a_체수분))
# 
# names(mb_tmp0)
# names(mb_tmp1)
# 
# length(a)
# 
# cor(mb_tmp0[,a])

#전체
mb_tmp1_1<-mb_tmp0
# 
# #계측
# mb_tmp1_1<-data.frame()
# mb_tmp1_1<-select(mb_tmp0,c(수축기혈압:통각_어깨_좌),c(성별:미병분류fi))
# mb_tmp1_1<-mb_tmp0[,c(1:29,62,63)]
# 
# 
# #인바디
# mb_tmp1_1<-select(mb_tmp0,c(a_세포내액:aa_전체_부종_ECF__TBF_),c(성별:미병분류fi))
# mb_tmp1_1<-mb_tmp0[,c(30:46,62,63)]
# 
# #운동부하대사량
# mb_tmp1_1<-select(mb_tmp0,c(VO2_Peak:VE_VCO2비),c(체질진단fi:미병분류fi),c(성별))
# mb_tmp1_1<-mb_tmp0[,c(47:61,62,63)]
# 
# colnames(mb_tmp1)
# colnames(mb_tmp0)
# 
# 
# 
# require(caret)
# require(rpart)
# #mb_tmp3<-select(mb_tmp2, -미병점수fi)
# 


ltmp<-mb_tmp1_1

mb1<-downsamplingdataset
mb1<-upsamplingdataset
names(mb1)



#전체
mb1_1<-mb1[,-c(62)]

#계측
mb1_1<-mb1[,c(1:29,63)]

#인바디
mb1_1<-mb1[,c(30:46,63)]

#운동부하대사량
mb1_1<-mb1[,c(47:61,63)]



ltmp.scale<-cbind(scale(mb1_1[-length(mb1_1)]),mb1_1[length(mb1_1)])  

summary(ltmp.scale)



##변수 중요도 

inTrain=createDataPartition(ltmp.scale$미병분류fi, p=0.7, list=FALSE)
traindata=ltmp.scale[inTrain,]
testdata=ltmp.scale[-inTrain,]


m<-randomForest(미병분류fi~., data=traindata, importance = TRUE)



class(importance(m))
imp<-as.data.frame(importance(m))
imp[order(imp$MeanDecreaseGini, decreasing=T),]


varImpPlot(m,main="Impotance variables of Mibyeong ")
## end  of 변수 중요도



#summary(ltmp.scale)                
 
#inTrain=createDataPartition(ltmp.scale$미병분류fi, p=0.7, list=FALSE)

rsltall<-list()
rsltDT<-list()
rsltSVM<-list()
rsltNN<-list()


rslttotal <- data.frame(names= character(), times=numeric(), 
                       Sensitivityss= numeric(),Specificity=numeric(),PPV= numeric(), NPV= numeric(),accurary=numeric(),stringsAsFactors=FALSE)






for(i in 1:1000)
{
  

#데이터셋 분류 
#inTrain=createDataPartition(ltmp$미병분류fi, p=0.7, list=FALSE)
  
inTrain=createDataPartition(ltmp.scale$미병분류fi, p=0.7, list=FALSE)
traindata=ltmp.scale[inTrain,]
testdata=ltmp.scale[-inTrain,]

myformula<-미병분류fi~.

#############################################
#DM 수행
##########################################

##classification / decision tree -> random forest
#rf<-randomForest(myformula,data=traindata,ntree=50,proximity=TRUE)

rf<-rpart(myformula,data=traindata)

#test set으로 성능검증.

#테스트 데이터를 class로 예측. 분류 정확도를 측정하기 위해
rf.predicted_test <-predict(rf, newdata=testdata, type="class")

#테스트 데이터를 확률로 예측. ROC커브를 그리기위해
rf.predicted_test_prob <-1-unlist(predict(rf,newdata=testdata,type="prob"),use.names=F)[seq(1,nrow(testdata))]

#정확도 계산을 아래와 같이 할수도 있다. 참고사항.
#rf.testAccuracy<-sum(rf.predicted_test==testdata$미병분류fi)/NROW(testdata)

#분류 결과를 confusionMatrix로 표현.
(rltCM_DT<-confusionMatrix(rf.predicted_test,testdata$미병분류fi))

#rltCM$byClass[1][1]

#ROC커브 그리기위해...
pred.rf<-prediction(rf.predicted_test_prob,testdata$미병분류fi)
#ROC를 그리기위해 performance 함수를사용
pred.rfpm<-performance(pred.rf,"tpr","fpr")
#plot(pred.rfpm, type='s', ylab=pred.rfpm@y.name, xlab=pred.rfpm@x.name, col='grey50', lwd=1)

rsltDT[[i]]<-list(dt=rf,predicted_test_prob=rf.predicted_test_prob,confusionMatrix=rltCM_DT,ROC_pred=pred.rf,ROC_PERFORMACE=pred.rfpm)

values<-rltCM_DT$byClass
rslttotal<-rbind(rslttotal,data.frame(names="CART" , times=i, 
                                     Sensitivityss= values["Sensitivity"],Specificity=values["Specificity"],PPV= values["Pos Pred Value"],
                                     NPV= values["Neg Pred Value"],accurary= values["Balanced Accuracy"],row.names=NULL))


######
#svm
#####


#roc커브를 그리기 위해 probability =TRUE로 해줘야함.
#sm<-svm(myformula, data = traindata, cost = 100, gamma = 2)

#cost와 gamma값에 따라 다름.
sm<-svm(myformula, data = traindata, cost=4, gamma=0.0625, probability = TRUE)
#sm<-svm(myformula, data = traindata, cost=100, gamma=2, probability = TRUE)


sm.predicted_test <-predict(sm,newdata=testdata, type="class")
sm.predicted_test_prob <-predict(sm,newdata=testdata,type="prob", probability = TRUE)

(rstCM_svm<-confusionMatrix(sm.predicted_test,testdata$미병분류fi))

# svm
x.svm.prob.rocr <- prediction(attr(sm.predicted_test_prob, "probabilities")[,2], testdata$미병분류fi)
x.svm.perf <- performance(x.svm.prob.rocr, "tpr","fpr")

rsltSVM[[i]]<-list(sm=sm,predicted_test_prob=sm.predicted_test_prob,confusionMatrix=rstCM_svm,ROC_pred=x.svm.prob.rocr,ROC_PERFORMACE=x.svm.perf)

#confusionMatrix 중 결과 vector를 value 변수에 저장
values<-rstCM_svm$byClass
rslttotal<-rbind(rslttotal,data.frame(names="SVM" , times=i, 
                                      Sensitivityss= values["Sensitivity"],Specificity=values["Specificity"],PPV= values["Pos Pred Value"],
                                      NPV= values["Neg Pred Value"],accurary= values["Balanced Accuracy"],row.names=NULL))


#dt.auc<-performance(pred.rf,"auc")
#svm.auc<-performance(x.svm.prob.rocr,"auc")
#ann.auc<-performance(x.ann.prob.rocr,"auc")

#dt.auc@y.values[[1]]
#svm.auc@y.values[[1]]
#ann.auc@y.values[[1]]

#auc.all<-c(dt.auc@y.values[[1]],svm.auc@y.values[[1]],ann.auc@y.values[[1]])
#auc.all<-c("88","55","52")
###
#ann
###

#데이터셋 분류 
# names(ltmp[-c(62,63)])
# ltmp.scale<-cbind(ltmp[63],scale(ltmp[-c(62,63)]))                  
# summary(ltmp.scale)                
# 
# inTrain=createDataPartition(ltmp.scale$미병분류fi, p=0.7, list=FALSE)
# traindata=ltmp.scale[inTrain,]
# testdata=ltmp.scale[-inTrain,]
# 
# summary(traindata)

nn<-nnet(myformula, data=traindata, size = 10, rang = 0.1,
         decay = 5e-4, maxit = 200)


nn.predicted_test <-predict(nn,newdata=testdata,type="class")
(rsltCM_ANN<-confusionMatrix(nn.predicted_test,testdata$미병분류fi))

ann.predicted_test_prob <-predict(nn,newdata=testdata,type="raw")
x.ann.prob.rocr <- prediction(ann.predicted_test_prob, testdata$미병분류fi)

x.ann.perf <- performance(x.ann.prob.rocr, "tpr","fpr")
rsltNN[[i]]<-list(sm=sm,predicted_test_prob=ann.predicted_test_prob,confusionMatrix=rsltCM_ANN,ROC_pred=x.ann.prob.rocr,ROC_PERFORMACE=x.ann.perf)

values<-rsltCM_ANN$byClass
rslttotal<-rbind(rslttotal,data.frame(names="ANN" , times=i, 
                                      Sensitivityss= values["Sensitivity"],Specificity=values["Specificity"],PPV= values["Pos Pred Value"],
                                      NPV= values["Neg Pred Value"],accurary= values["Balanced Accuracy"],row.names=NULL))

}



#데이터 확인
rslttotal

(tp2<-mean(filter(rslttotal,names=="SVM")$accurary))
#summary(filter(rslttotal,names=="ANN"))
#summary(filter(rslttotal,names=="CART"))

summary(filter(rslttotal,names=="SVM"))
summary(filter(rslttotal,names=="ANN"))
summary(filter(rslttotal,names=="CART"))
#데이터 확인 끝.



#전체 데이터로 upsampling 수행시
rslttotal_up_all<-rslttotal
#rslt_Source_up_all<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #전체 데이터를 이용한각각 모델의 데이터 및 결과들이 저장됨


rslttotal_up_gye<-rslttotal
#rslt_Source_up_gye<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #계측 자료를 이용한각각 모델의 데이터 및 결과들이 저장됨


rslttotal_up_inbody<-rslttotal
#rslt_Source_up_indoby<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #계측 자료를 이용한각각 모델의 데이터 및 결과들이 저장됨


rslttotal_up_exerciseStress<-rslttotal
#rslt_Source_up_exerciseStress<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #계측 자료를 이용한각각 모델의 데이터 및 결과들이 저장됨



#전체 데이터로 downsampling 수행시
rslttotal_down_all<-rslttotal
#rslt_Source_down_all<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #전체 데이터를 이용한각각 모델의 데이터 및 결과들이 저장됨

rslttotal_down_gye<-rslttotal
#rslt_Source_down_gye<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #계측 자료를 이용한각각 모델의 데이터 및 결과들이 저장됨


rslttotal_down_inbody<-rslttotal
#rslt_Source_down_indoby<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #계측 자료를 이용한각각 모델의 데이터 및 결과들이 저장됨


rslttotal_down_exerciseStress<-rslttotal
#rslt_Source_down_exerciseStress<-list(DT=rsltDT,SVM=rsltSVM,NN=rsltNN) #계측 자료를 이용한각각 모델의 데이터 및 결과들이 저장됨


rslttotal_up_all$flag<-"All variables"
rslttotal_up_gye$flag<-"Measurement info."
rslttotal_up_inbody$flag<-"Body composition analysis"
rslttotal_up_exerciseStress$flag<-"Body stress test"

rslttotal_down_all$flag<-"All variables"
rslttotal_down_gye$flag<-"Measurement info."
rslttotal_down_inbody$flag<-"Body composition analysis"
rslttotal_down_exerciseStress$flag<-"Body stress test"


upsamplingAllRslt<-rbind(rslttotal_up_all,rslttotal_up_gye,rslttotal_up_inbody,rslttotal_up_exerciseStress)
downsamplingAllRslt<-rbind(rslttotal_down_all,rslttotal_down_gye,rslttotal_down_inbody,rslttotal_down_exerciseStress)
upsamplingAllRslt$flag2<-"upsampling"
downsamplingAllRslt$flag2<-"downsampling"

updownIncluded_AllRslt<-rbind(upsamplingAllRslt,downsamplingAllRslt)


write.csv(upsamplingAllRslt,"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/논문작성/논문데이터/upsamplingAllRslt.csv")
write.csv(downsamplingAllRslt,"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/논문작성/논문데이터/downsamplingAllRslt.csv")
write.csv(updownIncluded_AllRslt,"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/논문작성/논문데이터/updownIncluded_AllRslt.csv")


summary(updownIncluded_AllRslt)

ggplot(data=updownIncluded_AllRslt,aes(x=factor(names),y=accurary,fill=names))+ 
  geom_boxplot()+ 
  facet_grid(flag2 ~ flag)



ggplot(data=upsamplingAllRslt,aes(x=names,y=accurary,fill=names))+ 
  geom_boxplot()+ 
  facet_grid(flag ~ .)+ 
  facet_wrap(~flag)+ 
  theme_minimal()

ggplot(data=upsamplingAllRslt,aes(x=factor(names),y=accurary,fill=names))+ 
  geom_boxplot()+ 
  scale_fill_brewer(palette='PuBu')+ 
  facet_grid(. ~ flag)



data.frame(tmpp1,flag="all information")
tmpp1$flag<-"all information"






rslttotal_down_apen_notinclued<-rslttotal


summary(filter(rslttotal,names=="SVM"))
summary(filter(rslttotal,names=="ANN"))
summary(filter(rslttotal,names=="CART"))

t.test(filter(rslttotal,names=="SVM")$accurary)

par(mfrow=c(1,1))

library(psych)
describeBy(rslttotal[c("accurary")],rslttotal$names)

install.packages("doBy")
library(doBy)
summaryBy(accurary+PPV~names, data=rslttotal, FUN=c(mean, sd,min,max))


str(rslttotal)
plot(density(filter(rslttotal,names=="SVM")$accurary), main="SVM vs CART vs ANN",xlim=c(0.3,1.0))
lines(density(filter(rslttotal,names=="CART")$accurary),col="red",lty="dashed")
lines(density(filter(rslttotal,names=="ANN")$accurary),col="blue",lty="dashed")






## 변수 중요도 평가..

library(randomForest)

summary(traindata)

m<-randomForest(미병분류fi~., data=traindata, importance = TRUE)

class(importance(m))
rndslt<-as.data.frame(importance(m))

varImpPlot(m,main="varImpotance of Mibyeong")

##밀도 그림


rslttotal_up_all
rslttotal_up_gye
rslttotal_up_inbody
rslttotal_up_exerciseStress


rslttotal_down_all
rslttotal_down_gye
rslttotal_down_inbody
rslttotal_down_exerciseStress


rslttotalup1<-rslttotal_up_all
rslttotalup2<-rslttotal_up_gye
rslttotalup3<-rslttotal_up_inbody
rslttotal<-rslttotal_up_exerciseStress

rslttotal<-rslttotal_down_all
rslttotal<-rslttotal_down_gye
rslttotal<-rslttotal_down_inbody
rslttotal<-rslttotal_down_exerciseStress


# create value labels 
# plot densities 

library(sm)
par(mfrow=c(2,4))

sm.density.compare(rslttotal_up_all$accurary, rslttotal_up_all$names, xlab="Accuracy",)
title(main=" all variables after upsampling")
colfill<-c(2:(2+length(levels(rslttotal_up_all$names)))) 
legend('topright', levels(rslttotal_up_all$names), fill=colfill)



sm.density.compare(rslttotal_up_gye$accurary, rslttotal_up_gye$names, xlab="Accuracy",)
title(main=" measurement info. after upsampling")
colfill<-c(2:(2+length(levels(rslttotal_up_gye$names)))) 
legend('topright', levels(rslttotal_up_gye$names), fill=colfill)



sm.density.compare(rslttotal_up_inbody$accurary, rslttotal_up_inbody$names, xlab="Accuracy",)
title(main=" body composition analysis after upsampling")
colfill<-c(2:(2+length(levels(rslttotal_up_inbody$names)))) 
legend('topright', levels(rslttotal_up_inbody$names), fill=colfill)


sm.density.compare(rslttotal_up_exerciseStress$accurary, rslttotal_up_exerciseStress$names, xlab="Accuracy",)
title(main=" body stress test after upsampling")
colfill<-c(2:(2+length(levels(rslttotal_up_exerciseStress$names)))) 
legend('topright', levels(rslttotal_up_exerciseStress$names), fill=colfill)


##downsampling
# plot densities 
sm.density.compare(rslttotal_down_all$accurary, rslttotal_down_all$names, xlab="Accuracy",)
title(main=" all variables after downsampling")
colfill<-c(2:(2+length(levels(rslttotal_down_all$names)))) 
legend('topright', levels(rslttotal_down_all$names), fill=colfill)

sm.density.compare(rslttotal_down_gye$accurary, rslttotal_down_gye$names, xlab="Accuracy",)
title(main=" measurement info. after downsampling")
colfill<-c(2:(2+length(levels(rslttotal_down_gye$names)))) 
legend('topright', levels(rslttotal_down_gye$names), fill=colfill)

sm.density.compare(rslttotal_down_inbody$accurary, rslttotal_down_inbody$names, xlab="Accuracy",)
title(main=" body composition analysis after downsampling")
colfill<-c(2:(2+length(levels(rslttotal_down_inbody$names)))) 
legend('topright', levels(rslttotal_down_inbody$names), fill=colfill)







sm.density.compare(rslttotal_down_exerciseStress$accurary, rslttotal_down_exerciseStress$names, xlab="Accuracy",)
title(main=" body stress test after downsampling")
colfill<-c(2:(2+length(levels(rslttotal_down_exerciseStress$names)))) 
legend('topright', levels(rslttotal_down_exerciseStress$names), fill=colfill)


# add legend via mouse click
colfill<-c(2:(2+length(levels(rslttotal$names)))) 
legend('topright', levels(rslttotal$names), fill=colfill)



#그림그리기
plot(pred.rfpm, type='s', ylab=pred.rfpm@y.name, xlab=pred.rfpm@x.name, col='grey50', lwd=1)
plot(x.svm.perf, type='l',col=2, lwd=1, add=TRUE)
plot(x.ann.perf, col=3, add=TRUE)

legend('bottomright', c("CART (AUC=0.69) ", "SVM (AUC=0.76)","ANN  (AUC=0.52)"), col=c('grey50',2,3), lwd=c(1,1), lty=1:1, cex=.9, bty='n')
legend('bottom', auc.all, col=c('grey50',2,3), lwd=c(2,1), lty=1:2, cex=.9, bty='n')



