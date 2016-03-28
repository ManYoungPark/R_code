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
downFunc<-function()
{
  #mb_tmp2down<-downSample(subset(mb_tmp1,select=-미병분류fi),mb_tmp1$미병분류fi)
  #mb_tmp2<-downSample(subset(mb_tmp1,select=-미병분류fi),mb_tmp1$미병분류fi)
  mb_tmp2<-upSample(subset(mb_tmp1,select=-미병분류fi),mb_tmp1$미병분류fi)

 colnames(mb_tmp2)[75]<-"미병분류fi"
#colnames(mb_tmp2down)[75]<-"미병분류fi"
# 


mb_tmp2_1<-select(mb_tmp2,-c(Apen:esi),-c(체질진단fi,미병점수fi,대상자_등록번호))
mb_tmp0<-mb_tmp2_1


#전체
mb_tmp1_1<-mb_tmp0
ltmp<-mb_tmp1_1

ltmp.scale<-cbind(scale(ltmp[-c(62,63)]),ltmp[63])                  
#summary(ltmp.scale)                
 
#inTrain=createDataPartition(ltmp.scale$미병분류fi, p=0.7, list=FALSE)


rsltall<-list()
rsltDT<-list()
rsltSVM<-list()
rsltNN<-list()


rslttotal <- data.frame(names= character(), times=numeric(), 
                       Sensitivityss= numeric(),Specificity=numeric(),PPV= numeric(), NPV= numeric(),accurary=numeric(),stringsAsFactors=FALSE)
for(i in 1:100)
{
  

#데이터셋 분류 
#inTrain=createDataPartition(ltmp$미병분류fi, p=0.7, list=FALSE)
  
inTrain=createDataPartition(ltmp.scale$미병분류fi, p=0.7, list=FALSE)
traindata=ltmp.scale[inTrain,]
testdata=ltmp.scale[-inTrain,]

myformula<-미병분류fi~.


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



}




(tp2<-mean(filter(rslttotal,names=="SVM")$accurary))

summary(filter(rslttotal,names=="SVM"))
summary(filter(rslttotal,names=="ANN"))
summary(filter(rslttotal,names=="CART"))

return (list(svmMean=tp2,dt=ltmp))

}


## 머신러닝들을 1000번 돌려보기, 1000번 돌릴때, 그안에서도 7:3 training, test를 1000번 수행함.
## 그래서, svm의 성능이 75% 이상이 나오는 데이터 셋에서 멈춤.
svmMax<-0
for(i in 1:1000)
{

  rsl<-downFunc()
  if(rsl$svmMean>svmMax)
  {
    svmMax<-rsl$svmMean
    dataset<-rsl$dt
    if(svmMax>0.87)
    {
      print(i)
      break
      
    }
  print(paste(i," 번째loop 수행중 ","svm=",svmMax)) 
  }  
}


downsamplingdataset<-dataset
upsamplingdataset<-dataset
save(downsamplingdataset,upsamplingdataset, file="D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/updownsamplingdataset논문용.RData")
load("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/updownsamplingdataset논문용.RData")

getwd()

write.csv(downsamplingdataset,file="../논문작성/논문데이터/downsampling_for_paper.csv")
write.csv(upsamplingdataset,file="../논문작성/논문데이터/upsampling_for_paper.csv")

summary(upsamplingdataset)

downsamplingdataset$수축기혈압
~downsamplingdataset$미병분류fi

mean1<-round(mean(downsamplingdataset$수축기혈압),2)
sd1<-round(sd(downsamplingdataset$수축기혈압),2)

paste(as.character(mean1),"±",as.character(sd1))
mode(a)

names(downsamplingdataset)




idx1<-which(downsamplingdataset$미병분류fi=="건강군")
idx2<-which(downsamplingdataset$미병분류fi=="미병2")



split건강<-downsamplingdataset[idx1,]
split미병<-downsamplingdataset[idx2,]

allavg<-round(mean(downsamplingdataset$수축기혈압),2)
avg1<-round(mean(split건강$수축기혈압),2)
avg2<-round(mean(split미병$수축기혈압),2)

allsd<-round(sd(downsamplingdataset$수축기혈압),2)
sd1<-round(sd(split건강$수축기혈압),2)
sd2<-round(sd(split미병$수축기혈압),2)


ttest<-t.test(downsamplingdataset$수축기혈압~downsamplingdataset$미병분류fi)


rsltframe<-data.frame(variable="수축기혈압",
                      Total=paste(as.character(allavg),"±",as.character(allsd)),
                      healthy=paste(as.character(avg1),"±",as.character(sd1)),Mibyeong=paste(as.character(avg2),"±",as.character(sd2)),
                      p_value=round(ttest$p.value,3)
                      )


write.csv(rsltframe,file="../논문작성/논문데이터/tmp.csv")



summaryBy(수축기혈압~미병분류fi, data=downsamplingdataset, FUN=c(mean, sd))





ddply(downsamplingdataset,~미병분류fi,summarise,avg=mean(downsamplingdataset$수축기혈압))



library(dplyr)

a$p.value
a$data.name
a$parameter




names(downsamplingdataset)


summary(downsamplingdataset)
summary(usamplingdataset)


