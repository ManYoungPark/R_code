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

library(stringr)

conn<-odbcConnect('mibyeong_2016',uid='sa',pwd='leo0515')

mb_tmp_data=sqlQuery(conn,"select * from 인바디분석자료0429")
mb_tmp_data=sqlQuery(conn,"select * from 맥분석자료0429")
mb_tmp_data=sqlQuery(conn,"select * from HRV분석자료0429")
mb_tmp_data=sqlQuery(conn,"select * from 계측분석자료_my")
mb_tmp_data=sqlQuery(conn,"select * from 아산병원경희1차_기초대사량비교_최종미병군결측제외$")




setwd("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/2016데이터")

names(mb_tmp_data)[names(mb_tmp_data) %in% "성별"]<-"Gender"
names(mb_tmp_data)[names(mb_tmp_data) %in% "미병분류"]<-"미병그룹"

names(mb_tmp_data)
#인바디 데이터 수행시

#NA값이 40개 이상인거 제거
mb_tmp_data<-mb_tmp_data[!colSums(is.na(mb_tmp_data))>40]
mb_tmp_data$Gender<-factor(mb_tmp_data$Gender,levels=c(1,2),labels=c("male","female"))

summary(mb_tmp_data)

mb_tmp_data_1_1<-filter(mb_tmp_data,미병그룹==1 | 미병그룹==2|미병그룹==3)
mb_tmp_data_1_1[mb_tmp_data_1_1$미병그룹==2,]$미병그룹<-3


summary(mb_tmp_data_1_1)

mb_tmp_data_1_1$미병그룹<-factor(mb_tmp_data_1_1$미병그룹,levels=c(1,3),labels=c("healthy","mibyeong"))
names(mb_tmp_data_1_1)[names(mb_tmp_data_1_1)=="미병그룹"]<-"mi_final"


#out of 95 작업한 df 를 가지고 조인해서 수행
#names(mb_tmp_data_tmp2_1)<-paste0(names(mb_tmp_data_tmp2_1),"_95")
#mergeTmp<-sqldf("select * from mb_tmp_data_1_1 a inner join mb_tmp_data_tmp2_1 b on a.[인바디 ID]=b.[인바디 ID_95]  ")
#mb_tmp_data_1_2<-select(mergeTmp, matches("^a."),Gender,미병그룹)




names(mb_tmp_data_1_1)
names(mb_tmp_data_1_2)


#기초대사량
mb_tmp_data_1_2<-select(mb_tmp_data_1_1,c(5:9),Gender,mi_final)
#계측정보
mb_tmp_data_1_2<-select(mb_tmp_data_1_1,c(15:20),Gender,mi_final)



names(mb_tmp_data_1_2)
summary(mb_tmp_data_1_2)

mb_tmp_data_1_2<-mb_tmp_data_1_2[complete.cases(mb_tmp_data_1_2),]


#
# 남자여자 데이터 분리 함수 ----------------------------------------------------------
split_data_pmy<-function(x,group,target)
{
  library(dplyr)
  library(caret)
  #함수 실생시 지워야함
  #x<-mb_tmp_data_1_2
  #names(x)
  
  #group<-"Gender"
  #target<-"mi_final"
  
  groupTmp<-unique(x[,group])
 
  dataTmp<-list() 
  sampleNames<-c("normal","upsample","downsample")
  

  
  for(sampleName in sampleNames)
  {
    
    #sampleName<-"upsample"
    if(sampleName==sampleNames[1])
    {
      sampleDT<-x
      
    }
    
    if(sampleName==sampleNames[2])
    {
      sampleDT<-upSample(subset(x,select=-mi_final),x$mi_final)
    }
    if(sampleName==sampleNames[3])
    {
      sampleDT<-downSample(subset(x,select=-mi_final),x$mi_final)
    }
  
    names(sampleDT)[names(sampleDT)=="Class"]<-"mi_final"
    str<-"summary(sampleDT$target)"
    str<-str_replace_all(str,"target",target)
    print(eval(parse(text=str)))
    
    
    
    strTmp0<-"dataTmp$sampleName<-sampleDT"
    strTmp0<-str_replace_all(strTmp0,"sampleName",sampleName)
    eval(parse(text=strTmp0))
    
    for(factorNm in groupTmp)
    {
      #  factorNm<-"male"
      strTmp1<-"Tmp<-filter(sampleDT,group=='factorNm')"
      strTmp1<-str_replace_all(strTmp1,"group",group)
      strTmp1<-str_replace_all(strTmp1,"factorNm",factorNm)
      
      eval(parse(text=strTmp1))
      
      strTmp2<-"dataTmp$factorNm<-Tmp"
      strTmp2<-str_replace_all(strTmp2,"factorNm",paste0(factorNm,"_",sampleName))
      eval(parse(text=strTmp2))
      
    }
    
    
  }
  
  return(dataTmp)
  
}

# 남자여자 데이터 분리 끝 -----------------------------------------------------------


#남, 여 데이터 분리 함수를 이용해서 데이터를 분리함.
summary(mb_tmp_data_1_2)

#맥데이터에서 a36번은 지워야함.
mb_tmp_data_1_2<-select(mb_tmp_data_1_2,-a36) 


datasetlist<-NULL
datasetlist<-split_data_pmy(mb_tmp_data_1_2,"Gender","mi_final")
class(datasetlist)

lapply(datasetlist,summary)



#맥데이터 수행시
mb_tmp2<-mb_tmp_data
mb_tmp2[mb_tmp2=='*']<-NA
mb_tmp2_1<-mb_tmp2[!colSums(is.na(mb_tmp2))>6]
mb_tmp2_1<-mb_tmp2_1[complete.cases(mb_tmp2_1),]




#정규화 하고, na 가 500개 이상일때, 그 변수는 제거
#ltmp<-ltmp[complete.cases(ltmp),]
#ltmp.scale<-cbind(scale(ltmp[-length(ltmp)]),ltmp[length(ltmp)])  
#ltmp.scale<-ltmp.scale[!colSums(is.na(ltmp.scale))>500]
#


# DM,SVM, ANN 분류기 함수  -----------------------------------------------------
#다 돌리면, 리스트로 각 요소에는 dataframe으로 결과 반환

DM_fn_pmy <- function(x) {
  
  DM_data=""
  
  DM_data <- x
  
  #DM_data<-datasetlist$normal
  
  
  rsltall <- list()
  rsltDT <- list()
  rsltSVM <- list()
  rsltNN <- list()
  
  rslttotal <- data.frame(names = character(), times = numeric(), Sensitivityss = numeric(), 
                          Specificity = numeric(), PPV = numeric(), NPV = numeric(), accurary = numeric(), 
                          stringsAsFactors = FALSE)
  
  
  
  for (i in 1:30) {
    
    
    # 데이터셋 분류 inTrain=createDataPartition(ltmp$mi_final, p=0.7,
    # list=FALSE)
    
    inTrain = createDataPartition(DM_data$mi_final, p = 0.7, 
                                  list = FALSE)
    traindata = DM_data[inTrain, ]
    testdata = DM_data[-inTrain, ]
    
    myformula <- mi_final ~ .
    
    ############################################# DM 수행
    
    ## classification / decision tree -> random forest
     rf<-randomForest(myformula,data=traindata,ntree=50,proximity=TRUE)
    
    #rf <- rpart(myformula, data = traindata)
    
    # test set으로 성능검증.
    
    # 테스트 데이터를 class로 예측. 분류 정확도를 측정하기 위해
    rf.predicted_test <- predict(rf, newdata = testdata, type = "class")
    
    # 테스트 데이터를 확률로 예측. ROC커브를 그리기위해
    rf.predicted_test_prob <- 1 - unlist(predict(rf, newdata = testdata, 
                                                 type = "prob"), use.names = F)[seq(1, nrow(testdata))]
    
    # 정확도 계산을 아래와 같이 할수도 있다. 참고사항.
    # rf.testAccuracy<-sum(rf.predicted_test==testdata$mi_final)/NROW(testdata)
    
    # 분류 결과를 confusionMatrix로 표현.
    (rltCM_DT <- confusionMatrix(rf.predicted_test, testdata$mi_final))
    
    # rltCM$byClass[1][1]
    
    # ROC커브 그리기위해...
    pred.rf <- prediction(rf.predicted_test_prob, testdata$mi_final)
    # ROC를 그리기위해 performance 함수를사용
    pred.rfpm <- performance(pred.rf, "tpr", "fpr")
    # plot(pred.rfpm, type='s', ylab=pred.rfpm@y.name,
    # xlab=pred.rfpm@x.name, col='grey50', lwd=1)
    
    rsltDT[[i]] <- list(dt = rf, predicted_test_prob = rf.predicted_test_prob, 
                        confusionMatrix = rltCM_DT, ROC_pred = pred.rf, ROC_PERFORMACE = pred.rfpm)
    
    values <- rltCM_DT$byClass
    rltCM_DT$overall
    
    rslttotal <- rbind(rslttotal, data.frame(names = "CART", times = i, 
                                             Sensitivity = values["Sensitivity"], Specificity = values["Specificity"], 
                                             PPV = values["Pos Pred Value"], NPV = values["Neg Pred Value"], 
                                             #accurary = values["Balanced Accuracy"], row.names = NULL))
                                             accuracy = rltCM_DT$overall[1], row.names = NULL))
                                             
    
    
    ###### svm
    
    
    # roc커브를 그리기 위해 probability =TRUE로 해줘야함.
    # sm<-svm(myformula, data = traindata, cost = 100, gamma = 2)
    
    # cost와 gamma값에 따라 다름.
    sm <- svm(myformula, data = traindata, cost = 100, gamma = 0.0625, probability = TRUE)
    # sm<-svm(myformula, data = traindata, cost=100, gamma=2, probability =
    #sm <- svm(myformula, data = traindata, cost = 20, gamma = 0.024124, probability = TRUE)
    # sm<-svm(myformula, data = traindata, cost=100, gamma=2, probability =
    # TRUE)
    
    
    sm.predicted_test <- predict(sm, newdata = testdata, type = "class")
    sm.predicted_test_prob <- predict(sm, newdata = testdata, type = "prob", 
                                      probability = TRUE)
    
    (rstCM_svm <- confusionMatrix(sm.predicted_test, testdata$mi_final))
    
    # svm
    x.svm.prob.rocr <- prediction(attr(sm.predicted_test_prob, "probabilities")[,2], testdata$mi_final)
    x.svm.perf <- performance(x.svm.prob.rocr, "tpr", "fpr")
    
    rsltSVM[[i]] <- list(sm = sm, predicted_test_prob = sm.predicted_test_prob, 
                         confusionMatrix = rstCM_svm, ROC_pred = x.svm.prob.rocr, ROC_PERFORMACE = x.svm.perf)
    
    # confusionMatrix 중 결과 vector를 value 변수에 저장
    values <- rstCM_svm$byClass
    rslttotal <- rbind(rslttotal, data.frame(names = "SVM", times = i, 
                                             Sensitivity = values["Sensitivity"], Specificity = values["Specificity"], 
                                             PPV = values["Pos Pred Value"], NPV = values["Neg Pred Value"], 
                    #                         accuracy = values["Balanced Accuracy"], row.names = NULL))
                                              accuracy = rstCM_svm$overall[1], row.names = NULL))
    
    
    
    # dt.auc<-performance(pred.rf,'auc')
    # svm.auc<-performance(x.svm.prob.rocr,'auc')
    # ann.auc<-performance(x.ann.prob.rocr,'auc')
    
    # dt.auc@y.values[[1]] svm.auc@y.values[[1]] ann.auc@y.values[[1]]
    
    # auc.all<-c(dt.auc@y.values[[1]],svm.auc@y.values[[1]],ann.auc@y.values[[1]])
    # auc.all<-c('88','55','52') ann
    
    # 데이터셋 분류 names(ltmp[-c(62,63)])
    # ltmp.scale<-cbind(ltmp[63],scale(ltmp[-c(62,63)]))
    # summary(ltmp.scale)
    # inTrain=createDataPartition(ltmp.scale$mi_final, p=0.7, list=FALSE)
    # traindata=ltmp.scale[inTrain,] testdata=ltmp.scale[-inTrain,]
    # summary(traindata)
    
    nn <- nnet(myformula, data = traindata, size = 10, rang = 0.1, 
               decay = 5e-04, maxit = 200)
    
    
    nn.predicted_test <- predict(nn, newdata = testdata, type = "class")
    (rsltCM_ANN <- confusionMatrix(nn.predicted_test, testdata$mi_final))
    
    ann.predicted_test_prob <- predict(nn, newdata = testdata, type = "raw")
    x.ann.prob.rocr <- prediction(ann.predicted_test_prob, testdata$mi_final)
    
    x.ann.perf <- performance(x.ann.prob.rocr, "tpr", "fpr")
    rsltNN[[i]] <- list(sm = sm, predicted_test_prob = ann.predicted_test_prob, 
                        confusionMatrix = rsltCM_ANN, ROC_pred = x.ann.prob.rocr, ROC_PERFORMACE = x.ann.perf)
    
    values <- rsltCM_ANN$byClass
    rslttotal <- rbind(rslttotal, data.frame(names = "ANN", times = i, 
                                             Sensitivity = values["Sensitivity"], Specificity = values["Specificity"], 
                                             PPV = values["Pos Pred Value"], NPV = values["Neg Pred Value"], 
                                        #     accurary = values["Balanced Accuracy"], row.names = NULL))
                                        accuracy =rsltCM_ANN$overall[1], row.names = NULL))
    
  }
  
  #summary(rslttotal)
  return(rslttotal)
  
}


#execute the DM algorithms and return the results

lapply(datasetlist,summary)

DM_data=""
rsltlists<-lapply(datasetlist,DM_fn_pmy)

head(rsltlists)

##view the result of classification
for(i in 1:9)
{
  print(ddply(rsltlists[[i]],.(names),colwise(mean, is.numeric))  )
}

ddply(rsltlists[[1]],.(names),colwise(mean, is.numeric))

quan_rslt<-data.frame()
for(i in 1:9)
{
  tb<-ddply(rsltlists[[i]],.(names),
        function(x){
          q1<-round(quantile(x$accuracy,na.rm=TRUE),2)
          Min<-q1[1]
          a1st_Qu<-q1[2]
          Median<-q1[3]
          Mean<-round(mean(x$accuracy,na.rm = TRUE),2)
          a3rd_Qu<-q1[4]
          Max<-q1[5]
          return(data.frame(Min,a1st_Qu,Median,Mean,a3rd_Qu,Max))
          
        })
  
  quan_rslt<-rbind(quan_rslt,tb)
  print(quan_rslt)
  
}

write.csv(quan_rslt,"quan_rslt_HRV.csv")





rsltlists


ddt_total<-ddply(rsltlists[[1]],"names",colwise(mean, is.numeric))
ddt_total$flag2<-"total"

ddt_female<-ddply(rsltlists[[2]],"names",colwise(mean, is.numeric))
ddt_female$flag2<-"female"

ddt_male<-ddply(rsltlists[[3]],"names",colwise(mean, is.numeric))
ddt_male$flag2<-"male"

ddt_integrated<-rbind(ddt_total,ddt_female,ddt_male)
write.csv(ddt_integrated,"ddt_integrated_inbodyonly_미병군전체.csv")


##연습용..
lapply(rsltlists,summary)
mat1 <- matrix(rep(seq(4), 4), ncol = 4)
mat1.df <- data.frame(mat1)
mat1.df.list<-list(a1=mat1.df,a2=mat1.df*2)

testfunc<-function(x, y) sum(x) + y
y1 <- lapply(mat1.df, testfunc, y = 5)
lapply(mat1.df.list, testfunc, y = 5)



rsltlists$total$flag<-'total'
rsltlists$female$flag<-'female'
rsltlists$male$flag<-'male'

integratedRslt<-do.call(rbind,rsltlists)
names(integratedRslt)
integratedRsltlong<-melt(integratedRslt, id.vars=c("names", "flag"),measure.vars=c( "Sensitivityss", "Specificity","PPV","NPV","accurary"  ))
summary(integratedRsltlong)

write.csv(integratedRslt,"integratedRslt.csv")
write.csv(integratedRsltlong,"integratedRsltlong.csv")

#
#




























#예전 논문쓸때 햇던것들...

summary(updownIncluded_AllRslt)

ggplot(data=rslttotal,aes(x=factor(names),y=accurary,fill=names))+ 
  geom_boxplot()+ 
  facet_grid(flag2 ~ flag)



ggplot(data=rslttotal,aes(x=names,y=accurary,fill=names))+ 
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

m<-randomForest(mi_final~., data=traindata, importance = TRUE)

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



