
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

mb_tmp_data=sqlQuery(conn,"select * from 대전대만")

setwd("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/2016데이터")


namestmp <- names(mb_tmp_data)

summary(mb_tmp_data)


colnms<-c("sub_ID","a_Gender","a_Weight","aa_TBW_TotalBodyWater_", "aa_ICW_IntracellularWater_","aa_ECW_ExtracellularWater_","aa_Protein","aa_Minerals"
          ,"aa_BFM_BodyFatMass_","aa_SLM_SoftLeanMass_","aa_FFM_FatFreeMass_","aa_BMI_BodyMassIndex_","aa_PBF_PercentBodyFat_"
          ,"aa_FFM_per_ofRightArm","aa_FFM_per_ofLeftArm","aa_FFM_per_ofTrunk","aa_FFM_per_ofRightLeg","aa_FFM_per_ofLeftLeg","aaa_ECW_TBW","aaa_WHR_Waist_HipRatio_"
          ,"aaa_ObesityDegree" ,"aaa_BCM_BodyCellMass_","aaa_TBW_FFM","aaa_aakHz_RAPhaseAngle","aaa_aakHz_LAPhaseAngle"
          ,"aaa_aakHz_TRPhaseAngle","aaa_aakHz_RLPhaseAngle","aaa_aakHz_LLPhaseAngle","aaa_aakHz_WholeBodyPhaseAngle"
          , "aaa_PulsePressure","mi_total_score","mi_total_group3")




library(plyr)

##데이터 컬럼 선택
mb_tmp1_1<-mb_tmp_data[,names(mb_tmp_data) %in% colnms]

##컬럼명 변경
#names(mb_tmp1_1)[names(mb_tmp1_1) == 'aa_FFM%ofRightArm'] <- 'aa_FFM_per_ofRightArm'
names(mb_tmp1_1)


library(dplyr)
mb_tmp1_1<-select(mb_tmp1_1,-c(sub_ID))

mb_tmp1_2<-transform(mb_tmp1_1,
                     pp_aa_TBW_TotalBodyWater_div_Weight=aa_TBW_TotalBodyWater_/a_Weight
                     ,pp_aa_ICW_IntracellularWater_div_aa_ECW_ExtracellularWater_=aa_ICW_IntracellularWater_/aa_ECW_ExtracellularWater_
                     ,pp_aa_Protein_div_a_Weight=aa_Protein/a_Weight
                     ,pp_aa_Minerals_div_a_Weight=aa_Minerals/a_Weight
                     ,pp_aa_BFM_BodyFatMass_div_weight=aa_BFM_BodyFatMass_/a_Weight
                     ,pp_aa_SLM_SoftLeanMass_div_weight=aa_SLM_SoftLeanMass_/a_Weight
                     ,pp_aa_FFM_FatFreeMass_div_weight=aa_FFM_FatFreeMass_/a_Weight
                     ,pp_FFMSum=aa_FFM_per_ofRightArm+aa_FFM_per_ofLeftArm+aa_FFM_per_ofTrunk+aa_FFM_per_ofRightLeg+aa_FFM_per_ofLeftLeg
                     ,pp_TBW_div_FFM=aa_TBW_TotalBodyWater_/aa_FFM_FatFreeMass_
                     ,mi_total_group3=factor(mi_total_group3)
)


mb_tmp1_2<-mb_tmp1_2[complete.cases(mb_tmp1_2),]

#mb_tmp1_2.Female<-filter(mb_tmp1_2,a_Gender=="F")
mb_tmp1_2.Female<-mb_tmp1_2[mb_tmp1_2$a_Gender=="F",-1]
#위에것과 같은거임. 
#mb_tmp1_2.Female2<-subset(mb_tmp1_2,a_Gender=="F",select =-c(a_Gender))
mb_tmp1_2.Male<-subset(mb_tmp1_2,a_Gender=="M",select =-c(a_Gender))


mb_tmp1_2<-mb_tmp1_2.Female
mb_tmp1_2<-mb_tmp1_2.Male

a<-names(mb_tmp1_2)
nas2<-''
for(nas in a)
{
  nas2<-paste(nas,nas2,sep=',')
}

#여기서 부터 돌려야 하는곳.. 
rsltSeq <- data.frame(models= character(), health=numeric(),mibyeong=numeric(),sigNum=numeric(), SVMAccuracy=numeric(),DT_Accuracy=numeric(),vars=character(),stringsAsFactors=FALSE)
                      
                         
criteriaNums<-21:95

for(criteriaNum in criteriaNums)
{
  
  var<-""
  
  mb_tmp1_2$mi_total_group3[mb_tmp1_2$mi_total_score<=criteriaNum]<-1
  mb_tmp1_2$mi_total_group3[mb_tmp1_2$mi_total_score>criteriaNum]<-2
  mb_tmp1_2$mi_total_group3<-factor(mb_tmp1_2$mi_total_group3)
  mb_tmp2<-select(mb_tmp1_2,c(aa_BMI_BodyMassIndex_:aaa_aakHz_WholeBodyPhaseAngle,pp_aa_TBW_TotalBodyWater_div_Weight:pp_TBW_div_FFM,mi_total_group3))
  
  #setdiff(names(mb_tmp1_2),names(mb_tmp2))
  
  nn<-names(mb_tmp2)
  nn<-nn[-length(nn)]
  
  cnt<-0
  for(varname in nn)
  {
    (formul<-paste0("mb_tmp2$",varname,"~mb_tmp2$mi_total_group3"))
  
    (rslts<-t.test(formula(formul)))
    print(rslts)
    
    if(rslts$p.value<0.05)
    {
      cnt<-cnt+1
      var<-paste(var,varname,sep = ',')
      
    }
    
  }
  
  popul<-summary(mb_tmp2$mi_total_group3)
  
  
  valuesbalancedSVM<-as.integer()
  valuesbalancedDT<-as.integer()
  for(i in 1:10)
  {
    
    
    inTrain=createDataPartition(mb_tmp2$mi_total_group3, p=0.7, list=FALSE)
    traindata=mb_tmp2[inTrain,]
    testdata=mb_tmp2[-inTrain,]
    
    myformula<-mi_total_group3~.
    
    #cost와 gamma값에 따라 다름.
    sm<-svm(myformula, data = traindata, cost=400, gamma=0.0325, probability = TRUE)
    #sm<-svm(myformula, data = traindata, cost=100, gamma=2, probability = TRUE)
    
    
    sm.predicted_test <-predict(sm,newdata=testdata, type="class")
    sm.predicted_test_prob <-predict(sm,newdata=testdata,type="prob", probability = TRUE)
    
    (rstCM_svm<-confusionMatrix(sm.predicted_test,testdata$mi_total_group3))
    
    
    valuesbalancedSVM<-rbind(valuesbalancedSVM,rstCM_svm$byClass[8])
    
    
    
    m<-randomForest(myformula, data=traindata, importance = TRUE)
    
    
    m.test <-predict(m,newdata=testdata, type="class")
    (rstCM_rf<-confusionMatrix(m.test,testdata$mi_total_group3))
    valuesbalancedDT<-rbind(valuesbalancedDT,rstCM_rf$byClass[8])
    
    
    
  }
  accbalancedSVM<-mean(valuesbalancedSVM)
  accbalancedDT<-mean(valuesbalancedDT)
  
  
  rsltSeq <- rbind(rsltSeq,
                   data.frame(models= criteriaNum, health=popul[1],mibyeong=popul[2],sigNum=cnt, SVMAccuracy=accbalancedSVM,DT_Accuracy=accbalancedDT,vars=var))

}



rsltSeq

write.csv(rsltSeq,file="male_inbody.csv")

