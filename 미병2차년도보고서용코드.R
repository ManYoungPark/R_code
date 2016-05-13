
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
library(ggplot2)



conn<-odbcConnect('mibyeong_2016',uid='sa',pwd='leo0515')

mb_tmp_data=sqlQuery(conn,"select * from 인바디분석자료0429")

setwd("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/2016데이터")

names(mb_tmp_data)
# 데이터가 1/3 이상 없는 데이터 컬럼 제거 ------------------------------------------------


#NA값이 40개 이상인거 제거
mb_tmp_data<-mb_tmp_data[!colSums(is.na(mb_tmp_data))>40]

# 건강군 모든 변수 95%구간 구하기. --------------------------------------------------

# 건강군만 추출
mb_tmp_data.group1<-subset(mb_tmp_data,미병버전==1 & 미병그룹==1)

#nms<-names(mb_tmp_data.group1)
#nms.inbodyVR<-grep("^a",nms,value=TRUE)

names(mb_tmp_data.group1)
#inbody
tmpdata<-mb_tmp_data.group1[,c(14,17:ncol(mb_tmp_data.group1))]
#mac
tmpdata<-select(mb_tmp_data.group1,matches("^a[0-9]+"),Gender)
#hrv
tmpdata<-select(mb_tmp_data.group1,c(10:40),Gender)
varName<-names(tmpdata)[1:15]
tmpName<-paste0("k",rep(1:15))
variable_mapping<-data.frame(fullName=varName,shortName=tmpName)

for(i in 1:15)
{
  names(tmpdata)[i]<-tmpName[i]
}

namTm<-names(mb_tmp_data)
# to replace the korean variables to english in mb_tmp_data
for(i in 10:24)
{
  names(mb_tmp_data)[i]<-tmpName[i-9]
}
names(mb_tmp_data)

#end of hrv related



names(tmpdata)

tmpdata$Gender=factor(tmpdata$Gender)
tmpdata_male<-tmpdata[tmpdata$Gender=='male',]
tmpdata_female<-tmpdata[tmpdata$Gender=='female',]

summary(tmpdata_male)
summary(tmpdata_female)

tmpdata1_1<-tmpdata #전체
tmpdata1_1<-tmpdata_male #man
tmpdata1_1<-tmpdata_female #women


numericVR<-sapply(tmpdata1_1,is.numeric)
tmpdata2<-tmpdata1_1[numericVR]
summary(tmpdata2)
normal95total<-fun_95percent(tmpdata2) #전체
normal95man<-fun_95percent(tmpdata2) #man
normal95woman<-fun_95percent(tmpdata2) #female

names(normal95man)<-paste0(names(normal95man),"_male")
names(normal95woman)<-paste0(names(normal95woman),"_female")

normal95totalbind<-cbind(normal95total,normal95man,normal95woman)

names(normal95totalbind)


library(sqldf)

mappingVR=sqlQuery(conn,"select * from [인바디변수매핑]")
mappingVR=sqlQuery(conn,"select * from 맥변수명mapping")

data.frame(list(A=c("","xyz","jkl"), B=c(12,"",100)))
sw["new1"] <- LETTERS[1:5] 
format(c(1.12,1.52342,132,1),digits=3)
#hrv
#hrv는 변수명을 한번더 mapping 시켜야함, 한글때문에 임시로 make the variables for tmp
normal95totalbind_fullNm<-normal95totalbind
normal95totalbind_fullNm2<-select(normal95totalbind_fullNm,vnames,matches(".95"),-matches("^conf"))
#end of hrv part

normal95totalbind_fullNm<-sqldf("select * from normal95totalbind a inner join mappingVR b on a.vnames=b.shortName")
normal95totalbind_fullNm2<-select(normal95totalbind_fullNm,fullName,matches(".95"),-matches("^conf"))


write.csv(normal95totalbind_fullNm2,"normal95totalbind_fullNm2.csv")
write.csv(normal95totalbind_fullNm2,"normal95totalbind_fullNm2Mac.csv")
write.csv(normal95totalbind_fullNm2,"normal95totalbind_fullNm2HRV.csv")
inbody95p_range<-normal95totalbind_fullNm2
mac95p_range<-normal95totalbind_fullNm2
HRV95p_range<-normal95totalbind_fullNm2





# 정상군 95% 범위 및 평균 구하기 -----------------------------------------------------


fun_95percent<-function(x)
{
    
  df_95<-data.frame(vnames=character(),lw95=numeric(),up95=numeric(),avg=numeric(),confi_lw95=numeric(),confi_up95=numeric())
  
  for(VRnms in names(x))
  {
    
    (tmp1<-paste0("x$",VRnms))
    tmp2<-paste0("t.test(",tmp1,")")
    rslts<-eval(parse(text=tmp2))
  
    print(rslts)
  
    lw95str<-paste0("quantile(",tmp1,",0.025,na.rm = TRUE)")
    up95str<-paste0("quantile(",tmp1,",0.975,na.rm = TRUE)")
    
    lw95s<-eval(parse(text=lw95str))
    up95s<-eval(parse(text=up95str))
    
    tmp95<-data.frame(vnames=VRnms,lw95=lw95s,up95=up95s,avg=rslts$estimate,confi_lw95=rslts$conf.int[1],confi_up95=rslts$conf.int[2])
    df_95<-rbind(df_95,tmp95)
    
  }
  return(df_95)
  
}

# 정상군 95% 구간 구하기 끝 --------------------------------------------------------

# 변수코딩95%값으로 1또는 0 또는 -1 --------------------------------------------------


outofnormalCheck<-function(mb_tmp_data_tmp1,normal95totalbind)
{
  #mb_tmp_data_tmp1<-mb_tmp_data_tmp2
  vnames_only<-normal95totalbind$vnames
  
  for(vnm in vnames_only)
  {
   # vnm<-"a37"
    
    str1<-"mb_tmp_data_tmp1[mb_tmp_data_tmp1$Gender=='male' & mb_tmp_data_tmp1$vnm<filter(normal95totalbind,vnames=='vnm')$lw95_male,]$vnm<-1000001"
    str2<-"mb_tmp_data_tmp1[mb_tmp_data_tmp1$Gender=='male' & mb_tmp_data_tmp1$vnm>=filter(normal95totalbind,vnames=='vnm')$lw95_male & mb_tmp_data_tmp1$vnm<=filter(normal95totalbind,vnames=='vnm')$up95_male,]$vnm<-1000002"
    str3<-"mb_tmp_data_tmp1[mb_tmp_data_tmp1$Gender=='male' & mb_tmp_data_tmp1$vnm>filter(normal95totalbind,vnames=='vnm')$up95_male  & mb_tmp_data_tmp1$vnm<1000000,]$vnm<-1000003"
    
    str4<-"mb_tmp_data_tmp1[mb_tmp_data_tmp1$Gender=='female' & mb_tmp_data_tmp1$vnm<filter(normal95totalbind,vnames=='vnm')$lw95_female,]$vnm<-1000001"
    str5<-"mb_tmp_data_tmp1[mb_tmp_data_tmp1$Gender=='female' & mb_tmp_data_tmp1$vnm>=filter(normal95totalbind,vnames=='vnm')$lw95_female & mb_tmp_data_tmp1$vnm<=filter(normal95totalbind,vnames=='vnm')$up95_female,]$vnm<-1000002"
    str6<-"mb_tmp_data_tmp1[mb_tmp_data_tmp1$Gender=='female' & mb_tmp_data_tmp1$vnm>filter(normal95totalbind,vnames=='vnm')$up95_female & mb_tmp_data_tmp1$vnm<1000000 ,]$vnm<-1000003"
    
    str1<-str_replace_all(str1,"vnm",vnm)
    str2<-str_replace_all(str2,"vnm",vnm)
    str3<-str_replace_all(str3,"vnm",vnm)
    str4<-str_replace_all(str4,"vnm",vnm)
    str5<-str_replace_all(str5,"vnm",vnm)
    str6<-str_replace_all(str6,"vnm",vnm)
    
    tryCatch({
    
      eval(parse(text=str1))  
    }, error = function(e) {
      print("occur error")
    }
    )
    
    
    tryCatch({
      
      eval(parse(text=str2))
    }, error = function(e) {
      print("occur error")
    }
    )
    tryCatch({
      eval(parse(text=str3))
    }, error = function(e) {
      print("occur error")
    }
    )
    
    
    tryCatch({
      eval(parse(text=str4))
    }, error = function(e) {
      print("occur error")
    }
    )
    
    tryCatch({
      eval(parse(text=str5))
    }, error = function(e) {
      print("occur error")
    }
    )
    
    tryCatch({
      eval(parse(text=str6))
    }, error = function(e) {
      print("occur error")
    }
    )
    
    
    str7<-"mb_tmp_data_tmp1$vnm<-factor(mb_tmp_data_tmp1$vnm, levels=c(1000001,1000002,1000003), labels=c('out of 95% lower','normal','out of 95% upper'))"
    str7<-str_replace_all(str7,"vnm",vnm)
    
    tryCatch({
      eval(parse(text=str7))
    }, error = function(e) {
      print("occur error")
    }
    )
    mb_tmp_data_tmp1$a17
    
    
    print(i)
    i<-i+1
    print(vnm)
    
  }
  summary(mb_tmp_data_tmp1)
  return(mb_tmp_data_tmp1)
  
  
}


# 변수코딩95%값으로 1또는 0 또는 -1 끝 ------------------------------------------------


mb_tmp_data_tmp2<-mb_tmp_data
mb_tmp_data_tmp2<-mb_tmp_data_tmp2[complete.cases(mb_tmp_data_tmp2),]
summary(mb_tmp_data_tmp2)
names(mb_tmp_data)
mb_tmp_data_tmp2_1<-outofnormalCheck(mb_tmp_data_tmp2,normal95totalbind)

summary(mb_tmp_data_tmp2_1)


mb_tmp_data_tmp2_1$미병그룹recode<-ifelse(mb_tmp_data_tmp2_1$미병그룹==3,2,mb_tmp_data_tmp2_1$미병그룹)
mb_tmp_data_tmp2_1$미병그룹recode<-factor(mb_tmp_data_tmp2_1$미병그룹recode,levels=c(1,2),labels=c('건강군','미병'))

summary(mb_tmp_data_tmp2_1$미병그룹recode)

# 각변수별로, 0또는 2 즉, 정상군의 95%범위를 벗어나는 데이터들의 비율이 미병1이 많은지 미병2가 많은지 확인 ---------


##정상군의 데이터에서 가장많은 n수가 벗어나는 변수 찾기


#컬럼명 이상한것들 일괄적으로 바꾸기.
nms<-names(mb_tmp_data_tmp1)
nms_1<-str_replace_all(nms,"[# -]|^[1-9]","")
names(mb_tmp_data_tmp1)<-nms_1

names(mb_tmp_data_tmp1)
























mb_tmp_data1<-within(mb_tmp_data_tmp1,
                     {
                     Gender<-factor(Gender)
                     미병분류fi<-factor(미병그룹recode)
                     }
             )


summary(mb_tmp_data_tmp1)
summary(mb_tmp_data1)
names(mb_tmp_data1)
mb_tmp2<-mb_tmp_data1[,c(13:29,31:ncol(mb_tmp_data1))]

names(mb_tmp2)
summary(mb_tmp2)

mb_tmp2<-mb_tmp2[,c(1:21,ncol(mb_tmp2))]


mb_tmp2.Male<-subset(mb_tmp2,성별f=="M",select =-c(성별f))
mb_tmp2.Female<-subset(mb_tmp2,성별f=="F",select =-c(성별f))

summary(mb_tmp2.Male)
summary(mb_tmp2)

svmfn(mb_tmp2.Male)

mb_tmp2<-mb_tmp2[complete.cases(mb_tmp2),]


svmfn<-function(ltmpTmp){
  
  ltmpTmp<-mb_tmp2
  summary(ltmpTmp)
  
  
  values1<-as.integer()
  for(i in 1:10)
  {
    
    inTrain=createDataPartition(ltmpTmp$미병분류fi, p=0.7, list=FALSE)
    traindata=ltmpTmp[inTrain,]
    testdata=ltmpTmp[-inTrain,]
    
    myformula<-미병분류fi~.
    
    m<-randomForest(myformula, data=traindata, importance = TRUE)
    
    m.test <-predict(m,newdata=testdata, type="class")
    (rstCM_rf<-confusionMatrix(m.test,testdata$미병분류fi))
    values1<-rbind(values1,rstCM_rf$overall[1])
    
  }
  
}

mean(values1) 



#변수코딩95%값으로 1또는 0 또는 -1 --
for(vnm in vnames_only)
{
  i<-1
  tableNm<-"mb_tmp_data_tmp1$"
  
  str1<-paste0(tableNm,nms.inbodyVR[i],"<-ifelse(",tableNm,nms.inbodyVR[i],"<=df_95[",i,",]$lw95,0,ifelse(",tableNm,nms.inbodyVR[i],">df_95[",i,",]$lw95 & ",
               tableNm,nms.inbodyVR[i],"<df_95[",i,",]$up95,1,2))")
  
  str2<-paste0(tableNm,nms.inbodyVR[i],"<-factor(",tableNm,nms.inbodyVR[i],", levels=c(0,1,2),labels=c('out of 95% lower','normal','out of 95% upper'))")
  
  eval(parse(text=str1))
  eval(parse(text=str2))
  
}
