#엑셀파일 로딩.
library("xlsx")

##디렉토리 지정시, \\ 또는 / 로 하면 디렉토리의 구분이된다.
workDir<-"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터"
setwd(workDir)

options(java.parameters = "-Xmx16000m")

fileName<-"01. 미래부 미병 코딩 통합자료_150313.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
#미병EQ5D<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
삶의질<- read.xlsx2(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
미병<- read.xlsx(dirFile, sheetIndex=2 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.


head(삶의질)
삶의질<-삶의질[c(-1,-2),]
삶의질<-삶의질[,c("대상자.등록번호","Visit","삶의질점수","SF12_PCS","SF12_MCS","EQ5D_Values")]

head(미병)
미병<-미병[c(-1,-2),]
미병<-미병[,c("대상자.등록번호","Visit","피로미병점수","통증미병점수","수면미병점수","소화미병점수","우울미병점수","분노미병점수","불안미병점수","미병점수_총점")]


미병삶의질<-merge(미병,삶의질,by.x=c("대상자.등록번호","Visit"),by.y=c("대상자.등록번호","Visit"), all.x=FALSE, all.y=FALSE, trace = FALSE)


미병삶의질$피로미병점수<-as.numeric(미병삶의질$피로미병점수)
미병삶의질$통증미병점수<-as.numeric(미병삶의질$통증미병점수)
미병삶의질$수면미병점수<-as.numeric(미병삶의질$수면미병점수)
미병삶의질$소화미병점수<-as.numeric(미병삶의질$소화미병점수)
미병삶의질$우울미병점수<-as.numeric(미병삶의질$우울미병점수)
미병삶의질$분노미병점수<-as.numeric(미병삶의질$분노미병점수)
미병삶의질$불안미병점수<-as.numeric(미병삶의질$불안미병점수)
미병삶의질$미병점수_총점<-as.numeric(미병삶의질$미병점수_총점)
미병삶의질$삶의질점수<-as.numeric(미병삶의질$삶의질점수)
미병삶의질$SF12_PCS<-as.numeric(미병삶의질$SF12_PCS)
미병삶의질$SF12_MCS<-as.numeric(미병삶의질$SF12_MCS)
미병삶의질$EQ5D_Values<-as.numeric(미병삶의질$EQ5D_Values)


미병삶의질<-subset(미병삶의질,피로미병점수<999 & 통증미병점수<900 & 수면미병점수<900& 소화미병점수<900& 우울미병점수<900& 분노미병점수<900& 불안미병점수<900)


str(미병삶의질)
head(미병삶의질)
summary(미병삶의질)


if(require(psych)==FALSE)
  install.packages("psych")  


library(psych)

names(미병삶의질)
#미병삶의질.con<-미병삶의질[,c(-1,-2,-10,-11,-12)]
미병삶의질.con<-미병삶의질[,c(-1,-2)]
pairs.panels(미병삶의질.con)

미cor<-cor(미병삶의질.con)
round(미cor,2)

install.packages("corrplot")
library(corrplot)

corrplot(미cor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)
corrplot(미cor)
corrplot(미cor, method = "number", tl.cex = 1)


fileName<-"일반인 미병점수_삶의질_0326.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
일반인<- read.xlsx2(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
names(일반인)
str(일반인)

일반인step1<-일반인[,c(29:35)]

str(일반인.step1)
names(일반인.step1)


일반인step1$피로MI<-as.numeric(일반인step1$피로MI)
일반인step1$통증MI<-as.numeric(일반인step1$통증MI)
일반인step1$수면장애MI<-as.numeric(일반인step1$수면장애MI)
일반인step1$소화불량MI<-as.numeric(일반인step1$소화불량MI)
일반인step1$우울MI<-as.numeric(일반인step1$우울MI)
일반인step1$분노MI<-as.numeric(일반인step1$분노MI)
일반인step1$불안MI<-as.numeric(일반인step1$불안MI)
일반인step1$신체MI<-as.numeric(일반인step1$신체MI)
일반인step1$정서MI<-as.numeric(일반인step1$정서MI)

일반cor<-cor(일반인step1)
corrplot(일반cor, method = "number", tl.cex = 1)


#day별 삶의질 변화 차 분석.. 

merge(미병삶의질,미병삶의질,by.x=c("대상자.등록번호","Visit"),by.y=c("대상자.등록번호","Visit"), all.x=FALSE, all.y=FALSE, trace = FALSE)

mydata4<-미병삶의질

library("sqldf")

mydata5<-sqldf(" 
 SELECT a.[삶의질점수] as 삶의질점수1,a.SF12_PCS as SF12_PCS1,a.SF12_MCS as SF12_MCS1,a.EQ5D_VALUES as EQ5D_VALUES1,
 b.[삶의질점수] as 삶의질점수3,b.SF12_PCS  as SF12_PCS3,b.SF12_MCS as SF12_MCS3,b.EQ5D_VALUES as EQ5D_VALUES3
  ,c.[삶의질점수] as 삶의질점수8,c.SF12_PCS as SF12_PCS8,c.SF12_MCS SF12_MCS8,c.EQ5D_VALUES as EQ5D_VALUES8,
a.미병점수_총점 as 미병점수_총점1,b.미병점수_총점 as 미병점수_총점3,c.미병점수_총점 as 미병점수_총점8
 FROM ( select * from mydata4 WHERE visit='D1') a INNER JOIN 
 (select * from mydata4 WHERE visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]  INNER JOIN 
  ( select * from mydata4 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]   ")


write.csv(mydata5,"D:\\KIOM\\프로젝트문서들\\미래부_수면박탈_미병\\데이터\\tmp.csv")

names(mydata5)
summary(mydata5)

t.test(mydata5$SF12_PCS1,mydata5$SF12_PCS3, paired=TRUE)
t.test(mydata5$SF12_MCS1,mydata5$SF12_MCS3, paired=TRUE)
t.test(mydata5$EQ5D_VALUES1,mydata5$EQ5D_VALUES3, paired=TRUE)
t.test(mydata5$삶의질점수1,mydata5$삶의질점수3, paired=TRUE)
t.test(mydata5$미병점수_총점1,mydata5$미병점수_총점3, paired=TRUE)


t.test(mydata5$SF12_PCS1,mydata5$SF12_PCS8, paired=TRUE)
t.test(mydata5$SF12_MCS1,mydata5$SF12_MCS8, paired=TRUE)
t.test(mydata5$EQ5D_VALUES1,mydata5$EQ5D_VALUES8, paired=TRUE)
t.test(mydata5$삶의질점수1,mydata5$삶의질점수8, paired=TRUE)
t.test(mydata5$미병점수_총점1,mydata5$미병점수_총점8, paired=TRUE)


t.test(mydata5$SF12_PCS3,mydata5$SF12_PCS8, paired=TRUE)
t.test(mydata5$SF12_MCS3,mydata5$SF12_MCS8, paired=TRUE)
t.test(mydata5$EQ5D_VALUES3,mydata5$EQ5D_VALUES8, paired=TRUE)
t.test(mydata5$삶의질점수3,mydata5$삶의질점수8, paired=FALSE)
t.test(mydata5$미병점수_총점3,mydata5$미병점수_총점8, paired=TRUE)