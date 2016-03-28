library("xlsx")

library("sqldf")

##디렉토리 지정시, \\ 또는 / 로 하면 디렉토리의 구분이된다.
workDir<-"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터"
setwd(workDir)
fileName<-"01. 미래부 미병 코딩 통합자료_150313.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
삶의질2<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
colnames(삶의질2)
신체점수<-삶의질2[,c(1,2,56)]
head(신체점수)


a<-sqldf("SELECT a.[대상자.등록번호],a.신체점수 as 신체점수1,b.신체점수  as 신체점수3,c.신체점수 as 신체점수8
               FROM ( select * from 신체점수 WHERE visit='D1') a INNER JOIN 
               (select * from 신체점수 WHERE visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]  INNER JOIN 
               ( select * from 신체점수 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]  ")

a$d1d3<-a$신체점수1-a$신체점수3
a$d1d8<-a$신체점수1-a$신체점수8
head(a)


write.csv(a,"c:/test.csv")
summary(a)
