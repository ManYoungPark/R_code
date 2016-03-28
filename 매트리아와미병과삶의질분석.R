#엑셀파일 로딩.
library("xlsx")

##디렉토리 지정시, \\ 또는 / 로 하면 디렉토리의 구분이된다.
workDir<-"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터"
setwd(workDir)

fileName<-"01. 미래부 미병 코딩 통합자료_150206.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
미병EQ5D<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
미병EQ5D<-미병EQ5D[c(-1,-2),]
미병EQ5D$flagM<-c("미병")
#미병EQ5Dvisit<-subset(미병EQ5D,Visit=='D1'|Visit=='D3'|Visit=='D8')
미병EQ5Dvisit<-subset(미병EQ5D,Visit=='D3')
미병EQ5Dvisit<-subset(미병EQ5D,Visit=='D8')
미병EQ5DvisitTmp<-subset(미병EQ5D,Visit=='D1')
summary(미병EQ5DvisitTmp[,c(1:3)])
미병EQ5Dvisit<-미병EQ5Dvisit[,c("대상자.등록번호","Visit","삶의질점수","SF12_PCS","SF12_MCS","EQ5D_Values")]
head(미병EQ5Dvisit)

summary(미병EQ5Dvisit[1:3])
sqlSave(conn,미병EQ5Dvisit,tablename="미병EQ5DvisitD8")

library(RODBC)
library(sqldf)
conn<-odbcConnect('metria54',uid='sa',pwd='@mibyeong106')
data=sqlQuery(conn,"select * from tmp_metria_summaryWithIn3Day_PMY_점수 ")
data=sqlQuery(conn,"select * from tmp_metria_summaryWithIn3_8Day_PMY_점수 ")


str(data)
head(data)
str(미병EQ5Dvisit)
str(data)

colnames(data)
library(stringr)

## id타입을 factor를 character 로 수정후 양옆에 있는 공백을 제거
data$id<-str_trim(as.character(data$id))
library(plyr)



a$SF12_PCS <-as.numeric(a$SF12_PCS)
a$삶의질점수 <-as.numeric(a$삶의질점수)
a$SF12_MCS <-as.numeric(a$SF12_MCS)
a$EQ5D_Values <-as.numeric(a$EQ5D_Values)

##상관성 분석
library(corrplot)

library(psych)

c<-round(cor(data[,c(-1)]),2)

write.csv(c,"c:/test2.csv")
corrplot(c, method = "number", tl.cex = 1)

colnames(data)
pairs.panels(data[,c(2:7,30:33)])
pairs.panels(data[,c(8:15,30:33)])



