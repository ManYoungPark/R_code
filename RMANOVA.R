#엑셀파일 로딩.
install.packages("xlsx")
install.packages("sqldf")
library(xlsx)
library(dplyr)
library(sqldf)

미병점수<- read.xlsx("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/01. 미래부 미병 코딩 통합자료_150416(탈락자 제외).xlsx"
                   , sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
#미병점수<- read.xlsx("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/미래부미병_수면제한박탈_미병점수재계산_150424_정리.xlsx"
#                   , sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.




미병점수2<-미병점수[ ,1:5]

summary(미병점수2)
미병점수3<-미병점수2[미병점수2$미병점수_총점<999,]

head(미병점수3)


미병<-sqldf("select a.대상자등록번호 as ids,
       a.미병점수_총점 as 미병day1,
       b.미병점수_총점 as 미병day3, 
       c.미병점수_총점 as 미병day8, 
       a.신체총점 as 신체총점day1,
       b.신체총점 as 신체총점day3, 
       c.신체총점 as 신체총점day8, 
       a.정신총점 as 정신총점day1,
       b.정신총점 as 정신총점day3, 
       c.정신총점 as 정신총점day8 

       FROM ( select * from 미병점수3 WHERE visit='D1') a INNER JOIN (select * from 미병점수3 WHERE visit='D3') b 
                ON a.대상자등록번호 =b.대상자등록번호  INNER JOIN 
            ( select * from 미병점수3 WHERE visit='D8') c on a.대상자등록번호 =c.대상자등록번호")


미병지수그룹s<-read.xlsx("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/미병회복군_그룹_150427.xlsx"
                  , sheetIndex=2 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
미병지수그룹s<-미병지수그룹s[,c(1:3,11,12)]

미병지수그룹s<-na.omit(미병지수그룹s)
미병지수그룹case1 <-미병지수그룹s[미병지수그룹s$case==1 & (미병지수그룹s$미병지수그룹==1 |미병지수그룹s$미병지수그룹==2)  ,]


summary(미병지수그룹case1)

삶의질<- read.xlsx("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/01. 미래부 미병 코딩 통합자료_150416(탈락자 제외).xlsx"
                , sheetIndex=2 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.






str(삶의질)
colnames(삶의질)
삶의질2<-삶의질[,c(1:2,23:26)]
삶의질2<-삶의질[삶의질$SF12_PCS<999 & 삶의질$SF12_MCS<999 ,c(1:2,23:26)]
head(삶의질2)
str(삶의질2)
colnames(삶의질2)

summary(삶의질2)


삶의질C<-sqldf("select a.[대상자.등록번호] as ids,
       a.삶의질점수 as 삶의질점수day1,
       b.삶의질점수 as 삶의질점수day3, 
       c.삶의질점수 as 삶의질점수day8 ,
       a.SF12_PCS as SF12_PCSday1,
       b.SF12_PCS as SF12_PCSday3, 
       c.SF12_PCS as SF12_PCSday8,
       a.SF12_MCS as SF12_MCSday1,
       b.SF12_MCS as SF12_MCSday3, 
       c.SF12_MCS as SF12_MCSday8,
       a.EQ5D_Values as EQ5D_Valuesday1,
       b.EQ5D_Values as EQ5D_Valuesday3, 
       c.EQ5D_Values as EQ5D_Valuesday8 

       FROM ( select * from 삶의질2 WHERE visit='D1') a INNER JOIN (select * from 삶의질2 WHERE visit='D3') b 
                ON a.[대상자.등록번호] =b.[대상자.등록번호]  INNER JOIN 
            ( select * from 삶의질2 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]   ")


미병삶의질<-sqldf("select * from 삶의질C a inner join 미병 b on a.ids =b.ids")

head(미병삶의질)
names(미병삶의질)
summary()
미병삶의질<-미병삶의질[,-c(14)]

미병지수그룹case1_day별차이<-sqldf("select * from 미병지수그룹case1 a inner join 미병삶의질 b on a.대상자등록번호_D1=b.ids")
write.csv(미병지수그룹case1_day별차이,"c:/미병지수그룹case1_day별차이.csv")






#상관분석 시작

미병지수회복그룹<- read.xlsx("D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터/20150416_미병지수그룹_회복그릅_pmy.xlsx"
                , sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.

head(미병지수회복그룹)

#NA값을 생략하고 출력
미병지수회복그룹<-na.omit(미병지수회복그룹)

#미병삶의질 frame을엑셀로 올려서 작업하고 다시 내림.
write.csv(미병삶의질,"c:/미병삶의질.csv")

미병삶의질2<-read.csv("c:/미병삶의질.csv")
head(미병삶의질2)


joineddataSQL<-sqldf("select * from 미병삶의질2 a inner join (select * from 미병지수회복그룹 where case구분='Case') b on a.ids=b.ids")

joineddataSQL2<-joineddataSQL[,c(18:31)]


joineddataSQL2<-tbl_df(joineddataSQL2)


head(joineddataSQL2)



전체2번전체D1_D3<-select(joineddataSQL2,c(2,4,6,8))



전체2번<-filter(joineddataSQL2,미병지수그룹==1 )
전체2번D1_3 <-select(전체2번,c(2,4,6,8))


##상관성 분석
library(corrplot)
library(psych)

a<-cor(전체2번전체D1_D3)
a<-cor(전체2번D1_3)

분석3번47명 <-filter(joineddataSQL2,미병지수그룹==1 & 회복그룹==1 )


분석3번47명<-select(분석3번47명,c(3,5,7,9))

a<-cor(분석3번47명)


filter(joineddataSQL2,미병지수그룹==1 & 회복그룹==1)




head(미병삶의질2)


select * from 
