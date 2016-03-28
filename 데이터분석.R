##수면박탈 데이터 분석
##2015-01-26 made by park man young

if(require(xlsx)==FALSE)
  install.packages("xlsx")  


install.packages("KoNLP")
install.packages("xlsx")
install.packages("rJava")

install.packages("XLConnect")
install.packages("sqldf")


library("XLConnect")
library("xlsx")

library("sqldf")

##디렉토리 지정시, \\ 또는 / 로 하면 디렉토리의 구분이된다.
workDir<-"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터"
setwd(workDir)
getwd()


fileName<-"01. 미래부 미병 코딩 통합자료_150206.xlsx"
dirFile<-paste(workDir,fileName,sep="/")

#mydata<- read.xlsx(dirFile, sheetIndex = 1,encoding="UTF-8") ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.

삶의질<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
미병<- read.xlsx(dirFile, sheetIndex=2 ,encoding="UTF-8") ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.

#mydata<-read.csv(dirFile,sep=",",header=T)
#mydata=readWorksheetFromFile(dirFile, sheetName="미병") 

삶의질<-삶의질[c(-1,-2),]
삶의질<-삶의질[,c(1,2,22,23,24,25)]
str(삶의질)


names(미병)
head(미병2)

미병<-미병[,c("대상자.등록번호","Visit","미병점수_총점","미병그룹")]

미병2<-subset(미병, 미병그룹==1|미병그룹==2|미병그룹==3)

mydata<-sqldf("select * from 미병 as a inner join 삶의질 as b on a.[대상자.등록번호]=b.[대상자.등록번호] and a.Visit=b.Visit")
mydata<-mydata[,c(1,2,3,4,7:10)]

names(mydata)


mydata$EQ5D_Values<-as.numeric(mydata$EQ5D_Values)
mydata$SF12_MCS<-as.numeric(mydata$SF12_MCS)
mydata$미병점수_총점<-as.numeric(mydata$미병점수_총점)
mydata$삶의질점수<-as.numeric(mydata$삶의질점수)
mydata$SF12_PCS<-as.numeric(mydata$SF12_PCS)



str(mydata)

sm<-summary(my_qol)
out1<-as.matrix(sm)
write.csv(out1,"D:\\KIOM\\프로젝트문서들\\미래부_수면박탈_미병\\데이터\\abc_out.csv")
pairs(mydata[,c(3,5:7)])


#위 두개의 파일을 조인시켜서 다시 LOADING시킴

head(mydata)
summary(mydata)

str(mydata)


if(require(psych)==FALSE)
  install.packages("psych")  

library(psych)

attach(mydata) 
names(mydata)

mydata
mydata2<-mydata[,c("미병점수_총점","미병그룹","삶의질점수","SF12_PCS","SF12_MCS","EQ5D_Values","미병점수_총점")]
mydata3<-mydata[,c("미병점수_총점","삶의질점수","SF12_PCS","SF12_MCS","EQ5D_Values","미병점수_총점")]


dc<-describe(mydata3)

colnames(mydata2)
names(mydata2)

#히스토그램을 그려봅시다.
pairs(mydata1)
multi.hist(mydata3,freq=TRUE)
multi.hist(mydata1)
pairs.panels(mydata1)
c<-cor(mydata3)
t<-paste(workDir,"tmp.csv",sep="/")
write.csv(c,paste(workDir,"tmp.csv",sep='/')) 



#그룹별로 나눠보기
mydata2$미병그룹 <- factor(mydata2$미병그룹, levels=c(1,2,3), labels=c("건강군","미병1","미병2"))

boxplot(mydata2[,1]~mydata2[,c("미병그룹")])
write.csv(mydata2,paste(workDir,"tmp2.csv",sep='/')) 
names(mydata2)



head(mydata2)

par(mfrow=c(2,2))

boxplot(mydata2$삶의질점수 ~ mydata2$미병그룹, 
     col = 'pink',
     xlab = "미병그룹", ylab = "삶의질점수",
     main = "미병그룹간 삶의질점수")



boxplot(mydata2$SF12_PCS ~ mydata2$미병그룹, 
        col = 'pink',
        xlab = "미병그룹", ylab = "SF12_PCS",
        main = "미병그룹간 SF_12PCS점수")



boxplot(mydata2$SF12_MCS ~ mydata2$미병그룹, 
        col = 'pink',
        xlab = "미병그룹", ylab = "SF12_MCS",
        main = "미병그룹간 SF12_MCS점수")



boxplot(mydata2$EQ5D_Values ~ mydata2$미병그룹, 
        col = 'pink',
        xlab = "미병그룹", ylab = "EQ5D점수",
        main = "미병그룹간 EQ5D점수")


boxplot(mydata2$미병점수_총점 ~ mydata2$미병그룹, 
        col = 'pink',
        xlab = "미병그룹", ylab = "EQ5D점수",
        main = "미병그룹간 EQ5D점수")


describe(mydata2$미병점수_총점)
#1,3,8day 
mydata=sqlQuery(conn,'select * from 미병_삶의질점수JOIN')
library(psych)
names(mydata)

mydata4<-mydata[,c("대상자.등록번호","Visit","삶의질점수","SF12_PCS","SF12_MCS","EQ5D_Values","미병점수_총점","미병그룹")]
attach(mydata4)
Visit


par(mfrow=c(3,2))



interaction.plot(Visit, 대상자.등록번호, SF12_MCS, col=c(1:20), legend=F,xlab="방문날짜",ylab="SF12_MCS점수",main="Day1,3,8일간에 SF12_MCS변화") 
interaction.plot(Visit, 대상자.등록번호, 삶의질점수, col=c(1:20), legend=F,xlab="방문날짜",ylab="삶의질점수",main="Day1,3,8일간에 삶의질점수 변화") 
interaction.plot(Visit, 대상자.등록번호, SF12_PCS, col=c(1:20), legend=F,xlab="방문날짜",ylab="SF12_PCS",main="Day1,3,8일간에 SF12_PCS 변화") 
interaction.plot(Visit, 대상자.등록번호, EQ5D_Values, col=c(1:20), legend=F,xlab="방문날짜",ylab="EQ5D_VALUES",main="Day1,3,8일간에 EQ5D_VALUES 변화") 
interaction.plot(Visit, 대상자.등록번호, 미병점수_총점 , col=c(1:20), legend=F,xlab="방문날짜",ylab="미병점수",main="Day1,3,8일간에 미병점수 변화") 


install.packages("lattice")

library(lattice)


factor(Visit)
levels(Visit)

str(mydata)

mydata4[Visit=="D1"]

mydata4


subset(mydata4,Visit=="D1")$SF12_PCS

mydata5<-sqldf(" 
 SELECT a.[삶의질점수] as 삶의질점수1,a.SF12_PCS as SF12_PCS1,a.SF12_MCS as SF12_MCS1,a.EQ5D_VALUES as EQ5D_VALUES1,
 b.[삶의질점수] as 삶의질점수3,b.SF12_PCS  as SF12_PCS3,b.SF12_MCS as SF12_MCS3,b.EQ5D_VALUES as EQ5D_VALUES3
  ,c.[삶의질점수] as 삶의질점수8,c.SF12_PCS as SF12_PC8,c.SF12_MCS SF12_MCS8,c.EQ5D_VALUES as EQ5D_VALUES8,
a.미병점수_총점 as 미병점수_총점1,b.미병점수_총점 as 미병점수_총점3,c.미병점수_총점 as 미병점수_총점8
 FROM ( select * from mydata4 WHERE visit='D1') a INNER JOIN 
 (select * from mydata4 WHERE visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]  INNER JOIN 
  ( select * from mydata4 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]	 ")


names(mydata5)

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

summary(mydata5$삶의질점수3)
summary(mydata5$삶의질점수8)


t.test(mydata5$SF12_PCS1,mydata5$SF12_PCS8, paired=TRUE)
t.test(mydata5$SF12_MCS1,mydata5$SF12_MCS8, paired=TRUE)
t.test(mydata5$EQ5D_VALUES1,mydata5$EQ5D_VALUES8, paired=TRUE)
t.test(mydata5$삶의질점수1,mydata5$삶의질점수8, paired=TRUE)
t.test(mydata5$미병점수_총점1,mydata5$미병점수_총점8, paired=TRUE)


summary(mydata5)



xyplot(SF12_MCS ~ Visit | 대상자.등록번호, data=test)




       , 
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") },
       as.table=T
       ,main = "Day1,3,8일간에 SF12_MCS변화")




xyplot(삶의질점수 ~ Visit | 대상자.등록번호, data=mydata2, 
         panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") },
        as.table=T
       ,main = "Day1,3,8일간에 삶의질점수 변화")


xyplot(SF12_PCS ~ visit | 대상자등록번호, data=mydata2, 
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") },
       as.table=T
       ,main = "Day1,3,8일간에 SF12_PCS 변화")


xyplot(EQ5D_VALUES ~ visit | 대상자등록번호, data=mydata2, 
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") },
       as.table=T
       ,main = "Day1,3,8일간에 EQ5D_VALUES 변화")


mydata2$flag <- factor(mydata2$visit, levels=c("D1","D3","D8"), labels=c("1","2","3"))



class(mydata2$flag)
print(mydata2)


mydata2

data<-rnorm(c(1:10))


write.csv(mydata2,paste(workDir,"tmp3.csv",sep='/')) 





##2015-02-12 미병점수 재 계산

workDir<-"D:/KIOM/프로젝트문서들/미래부_수면박탈_미병/데이터"
setwd(workDir)
fileName<-"01. 미래부 미병 코딩 통합자료_150206_a만영재코딩_회복여부만가중치.xlsx"
dirFile<-paste(workDir,fileName,sep="/")

#mydata<- read.xlsx(dirFile, sheetIndex = 1,encoding="UTF-8") ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
                                         
삶의질<- read.xlsx(dirFile, sheetIndex=1 ,stringsAsFactors=F,encoding="UTF-8") ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.

미병<- read.xlsx(dirFile, sheetIndex=2 ,stringsAsFactors=F,encoding="UTF-8") ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.

미병<-subset(미병,미병총점>=0 & 미병총점<100)


p_weight1=tapply(미병$신체점수,미병$Visit,mean)
p_weight1_5=tapply(미병$신체점수,미병$Visit,mean)
p_weight2=tapply(미병$신체점수,미병$Visit,mean)
p_weight2_5=tapply(미병$신체점수,미병$Visit,mean)
p_weight3=tapply(미병$신체점수,미병$Visit,mean)
p_weight3_5=tapply(미병$신체점수,미병$Visit,mean)
p_weight4=tapply(미병$신체점수,미병$Visit,mean)


m_weight1=tapply(미병$정신점수,미병$Visit,mean)
m_weight1_5=tapply(미병$정신점수,미병$Visit,mean)
m_weight2=tapply(미병$정신점수,미병$Visit,mean)
m_weight2_5=tapply(미병$정신점수,미병$Visit,mean)
m_weight3=tapply(미병$정신점수,미병$Visit,mean)
m_weight3_5=tapply(미병$정신점수,미병$Visit,mean)
m_weight4=tapply(미병$정신점수,미병$Visit,mean)


t_weight1=tapply(미병$미병총점,미병$Visit,mean)
t_weight1_5=tapply(미병$미병총점,미병$Visit,mean)
t_weight2=tapply(미병$미병총점,미병$Visit,mean)
t_weight2_5=tapply(미병$미병총점,미병$Visit,mean)
t_weight3=tapply(미병$미병총점,미병$Visit,mean)
t_weight3_5=tapply(미병$미병총점,미병$Visit,mean)
t_weight4=tapply(미병$미병총점,미병$Visit,mean)

t<-data.frame(rbind(p_weight1,p_weight1_5,p_weight2,p_weight2_5,p_weight3,p_weight3_5,p_weight4))
t<-data.frame(rbind(m_weight1,m_weight1_5,m_weight2,m_weight2_5,m_weight3,m_weight3_5,m_weight4))
t<-data.frame(rbind(t_weight1,t_weight1_5,t_weight2,t_weight2_5,t_weight3,t_weight3_5,t_weight4))
t<-data.frame(rbind(weight2,weight3,weight4))

names(미병)
sqldf("select visit,count(*) from  미병   group by visit")
#sqldf("select visit,count(*) from  미병 where 미병총점=0  group by visit")

a<-sqldf("select visit,count(*) from  미병  group by visit")
a<-sqldf("select count(*) from  미병 where 미병_피로여부=2 group by visit")
a<-sqldf("select visit,count(*) from  미병 where 미병_피로여부=2 group by visit")
a<-sqldf("select visit,count(*) from  미병 where 미병_피로지속=1 and 미병_피로회복=1 group by visit")

b<-sqldf("select visit,count(*) from  미병 where 미병_통증여부=2 group by visit")
b<-sqldf("select visit,count(*) from  미병 where 미병_통증지속=1 and 미병_통증회복=1 group by visit")

c<-sqldf("select visit,count(*) from  미병 where 미병_수면장애여부=2  group by visit")
c<-sqldf("select visit,count(*) from  미병 where 미병_수면장애지속=1 AND 미병_수면장애회복=1  group by visit")

d<-sqldf("select visit,count(*) from  미병 where 미병_소화장애여부=2  group by visit")
d<-sqldf("select visit,count(*) from  미병 where 미병_소화장애지속=1 and 미병_소화장애회복=1  group by visit")



e<-sqldf("select visit,count(*) from  미병 where 미병_우울여부=2   group by visit")
e<-sqldf("select visit,count(*) from  미병 where 미병_우울지속=1 and 미병_우울회복=1   group by visit")

f<-sqldf("select visit,count(*) from  미병 where 미병_분노여부=2   group by visit")
f<-sqldf("select visit,count(*) from  미병 where 미병_분노지속=1 and 미병_분노회복=1  group by visit")

g<-sqldf("select visit,count(*) from  미병 where 미병_불안여부=2 group by visit")
g<-sqldf("select visit,count(*) from  미병 where 미병_불안지속=1 and 미병_불안회복=1 group by visit")

a
rbind(a,b,c,d,e,f,g)


mydata5<-sqldf(" 
 SELECT a.신체점수 as 미병총점D1,b.신체점수 as 미병총점D3,c.신체점수 as 미병총점D8
 FROM ( select * from 미병 WHERE visit='D1') a INNER JOIN 
 (select * from 미병 WHERE visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]  INNER JOIN 
  ( select * from 미병 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]   ")


t.test(mydata5$미병총점D1,mydata5$미병총점D3, paired=TRUE)
t.test(mydata5$미병총점D3,mydata5$미병총점D8, paired=TRUE)




names(삶의질)
삶의질<-삶의질[c(-1,-2),]
삶의질<-삶의질[,c(1,2,22,23,24,25)]

str(삶의질)

names(미병)
미병<-미병[,c(1,2,56,57,58)]
str(삶의질)



mydata<-sqldf("select * from 미병 as a inner join 삶의질 as b on a.[대상자.등록번호]=b.[대상자.등록번호] and a.Visit=b.Visit")
  

str(mydata)

mydata$EQ5D_Values<-as.numeric(mydata$EQ5D_Values)
mydata$SF12_MCS<-as.double(mydata$SF12_MCS)
mydata$삶의질점수<-as.numeric(mydata$삶의질점수)
mydata$SF12_PCS<-as.numeric(mydata$SF12_PCS)

names(mydata)

mydata<-mydata[,c(4,5,8,9,10,11)]



mydata1<-subset(mydata,Visit=='D8')
require(psych)
names(mydata)
str(mydata)
pairs.panels(mydata,pch=21,lm=FALSE)
pairs(mydata)

aa<-data.frame(cor(mydata))

out1<-as.matrix(aa)
write.csv(out1,"D:\\KIOM\\프로젝트문서들\\미래부_수면박탈_미병\\데이터\\abc_out.csv")


str(mydata)

names(mydata)


######################
#필요 없는 코드##
#######################



#미병계산자료와 통합자료를 join하기 위해 엑셀에서 읽어와 DB로 보낸후, 
#DB에서 join 수해후 다시 불러들임. 2015.01.28
require(RODBC)
fileName<-"미병 점수계산식_원본.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
mydata<- read.xlsx(dirFile, sheetIndex = 2,encoding="UTF-8")
conn<-odbcConnect('미병DB',uid='sa',pwd='leo0515')
#전처리를 위해 DB로 보내기
sqlSave(conn,mydata,tablename='미병점수',safer=FALSE,verbose=TRUE)

#
fileName<-"미래부 미병 코딩 통합자료_150121_삶의질_만영_excel.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
mydata<- read.xlsx(dirFile, sheetIndex = 1,encoding="UTF-8")
sqlSave(conn,mydata,tablename='삶의질점수',safer=FALSE,verbose=TRUE)



삶의질

sqldf("select * from 삶의질 WHERE visit='D8'")

##

conn<-odbcConnect('TEST',uid='sa',pwd='leo0515')

sqlSave(conn,삶의질)

mydata5<-sqldf(" 
 SELECT a.SF12_PCS as SF12_PCS1,b.SF12_PCS  as SF12_PCS3,c.SF12_PCS as SF12_PC8
               FROM ( select * from 삶의질 WHERE visit='D1') a INNER JOIN 
               (select * from 삶의질 WHERE visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]  INNER JOIN 
               ( select * from 삶의질 WHERE visit='D8') c on a.[대상자.등록번호] =c.[대상자.등록번호]   ")

head(삶의질)


mydata5<-sqldf(" 
 SELECT a.SF12_PCS as SF12_PCS1,b.SF12_PCS  as SF12_PCS3
               FROM ( select * from 삶의질 WHERE visit='D1') a INNER JOIN 
               (select * from 삶의질 WHERE Visit='D3') b ON a.[대상자.등록번호] =b.[대상자.등록번호]    ")


