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
미병EQ5D<-미병EQ5D[,c("flagM","EQ5D_Values")]

head(미병EQ5D)
names(미병EQ5D)

fileName<-"경주_EQ5D.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
경주EQ5D<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
경주EQ5D$flagM<-c("경주")
경주EQ5D<-경주EQ5D[,c("flagM","EQ5D")]
head(경주EQ5D)


fileName<-"갤럽_EQ5D.xlsx"
dirFile<-paste(workDir,fileName,sep="/")
갤럽EQ5D<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
갤럽EQ5D$flagM<-c("갤럽")
갤럽EQ5D<-갤럽EQ5D[,c("flagM","EQ5D_value")]
head(갤럽EQ5D)


fileName<-"국건영2013EQ5D (1).xlsx"
dirFile<-paste(workDir,fileName,sep="/")
국건영EQ5D<- read.xlsx(dirFile, sheetIndex=1 ,encoding="UTF-8", stringsAsFactors=FALSE) ##또는 sheetIndex 대신 sheetName=”abcd” 등으로 불러와야한다.
국건영EQ5D$flagM<-c("국건영")
국건영EQ5D<-국건영EQ5D[,c("flagM","Predicted.Values")]
head(국건영EQ5D)


colnames(미병EQ5D)[2]<-"EQ5Ds"
colnames(경주EQ5D)[2]<-"EQ5Ds"
colnames(갤럽EQ5D)[2]<-"EQ5Ds"
colnames(국건영EQ5D)[2]<-"EQ5Ds"

미병EQ5D$EQ5Ds<-as.numeric(미병EQ5D$EQ5Ds)
미병EQ5D<-미병EQ5D[미병EQ5D$EQ5Ds<2,]
quantile
quantile(미병EQ5D$EQ5Ds)

summary(국건영EQ5D)
summary(미병EQ5D)
summary(경주EQ5D)
summary(갤럽EQ5D)


total<-rbind(미병EQ5D,경주EQ5D,국건영EQ5D,갤럽EQ5D)

shapiro.test(미병EQ5D$EQ5Ds)

shapiro.test(국건영EQ5D$EQ5Ds)

shapiro.test(경주EQ5D$EQ5Ds)
shapiro.test(갤럽EQ5D$EQ5Ds)

par(mfrow=c(2,2))
hist(미병EQ5D$EQ5Ds)
hist(국건영EQ5D$EQ5Ds)
hist(경주EQ5D$EQ5Ds)
hist(갤럽EQ5D$EQ5Ds)

summary(total)
boxplot(total$EQ5Ds~total$flagM)

multi.hist(total$EQ5Ds,freq=TRUE)
multi.hist(미병EQ5D$EQ5Ds,freq=TRUE)



t<-scale(미병EQ5D$EQ5Ds, center = TRUE, scale = TRUE)
summary(t)
boxplot(t)

미병EQ5D$EQ5Ds_norm<-scale(미병EQ5D$EQ5Ds)
국건영EQ5D$EQ5Ds_norm<-scale(국건영EQ5D$EQ5Ds)
경주EQ5D$EQ5Ds_norm<-scale(경주EQ5D$EQ5Ds)
갤럽EQ5D$EQ5Ds_norm<-scale(갤럽EQ5D$EQ5Ds)


total<-rbind(미병EQ5D,경주EQ5D,국건영EQ5D,갤럽EQ5D)



par(mfrow=c(1,1))
boxplot(total$EQ5Ds_norm~total$flagM)


summary(국건영EQ5D)
summary(미병EQ5D)
summary(경주EQ5D)
summary(갤럽EQ5D)


shapiro.test(미병EQ5D$EQ5Ds_norm)

shapiro.test(국건영EQ5D$EQ5Ds_norm)

shapiro.test(경주EQ5D$EQ5Ds_norm)
shapiro.test(갤럽EQ5D$EQ5Ds_norm)

x<-미병EQ5D$EQ5Ds_norm
normalized미병 = (x-min(x))/(max(x)-min(x))

hist(normalized미병)

hist(x)

#log transformation
x<-미병EQ5D$EQ5Ds
hist(x)
x.abs<-abs(미병EQ5D$EQ5Ds-1)

x.tran<-log(x,base)
hist(x.tran)

x.square<-sqrt(sqrt(x))
hist(x.square)


#
x.signedlogTran<-signedlog10(x)

hist(x.signedlogTran)
#normalization

#Example Data
x = sample(-100:100,50)

hist(x)
boxplot(x)

#Normalized Data

normalized = (x-min(x))/(max(x)-min(x))

#Histogram of example data and normalized data
par(mfrow=c(1,2))
hist(x,xlab="Data",col="lightblue",main="")
hist(normalized,xlab="Normalized Data",col="lightblue",main="")

shapiro.test(x)
shapiro.test(normalized)


#power transform to normalize the skewed-to-the-right distributions
# on positive axis
#[Reference] : Myung-Hoe Huh, 「허명회 칼럼 - 기운 분포의 정규화를 위한 거듭 곱 변환 
#(power transform to normalize a skewed-to-the-right distribution on positive values)」 
#http://www.statground.org/?mid=board_KwLN83&document_srl=2526.
prob.5 <- c(0.1,0.25,0.5,0.75,0.9)
base <- exp(1) # for common log, put "base <- 10"

# simulated data 1
set.seed(1)+
x <- rexp(1000,1)
hist(x)

# simuated data 2
set.seed(12)
x <- rlnorm(1000,1,10)

x = (x-min(x))/(max(x)-min(x))
summary(normalized)

hist(x)
boxplot(x)
x.t<-scale(x)
hist(x.t)
#x<-total$EQ5Ds
x<-abs(total$EQ5Ds-1)

x.tran<-log(x,base)

x.log<-log(x,base)
hist(x)
hist(x.log)
# optimization module
qnorm.5 <- qnorm(prob.5)
q.x <- quantile(x, prob=prob.5,na.rm=TRUE)

f <- function(p, q.x, prob.5=c(0.1,0.25,0.5,0.75,0.9)){
  fit <- lm(q.x^p ~ qnorm.5)
  rss <- sum(fit$resid^2)/var(q.x^p)
  return(rss)
}

p.star <- optimize(f, c(0.1,1), q.x, prob=prob.5)$minimum

p.star <- round(p.star, 2); p.star

# optimal transform
if(p.star > 0.1) x.transformed <- x^p.star
if(p.star <= 0.1) x.transformed <- log(x, base)

p.star.1 <- ifelse(p.star> 0.1, p.star, 0)
p.star.1

# histograms before and after
windows(height=4,width=9)
par(mfrow=c(1,2))
hist(x, nclass=20, xlab="x", main="not transformed")
hist(x.transformed, nclass=20, xlab="x transformed", main=paste("power =",p.star.1))
x.transformed
# savePlot(filename="exponential",type="png")
#[Reference] : Myung-Hoe Huh, 「허명회 칼럼 - 기운 분포의 정규화를 위한 거듭 곱 변환 (power transform to normalize a skewed-to-the-right distribution on positive values)」 http://www.statground.org/?mid=board_KwLN83&document_srl=2526.

hist(x)
x.tran<-log(x,base)
hist(x.tran)
x<-x.transformed

normalized = (x-min(x))/(max(x)-min(x))
hist(normalized)


quantile(x)
boxplot(x)
quantile(normalized)
boxplot(normalized)


## 40대만 EQ5D의 quantile 보여주기
# 엑셀파일 reading

library(dplyr)

df<-tbl_df(갤럽EQ5D)
str(df)
colnames(df)
a<-select(filter(df,age>=40 & age<50),id,age,EQ5D_VAS)
a<-a$EQ5D_VAS

summary(a[,2:3])
quantile(a$EQ5D_VAS)

boxplot(a)

text(0.7, median(a), "median")
text(0.7, quantile(a)[2], "Q1")
text(0.7, quantile(a)[4], "Q3")
text(0.7, fivenum(a)[4] + 1.5*IQR(a), "Q3+1.5*IQR----")
text(0.7, fivenum(a)[2] - 1.5*IQR(a), "Q1-1.5*IQR-------")


mean()
df경주<-tbl_df(경주EQ5D)
str(df경주)


b<-select(filter(df경주,age>=40 & age<50),age,건강수준_EQ5D_VAS)
quantile(b,na.rm=TRUE)
summary(b)
boxplot(b)
text(0.7, median(b), "median")
text(0.7, quantile(b)[2], "Q1")
text(0.7, quantile(b)[4], "Q3")
text(0.7, fivenum(b)[4] + 1.5*IQR(a), "Q3+1.5*IQR----")
text(0.7, fivenum(b)[2] - 1.5*IQR(a), "Q1-1.5*IQR-------")




#

