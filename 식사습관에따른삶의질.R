
setwd("C:/Users/Administrator/Downloads")

require(xlsx)

df<-read.csv("gallup_alls.csv")

df2<-transform(df
               ,d2=factor(d2,levels=c(1:6),labels=c("Unmarried","Married","Bereavement","Divorced","Seperated","Etc"))
               ,d4=factor(d4,levels=c(1:5),labels=c("Elementary school & under","Middle school","High school","University graduate","Graduate school & over"))
               ,d7=factor(d7,levels=c(1:15),labels=c("<49","50-99","100-149","150-199","200-249","250-299"
                                                     ,"300-349","350-399","400-499","500-599","600-699","600-799"
                                                     ,"800-899","900-999",">1000"))
               #,q65=factor(q65,levels=c(1:3),labels=c("Healthy","Mibyeong","Disease"))
               ,sex=factor(sex,levels=c(1:2),labels=c("Male","Female"))
               ,KS15=factor(KS15,levels=c(1:3),labels=c("Taeyangin","Soeumin","Soyangin"))
              # ,mi_group=factor(mi_group,levels=c(1:3),labels=c("Healthy","Mibyeong1","Mibeyong2"))
              )

install.packages("dplyr")
require(dplyr)
str(df2)
names(df2)



df3<-select(df2,age,sex,q4_1_1,d7,d2,d4,q27,q42_1,q42_2,q42_3,q65,KS15,mi_score,mi_group)

names(df3)<-c("Age","Sex","Exercise","Household_income","Marital_status","Education_level","QOL","Breakfast","Lunch","Dinner","Mibyeong_Check_bySelf","Constitution","Mibyeongscore","MibyeongGroup")
df3$Mibyeong_Check_bySelf<-df3$Mibyeong_Check_bySelf-1
df3$MibyeongGroup<-df3$MibyeongGroup-1

df3[df3$Mibyeong_Check_bySelf==3,]$Mibyeong_Check_bySelf<-2
df3[df3$MibyeongGroup==3,]$MibyeongGroup<-2





#다중 회귀분석.
summary(df3)
rslt<-lm(미병점수~.,data=df3)
rslt<-lm(QOL~.,data=df3)
rslt<-step(lm(QOL~.,data=df3),df3,direction="both")
write.csv(df3,"qol.csv")
anova(rslt)

summary(rslt)

par(mfrow=c(2,2))
plot(rslt)

#case군 control군 분리
##3 ways of Conditional Element Selection


df3$flag<-factor(with(df3,ifelse(Breakfast==1 & Lunch==1 & Dinner==1 ,1,ifelse(Breakfast==4 & Lunch==1 & Dinner==1,0,9))))
summary(df3)   

df4<-transform(df3,flags3=factor(ifelse(Breakfast==1 & Lunch==1 & Dinner==1 ,1,ifelse(Breakfast==4 & Lunch==1 & Dinner==1,0,9))))
summary(df4)

df3_1<-filter(df3,flag==1 | flag==0)

df3_1$flag<-droplevels(df3_1$flag)
levels(df3_1$Mibyeong_Check_bySelf)
summary(df3_1)


write.csv(df3_1,"breakfast.csv")
install.packages("MatchIt")
require(MatchIt)

m.out <-matchit(formula = flag ~ Age+Sex+Exercise+Marital_status+ Education_level+Constitution ,ratio=1, data = df3_1, method = "nearest")
m.out <-matchit(formula = flag ~ Age+Sex+Exercise+Marital_status+ Education_level+Constitution ,ratio=1, data = df3_1, method = "exact")


names(df3_1)
matched <-match.data(m.out)
chisq.test(matched$flag,matched$Marital_status)
chisq.test(matched$flag,matched$Education_level)

xtabs(~df3_1$Marital_status+df3_1$flag)
xtabs(~matched$Education_level+matched$flag)

t.test(matched$QOL~matched$flag)
t.test(matched$Mibyeongscore~matched$flag)

t.test(df3_1$QOL~df3_1$flag)
t.test(df3_1$Mibyeongscore~df3_1$flag)

names(matched)

df3_1_Taeyangin<-filter(df3_1,Constitution=="Taeyangin")
df3_1_Soeumin<-filter(df3_1,Constitution=="Soeumin")
df3_1_Soyangin<-filter(df3_1,Constitution=="Soyangin")

names(df3_1_Taeyangin)[11]<-"Mibyeong_Check_bySelf_Taeyangin"
names(df3_1_Soeumin)[11]<-"Mibyeong_Check_bySelf_Soeumin"
names(df3_1_Soyangin)[11]<-"Mibyeong_Check_bySelf_Soyangin"
#roc
install.packages("Epi")
install.packages("pROC")
install.packages("ztable")
install.packages("moonBook")
require(Epi)
require(pROC)
require(ztable)
require(moonBook)
source("ROC_sub.R")

head(df3_1_Taeyangin)
a1=ROC(form=Mibyeong_Check_bySelf_Taeyangin~Mibyeongscore,data=df3_1_Taeyangin,plot="ROC")
a2=ROC(form=Mibyeong_Check_bySelf_Soeumin~Mibyeongscore,data=df3_1_Soeumin,plot="ROC")
a3=ROC(form=Mibyeong_Check_bySelf_Soyangin~Mibyeongscore,data=df3_1_Soyangin,plot="ROC")
a4=ROC(form=Mibyeong_Check_bySelf~Mibyeongscore,data=df3_1,plot="ROC")
a1=ROC(form=male~height,data=radial,plot="ROC")
  

summary(df3_1_Soeumin)
df3_1_Soeumin$Mibyeong_Check_bySelf <-factor(df3_1_Soeumin$Mibyeong_Check_bySelf)
head(radial)
plot_ROC(a1,a2,a3,show.sens=TRUE)
plot_ROC(a4,show.sens=TRUE)
