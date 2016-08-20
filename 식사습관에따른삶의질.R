
setwd("D:/KIOM/프로젝트문서들/국민건강영양조사/갤럽_식사끼니에따른삶의질")

require(xlsx)

df<-read.csv("gallup_alls.csv")

df[is.na(df$q2),]$q2<-0

summary(df$q65)
df2<-transform(df
               ,d2=factor(d2,levels=c(1:6),labels=c("Unmarried","Married","Bereavement","Divorced","Seperated","Etc"))
               ,d4=factor(d4,levels=c(1:5),labels=c("Elementary school & under","Middle school","High school","University graduate","Graduate school & over"))
               ,d7=factor(d7,levels=c(1:15),labels=c("<49","50-99","100-149","150-199","200-249","250-299"
                                                     ,"300-349","350-399","400-499","500-599","600-699","700-799"
                                                     ,"800-899","900-999",">1000"))
               
               ,sex=factor(sex,levels=c(1:2),labels=c("Male","Female"))
               ,KS15=factor(KS15,levels=c(1:3),labels=c("Taeeumgin","Soeumin","Soyangin"))
               ,q2=factor(q2,levels=c(1,2,3,0),labels=c("Everyday","Sometimes","Have ever","Never"))
               ,q3=factor(q3,levels=c(1:3),labels=c("Everyday","Have ever","Never"))
               ,q14_1_cnts=factor(q14_1_cnts)

               ,q42_1=factor(q42_1,levels=(1:4),labels=c("Everday","4 - 5 times a week","2 - 3 times a week","0 - 1 times a week"))
               ,q42_2=factor(q42_2,levels=(1:4),labels=c("Everday","4 - 5 times a week","2 - 3 times a week","0 - 1 times a week"))
               ,q42_3=factor(q42_3,levels=(1:4),labels=c("Everday","4 - 5 times a week","2 - 3 times a week","0 - 1 times a week"))
               ,q65=factor(q65,levels=c(1:3),labels=c("Healthy group","Mibyeong group","Disease group"))
               
              # ,mi_group=factor(mi_group,levels=c(1:3),labels=c("Healthy","Mibyeong1","Mibeyong2"))
              )


summary(df2$q2)
require(dplyr)
str(df2)
names(df2)





df3<-select(df2,age,sex,q2,q3,q4,q4_1_1,q6,d7,d2,d4,q42_1,q42_2,q42_3,q65,q14_1_cnts,KS15,mi_score,mi_group,q27)

summary(df3)
names(df3)<-c("Age","Sex","Smoking","Alcohol_consumption","Having_physical_excercise","Exercise_hour_a_day","Having_physical_excercise_hardly",
              "Household_income","Marital_status","Education_level","Breakfast","Lunch","Dinner","Mibyeong_Check_bySelf","Number_of_diseases",
              "Constitution","Mibyeongscore","MibyeongGroup","QOL")


# revalue it
tmp<-as.character(df3$Household_income)
tmp2<-ifelse((tmp=="500-599" |tmp=="600-699" | tmp=="700-799" | tmp=="800-899"| tmp=="900-999"| tmp==">1000") ,">500",tmp)
tmp3<-factor(tmp2,levels=c("<49","50-99", "100-149", "150-199", "200-249", "250-299", "300-349", "350-399", "400-499",">500"))
df3$Household_income<-tmp3



tmp<-as.numeric(df3$Number_of_diseases)-1 #reason for -1 is because  value converting character to numeric start 1 not 0.


tmp2<-ifelse(tmp>3,">3",tmp)
tmp3<-factor(tmp2,levels=c("0","1","2","3",">3"))
df3$Number_of_diseases<-tmp3





#df3$Mibyeong_Check_bySelf<-df3$Mibyeong_Check_bySelf-1
#df3$MibyeongGroup<-df3$MibyeongGroup-1
#upside code is same as below actually. 
#df3[df3$Mibyeong_Check_bySelf==3,]$Mibyeong_Check_bySelf<-2
#df3[df3$MibyeongGroup==3,]$MibyeongGroup<-2




#다중 회귀분석.
summary(df3)
rslt<-lm(미병점수~.,data=df3)
rslt<-lm(QOL~.,data=df3)
rslt<-lm(QOL~Sex+Age,data=df3)
rslt<-lm(QOL~Age,data=df3)
rslt<-step(lm(QOL~.,data=df3),df3,direction="both")
write.csv(df3,"qol.csv")

summary.aov(rslt)
anova(rslt)
summary(rslt)

par(mfrow=c(2,2))
plot(rslt)

### aov() 와 anova()차이 확인..


aov(Sepal.Width~Species, data=iris)

analysis<-aov(Sepal.Width~Species, data=iris)
#anova(Sepal.Width~Species, data=iris) # 이건 안됨. 분산분석은 aov로 해야함.


anova(analysis)
summary(analysis)
tmp<-TukeyHSD(analysis)
ztable(tmp)
#case군 control군 분리
##3 ways of Conditional Element Selection

## 아침 식사군과 결식군으로 나눈 케이스
#1) 아침을 전혀 안먹는 군(결식군), 점심, 저녁 전부 식사함.
#df3$flag<-factor(with(df3,ifelse(Breakfast==1 & Lunch==1 & Dinner==1 ,1,ifelse(Breakfast==4 & Lunch==1 & Dinner==1,0,9)))) #

#) 주 2-3회 먹는다는 사람도 결식자로 포함해보기.
#df3$flag<-factor(with(df3,ifelse(Breakfast==1 & Lunch==1 & Dinner==1 ,1,ifelse((Breakfast==4 |Breakfast==3) & Lunch==1 & Dinner==1,0,9))))

# 1) 미병군과 정상군에서 삶의질이 차이가 있는지 보자.
df3$flag<-factor(with(df3,ifelse(Breakfast==1 & Lunch==1 & Dinner==1 ,1,ifelse((Breakfast==3) & Lunch==1 & Dinner==1,0,9)))) 



summary(df3)   
# 아래코드는 conditional selection 수행을 trasnform 방법을 넣어서 해본거임. 위코드는 아래 코드나 같은것임. 선택해서 쓰면 됨.
#df4<-transform(df3,flags3=factor(ifelse(Breakfast==1 & Lunch==1 & Dinner==1 ,1,ifelse(Breakfast==4 & Lunch==1 & Dinner==1,0,9))))

# end of 아침 식사군과 결식군으로 나눈 케이스


summary(df4)

df3_1<-filter(df3,flag==1 | flag==0)
summary(df3_1)

df3_1$flag<-droplevels(df3_1$flag)
levels(df3_1$Mibyeong_Check_bySelf)
summary(df3_1)

require(moonBook)
mytable(flag~.,data=df3_1)
summary(df3$MibyeongGroup)
out<-mytable(Mibyeong_Check_bySelf~.-MibyeongGroup-Constitution,data=df3)
mytable(Sex~.-MibyeongGroup-Constitution,data=df3)
mytable(Breakfast~.-MibyeongGroup-Constitution,data=df3)
mycsv(out,file="table1.csv")


mytable(Constitution~.,data=df3)


#spearman rank correlation saving into the dataframe.
#rslt<-cor.test(as.numeric(df3$Smoking),df3$QOL,method = "spearman")

x<-df3
df_rslt<-data.frame(vnames=character(),rs=numeric(),pValue=numeric())

for(VRnms in names(x))
{
  
  #VRnms<-'Age'
  tmp1<-paste0("cor.test(as.numeric(x$",VRnms,"),x$QOL,method = 'spearman')")
  
  rslts<-NULL
  rslts<-eval(parse(text=tmp1))
  
  print(rslts)
  
  tmp2<-data.frame(vnames=VRnms,rs=rslts$estimate,pValue=rslts$p.value)
  df_rslt<-rbind(df_rslt,tmp2)
  
}
write.csv(df_rslt,file="table2.csv")

# end of spearman rank cor...




#PS matching
write.csv(df3_1,"breakfast.csv")
install.packages("MatchIt")
require(MatchIt)

m.out <-matchit(formula = flag ~ Sex+Exercise+ Education_level+Constitution ,ratio=1, data = df3_1, method = "nearest")
m.out <-matchit(formula = flag ~ Age+Sex+Exercise+Marital_status+ Education_level+Constitution ,ratio=1, data = df3_1, method = "exact")


matched <-match.data(m.out)

write.csv(matched,"matched_QOL_exact.csv")
chisq.test(matched$flag,matched$Marital_status)
chisq.test(matched$flag,matched$Education_level)
chisq.test(matched$flag,matched$Exercise)
chisq.test(matched$flag,matched$Constitution)

xtabs(~df3_1$Marital_status+df3_1$flag)
xtabs(~matched$Education_level+matched$flag)

t.test(matched$QOL~matched$flag)
t.test(matched$Mibyeongscore~matched$flag)

t.test(df3_1$QOL~df3_1$flag)
t.test(df3_1$Mibyeongscore~df3_1$flag)

names(matched)

resultfinal<-glm(Mibyeong_Check_bySelf~Age+Sex+Exercise+Marital_status+ Education_level+Constitution,data=matched,family=binomial)
summary(resultfinal)
exp(coef(resultfinal))
exp(confint(resultfinal))

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

a2=ROC(form=MibyeongGroup~QOL,data=df3_1,plot="ROC") #QOL 76이 best cut point임.
a1=ROC(form=Mibyeong_Check_bySelf~QOL,data=df3_1,plot="ROC")

head(df3_1)  

summary(df3_1_Soeumin)
df3_1_Soeumin$Mibyeong_Check_bySelf <-factor(df3_1_Soeumin$Mibyeong_Check_bySelf)
head(radial)
plot_ROC(a1,a2,a3,show.sens=TRUE)
plot_ROC(a1,a2,show.sens=TRUE)

summary(df3_1)
result=step_ROC(Mibyeong_Check_bySelf~Mibyeongscore+Age,data=df3_1,plot=FALSE)
result=step_ROC(male~height+weight+TC,data=radial,plot=FALSE)
summary(radial)
plot_ROC(result$initial,result$final,show.lr.eta=FALSE,show.sens=TRUE,type=1)




#QOL best cut point=76를 이용하여, 76이상이면, 삶의질이 좋다. 76이하이면 삶의질이 떨어진다.
#logistic 을 수행.

df3_1$QOL_flag<-with(df3_1,ifelse(QOL<76,1,0)) #76이하면 삶의질이 안좋으니깐, 1로 코딩.
#설명변수 

head(df3_1)

summary(df3_1)

rslt<-glm(QOL_flag~Age+Sex+flag,data=df3_1,family = binomial)
summary(rslt)

write.csv(df3_1,"QOL_flag.csv")


########## 테이블 만들기 예제#######

install.packages("moonBook")
require(moonBook)  # 패키지불러오기 
data(acs)          # 데이타 불러오기        
mytable(Dx~age+sex,data=acs)

res=mytable(Dx~.,data=acs)
myhtml(res)

out=mytable(sex~age+Dx,data=acs)
for(i in c(3,5)) 
  mylatex(out,size=i,caption=paste("Table ",i,". Fontsize=",i,sep=""))

mycsv(out,file="test.csv")

require(ztable)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))

weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
summary(lm.D9)
ztable(lm.D9)

