
names(mb_tmp_data_tmp2_1)

mb_tmp_data_tmp2_2<-select(mb_tmp_data_tmp2_1,matches("a[0-9]"),Gender,미병그룹recode)
mb_tmp_data_tmp2_2$Gender<-factor(mb_tmp_data_tmp2_2$Gender)

mb_tmp_data_tmp2_2.male<-filter(mb_tmp_data_tmp2_2,Gender==1)
mb_tmp_data_tmp2_2.female<-filter(mb_tmp_data_tmp2_2,Gender==2)

mb_tmp_data_tmp3<-mb_tmp_data_tmp2_2 #전체
mb_tmp_data_tmp3<-mb_tmp_data_tmp2_2.male #남자
mb_tmp_data_tmp3<-mb_tmp_data_tmp2_2.female #여자





summary(mb_tmp_data_tmp3)

df_sub<-data.frame(variable=character(),Healthy=character(),Mibyeong=character())
df_sub_up<-data.frame(variable=character(),Healthy=character(),Mibyeong=character())

names(mb_tmp_data_tmp3)
#95% 이하 범위 밖으로 하한.. -2는 필요없는 컬럼을계산 안하기 위함 Gender, 미병그룹
for(i in 1:(ncol(mb_tmp_data_tmp3)-2))
{
  
  mb_tmp_data_tmp3_sub<-mb_tmp_data_tmp3[mb_tmp_data_tmp3[i]=='out of 95% lower',c(i,ncol(mb_tmp_data_tmp3))]  
  print(tb<-table(mb_tmp_data_tmp3_sub))
  tbtmp<-data.frame(variable=names(mb_tmp_data_tmp3[i]),Healthy=tb[1,1],Mibyeong=tb[1,2],flag="out of 95% lower")
  df_sub<-rbind(df_sub,tbtmp)
  
  mb_tmp_data_tmp3_sub<-mb_tmp_data_tmp3[mb_tmp_data_tmp3[i]=='out of 95% upper',c(i,ncol(mb_tmp_data_tmp3))]  
  print(tb<-table(mb_tmp_data_tmp3_sub))
  tbtmp<-data.frame(variable=names(mb_tmp_data_tmp3[i]),Healthy=tb[3,1],Mibyeong=tb[3,2],flag="out of 95% upper")
  
  df_sub_up<-rbind(df_sub_up,tbtmp)
}

df_sub_sum_male<-rbind(df_sub_up,df_sub)
df_sub_sum_female<-rbind(df_sub_up,df_sub)
df_sub_sum$Gender<-"total"
df_sub_sum_female$Gender<-"Female"


df_sub_sum<-rbind(df_sub_sum,df_sub_sum_female,df_sub_sum_male)
df_sub_sum$Gender<-factor(df_sub_sum$Gender)
summary(df_sub_sum)




df_sub_sum[df_sub_sum==0]<-1
df_sub_sum$multiple<-df_sub_sum$Mibyeong/(df_sub_sum$Healthy*4)

df_sub_fullNm<-sqldf("select * from df_sub_sum a inner join mappingVR b on a.variable=b.shortName")
write.csv(df_sub_fullNm,"outof_95.csv")



library(plyr)


ggplot(df_sub_fullNm,aes(x=reorder(fullName,multiple),y=multiple,fill=Gender))+geom_bar(stat="identity")+
  facet_grid(flag~.)+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  theme_classic()+
  theme_bw() +labs(y="", x="")+theme(axis.text.x = element_text(angle = 60, hjust = 1))


  








dn.female<-ggplot(df_sub_fullNm,aes(x=reorder(fullName,multiple),y=multiple))+geom_bar(stat = "identity")+
  geom_text(aes(y=multiple, label=round(multiple,1)),colour="white",size=4,hjust=1.3)+
  coord_flip()+
  ggtitle("여성 하위 95% 범위밖의 미병분 비율")+
  theme_bw() +labs(y="", x="")


#95% 이하 범위 밖으로 하한.. 



df_sub_up[df_sub_up==0]<-1
df_sub_up$multiple<-df_sub_up$Mibyeong/(df_sub_up$Healthy*4)

df_sub_fullNm<-sqldf("select * from df_sub_up a inner join mappingVR b on a.variable=b.shortName")


up.female<-ggplot(df_sub_fullNm,aes(x=reorder(fullName,multiple),y=multiple))+geom_bar(stat = "identity")+
  geom_text(aes(y=multiple, label=round(multiple,1)),colour="white",size=4,hjust=1.3)+
  coord_flip()+
  ggtitle("여성 상위 95% 범위밖의 미병분 비율")+
  theme_bw() +labs(y="", x="")


library(gridExtra)
grid.arrange(dn,up,dn.male,up.male,dn.female,up.female,nrow=3,ncol=2)



#건강군 변수들의 range 95% 범위 밖에 있는 미병군들의 특성 분석


tmpdata<-select(mb_tmp_data_tmp3,matches("^a."))
names(tmpdata)<-df_sub_fullNm$fullName

summary(tmpdata)

tmpdata_total<-data.frame()
for(i in 1:ncol(tmpdata))
{

  tmpdata_1<-tmpdata[tmpdata[i]=='out of 95% lower',]
  n=dim(tmpdata_1)[1]
  tmpdata_3<-round((colSums(tmpdata_1=='out of 95% lower')/n),2)
  tmpdata_total<-rbind(tmpdata_total,tmpdata_3)
}




names(tmpdata_total)<-names(tmpdata)
row.names(tmpdata_total)<-names(tmpdata)
tmpdata_total

library(d3heatmap)
url <- "http://datasets.flowingdata.com/ppg2008.csv"
nba_players <- read.csv(url, row.names = 1)
nba_players$tmp<-rep(-2:-20,len=NROW(nba_players))
class(nba_players)
names(nba_players)

d3heatmap(nba_players, scale = "column")

d3heatmap(tmpdata_total, scale = "column",yaxis_font_size	=10,xaxis_font_size	=10)

hm.female<-d3heatmap(tmpdata_total, scale = "column", 
          color = scales::col_quantile("Blues", NULL, 3))

d3heatmap(tmpdata_total, colors = "Blues", scale = "col",
          dendrogram = "row", k_row = 3)

library(d3heatmap)
d3heatmap(tmpdata_total, colors = "Blues", scale = "col",
          dendrogram = "row", k_row = 3)
par(mfrow=c(2,2))

str(tmpdata_total)

hist(c(1:10))

grid.arrange(hm.male,hm.female,nrow=2,ncol=1)


as.data.frame(rbind(tmpdata_2,tmpdata_3))

row.names()


summary(tmpdata_1)
names(tmpdata)

mb_tmp_data_tmp3_sub1<-mb_tmp_data_tmp3[mb_tmp_data_tmp3$a10=='out of 95% lower',]



mb_tmp_data_tmp3_sub1<-mb_tmp_data_tmp3[mb_tmp_data_tmp3$a10_a7=='out of 95% lower',]
mb_tmp_data_tmp3_sub1<-mb_tmp_data_tmp3[mb_tmp_data_tmp3$a13_16=='out of 95% lower',]

names(mb_tmp_data_tmp3_sub1)

dim(mb_tmp_data_tmp3_sub1)








summary(mb_tmp_data_tmp3_sub1)

summary(mb_tmp_data_tmp3)

names(mb_tmp_data_tmp3_sub1)

summary(mb_tmp_data_tmp3_sub1$a13_16)
summary(mb_tmp_data_tmp3_sub1$a10)

sm<-summary(mb_tmp_data_tmp3_sub1[,c(17,24,25,26)])

aa<-aply(iris$Sepal.Length, iris$Species, mean)

tapply(iris$Sepal.Length, iris$Species, mean)

apply(iris[,1:4],2,sum)

sum(iris[,2])


colMeans(iris[,1:4])

summary(iris)

ddply(iris,.(Species),summarize,hap=sum(Sepal.Length))

  aa[2]
class(aa)


sm[2,3]
class(sm)
smt<-as.data.frame(sm)

sm[1,2]
summary(mb_tmp_data_tmp3_sub1)


names(mb_tmp_data_tmp3_sub1)
tapply(df[7], df$team, NROW)
aat<-tapply(mb_tmp_data_tmp3_sub1$a10, mb_tmp_data_tmp3_sub1$a10, NROW)

aat<-rbind(aat,aat)
as.data.frame(aat)
class(aat)

summary(mb_tmp_data_tmp3_sub1)
ddply(mb_tmp_data_tmp3_sub1,.(a10),summarize,cnt=mean(미병점수))

set.seed(1)
d <- data.frame(year = rep(2000:2002, each = 3),
                count = round(runif(9, 0, 20)))
print(d)

ddply(d, "year", function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count/mean.count
  data.frame(cv.count = cv)
})

summary(mb_tmp_data_tmp3_sub1)
ddply(d, "year", transform, total.count = sum(count))
ddply(d, "year", summarise, mean.count = mean(count))

d$year<-factor(d$year)
summary(d)
ddply(mb_tmp_data_tmp3_sub1, "a28_a7", summarise, total.count = mean(미병점수))






##################필요없는 코드들..

geom_text(aes(label=ifelse(percent >= 0.07, "여",""),
              y=pos), colour="white") +
  ggplot(data=df_sub_melt,aes(x=reorder(변수,value),y=value,fill=variable))+ 
  
  
  df_sub$flag<-ifelse(df_sub$Mibyeong1>df_sub$Healthy*3 | df_sub$Mibyeong2>df_sub$Healthy*2 ,"need to check","don't need")

df_subT<-df_sub[1:4]

df_subT<-df_subT[order(-df_subT$Mibyeong1),]




names(mb_tmp_data_tmp3)

names(df_subT)[1]<-'변수'
df_sub_melt<-melt(df_subT,id.vars = "변수")



bar<-ggplot(df_sub_melt, aes(x = 변수, y = value, fill = variable)) 

ggplot(data=df_sub_melt,aes(x=reorder(변수,value),y=value,fill=variable))+ 
  geom_bar(stat='identity',position='fill')+ 
  scale_fill_brewer(palette='YlGnBu')+ 
  coord_flip()+
  labs(y="", x="")


ggplot(df.summary, aes(x=reorder(Brand,USD,function(x)+sum(x)), y=percent, fill=Category)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  
  geom_text(aes(label=ifelse(percent >= 0.07, "여",""),
                y=pos), colour="white") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(y="", x="")




write.csv(df_sub_melt,"c://df_sub_melt.csv")





