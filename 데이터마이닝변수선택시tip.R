##데이터 마이닝 변수 선택시 tip


# ##분산이 0에 가까운 컬럼들 확인
nearZeroVar(mb_tmp1_1, saveMetrics = TRUE)
v<-linear.correlation(X1_세포내액~. , data=subset(mb_tmp1_1,select = -c(미병통합그룹1)))
a<-cutoff.k(v,10)
a<-findCorrelation(cor(subset(mb_tmp1_1,select = -c(미병통합그룹1))))

cor(mb_tmp1_1[-length(mb_tmp1_1)])
M<-cor(mb_tmp1_1[-length(mb_tmp1_1)])
M<-cor(mb_tmp1_1[c(1:15)])
install.packages("corrplot")
library(corrplot)
corrplot(M, method="number")

#정규화 하는 방법 
ltmp.scale<-cbind(scale(ltmp[-length(ltmp)]),ltmp[length(ltmp)])  