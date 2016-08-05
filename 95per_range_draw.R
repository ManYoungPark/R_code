x1 <- c(33:93)


summary(tmpdata2)
obesity_degree<-tmpdata2$a135
obesity_degree<-tmpdata2$a135
obesity_degree<-tmpdata2$a135
obesity_degree_all<-mb_tmp_data$a135
tmpdata

hist(obesity_degree,freq=FALSE)

lines(density(obesity_degree))

plot(density(obesity_degree),main="Obesity degree distribution of healthy")
plot(density(obesity_degree_all),main="Obesity degree distribution mibyeong group")
abline(v=90, col="blue", lty=3)
abline(v=122 , col="red", lty=2)
text(127, 0.003, labels = "------->")


mcnemar.test