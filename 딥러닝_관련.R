# 예제 데이터 생성
Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)

# 결과 데이터 생성
y <- c(rep(1, 50), rep(0, 50))

install.packages("deepnet")
library("deepnet")
# 노드 5개, 레이어 2개를 exDnn에 학습

exDnn <- dbn.dnn.train(x, y, hidden = c(5, 5))


Var1 <- c(rnorm(1000, 1, 0.5), rnorm(1000, -0.6, 0.2))
Var2 <- c(rnorm(1000, -0.8, 0.2), rnorm(1000, 2, 1))
x <- matrix(c(Var1, Var2), nrow = 2000, ncol = 2)
y <-c(rep(1,1000),rep(0,1000))
exDnn <- dbn.dnn.train(x, v1, hidden = c(1000, 1000,1000,1000))



-------
  input = matrix(c(0,0,1,1,0,1,0,1), ncol=2)
output = matrix(c(0,1,1,0), ncol=1)
nn <- nn.train(input, output, hidden=c(2),numepoch=10000, learningrate=10)
# hidden은 hidden layer의 노드 수

nn.predict(nn,input)
