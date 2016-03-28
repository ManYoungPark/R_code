library(RCurl)
URL <- "http://www.pjreddie.com/media/files/mnist_train.csv"
x <- getURL(URL)
## Or 
## x <- getURL(URL, ssl.verifypeer = FALSE)
train <- read.csv(textConnection(x))

URL <- "http://www.pjreddie.com/media/files/mnist_test.csv"
x <- getURL(URL)
## Or 
## x <- getURL(URL, ssl.verifypeer = FALSE)
test <- read.csv(textConnection(x))

train.mat <- data.matrix(train)
test.mat <- data.matrix(test)

y.train <- as.factor(train.mat[,1])
image.train <- train.mat[, -1]
y.test <- as.factor(test.mat[,1])
image.test <- test.mat[, -1]

image.train <- image.train/255
image.test <- image.test/255

outnode.train <- model.matrix( ~ y.train -1)
outnode.test <- model.matrix( ~ y.test -1)
#http://stackoverflow.com/questions/5048638/automatically-expanding-an-r-factor-into-a-collection-of-1-0-indicator-variables

library(deepnet)

rm(test); rm(test.mat); rm(train); rm(train.mat)

t.start <- Sys.time()
nn <- dbn.dnn.train(image.train,outnode.train, 
                    hidden=c(500,500,250,125),
                    output="softmax",
                    batchsize=100, numepochs=100, learningrate = 0.1) 0.1)
#deepnet01.RData
t.end <- Sys.time()

train.pred <- nn.predict(nn, image.train)
train.pred.num <- apply(train.pred, 1, function(x) which(max(x)==x))-1

sum(y.train==train.pred.num)/length(train.pred.num)

test.pred <- nn.predict(nn, image.test)
test.pred.num <- apply(test.pred, 1, function(x) which(max(x)==x))-1

sum(y.test==test.pred.num)/length(test.pred.num)
#save(nn, file="deepnet01.RData")
#  test error 2%, train 0.004%

