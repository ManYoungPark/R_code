library(kmeans)
library(stats)
iris2 <- iris                    # 새로운 변수를 설정
iris2$Species <- NULL   # 5번째 컬럼의 종을 제거; 실제로 클러스터링이 잘 작동하는지 확인하기 위해서
(kmeans.result <- kmeans(iris2, 3))            # kmeans 알고리즘으로 클러스터링 3개를 만드는 것을 수행함. 양끝에 괄호가 있어서 바로 수행 결과를 확인 가능

summary(iris2)
table(iris$Species, kmeans.result$cluster)  # 실제로 클러스터링 결과를 비교해보고자 테이블을 생성하여 비교 - 

kmeans.result
(kmeans.result <- kmeans(iris2, 3))

iris3<-iris

iris3$clus<-factor(kmeans.result$cluster)

summary(iris3)
kmeans.result$withinss

plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)   #
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col= 1:3, pch=8, cex=2)




set.seed(2835)

idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx, ]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method = "ave")

hc <- hclust(dist(iris3), method = "ave")

plot(hc, hang = -1, labels = iris$Species[idx])

# cut tree into 3 clusters
rect.hclust(hc, k = 3)
groups <- cutree(hc, k = 3)


library(fpc)
iris2 <- iris[-5]  # remove class tags
ds <- dbscan(iris2, eps = 0.42, MinPts = 5)
# compare clusters with original class labels
table(ds$cluster, iris$Species)
