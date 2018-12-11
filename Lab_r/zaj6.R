setwd('C:/Users/matt/Documents/IntObl/zaj6/')

# 1
iris

iris.norm <- data.frame("Sepal.Length" = scale(iris$Sepal.Length), 
                        "Sepal.Width" = scale(iris$Sepal.Width), 
                        "Petal.Length" = scale(iris$Petal.Length),
                        "Petal.Width" = scale(iris$Petal.Width),
                        "Species"     = iris$Species)
iris.norm

ind <-  sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.norm.training <-iris.norm[ind==1,1:5]
iris.norm.test <-iris.norm[ind==2,1:5]
iris.norm.test

#install.packages("class")
library("class")

knn.3<-knn(iris.norm.training[,1:4], iris.norm.test[,1:4], cl=iris.norm.training[,5], k = 3, prob=FALSE)
knn.3

predicted <-knn.3
real <-iris.norm.test[,5]
conf.matrix <-table(predicted,real)
accuracy <-sum(diag(conf.matrix))/sum(conf.matrix)

conf.matrix
accuracy

# 2



