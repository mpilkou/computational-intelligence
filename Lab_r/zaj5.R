
setwd('C:/Users/matt/Documents/IntObl/zaj5/')
iris

iris[order(iris$Species),]

myPredictRow <- function(sl,sw,pl,pw) {
    if (pw<1.0) {
      return("setosa")
    } else {
      if (pl<4.5) {
        return("versicolor")
      } else {
        return("virginica")
      }
    }
}

myPredict <- function() {
  sr <- 0
  for (i in 1:150) {
    if(iris[i,5] == myPredictRow(iris[i,1],iris[i,2],iris[i,3],iris[i,4])){
      sr <- sr+1
    }
  }
  sr <- (sr/150)
  return(sr)
}

sr <- myPredict()
sr


install.packages("party")
library("party")

set.seed(1234)
ind <-  sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <-iris[ind==1,1:5]
iris.test <-iris[ind==2,1:5]
iris.ctree <-ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris.training)
print(iris.ctree)
plot(iris.ctree)
plot(iris.ctree, type="simple")



myNewPredict <- function() {
  a <- iris.test$Species
  b <- predict(iris.ctree, iris.test[,1:4])

  sr <- 0
  for (i in 1:40){
    if(a[i] == b[i]){
      sr = sr+1
    }
  }
  return(sr/40)
}

myNewPredict()

tab <- table(predicted,real)
tab[0,0]

predicted <-predict(iris.ctree, iris.test[,1:4])
real <-iris.test[,5]
tab <- table(predicted,real)
tab[1,2]

myError <- function() {
  predicted <-predict(iris.ctree, iris.test[,1:4])
  real <-iris.test[,5]
  tab <- table(predicted,real)
  
  true_data <- 0
  false_data <- 0
  
  for (i in 1:3){
    for (j in 1:3){  
        if(i == j){
          true_data = tab[i,i] + true_data
        }else {
          false_data = false_data + tab[i,j]
        }
    }
  }
  return (c(true_data,false_data))
}
wynik <- myError()

wynik[1]/(wynik[1]+wynik[2])

