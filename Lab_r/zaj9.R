setwd('C:/Users/matt/Documents/IntObl/zaj9/')

# z1
df <-data.frame(
  wiek =     c(23,25,28,22,46,50,48),
  waga =     c(75,67,120,65,70,68,97),
  wzrost =   c(176,180,175,165,187,180,178),
  gra =      c(TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE)
)

#siatk.predict <- compute(siatk.nn, siatk.dane[1:3])
forwardPass <- function(wiek, waga, wzrost) {
  hidden1 <-  wiek*(-0.46122) + 
              waga*(0.97314) + 
              wzrost*(-0.39203) +
              0.80109
  hidden1 <- 1/(1+exp(-hidden1))
  hidden2 <-  wiek*(0.78548) + 
              waga*(2.10584) + 
              wzrost*(-0.57847) +
              (0.43529)
  hidden2 <- 1/(1+exp(-hidden2))
  output <-   hidden1*(-0.81546) +
              hidden2*(1.03775) +
              (-0.2368)
  return(output)
}

forwardPass(23,75,176)


# z 2

#data(iris)
#data <- iris
#data(iris)
normalize(1)
iris.norm <- scale(iris)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))


#trainset = iris[ind == 1,] testset = iris[ind == 2,]
#max = apply(data , 2 , max)
#min = apply(data, 2 , min)
#scaled = as.data.frame(scale(data, center = min, scale = max - min))
iris.scaled <- iris[1:4]
iris.scaled
maxs <- apply(iris.scaled, 2, max) 
mins <- apply(iris.scaled, 2, min)
iris.scaled <- as.data.frame(scale(iris.scaled, center = mins, scale = maxs - mins))
iris.scaled$setosa = 0
iris.scaled$versicolor = 0
iris.scaled$virginica = 0

ind <-  sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
ind
iris.norm.training <-iris.scaled[ind==1,1:7]
iris.norm.test <-iris.scaled[ind==2,1:7]
iris.norm.test

#install.packages("neuralnet")
library(neuralnet)
#iris.scaled

network <- neuralnet(versicolor + virginica + setosa~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris.norm.training, hidden=3)
network


compute(network,iris.norm.test[1:4])




