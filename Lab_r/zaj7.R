setwd('C:/Users/matt/Documents/IntObl/zaj7/')

iris

iris.log <- log(iris[, 1:4])
ir.species <- iris[, 5]

#iris.scale <- scale(iris.log,center = TRUE)

iris.pca <- prcomp(iris.log, # iris.scale)
                 center = TRUE,
                 scale. = TRUE) 

iris.final <- predict(iris.pca)[,1:2]

iris.final

kmiris <- kmeans(iris.final,3)

kmiris$cluster
kmiris


list <- c()
for (i in 1:150) {
  if (iris$Species[i] == "setosa") {
    list[[i]] = 1
  }else if (iris$Species[i] == "versicolor") {
    list[[i]] = 2
  }
  else {
    list[[i]] = 3
  }
}
list

#iris.final[,1]
#iris.final[,2]
#plot(iris.final[,1],iris.final[,2],xlab="pokolenie",ylab="fitness (ocena)",col="red", main = "Kmeans")
#plot(iris.final[,1],iris.final[,2],xlab="pokolenie",ylab="fitness (ocena)",col="blue", main = "Kmeans")


plot(iris.final, col = kmiris$cluster)
points(kmiris$centers, col = 1:2, pch = 8, cex = 2)
