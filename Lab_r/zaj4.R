# a
setwd('C:/Users/matt/Documents/IntObl/zaj4/')
dirty.iris <- read.csv("dirty_iris.csv", header=TRUE, sep=",")

dirty.iris

#nrow(subset(dirty.iris, !is.na(Sepal.Length) & !is.na(Sepal.Width) & !is.na(Petal.Length) & !is.na(Petal.Width), select = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))
nrow(subset(dirty.iris, is.finite(Sepal.Length) & is.finite(Sepal.Width) & is.finite(Petal.Length) & is.finite(Petal.Width), select = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))

nrow(na.omit(dirty.iris))

# b

#install.packages("editrules")
library(editrules)

E <- editset(c("Sepal.Length <= 30"))
E

ve <-violatedEdits(E, dirty.iris)
ve

summary(ve)
plot(ve)

# c
#subset(dirty.iris,Species == c(setosa, versicolor, virginica),select = c(Species))

E <- editset(c("Species %in% c('setosa', 'versicolor', 'virginica')",
               "Sepal.Length >= 0 "," Sepal.Width >=0 "," Petal.Length >=0 "," Petal.Width >= 0",
               "Petal.Length > (Petal.Width*2)",
               "Sepal.Length > Petal.Length",
               "Sepal.Width > Petal.Length"
               ))
E
ve <-violatedEdits(E, dirty.iris)
ve
