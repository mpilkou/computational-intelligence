# Dimacs CNF

## 1
#a
45*678

#b
x<-c(7,4,2,0,9)
y<-c(2,1,5,3,3)

#c
x+y

#d
x*y

#e
A = matrix(c(0, 1, 5, 2, 6, 0, 1, 4, 3), nrow = 3,ncol = 3)
B = matrix(c(9, 8, 7, 1, 2, 7, 4, 9, 2), nrow = 3,ncol = 3,byrow = TRUE)
A%*%B

#f
suma <- function(a,b) {
  return (a+b)
}
suma(1,2)

#g
save.image()
load(".RData")

## 2
#a
getwd()
setwd('C:/Users/matt/Documents/IntObl/zaj1/')

#b
MyData <- read.csv(file="osoby.csv", header=TRUE, sep=",")
MyData
#c
MyData['imie']

#d
#MyData['plec'] == 'k'
#MyData[MyData['plec'] == 'k']
subset(MyData, plec=='k')

#e
myNewData <- subset(MyData, plec=='m' & wiek> 50)
write.csv(myNewData, file = "osoby2.csv")

## 3
#a
newColumn <- sample(2000.00:5000.00,7)
newColumn

MyData

newColumn <- runif(7, 2000.00, 5000.00)
newColumn

MyData['wplata'] = newColumn


#b
newOsoba <- data.frame("imie" = "Jan", "nazwisko" = "Kowalski", "plec" = "m","wiek" = 99,"wplata" = runif(1, 2000.00, 5000.00))
MyData
class(MyData)
newOsoba
class(newOsoba)




#tmp <- cbind(MyData, newOsoba)
tmp <- rbind(MyData, newOsoba)
tmp

#c
#sr ar
colMeans(tmp['wplata'])

#SD(tmp['wplata'])
apply(tmp['wplata'], 2, sd, na.rm = TRUE)

min(tmp['wplata'])
max(tmp['wplata'])



# https://www.datamentor.io/r-programming/data-frame/
# https://inf.ug.edu.pl/~gmadejsk/io.html


