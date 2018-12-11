
#install.packages("genalg")
library(genalg)

#duzyProblemPlecakowy <-data.frame(wartosc = sample(10:100,30), waga = sample(10:100,30))

duzyProblemPlecakowy <-data.frame(wartosc = sample(10:100,30), waga = sample(10:100,30))

duzyProblemPlecakowy

duzyLimit <- 600

duzyLimit

fitnessFunc2 <-function(chr) {
  
    calkowita_wartosc_chr <-
      chr %*% duzyProblemPlecakowy$wartosc
    
    calkowita_waga_chr <- 
      chr %*% duzyProblemPlecakowy$waga
    if (calkowita_waga_chr > duzyLimit)
      return(0) else return(-calkowita_wartosc_chr)
}

duzyPlecakGenAlg <-
    rbga.bin(size = 30, popSize = 200, iters = 50,mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc2)

duzyPlecakGenAlg

chartData <-data.frame(srednia = 
               -duzyPlecakGenAlg$mean, maksymalne = 
               -duzyPlecakGenAlg$best)

chartData

plot(chartData$maksymalne,xlab="pokolenie",ylab="fitness (ocena)",type="l",col="red", main = "Dzialanie Alg. Genetycznego")

lines(chartData$srednia,col="blue")

legend("bottomright",legend = c("srednia","maksymalnie"),lty=c(1,1),col=c("blue","red"))





