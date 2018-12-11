setwd('C:/Users/matt/Documents/IntObl/zaj10/')

# __________z1
# nosnik(support) - nie zerowe
# rdzen(core) - odcinek z jedynka
A <- function(x) {
  if (x>2 && x <= 4){
    return(0)
  }else {
    return(1)
  }
}

B <- function(x) {
  if (x<=1 || x >= 6){
    return(0)
  }else if (x>3 && x<=5) {
    return(1)
  } else if (x>1 && x<=3) {
    return((0.5*x) - 0.5)
  } else if (x>5 && x<=6) {
    return((-x) + 6)
  }
}

C <- function(x) {
  if (x<=0 || x >= 3){
    return(0)
  }else if (x>0 && x<=1) {
    return(1)
  } else if (x>1 && x<=3) {
    return(-(0.5*x) - 0.5)
  }
}
# A
Ac <- c()
for (i in 1:10){
  Ac[i] = A(i)
}
plot(1:10,Ac,type="l",col="red")

# B
Bc <- c()
for (i in 1:10){
  Bc[i] = B(i)
}
lines(1:10,Bc,type="l",col="green")
# C
Cc <- c()
for (i in 1:10){
  Cc[i] = C(i)
}
lines(1:10,Cc,type="l",col="blue")


AlubB <- c()
for (i in 1:10){
  if(A(i) > B(i)){
    AlubB[i] = A(i)
  } else {
    AlubB[i] = B(i)
  }
}

AiB <- c()
for (i in 1:10){
  if(A(i) > B(i)){
    AiB[i] = B(i)
  } else {
    AiB[i] = A(i)
  }
}
plot(1:10,AiB,type="l",col="green")
lines(1:10,AlubB,type="l",col="red")

# __________z2

# __________z3
#install.packages("sets")
library(sets)
sets_options("universe", seq(from = 0, to = 40, by = 0.1))

variables <- set(
  bmi =
    fuzzy_partition(varnames =
                      c(niedow = 9.25, zdro = 21.75,
                        nadw = 27.5, otyl = 35),
                    sd = 3.0),
  a1c =
    fuzzy_partition(varnames =
                      c(nisk = 4, norm = 5.25, wys = 7),
                    FUN = fuzzy_cone, radius = 5),
  rating =
    fuzzy_partition(varnames =
                      c(odm = 10, stand = 5, pref = 1),
                    FUN = fuzzy_cone, radius = 5),
  bp =
    fuzzy_partition(varnames =
                      c(norm = 0, mnadcis = 10, nadcis = 10,
                        dnadci = 30), sd = 2.5)
)

rules <-  set(
    fuzzy_rule(bmi %is% niedow || bmi %is% otyl || a1c %is% nisk, rating %is% odm),
    fuzzy_rule(bmi %is% nadw || a1c %is% nisk || bp %is% mnadcis, rating %is% stand),
    fuzzy_rule(bmi %is% zdro && a1c %is% norm && bp %is% norm, rating %is% pref)
  )

system <- fuzzy_system(variables, rules)

print(system)
plot(system) 

fi <- fuzzy_inference(system, list(bmi = 29, a1c=5, bp=10))
plot(fi)

gset_defuzzify(fi, "centroid")
