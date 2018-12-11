setwd('C:/Users/matt/Documents/IntObl/zaj8/')

df <-data.frame(
                maslo = c(TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE),
                chleb = c(TRUE,TRUE,TRUE ,FALSE,TRUE, TRUE, TRUE,TRUE ,TRUE,TRUE),
                ser =   c(FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE),
                piwo =   c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
                czipsy =   c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE)
                )

df
a = 1
# z0
df
A1 = 0
B1 = 0
N=10
for (a in 1:N) {
    if (df[a,"maslo"] && df[a,"chleb"] && df[a,"ser"]) {
      A1 = A1 + 1
    }
    if(df[a,"maslo"]&&df[a,"chleb"]){
      B1 = B1+1
    }
    
}
#A1: „Jeśli klient kupuje masło i chleb, to kupuje też ser.”
#A1: {masło=TRUE, chleb=TRUE} => {ser=TRUE}
#Support (czyli: wsparcie)
A1/10
#Confidence (czyli: wiarygodność)
A1/B1

load("~/IntObl/zaj8/titanic.raw.rdata")
str(titanic.raw)
#install.packages("arules")
library("arules")
#load("arules")
rules <- apriori(titanic.raw)
inspect(rules)
rules <- apriori(titanic.raw,
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"),
                 control = list(verbose=F))
rules
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)

subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
#install.packages("arulesViz")
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))



titanic.raw[titanic.raw$Survived == "Yes",]
titanic.raw$Sex

titanic.raw[1,"Sex"]

sum = 0
men = 0
vmen = 0
for(i in 1:1201){
  if(titanic.raw[i,"Survived"] == "No"){
    if(titanic.raw[i,"Sex"] == "Male"){
      men = men + 1
    }
    if(titanic.raw[i,"Sex"] == "Female"){
      vmen = vmen + 1
    }
    sum = sum + 1
  }
}
men/sum
vmen/sum



