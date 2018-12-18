setwd('C:/Users/matt/Documents/IntObl/zaj11/')

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
#install.packages(Needed, dependencies = TRUE)

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")



cname <- file.path(".", "texts")   
dir(cname)
library(tm)
docs <- VCorpus(DirSource(cname))   
summary(docs) 


docs <- tm_map(docs,removePunctuation)

for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # This is an ascii character that did not translate, so it had to be removed.
}

docs <- tm_map(docs, removeNumbers)  


docs <- tm_map(docs, tolower)   


docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)


docs <- tm_map(docs, removeWords, c("syllogism", "tautology")) 

for (j in seq(docs)){
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}


docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, removeWords, c("syllogism", "tautology")) 

for (j in seq(docs)){
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))

# Store data
dtm <- DocumentTermMatrix(docs)   
dtm 
tdm <- TermDocumentMatrix(docs)   
tdm   

freq <- colSums(as.matrix(dtm))   
length(freq) 

ord <- order(freq)  

m <- as.matrix(dtm)   
dim(m)


# Focus
dtms <- removeSparseTerms(dtm, 0.2)
dtms


# Frequency
freq <- colSums(as.matrix(dtm))
head(table(freq), 20)
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))   
freq 
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

library(ggplot2) 

p <- ggplot(subset(wf, freq>25), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 


## Relationships Between Terms
findAssocs(dtm, c("country" , "american"), corlimit=0.85)

set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)  



# Clustering by Term Similarity
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
dtmss

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete")   # for a different look try substituting: method="ward.D"
fit  

plot(fit, hang=-1)  




plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red")
          

# K-mean
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

