## Text mining with the R package tm
library(tm)
library(stringr)

## get a sample (10 documents) of the Reuters dataset (comes with package tm)
#reut21578 <- system.file("texts", "crude", package = "tm")
#
#reuters <- Corpus(DirSource(reut21578), 
#	readerControl = list(reader = readReut21578XML))


### download reuters21578 data first (use first 1000 documents; 1984/85)
file <- "reut2-000.xml" 
reuters <- Corpus(ReutersSource(file), readerControl = list(reader = readReut21578XML))

tags <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/dataset_ubicomp2013_tags.txt",header = F,sep = "\t",colClasses = "character")    
str(tags)
tags$V1<- as.numeric(tags$V1)
tags$V2<- str_replace_all(tags$V2,pattern = ",",replacement = " ")
usuarioTips<- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/usuarioTipsmas1.csv",header = F)
str(usuarioTips)
length(unique(usuarioTips$V2))
idVenues<-unique(usuarioTips$V2)
inIdVenues <- tags$V1  %in% idVenues
tagsCompleted<-subset(tags, inIdVenues, select = V2)

reuters<- Corpus(DataframeSource(tagsCompleted))
reuters
reuters[[2]]

## Convert to Plain Text Documents
reuters <- tm_map(reuters, PlainTextDocument)
reuters[[2]]

## Convert to Lower Case
reuters <- tm_map(reuters, tolower)
reuters[[2]]

## Remove Stopwords
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[2]]

## Remove Punctuations
reuters <- tm_map(reuters, removePunctuation)
reuters[[2]]

## Stemming
reuters <- tm_map(reuters, stemDocument)
reuters[[2]]

## Remove Numbers
reuters <- tm_map(reuters, removeNumbers)
reuters[[2]]


## Eliminating Extra White Spaces
reuters <- tm_map(reuters, stripWhitespace)
reuters[[2]]

reuters <- tm_map(reuters, PlainTextDocument)
## create a term document matrix
dtm <- DocumentTermMatrix(reuters)
inspect(dtm)

freq<-findFreqTerms(dtm, lowfreq =5)
inspect(DocumentTermMatrix(reuters, list(dictionary = freq)))
inspect(removeSparseTerms(dtm, 0.001))
findAssocs(dtm, c("food","dinner"), .1)
#washington  secretari  political     reagan republican      white      regan 
#      1.00       0.49       0.46       0.45       0.45       0.42       0.41 
#staff strategist 
#0.41       0.41 


dtmMas5<-DocumentTermMatrix(reuters, list(dictionary = freq))
## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10, 1:100])
findAssocs(dtmMas5,"dogs",0.2)
library(wordcloud)
m <- as.matrix(dtm)
word.freq <- sort(colSums(m),decreasing = T)
wordcloud(words = names(word.freq),freq = word.freq, min.freq = 5,random.order = F)
## do document clustering
tdm2<- removeSparseTerms(dtm,sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(t(m2)))
fit <- hclust(distMatrix,method ="ward.D")
plot(fit)
set.seed(122)
k<-3
kmeansResult <- kmeans(m2,k)
library(fpc)

pamResult <- pamk(m2)
k<- pamResult$nc
pamResult <- pamResult$pamobject
for (i in 1:k) {
    cat("cluster ",i," : ",colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)],"\n" )
}
layout(matrix(c(1,2),1,2))
plot(pamResult, col.p= pamResult$clustering)


### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
m <- as.matrix(dtm)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
sum(!complete.cases(m_norm))
m_norm <-m_norm[complete.cases(m_norm),]
m_norm
dim(m_norm)

### cluster into 10 clusters
cl <- kmeans(m_norm, 100)
cl

table(cl$cluster)

### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)

findFreqTerms(dtm[cl$cluster==1], 50)
inspect(reuters[which(cl$cluster==2)])

cluster2<-reuters[which(cl$cluster==2)]
lapply(inspect(cluster2), as.character)

cluster10<-reuters[which(cl$cluster==10)]
lapply(inspect(cluster1), as.character)


## hierarchical clustering
library(proxy)

### this is going to take 4-ever (O(n^2))
d <- dist(m, method="cosine")
hc <- hclust(d, method="average")
plot(hc)

cl <- cutree(hc, 50)
table(cl)
plot(cl)
findFreqTerms(dtm[cl==1], 50)

