library(tm)
library(Matrix)
library(RWeka)
library(rgl)
tips <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/tipsCluster.csv",header = T,sep = ";",colClasses = "character")    
tips$random<- as.numeric(tips$random)

all_text <- Corpus(VectorSource(tips$Tips))
dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                              removeNumbers=TRUE,stopwords=TRUE,
                                              stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))

dtm <- removeSparseTerms(dtm,0.99)
df_unigram<-Matrix(as.matrix(dtm),sparse=T)
df_unigram<-as.data.frame(as.matrix(dtm))
colnames(df_unigram)<- paste("uni_",colnames(df_unigram),sep="")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtmBigram<- DocumentTermMatrix(all_text,control=list(tokenize = BigramTokenizer,
                                                    tolower=TRUE,removePunctuation=TRUE,
                                                    removeNumbers=TRUE,stopwords=TRUE,
                                                    stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))

dtmBigram <- removeSparseTerms(dtmBigram,0.99)
df_bigram <-Matrix(as.matrix(dtmBigram),sparse=T)
df_bigram <-as.data.frame(as.matrix(dtmBigram))
colnames(df_bigram) <- paste("bi_",colnames(df_bigram),sep="")

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtmTrigram<- DocumentTermMatrix(all_text,control=list(tokenize = TrigramTokenizer,
                                                      tolower=TRUE,removePunctuation=TRUE,
                                                      removeNumbers=TRUE,stopwords=TRUE,
                                                      stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))


dtmTrigram <- removeSparseTerms(dtmTrigram,0.99)
df_trigram<-Matrix(as.matrix(dtmTrigram),sparse=T)
df_trigram<-as.data.frame(as.matrix(df_trigram))
colnames(df_trigram)=paste("tri_",colnames(df_trigram),sep="")

df_total <- cbind(df_unigram,df_bigram,df_trigram)
str(df_total)
head(df_total,n=1)
df_total$random<- tips$random
df_total$clase<- tips$clase
df_total$user <- tips$User
df_total$item <- tips$Item

df_total[73,]
tips[73,]
