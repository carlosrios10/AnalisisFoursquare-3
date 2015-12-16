###It contains three files in tsv format, 
###including 3112 users and 3298 venues with 27149 check-ins and 10377 tips.
getwd()
tips <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/dataset_ubicomp2013_tips.txt",header = F,sep = "\t",colClasses = "character")    
str(tips)
library(plyr)
tipsUsers<- count( tips,c("V1","V2"))
names(tipsUsers)<-c("user","item","freq")
str(tipsUsers)
table(tipsUsers$freq)
length(unique(tipsUsers$user))
length(unique(tipsUsers$item))
sum(tipsUsers$freq)

### me quedo solo con los items con mas de 1 tips.
tipsUsersmas1<-tipsUsers[tipsUsers$freq>1,]
length(unique(tipsUsersmas1$user))
length(unique(tipsUsersmas1$item))
write.table(x = tipsUsersmas1,file ="../dataset_ubicomp2013/dataset_ubicomp2013/usuarioTipsmas1.csv",col.names = F,quote = F,row.names = F,sep=",")
write.table(x = tipsUsers,file ="../dataset_ubicomp2013/dataset_ubicomp2013/tipsUsers.csv",col.names = F,quote = F,row.names = F,sep=",")

tipsUser <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/usuarioTipsmas1.csv",header = F)
str(tipsUser)
summary(tipsUser)
sum(!complete.cases(tipsUser))
userFreq <- ddply(tipsUser,.(V1),summarise,cant=length(V1))
length(unique(tipsUser$V1))
tipsUser[tipsUser$V1==102,]
table(userFreq$cant)
melt<- dcast(tipsUser, V1  ~ variable)
matrix<- as.matrix.data.frame(tipsUser)
itemFreq <- ddply(tipsUser,.(V2),summarise,cant=length(V2))
table(itemFreq$cant)
### analisis de las similitudes
simCoseno<- read.csv(file = "../resultados/usersimlilitudCoseno.csv",header = F,sep="\t")
table(simCoseno$V3,useNA = "always")
hist(simCoseno$V3)
boxplot(simCoseno$V3)

overlap <- read.csv(file = "../resultados/scoringOverlap.csv",header = F,sep="\t")
table(overlap$V3,useNA = "always")



simEucl <- read.csv(file = "../resultados/similitudEuclideaUserUser.csv",header = F,sep="\t")
table(simEucl$V3)
simPear<- read.csv(file = "../resultados/similitudPearsonUserUser.csv",header = F,sep="\t")
table(simPear$V3)
simCosenoItem<-read.csv(file = "../resultados/ItemSimlilitudCoseno.csv",header = F,sep="\t")
table(simCosenoItem$V3)
hist(simCosenoItem$V3)
simTanimotoItem<-read.csv(file = "../resultados/ItemSimlilitudTanimoto.csv",header = F,sep="\t")
table(simTanimotoItem$V3)
hist(simTanimotoItem$V3)


## analisis tips
library(tm)
library(Matrix)
library(RWeka)
library(rgl)
tips <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/tipsCluster.csv",header = T,sep = ";",colClasses = "character")    
str(tips)
head(tips[,3],n=40)

corpus <- Corpus(VectorSource(tips$V3))
all_text <- Corpus(VectorSource(tips$V3))
dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                              removeNumbers=TRUE,stopwords=TRUE,
                                              stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))

dtm <- removeSparseTerms(dtm,0.99)
df_q<-Matrix(as.matrix(dtm),sparse=T)
df_q<-as.data.frame(as.matrix(dtm))
colnames(df_q)=paste("q_",colnames(df_q),sep="")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

dtmNgram<- DocumentTermMatrix(all_text,control=list(tokenize = BigramTokenizer,
                                                    tolower=TRUE,removePunctuation=TRUE,
                                                    removeNumbers=TRUE,stopwords=TRUE,
                                                    stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))


dtmNgram <- removeSparseTerms(dtmNgram,0.99)
df_ngram<-Matrix(as.matrix(dtmNgram),sparse=T)
df_ngram<-as.data.frame(as.matrix(df_ngram))
colnames(df_ngram)=paste("n_",colnames(df_ngram),sep="")

dtmTrigram<- DocumentTermMatrix(all_text,control=list(tokenize = TrigramTokenizer,
                                                    tolower=TRUE,removePunctuation=TRUE,
                                                    removeNumbers=TRUE,stopwords=TRUE,
                                                    stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))


dtmTrigram <- removeSparseTerms(dtmTrigram,0.99)
df_trigram<-Matrix(as.matrix(dtmTrigram),sparse=T)
df_trigram<-as.data.frame(as.matrix(df_trigram))
colnames(df_trigram)=paste("tri_",colnames(df_trigram),sep="")

df_total <- cbind(df_q,df_ngram,df_trigram)
summary(df_total)

findAssocs(dtm, c("food","price","bad"), .01)

princo<- prcomp(df_total,center = TRUE,scale. = TRUE)
print(princo)
plot(princo, type = "l")
plot(princo$x)
cl <- kmeans(df_total, 500)
plot(princo$x, col=cl$cl)
head(tips[which(cl$cluster==500),3],n=100)
plot3d(princo$x[,1:3],col=cl$cl)
summary(cl$cluster)
table(cl$cluster)
tips$Clase<-cl$cluster
tips[73,3:4]
write.table(x = tips$Clase,file ="../dataset_ubicomp2013/dataset_ubicomp2013/cluster.csv",col.names = T,quote = F,row.names = F,sep=",")
summary(tips$Clase)
mysample <- tips[sample(1:nrow(tips), 500,
                          replace=FALSE),]

sample(1:nrow(tips), 500,
       replace=FALSE)
mysample$
table(mysample$Clase)

##### topic model
library(topicmodels)
myDtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                              removeNumbers=TRUE,stopwords=TRUE,
                                              stemming=TRUE,weighting=function(x) weightTf(x)))
summary(df_q)
sum(!complete.cases(df_q))
dtm
myDtm <- removeSparseTerms(myDtm,0.99)
df_t<-Matrix(as.matrix(myDtm),sparse=T)
df_t<-as.data.frame(as.matrix(myDtm))
colnames(df_t)=paste("t_",colnames(df_t),sep="")
sum(!complete.cases(df_t))
rowTotals <- apply(myDtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- myDtm[rowTotals> 0, ] 
k = 20;
SEED = 1234;
my_TM =
    list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
         VEM_fixed = LDA(dtm, k = k,
                         control = list(estimate.alpha = FALSE, seed = SEED)),
         Gibbs = LDA(dtm, k = k, method = "Gibbs",
                     control = list(seed = SEED, burnin = 1000,
                                    thin = 100, iter = 10)),
         CTM = CTM(dtm, k = k,
                   control = list(seed = SEED,
                                  var = list(tol = 10^-4), em = list(tol = 10^-3))));

Gibbs = LDA(dtm.new, k = k, method = "Gibbs",
            control = list(seed = SEED, burnin = 1000,
                           thin = 100, iter = 1000))
VEM = LDA(dtm.new , k = k, control = list(seed = SEED))

Topic = topics(Gibbs, 1);

#top 5 terms for each topic in LDA
Terms = terms(Gibbs, 10);
Terms;

(my_topics =
     topics(VEM));

most_frequent = which.max(tabulate(my_topics));

terms(VEM, 10)[, most_frequent];
