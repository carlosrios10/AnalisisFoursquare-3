library(plyr)
tips <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/dataset_ubicomp2013_tips.txt",header = F,sep = "\t",colClasses = "character")    
tipsClases <-read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/tipsclase.csv",header = T,colClasses = "character")    
str(tipsClases)
str(tips)
tipsUsers<-count( tips,c("V1","V2"))
head(tipsUsers,n=50)
tips[tips$V1=="104"&tips$V2=="31027",]
tipsClases[tipsClases$user=="104"&tipsClases$item=="31027",]
head(tipsClases,n=600)
tipsClases$Sentimiento[tipsClases$PredNeg=="negativo"& tipsClases$PredPositivo=="neutro"]<- 1
tipsClases$Sentimiento[tipsClases$PredNeg=="neutro"& tipsClases$PredPositivo=="positivo"]<- 3
tipsClases$Sentimiento[tipsClases$PredNeg=="neutro"& tipsClases$PredPositivo=="neutro"]<- 2
tipsClases$Sentimiento[tipsClases$PredNeg=="negativo"& tipsClases$PredPositivo=="positivo"]<- 2.5
table(tipsClases$Sentimiento)
tipsClases$Sentimiento<- as.numeric(tipsClases$Sentimiento)


tipsUsersSenti <- ddply(tipsClases, .(user,item), summarise,
             rating = mean(Sentimiento, na.rm = TRUE))
head(tipsUsersSenti)
hist(tipsUsersSenti$rating)
summary(tipsUsersSenti$rating)
tipsUsers[tipsUsers$V1=="102",]
tipsUsersSenti[tipsUsersSenti$user=="104"&tipsUsersSenti$item=="31027",]
write.table(x =tipsUsersSenti,file = "../dataset_ubicomp2013/dataset_ubicomp2013/tipsUserSentiRating.csv",sep = ",",quote = F,row.names = F,col.names = F )
table(tipsUsersSenti$rating)
table(tipsUsers$freq)
tipsUsers[tipsUsers$freq==5,]
length( unique(tipsUsersSenti$user))
scoreci(x =10 ,n =10,conflev = 0.95)
2601+ 1233
