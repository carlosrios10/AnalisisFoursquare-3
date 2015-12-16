library("recommenderlab")
tipsUserAll <- read.csv(file = "../dataset_ubicomp2013/dataset_ubicomp2013/tipsUsers.csv",header = F)
head(tipsUserAll)
str(tipsUserAll)

head(tipsUser)
tipsUserRM <- as(tipsUserAll, "realRatingMatrix")
hist(getRatings(normalize(tipsUserRM)), breaks=100)
hist(getRatings(normalize(r, method="Z-score")), breaks=100)
hist(rowCounts(tipsUserRM), breaks=500)
MSWeb5 <- tipsUserRM[rowCounts(tipsUserRM) >2,]

tipsUserRM_norm<-normalize(tipsUserRM)
scheme <- evaluationScheme(MSWeb5, method="cross", k=3, given=3, goodRating=2)

r1 <- Recommender(getData(e, "train"), "UBCF")
r2 <- Recommender(getData(e, "train"), "IBCF")
p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")
error <- rbind(calcPredictionAccuracy(p1, getData(e, "unknown")), calcPredictionAccuracy(p2, getData(e, "unknown")) )

results <- evaluate(scheme, method="UBCF", n=c(1,3,5,10,15,20))
getConfusionMatrix(results)[[1]]
