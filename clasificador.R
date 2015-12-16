library(caret)
library(pROC)
# 1 -> positivo
# 0 -> neutro
# 2 -> negativo

df_total <- df_total[order(df_total$random),]
train <- df_total[1:501,c(-216,-215,-213)]
head(train)
table(train$clase)
################# Clasificador de positivos ########################
trainPositivos<- train
trainPositivos[trainPositivos$clase==2,]$clase<- 0
trainPositivos$clase <- as.factor(trainPositivos$clase)
levels(trainPositivos$clase)<-c("neutro","positivo")
trainPositivos$clase <- relevel(trainPositivos$clase, "positivo")

inTrain<-createDataPartition(y=trainPositivos$clase,p=0.8,list=F)
training<-trainPositivos[inTrain,]
testing<-trainPositivos[-inTrain,]

fitControl <- trainControl(method = "cv",
                           number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           )
TrainData <- training[,c(-213)]
TrainClasses <- training[,213]


#### J48
set.seed(842)
j48Grid<- expand.grid(C = c(0.05,0.1,0.2,0.25,0.3))
modJ48<-train(TrainData,TrainClasses,
              method="J48",
              trControl = fitControl,
              tuneGrid = j48Grid
              ,metric = "ROC"
)
modJ48
predJ48<-predict(modJ48, newdata = testing)
confusionMatrix(data = predJ48, testing$clase)

#### rpart
cps<-seq(0.00001, 0.0001, by= 0.00001)
cartGrid <-  expand.grid(cp = cps)
set.seed(842)
modCart<-train(TrainData,TrainClasses,
               method="rpart",
               trControl = fitControl,
               tuneGrid = cartGrid
               ,metric="ROC"
)
modCart
predCart<-predict(modCart, newdata = testing)
confusionMatrix(data = predCart, testing$clase)

#### random forest
rfGrid <- expand.grid(mtry = c(2,3,4,5,6,14))
set.seed(842)
modRF<-train(TrainData,TrainClasses,
             method="rf",
             ntree = 1000,
             trControl=fitControl,
             tuneGrid = rfGrid
             ,metric="ROC"
             )
modRF
predRF<-predict(modRF, newdata = testing)
confusionMatrix(data = predRF, testing$clase)
## curva ROC
predProb<-predict(modRF, newdata = testing, type = "prob")
rocRF <- roc(testing$clase,predProb[,1])
plot(rocRF, print.thres = c(.75), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
ci.thresholds(rocRF)
## SVM
set.seed(849)
modSVM <- train(
    clase~.,
    data=trainPositivos,
                method = "lssvmLinear",
                trControl = fitControl
                ,metric="ROC")

model <- train(method = 'svmLinear', 
               x =TrainData, 
               y = TrainClasses, 
               #weights = weights,
               maximize = T,
               tuneGrid = expand.grid(.C=3^(-15:15)),   
               preProcess = c('center', 'scale'),
               trControl = trainControl(method = 'cv', # cross validation
                                        number = 10,   # nr of cv sets
                                        #                                     repeats = 5, # use with method=repeatcv
                                        returnResamp = 'none', # return accuracy per cv partition and parameter setting
                                        classProbs = T, # return prediction probabilities along with predicted classes
                                        #                                     savePredictions=T, # returns all predictions (for all cv paritions) for each tuneGrid parameter set 
                                        returnData = F, # disable return of training data e.g. for big data sets
                                        allowParallel = F
               )
)
## Calculo las probabilidades para los datos que no tienen clase
predNewTips <- predict(modRF, newdata = df_total)
df_total$PredPositivo<- predNewTips
table(df_total$PredPositivo)
tail(df_total$PredPositivo)
head(df_total[,c(217,216,215,214,213)],n=550)
####### Clasificador de negativos ##############################
trainNegativos<- train
trainNegativos[trainNegativos$clase==1,]$clase<- 0
trainNegativos$clase <- as.factor(trainNegativos$clase)
levels(trainNegativos$clase)<-c("neutro","negativo")
trainNegativos$clase <- relevel(trainNegativos$clase, "negativo")
table(trainNegativos$clase)


inTrainNeg<-createDataPartition(y=trainNegativos$clase,p=0.8,list=F)
trainingNeg<-trainNegativos[inTrainNeg,]
testingNeg<-trainNegativos[-inTrainNeg,]

fitControlNeg <- trainControl(method = "cv",
                           number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)
TrainData <- training[,c(-213)]
TrainClasses <- training[,213]

#### random forest
rfGrid <- expand.grid(mtry = c(2,3,4,5,6,14))
set.seed(842)
modRF<-train(clase ~ .,
             data=trainingNeg,    
             method="rf",
             ntree = 1000,
             trControl=fitControlNeg,
             tuneGrid = rfGrid
             ,metric="ROC"
)
modRF
predRF<-predict(modRF, newdata = testingNeg)
confusionMatrix(data = predRF, testingNeg$clase)
str(modRF)
## curva ROC
predProb<-predict(modRF, newdata = testingNeg, type = "prob")
rocRF <- roc(testingNeg$clase,predProb[,1],levels = c("neutro","negativo"))
plot(rocRF, print.thres = c(.2), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
ci.thresholds(rocRF)
sum(predProb[,1]>0.03)
predCutOff<- rep("",100)
predCutOff[predProb[,1]>0.2]<-"negativo"
predCutOff[predProb[,1]<=0.2]<-"neutro"
predCutOff<- as.factor(predCutOff)
confusionMatrix(data = predCutOff, testingNeg$clase)

predNewNeg <- predict(modRF, newdata = df_total,type = "prob")
head(predNewNeg)

predNewNeg$clase[predNewNeg[,"negativo"]>0.2]<-"negativo"
predNewNeg$clase[predNewNeg[,"negativo"]<=0.2]<-"neutro"
df_total$PredNeg<- predNewNeg$clase
head(df_total[,c(218,217,216,215,214,213)])
str(df_total[,c(218,217,216,215,214,213)])
write.csv(x = df_total[,c(218,217,216,215,214,213)],row.names =F, file = "../dataset_ubicomp2013/dataset_ubicomp2013/tipsclase.csv")

################################
################################

library(e1071)
set.seed(1)
x=matrix(rnorm(200*2) , ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150 ,]=x[101:150,]-2
y=c(rep(1,150) ,rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
table(dat[train,]$y)
table(dat$y)
table(y)

train=sample (200,100,)
 
svmfit=svm(y~ ., data=dat[train ,], kernel="radial", gamma=1,cost=1)
plot(svmfit , dat[train ,])
summary(svmfit)
 
set.seed(1)
tune.out=tune(svm, y~ ., data=dat[train ,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
  
   summary(tune.out)
   table(true=dat[-train ,"y"], pred=predict(tune.out$best.model ,
                                             newdata=dat[-train ,]))
   
 set.seed(1)
 x=rbind(x, matrix(rnorm(50*2), ncol=2))
 y=c(y, rep(0,50))
 x[y==0,2]= x[y==0,2]+2
 dat=data.frame(x=x, y=as.factor(y))
 par(mfrow=c(1,1))
 plot(x,col=(y+1))
 svmfit=svm(y~ ., data=dat[train ,] , kernel="radial", cost=10, gamma=1)
 plot(svmfit , dat[train ,])
 pred=predict(svmfit , newdata=dat[-train ,])
 