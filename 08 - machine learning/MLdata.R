library(caret)
setwd("C://Users/Marco/Desktop/coursera/machine learning")
allData<- read.csv("pml-training.csv",stringsAsFactors = F,
                   na.strings = c("NA","#DIV/0!"))
predictors<-names(allData)[!is.na(allData[1,])&!nearZeroVar(allData,
                                                            saveMetrics = TRUE)$nzv]
predictors<-predictors[predictors!="classe"]
predictors<-predictors[7:length(predictors)]
myform<-as.formula(paste("classe~",paste(predictors,collapse="+")))
inTrain<- createDataPartition(allData$classe,p=0.6,list=FALSE)