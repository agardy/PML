setwd("c:/machine")
library(data.table)
library(caret)
library(ggplot2)
library(doMC)
library(knitr)
library(xtable)
library(randomForest)
library(kernlab)

set.seed(32323)
#loading data
training_data <- read.csv("pml-training.csv", header = T)
testing_data <- read.csv("pml-testing.csv", header = T)

#removing important columns
trainSub <- training_data[c(2,8,9,10,11,37,38,39,40,41,42,43,44,45,46,47,48,49,60,61,62,63,64,65,66,67,68,84,85,86,102,113,114,115,116,117,118,119,120,121,122,123,124,140,151,152,153,154,155,156,157,158,159,160)]
testSub <- testing_data[c(2,8,9,10,11,37,38,39,40,41,42,43,44,45,46,47,48,49,60,61,62,63,64,65,66,67,68,84,85,86,102,113,114,115,116,117,118,119,120,121,122,123,124,140,151,152,153,154,155,156,157,158,159,160)]

#cross-validation
inTrain <- createDataPartition(y=training_data$classe,p=0.7,list=FALSE)
training <- training_data[inTrain,]
testing <- training_data[-inTrain,]

#collumns with values
training.features <- drop.columns(raw.train[, eval(names(which(na.cols == F))), with=F])

#function for testing answers
write_files = function(x){
					n = length(x)
					for(i in 1:n){
								filename = paste0("problem_id_",i,".txt")
								write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
					}
}

#fiting the model
fit1 <- train(classe~ ., data = training, method = "rf", prox = FALSE)
final <- testing[c(2,8,9,10,11,37,38,39,40,41,42,43,44,45,46,47,48,49,60,61,62,63,64,65,66,67,68,84,85,86,102,113,114,115,116,117,118,119,120,121,122,123,124,140,151,152,153,154,155,156,157,158,159,160)]
fit2 <- predict(fit1, final)
fit2
table(pred,testing$classe)

#conf <- confusionMatrix(predict(fit2, testing, factor(testing$classe))$overall["Kappa"]
#conf
