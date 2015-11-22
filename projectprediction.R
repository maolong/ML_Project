#Loading libraries
  library(caret)
  library(gbm)
  library(randomForest)
#Download the dataset from internet location
  url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#local_train <- "dataset.zip"
  download.file(url_train, destfile="data_train.csv")
  download.file(url_test, destfile="data_test.csv")
#Prepare Data
  data_train <- read.csv("data_train.csv", na.strings=c("NA", "#DIV/0!"))
  data_test <- read.csv("data_test.csv", na.strings=c("NA", "#DIV/0!"))
  dataset_train <- data_train[,colSums(is.na(data_train)) <= (nrow(data_train)*0.9)]
  dataset_train <- dataset_train[,-c(1:7)]
#Modeling
  set-seed(97531)
  inTrain <- createDataPartition(dataset_train$classe, p=0.60, list=FALSE)
  trainData <- dataset_train[inTrain, ]
  validData <- dataset_train[-inTrain, ]
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats=5)
  modelFit <- train(classe ~ ., data=trainData, method="gbm", trControl = fitControl)
  prediction <- predict(modelFit, newdata=validData)
  confusionMatrix(prediction, validData$classe)
#Second Modeling  
  fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
  modelFit2 <-train(classe~.,data=trainData, method="rf", trControl=fitControl2, verbose=T)
  prediction2 <- predict(modelFit2, newdata=validData)
  confusionMatrix(prediction2, validData$classe)
#Out of sample error
  100 * (1 - sum(prediction2 == validData$classe)/length(prediction2))
#Applying to 20 cases test
  dataset_test <- data_test[,colSums(is.na(data_test)) <= (nrow(data_test)*0.9)]
  dataset_test <- dataset_test[,-c(1:7)]
  prediction3 <- predict(modelFit2, newdata=dataset_test)
## Function to create predicted files
  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
## Create files  
  pml_write_files(prediction3)