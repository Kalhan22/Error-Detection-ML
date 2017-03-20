
#Information Dataset
#Classifers Used:  KNN, Logistic Regression, Naive Bayes, KNN


ibrary(caret)
#install.packages('cvTools')
library(cvTools)
#install.packages('compare')
library(compare)


setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('information_data_tagged.csv')
data <- data[sample(nrow(data)),]
print('Actual Error in the dataset')
print(100*sum(data$T_Class == 1)/nrow(data))

## set the seed to make your partition reproductible

cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data)),breaks=cross_validation,labels=FALSE)

data$T_id <- as.numeric(as.character(data$T_Id))
data$Age_Error <- as.numeric(as.character(data$Age_Error))
data$Office_Error <- as.numeric(as.character(data$Office_Error))
data$Name_Error <- as.numeric(as.character(data$Name_Error ))
data$Position_Error <- as.numeric(as.character(data$Position_Error))
data$T_Class <- as.numeric(as.character(data$T_Class))



for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  model <- glm(T_Class ~ Age_Error + Office_Error + Name_Error + Position_Error ,family=binomial(link='logit'),data=trainData)
  results <- predict(model,testData,type='response')
  prediction <- ifelse(results >0.9,1,0)
  output <- cbind(testData,prediction)
  output$prediction <- as.character(output$prediction)
  accuracy <- (100*sum(output$T_Class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  print(confusionMatrix(as.factor(output$prediction),as.factor(output$T_Class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))



#KNN

setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('information_data_tagged.csv')
data <- data[sample(nrow(data)),]
print('Actual Error in the dataset')
print(100*sum(data$T_Class == 1)/nrow(data))
install.packages('class')
library(class)
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data)),breaks=cross_validation,labels=FALSE)

data$T_id <- as.numeric(as.character(data$T_Id))
data$Age_Error <- as.numeric(as.character(data$Age_Error))
data$Office_Error <- as.numeric(as.character(data$Office_Error))
data$Name_Error <- as.numeric(as.character(data$Name_Error ))
data$Position_Error <- as.numeric(as.character(data$Position_Error))
data$T_Class <- as.numeric(as.character(data$T_Class))



for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  knn_model_prediction <- knn(trainData, testData, cl = factor(trainData$T_Class), k = 5)
  output <- cbind(testData,knn_model_prediction)
  output$prediction <- as.character(output$knn_model_prediction)
  accuracy <- (100*sum(output$T_Class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$T_Class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))






##Naive Bayes Classifer
setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('information_data_tagged.csv')
data <- data[sample(nrow(data)),]
print('Actual Error in the dataset')
print(100*sum(data$T_Class == 1)/nrow(data))

cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data)),breaks=cross_validation,labels=FALSE)

data$T_id <- as.numeric(as.character(data$T_Id))
data$Age_Error <- as.numeric(as.character(data$Age_Error))
data$Office_Error <- as.numeric(as.character(data$Office_Error))
data$Name_Error <- as.numeric(as.character(data$Name_Error ))
data$Position_Error <- as.numeric(as.character(data$Position_Error))
data$T_Class <- as.numeric(as.character(data$T_Class))

install.packages('e1071')
library('e1071')

for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  model <- naiveBayes(as.factor(T_Class) ~ Age_Error + Office_Error + Name_Error + Position_Error , data = trainData)
  nb_prediction <- predict(model,testData)
  output <- cbind(testData,nb_prediction)
  output$prediction <- as.character(output$nb_prediction)
  accuracy <- (100*sum(output$T_Class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$T_Class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))




#SVM

setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('information_data_tagged.csv')
data <- data[sample(nrow(data)),]
print('Actual Error in the dataset')
print(100*sum(data$T_Class == 1)/nrow(data))

cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data)),breaks=cross_validation,labels=FALSE)

data$T_id <- as.numeric(as.character(data$T_Id))
data$Age_Error <- as.numeric(as.character(data$Age_Error))
data$Office_Error <- as.numeric(as.character(data$Office_Error))
data$Name_Error <- as.numeric(as.character(data$Name_Error ))
data$Position_Error <- as.numeric(as.character(data$Position_Error))
data$T_Class <- as.numeric(as.character(data$T_Class))


for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  svm_model <- svm(T_Class ~ Age_Error + Office_Error + Name_Error + Position_Error , data = trainData, gamma = 0.1, probability = TRUE)
  svm_prediction <- predict(svm_model,testData,probability = TRUE)
  output <- cbind(testData,ifelse(svm_prediction >0.5,1,0))
  output$prediction <- as.character(output$`ifelse(svm_prediction > 0.5, 1, 0)`)
  accuracy <- (100*sum(output$T_Class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$T_Class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))



