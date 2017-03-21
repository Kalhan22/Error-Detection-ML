setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('Customer_Sales_1_Dirty.csv')
head(data)

library(RMySQL)
con <- dbConnect(MySQL(),user="****", password="****",dbname="*****", host="****")
rs <- dbSendQuery(con, "
select a.ID,
case when concat('',a.Sales_Rep_id * 1) <>  a.Sales_Rep_id then 1 else 0 end as Sales_Rep_Error,
case when ((length(a.Order_Date) - length(replace(a.Order_Date,'/',''))) = 2) || ((length(a.Order_Date) - length(replace(a.Order_Date,'-',''))) = 2) then 0 else 1 end as order_date_error,
case when locate(':',a.Order_Time) = 0 then 1 else 0 end as order_time_error,
case when concat('',a.Order_id * 1) <>  a.Order_id then 1 else 0 end as Order_id_Error,
case when concat('',a.C_Name * 1) <>  a.C_Name then 0 else 1 end as Name_Error,
case when 
a.C_Name <> b.C_Name or 
a.Company_Name <> b.Company_Name or 
a.Telephone <> b.Telephone or 
a.Address_City <> b.Address_City or 
a.State <> b.State or a.ZIP <> b.ZIP or
a.Order_id <> b.Order_id 
then 1 else 0 end as class
from dwh.DIM_CUSTOMER_SALES_DIRTY as a join dwh.DIM_CUSTOMER_SALES_CLEAN as b on a.ID = b.ID
")
data <- fetch(rs,n = -1)
data <- data[sample(nrow(data)),]


#KNN
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data)),breaks=cross_validation,labels=FALSE)
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  knn_model_prediction <- knn(trainData, testData, cl = factor(trainData$class), k = 5)
  output <- cbind(testData,knn_model_prediction)
  output$prediction <- as.character(output$knn_model_prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  print(accuracy)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$T_Class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))






data <- fetch(rs,n = -1)

data <- data[sample(nrow(data)),]
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data)),breaks=cross_validation,labels=FALSE)

#Logistic Regression
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  model <- glm(class ~ Sales_Rep_Error + order_date_error + order_time_error + Order_id_Error ,family=binomial(link='logit'),data=trainData)
  results <- predict(model,testData,type='response')
  prediction <- ifelse(results >0.9,1,0)
  output <- cbind(testData,prediction)
  output$prediction <- as.character(output$prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))





