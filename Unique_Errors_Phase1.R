#Unique_Errors
#Customer Dataset


setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('Customer_Sales_1_Dirty.csv')
head(data)
library(RMySQL)
con <- dbConnect(MySQL(),user="****", password="*****",dbname="*****", host="*****")

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

rs <-  dbSendQuery(con,"select * from  dwh.DIM_CUSTOMER_SALES_CLEAN_HALF")


rs <- dbSendQuery(con,"select a.*, 
                 case when 
                  a.C_Name <> b.C_Name or 
                 a.Company_Name <> b.Company_Name or 
                 a.Telephone <> b.Telephone or 
                 a.Address_City <> b.Address_City or 
                 a.State <> b.State or a.ZIP <> b.ZIP or
                 a.Order_id <> b.Order_id 
                 then 1 else 0 end as class
                 from dwh.DIM_CUSTOMER_SALES_DIRTY as a join dwh.DIM_CUSTOMER_SALES_CLEAN_HALF as b on a.ID = b.ID ")





data <- fetch(rs,n = -1)
data <- data[sample(nrow(data)),]

previous_data <- data
data <- data.frame(data$C_Name,data$Company_Name,data$Address_City,data$State,data$ZIP,stringsAsFactors = FALSE)
colnames(data) <- c("C_Name","Company_Name","Address_City","State","ZIP")
occurences <- data.frame(-999,-999,-999,-999,-999,-999,-999,-999,-999,-999)
colnames(occurences) <- c("seq_c_name_2","seq_company_name_2","seq_a_city_2","seq_state_2","seq_zip_2","seq_c_name_3","seq_company_name_3","seq_a_city_3","seq_state_3","seq_zip_3")
for(i in 1:nrow(data)){
  
  ## Two letter occurences
  
  print(i)
  temp_c_name <- as.character(data$C_Name[i])
  num_uniques <- unique(unlist(strsplit(temp_c_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_c_name <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_c_name)[[1]] != -1)))
  
  
  temp_company_name <- as.character(data$Company_Name[i])
  num_uniques <- unique(unlist(strsplit(temp_company_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_company_name <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_company_name)[[1]] != -1)))
  
  
  temp_a_city <- as.character(data$Address_City[i])
  num_uniques <- unique(unlist(strsplit(temp_a_city,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_a_city <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_a_city)[[1]] != -1)))
  
  
  temp_state <- as.character(data$State[i])
  num_uniques <- unique(unlist(strsplit(temp_state,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_state <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_state)[[1]] != -1)))
  
  
  temp_zip <- as.character(data$ZIP[i])
  num_uniques <- unique(unlist(strsplit(temp_zip,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_zip<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_zip )[[1]] != -1)))
  
  
  ##Three Letter Occurences
  temp_c_name <- as.character(data$C_Name[i])
  num_uniques <- unique(unlist(strsplit(temp_c_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,num_uniques,sep = "")
  repetitions_c_name_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_c_name)[[1]] != -1)))
  
  
  temp_company_name <- as.character(data$Company_Name[i])
  num_uniques <- unique(unlist(strsplit(temp_company_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,num_uniques,sep = "")
  repetitions_company_name_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_company_name)[[1]] != -1)))
  
  
  temp_a_city <- as.character(data$Address_City[i])
  num_uniques <- unique(unlist(strsplit(temp_a_city,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,num_uniques,sep = "")
  repetitions_a_city_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_a_city)[[1]] != -1)))
  
  
  temp_state <- as.character(data$State[i])
  num_uniques <- unique(unlist(strsplit(temp_state,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,num_uniques,sep = "")
  repetitions_state_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_state)[[1]] != -1)))
  
  
  temp_zip <- as.character(data$ZIP[i])
  num_uniques <- unique(unlist(strsplit(temp_zip,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,num_uniques,sep = "")
  repetitions_zip_3<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_zip )[[1]] != -1)))
  
  
  
  
  
  occurence_row <- cbind(repetitions_c_name,repetitions_company_name,repetitions_a_city,repetitions_state,repetitions_zip,repetitions_c_name_3,repetitions_company_name_3,repetitions_a_city_3,repetitions_state_3,repetitions_zip_3)
  colnames(occurence_row) <- c("seq_c_name_2","seq_company_name_2","seq_a_city_2","seq_state_2","seq_zip_2","seq_c_name_3","seq_company_name_3","seq_a_city_3","seq_state_3","seq_zip_3")
  
  occurences <- rbind(occurences,occurence_row)
  
  
  
  
  
}

occurences <- subset(occurences,occurences$`seq_c_name_2` != -999)
head(occurences)

content_from_previous <-data.frame(previous_data$ID,previous_data$class)
colnames(content_from_previous) <- c("ID","class")
data_updated <- cbind(content_from_previous,occurences)













#KNN
library(class)
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
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







cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)

#Logistic Regression
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
  model <- glm(class ~ seq_c_name_2 + seq_company_name_2 + seq_a_city_2 + seq_state_2 + seq_zip_2 + seq_c_name_3 + seq_company_name_3 + seq_a_city_3 + seq_state_3 + seq_zip_3,family=binomial(link='logit'),data=trainData)
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


