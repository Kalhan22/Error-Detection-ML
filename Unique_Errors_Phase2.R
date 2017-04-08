setwd('/Users/Kalhan/Desktop/Waterloo Data/Winter 2017/BigData/Project')
data <- read.csv('Customer_Sales_1_Clean_Half_Dirty_Transposition.csv',header = FALSE,stringsAsFactors = FALSE)
head(data)
data <- data.frame(data$V2,data$V3,data$V5,data$V6,data$V7,stringsAsFactors = FALSE)
data$ID <- 1:nrow(data)
colnames(data) <- c("C_Name","Company_Name","Address_City","State","ZIP","ID")


original_data <- data

##Induce Errors in the Dataset
error_percentage <- 20
percent <- ceiling((nrow(data)*error_percentage)/100)
for(i in 1:percent){
  print(i)
  col <- sample(1:(ncol(data)-2),1)
  string_val <- data[i,col]
  string_val <- as.character(string_val)
  pos1 <- sample(c(2:length(unlist(strsplit(string_val,split = "")))-1),1)
  pos2 <- pos1 + 1
  part1 <- substring(string_val,0,pos1 - 1)
  replaced_char_1 <- substring(string_val,pos1,pos1)
  replaced_char_2 <- substring(string_val,pos2,pos2)
  part2 <- substring(string_val,pos2+1,length(unlist(strsplit(string_val,split = ""))))
  final_val <- paste(part1,replaced_char_2,replaced_char_1,part2,sep = "")
  print(string_val)
  print("changed to")
  print(final_val)
  data[i,col] <- final_val
  
}

head(data)
detach("package:RMySQL", unload=TRUE)
library(sqldf)
data_for_model <- sqldf("select a.*, 
                       case when
                       a.C_Name <> b.C_Name or 
                       a.Company_Name <> b.Company_Name or 
                       a.Address_City <> b.Address_City or 
                       a.ZIP <> b.ZIP or 
                       a.State <> b.State
                       then 1 else 0 end as class
                       from original_data as a join data as b on a.ID = b.ID ")

data_for_model <- data_for_model[sample(nrow(data_for_model)),]
#Run Prediction

previous_data <- data_for_model

occurences <- data.frame(-999,-999,-999,-999,-999,-999,-999,-999,-999,-999)

data <- data_for_model
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
  testData <- data_updated[testIndexes,2:ncol(data_updated) ]
  trainData <- data_updated[-testIndexes, 2:ncol(data_updated)]
  model <- glm(class ~ seq_c_name_2 + seq_company_name_2 + seq_a_city_2 + seq_state_2 + seq_zip_2 + seq_c_name_3 + seq_company_name_3 + seq_a_city_3 + seq_state_3 + seq_zip_3,family=binomial(link='logit'),data=trainData)
  results <- predict(model,testData[,2:ncol(testData)],type='response')
  prediction <- ifelse(results >=0.2094,1,0)
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
print(confusionMatrix(as.numeric(output$prediction),output$class))


library(nnet)
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
  model <- nnet(as.factor(class) ~ seq_c_name_2 + seq_company_name_2 + seq_a_city_2 + seq_state_2 + seq_zip_2 + seq_c_name_3 + seq_company_name_3 + seq_a_city_3 + seq_state_3 + seq_zip_3,data=trainData,size=50,maxit=10000,decay=.002)
  results <- predict(model,testData[,3:ncol(testData)],type = "class")
  prediction <- results
  output <- cbind(testData,prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  print(confusionMatrix(output$prediction,output$class))
  
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))
library(caret)
print(confusionMatrix(output$prediction,output$class))




