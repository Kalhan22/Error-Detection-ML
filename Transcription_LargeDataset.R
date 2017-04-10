#Creation of Transcription Errors
library(stringr)


data <- read.csv('movie_metadata.csv',header = TRUE,stringsAsFactors = FALSE,encoding = "en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8")
data <- na.omit(data)
data <- data.frame(data$ID,data$director_name,data$actor_2_name,data$genre,data$actor_1_name,data$movie_title,data$actor_3_name,data$plot_keywords,stringsAsFactors = FALSE)
colnames(data) <- c("ID","director_name","actor_2_name","genre","actor_1_name","movie_title","actor_3_name","plot_keywords")
data <- na.omit(data)
original_data <- data
colnames(data) <- c("ID","director_name","actor_2_name","genre","actor_1_name","movie_title","actor_3_name","plot_keywords")


##Induce Errors in the Dataset
error_percentage <- 80
percent <- ceiling((nrow(data)*error_percentage)/100)
for(i in 1:percent){
  random_elem <- sample(2:4,1)
  col <- sample(2:(ncol(data)-1),1)
  string_val <- data[i,col]
  Encoding(string_val) <- c("en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8")
  string_val <- as.character(string_val)
  pos1 <- sample(c(2:length(unlist(strsplit(string_val,split = "")))-1),1)
  pos2 <- pos1 + 1
  part1 <- substring(string_val,0,pos1 - 1)
  replaced_char_1 <- substring(string_val,pos1,pos1)
  replaced_char_2 <- substring(string_val,pos1,pos1)
  part2 <- substring(string_val,pos2+1,length(unlist(strsplit(string_val,split = ""))))
  #Transcrption Frequency
  if(random_elem == 2){
    final_val <- paste(part1,replaced_char_2,replaced_char_1,part2,sep = "")
  }
  else if(random_elem == 3){
    final_val <- paste(part1,replaced_char_2,replaced_char_1,replaced_char_1,part2,sep = "")
  }
  else{
    final_val <- paste(part1,replaced_char_2,replaced_char_1,replaced_char_1,replaced_char_1,part2,sep = "")
  }

  print(string_val)
  print("changed to")
  print(final_val)
  data[i,col] <- final_val
  
}

data <- na.omit(data)



library(sqldf)
data_for_model <- sqldf("select a.*, 
                        case when
                        a.director_name <> b.director_name or 
                        a.actor_2_name <> b.actor_2_name or 
                        a.genre <> b.genre or 
                        a.actor_1_name <> b.actor_1_name or 
                        a.movie_title  <> b.movie_title or
                        a.actor_3_name <> b.actor_3_name or
                        a.plot_keywords <> b.plot_keywords
                        then 1 else 0 end as class
                        from data as a join original_data as b on a.ID = b.ID ")

data_for_model <- data_for_model[sample(nrow(data_for_model)),]
#Run Prediction

previous_data <- data_for_model
data <- data_for_model
previous_data <- data
occurences <- data.frame(-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999)
colnames(occurences) <- c("seq_director_name_2","seq_actor_2_name_2","seq_genre_2","seq_actor_1_name_2","seq_movie_title_2","seq_actor_3_name_2","seq_plot_key_words_2","seq_director_name_3","seq_actor_2_name_3","seq_genre_3","seq_actor_1_name_3","seq_movie_title_3","seq_actor_3_name_3","seq_plot_key_words_3")
for(i in 1:nrow(data)){
  
  ## Two letter occurence
  temp_director_name <- as.character(data$director_name[i])
  temp_director_name <- str_replace_all(temp_director_name,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_director_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_director_name <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_director_name)[[1]] != -1)))
  
  
  temp_actor_2_name <- as.character(data$actor_2_name[i])
  temp_actor_2_name <- str_replace_all(temp_actor_2_name,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_actor_2_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_actor_2_name <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_actor_2_name)[[1]] != -1)))
  
  
  temp_genre <- as.character(data$genre[i])
  temp_genre <- str_replace_all(temp_genre,"[[:punct:]]","") 
  temp_genre <-gsub('\\|', '', temp_genre)
  num_uniques <- unique(unlist(strsplit(temp_genre,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_genre <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_genre)[[1]] != -1)))
  
  
  temp_actor_1_name <- as.character(data$actor_1_name[i])
  temp_actor_1_name <- str_replace_all(temp_actor_1_name,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_actor_1_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_actor_1_name <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_actor_1_name)[[1]] != -1)))
  
  
  temp_movie_title <- as.character(data$movie_title[i])
  temp_movie_title<- gsub("[^a-zA-Z0-9]", " ", temp_movie_title)
  num_uniques <- unique(unlist(strsplit(temp_movie_title,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_movie_title<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_movie_title )[[1]] != -1)))
  
  temp_actor_3_name <- as.character(data$actor_3_name[i])
  temp_actor_3_name<- str_replace_all(temp_actor_3_name,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_actor_3_name,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_actor_3_name<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_actor_3_name )[[1]] != -1)))
  
 
  temp_plot_keywords <- as.character(data$plot_keywords[i])
  temp_plot_keywords<- str_replace_all(temp_plot_keywords,"[[:punct:]]","") 
  temp_plot_keywords <-gsub('\\|', '', temp_plot_keywords)
  num_uniques <- unique(unlist(strsplit(temp_plot_keywords,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_plot_keywords<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_plot_keywords )[[1]] != -1)))
  
  
  ## 3occurences
  temp_director_name_3 <- as.character(data$director_name[i])
  temp_director_name_3 <- str_replace_all(temp_director_name_3,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_director_name_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_director_name_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_director_name_3)[[1]] != -1)))
  
  
  temp_actor_2_name_3 <- as.character(data$actor_2_name[i])
  temp_actor_2_name_3 <- str_replace_all(temp_actor_2_name_3,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_actor_2_name_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_actor_2_name_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_actor_2_name_3)[[1]] != -1)))
  
  
  temp_genre_3 <- as.character(data$genre[i])
  temp_genre_3 <- str_replace_all(temp_genre_3,"[[:punct:]]","") 
  temp_genre_3 <-gsub('\\|', '', temp_genre_3)
  num_uniques <- unique(unlist(strsplit(temp_genre_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_genre_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_genre_3)[[1]] != -1)))
  
  
  temp_actor_1_name_3 <- as.character(data$actor_1_name[i])
  temp_actor_1_name_3 <- str_replace_all(temp_actor_1_name_3,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_actor_1_name_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_actor_1_name_3 <- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_actor_1_name_3)[[1]] != -1)))
  
  temp_movie_title_3 <- as.character(data$movie_title[i])
  temp_movie_title_3<- str_replace_all(temp_movie_title_3,"[[:punct:]]","") 
  temp_movie_title_3 <- gsub("[^a-zA-Z0-9]", " ", temp_movie_title_3)
  num_uniques <- unique(unlist(strsplit(temp_movie_title_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_movie_title_3<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_movie_title_3 )[[1]] != -1)))
  
  temp_actor_3_name_3 <- as.character(data$actor_3_name[i])
  temp_actor_3_name_3 <- str_replace_all(temp_actor_3_name_3,"[[:punct:]]","") 
  num_uniques <- unique(unlist(strsplit(temp_actor_3_name_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_actor_3_name_3<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_actor_3_name_3 )[[1]] != -1)))
  
  
  temp_plot_keywords_3 <- as.character(data$plot_keywords[i])
  temp_plot_keywords_3<- str_replace_all(temp_plot_keywords_3,"[[:punct:]]","") 
  temp_plot_keywords_3 <-gsub('\\|', '', temp_plot_keywords_3)
  num_uniques <- unique(unlist(strsplit(temp_plot_keywords_3,split = "")))
  double_sequences <- paste(num_uniques,num_uniques,sep = "")
  repetitions_plot_keywords_3<- sum(apply(as.data.frame(double_sequences),1,function(x) sum(gregexpr(x, temp_plot_keywords_3 )[[1]] != -1)))
  
  

  occurence_row <- cbind(repetitions_director_name,repetitions_actor_2_name,repetitions_genre,repetitions_actor_1_name,repetitions_movie_title,repetitions_actor_3_name,repetitions_plot_keywords,    repetitions_director_name_3,repetitions_actor_2_name_3,repetitions_genre_3,repetitions_actor_1_name_3,repetitions_movie_title_3,repetitions_actor_3_name_3,repetitions_plot_keywords_3)
  colnames(occurence_row) <- c("seq_director_name_2","seq_actor_2_name_2","seq_genre_2","seq_actor_1_name_2","seq_movie_title_2","seq_actor_3_name_2","seq_plot_key_words_2","seq_director_name_3","seq_actor_2_name_3","seq_genre_3","seq_actor_1_name_3","seq_movie_title_3","seq_actor_3_name_3","seq_plot_key_words_3")
  occurences <- rbind(occurences,occurence_row)
  print(nrow(occurences))
  
}

occurences <- subset(occurences,occurences$seq_director_name_2 != -999)
head(occurences)

content_from_previous <-data.frame(previous_data$ID,previous_data$class)
colnames(content_from_previous) <- c("ID","class")
data_updated <- cbind(content_from_previous,occurences)






data_updated<- data_updated[sample(nrow(data_updated)),]





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
  knn_model_prediction <- knn(trainData[,3:ncol(testData)], testData[,3:ncol(testData)], cl = factor(trainData$class), k = 3)
  output <- cbind(testData,knn_model_prediction)
  output$prediction <- as.character(output$knn_model_prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  print(accuracy)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$T_Class)))
  print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
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
  model <- glm(class ~ seq_director_name_2 + seq_actor_2_name_2 + seq_genre_2 + seq_actor_1_name_2 + seq_movie_title_2 + seq_actor_3_name_2 + seq_plot_key_words_2 + seq_director_name_3 + seq_actor_2_name_3,seq_genre_3 + seq_actor_1_name_3 + seq_movie_title_3 + seq_actor_3_name_3 + seq_plot_key_words_3,family=binomial(link='logit'),data=trainData)
  results <- predict(model,testData,type='response')
  prediction <- ifelse(results >0.2,1,0)
  output <- cbind(testData,prediction)
  output$prediction <- as.character(output$prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  print(confusionMatrix(output$prediction,output$class))
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))



library('e1071')
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)

#Naive Bayes
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  print(x)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
  model <- naiveBayes(as.factor(class) ~ seq_director_name_2 + seq_actor_2_name_2 + seq_genre_2 + seq_actor_1_name_2 + seq_movie_title_2 + seq_actor_3_name_2 + seq_plot_key_words_2 + seq_director_name_3 + seq_actor_2_name_3,seq_genre_3 + seq_actor_1_name_3 + seq_movie_title_3 + seq_actor_3_name_3 + seq_plot_key_words_3,data=trainData)
  results <- predict(model,testData[,3:ncol(testData)])
  prediction <- results
  output <- cbind(testData,prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
  print(confusionMatrix(output$prediction,output$class))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))





#Neural Nets
library(nnet)
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)

#Logistic Regression
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
  model <- nnet(as.factor(class) ~ seq_director_name_2 + seq_actor_2_name_2 + seq_genre_2 + seq_actor_1_name_2 + seq_movie_title_2 + seq_actor_3_name_2 + seq_plot_key_words_2 + seq_director_name_3 + seq_actor_2_name_3 + seq_genre_3 + seq_actor_1_name_3 + seq_movie_title_3 + seq_actor_3_name_3 + seq_plot_key_words_3,data=trainData,size=50,maxit=10000,decay=.002)
  results <- predict(model,testData[,3:ncol(testData)],type = "class")
  prediction <- results
  output <- cbind(testData,prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
  print(confusionMatrix(output$prediction,output$class))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))
library(caret)
print(confusionMatrix(output$prediction,output$class))



##SVM
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)

#Logistic Regression
for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
  model <- svm(as.factor(class) ~ seq_director_name_2 + seq_actor_2_name_2 + seq_genre_2 + seq_actor_1_name_2 + seq_movie_title_2 + seq_actor_3_name_2 + seq_plot_key_words_2 + seq_director_name_3 + seq_actor_2_name_3 + seq_genre_3 + seq_actor_1_name_3 + seq_movie_title_3 + seq_actor_3_name_3 + seq_plot_key_words_3,data=trainData,kernel = "linear", C = 10, gamma = 0.000001, class.weights= c("0" = 1, "1" = 2))
  #model_tuned <- tune.svm(as.factor(class) ~ seq_c_name_2 + seq_company_name_2 + seq_a_city_2 + seq_state_2 + seq_zip_2 + seq_c_name_3 + seq_company_name_3 + seq_a_city_3 + seq_state_3 + seq_zip_3,data=trainData,scale = FALSE,gamma = 10^(-6:-1), cost = 10^(1:2))
  results <- predict(model,testData[,3:ncol(testData)])
  prediction <- results
  output <- cbind(testData,prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
  print(confusionMatrix(output$prediction,output$class))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))


#Random Forest
library(party)
library(randomForest)
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)

for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
  model <- randomForest(as.factor(class) ~ seq_c_name_2 + seq_company_name_2 + seq_a_city_2 + seq_state_2  + seq_a_city_3 + seq_state_3 + seq_zip_3,data=trainData)
  results <- predict(model,testData[,3:ncol(testData)])
  prediction <- results
  output <- cbind(testData,prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
  print(confusionMatrix(output$prediction,output$class))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))



##Decision Trees
library(rpart)
install.packages("party")
library(party)
cross_validation <- 10
final_accuracy <- data.frame(-1);
colnames(final_accuracy) <- c("accuracy")
folds <- cut(seq(1,nrow(data_updated)),breaks=cross_validation,labels=FALSE)

for(x in 1:cross_validation){
  #nrow(testData)
  #nrow(trainData)
  testIndexes <- which(folds==x,arr.ind=TRUE)
  testData <- data_updated[testIndexes, ]
  trainData <- data_updated[-testIndexes, ]
  model <- ctree(class ~ seq_c_name_2 + seq_company_name_2 + seq_zip_2 + seq_a_city_2 + seq_state_2  + seq_c_name_3 + seq_company_name_3 + seq_a_city_3 + seq_state_3 + seq_zip_3,data=trainData)
  results <- predict(model,testData[,3:ncol(testData)])
  prediction <- results
  output <- cbind(testData,prediction)
  accuracy <- (100*sum(output$class==output$prediction))/nrow(output)
  final_accuracy <- rbind(final_accuracy,accuracy)
  #print(confusionMatrix(as.factor(output$prediction),as.factor(output$class)))
  print(confusionMatrix(output$prediction,output$class))
}

final_accuracy <- subset(final_accuracy,accuracy != -1)
print(final_accuracy)
print("Average Accuracy")
print(mean(final_accuracy$accuracy))



