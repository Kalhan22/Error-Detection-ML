
#K Means Clustering in R

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

#Clustering Result with derived features
clustering_result <- kmeans(data[,2:5], 2, nstart = 100)
data <- cbind(data,data.frame(clustering_result$cluster))
x <- data.frame(data$T_Class,data$clustering_result.cluster)
colnames(x) <- c("actual","prediction")
table(x)



#Result: There is no concrete result from Kmeans clustering





