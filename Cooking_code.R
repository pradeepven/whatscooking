#Loading the Packages
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tm)
library(SnowballC)
library(caret)

#Reading the Dataset and Prepossesing
cook<- fromJSON("train.json", flatten=TRUE)
cook$ingredients<- lapply(cook$ingredients, FUN=tolower)
cook$ingredients<- lapply(cook$ingredients, FUN= function(x) gsub("-", "_",x))
cook$ingredients<- lapply(cook$ingredients, FUN= function(x) gsub(" ", "_",x))
View(cook)

#Getting the count of cuisines
cuisine_freq<- cook %>% group_by(cuisine) %>% summarize(count=n())
plot<- ggplot(cuisine_freq, aes(cuisine, count)) + geom_point() + theme_bw()

#Building a corpus and stemming
ingredients<- Corpus(VectorSource(cook$ingredients))
ingredients<- tm_map(ingredients, stemDocument)

#Building the document matrix and sparsing
dtm<- DocumentTermMatrix(ingredients,  control = list(bounds = list(global= c(0.006*length(ingredients), .20*length(ingredients)))))
final_ingredients<- as.data.frame(as.matrix(dtm))
final_ingredients$cuisine<- as.factor(cook$cuisine)
write.csv(final_ingredients, file="cooking.csv")

#Partitioning the Dataset
cooking<- read.csv("cooking.csv")
intrain<- createDataPartition(cooking$cuisine, p=0.7, list=FALSE)
training<- cooking[intrain,]
validation<- cooking[-intrain,]

#Count the ingredients in processed dataset
count<-table(training$cuisine)
count_df<-as.data.frame(count)
count_df<- arrange(count_df, desc(count))


#Decision Tree Model (Accuracy ~36%)
library(rpart)
library(rpart.plot)
model<- rpart(cuisine~.,data=training,method="class")
prp(model)
pred_model<- predict(model, newdata=validation, type="class")

#Naive Bayes (Accuracy ~28%)
library(e1071)
model<- naiveBayes(cuisine~., data=training)
pred<- predict(model, newdata= validation, type="class")

#Confusion Matrix
CM<- confusionMatrix(pred_model, validation$cuisine)
