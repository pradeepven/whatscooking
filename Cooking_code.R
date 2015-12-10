#load packages
library(bitops)
library(caret)
library(class)
library(e1071)
library(ggplot2)
library(jsonlite)
library(SnowballC)
library(tm)

#import data
cook <- fromJSON("train.json", flatten = TRUE)

#setting seed 
set.seed(74)

#re-code target feature as a factor
cook$cuisine <- factor(cook$cuisine)

#Pre-possesing
cook$ingredients<- lapply(cook$ingredients, FUN= function(x) gsub("-", "_",x))
cook$ingredients<- lapply(cook$ingredients, FUN= function(x) gsub(" ", "_",x))

#load explanatory text data into a corpus
cook_corpus <- VCorpus(VectorSource(cook$ingredients))

#convert corpus to a document term matrix
#control operations are TOLOWER, REMOVENUMBERS, STOPWORDS, REMOVEPUNCTUATION and STEMMING
#Pre-Processing
cook_DTM <- DocumentTermMatrix(cook_corpus, control = list(
  tolower = TRUE, 
  removeNumbers = TRUE, 
  stopwords = TRUE, 
  removePunctuation = TRUE, 
  stemming = TRUE))

#Filtering the words and returning vector of all words repeated at least 20 times
#Removing the most common words as well i.e which have a count of more than 4000
cookfreqwords <- findFreqTerms(x = cook_DTM, 20, 4000)

#draw random sample (vector of indices)
q <- sample(39774, size = 39774*.70)

#split dtm into testing and training sections
cook_train <- cook_DTM[q,]
cook_test <- cook_DTM[-q,]

#draw correpsonding labels for testing and training data from the raw data
cook_training_labels <- cook[q,]$cuisine
cook_test_labels <- cook[-q,]$cuisine 

#applying the frequent words vector to the training and testing sets to refine them
cookfreqtrain <- cook_train[,cookfreqwords]
cookfreqtest <- cook_test[,cookfreqwords]

#short function to convert counting the number of word occurences, as it is listed in the DTm,
#to a binary Yes/No
convert_counts <- function(x) {x <- ifelse(x >0, "Yes", "No")}

#applying the convert function to the training and testing sets
cook_train <- apply(cookfreqtrain, FUN = convert_counts, MARGIN = 2)
cook_test <- apply(cookfreqtest, FUN = convert_counts, MARGIN = 2)

#creating Naive Bayes model
NBmodel <- naiveBayes(cook_train, cook_training_labels)

#predicting
preds <- predict(NBmodel, cook_test)

#confusion matrix - 72.5% accuracy
confusionMatrix(preds, cook_test_labels)


#Decision Tree Model (Accuracy ~36%)
library(rpart)
library(rpart.plot)
model<- rpart(cook_train, cook_training_labels, method="class")
prp(model)
pred_model<- predict(model, newdata=cook_, type="class")


#Linear SVM (~52%)
x<- apply(cook_train, MARGIN = 2, FUN= function(x) gsub("Yes", 1, x))
x<- apply(x, MARGIN = 2, FUN= function(x) gsub("No", 0, x))
View(x)
y<- apply(cook_test, MARGIN = 2, FUN= function(x) gsub("Yes", 1, x))
y<- apply(y, MARGIN = 2, FUN= function(x) gsub("No", 0, x))
View(y)
n<- svm(x,cook_training_labels, scale= F, type="C-classification", degree=20)
preds<- predict(n, y, decision.values = T)
confusionMatrix(preds, cook_test_labels)
