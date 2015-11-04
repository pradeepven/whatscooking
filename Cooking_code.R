library("jsonlite")
library(dplyr)
cook<- fromJSON("train.json")
View(cook)
cuisine_freq<- cook %>% group_by(cuisine) %>% summarize(count=n())
library(ggplot2)
plot<- ggplot(cuisine_freq, aes(cuisine, count)) + geom_point() + theme_bw()
library(tm)
ingredients<- Corpus(VectorSource(cook$ingredients))
tdm<- TermDocumentMatrix(ingredients)
sparse<- removeSparseTerms(tdm, 0.99)
final_ingredients<- as.data.frame(as.matrix(sparse))
final_ingredients<- t(final_ingredients)
cuisine<- as.data.frame(cuisine=cook$cuisine)
final_cook<- cbind(final_ingredients, cuisine)
write.csv(final_cook, file="cooking.csv")
#Partitioning the Dataset into training and validation
library("caret")
intrain<- createDataPartition(cook$cook.cuisine, p=0.7, list=FALSE)
training<- cook[intrain,]
validation<- cook[-intrain,]
#Building the decision tree model
library(rpart)
library(rpart.plot)
model<- rpart(cook.cuisine~.,data=training,method="class")
prp(model)
pred_model<- predict(model, newdata=validation, type="class")
head(pred_model,1)
CM<- confusionMatrix(pred_model, validation$cook.cuisine)
CM

