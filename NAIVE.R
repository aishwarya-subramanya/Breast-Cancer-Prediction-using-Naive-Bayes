library(corrplot) 
library(ggplot2)
library(GGally)
# load the breast cancer data
breast_cancer <- read_csv('wdbc.csv')
View(breast_cancer)
# check structure of data set
str(breast_cancer)
# remove the ID variable from data set
breast_cancer <- breast_cancer[-1]
# check diagnosis variable of data set
table(breast_cancer$diagnosis)
# recode the diagnosis variable
breast_cancer$diagnosis<- factor(breast_cancer$diagnosis, levels = c("B", "M"),
                                 labels = c("Benign", "Malignant"))
# check the proportion of diagnosis variable
round(prop.table(table(breast_cancer$diagnosis)) * 100, digits = 1)
set.seed(1)
library(caret)
partition <- createDataPartition(breast_cancer$diagnosis, p=0.7, list=F)
train <- breast_cancer[partition,]
test <- breast_cancer[-partition,]
sc_train<-as.data.frame(scale(breast_cancer[,-1]), scale=T, center=T)
sc_test<-as.data.frame(scale(breast_cancer[,-1]), scale=T, center=T)
train

#Naive Bayes method of classification
library(caret)
library(e1071)
model<-naiveBayes(diagnosis~.,data=train, laplace=1)
model$apriori
model$tables
pred<-predict(model,newdata=test)
confusionMatrix(pred,reference=test$diagnosis, positive="Malignant")
pred_prob<-predict(model, newdata = test, type="raw")
library(ROCR)
p_test<-prediction(pred_prob[,2], test$diagnosis)
perf<-performance(p_test,"tpr", "fpr")
plot(perf)
performance(p_test,"auc")@y.values
