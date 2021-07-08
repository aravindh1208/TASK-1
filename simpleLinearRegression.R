#                                      THE SPARK FOUNDATION  
#                                       GRIP JULY-21 INTERN
#                                                                              NAME :  ARAVINDHAN V
#                                          TASK-1
#             Predict the percentage of a student based on the no. of study hours.
#                                 Using simple linear regression

#data pre-processing
#hours<-c(2.5,5.1,3.2,8.5,3.5,1.5,9.2,5.5,8.3,2.7,7.7,5.9,4.5,3.3,1.1,8.9,2.5,1.9,6.1,7.4,2.7,4.8,3.8,6.9,7.8)
#Scores<-c(21,47,27,75,30,20,88,60,81,25,85,62,41,42,17,95,30,24,67,69,30,54,35,76,86)
data<-read.csv(file.choose())
colnames(data)<-c("hours","scores")
data
#splitting into test and train data
installed.packages('caTools')
library(caTools)
set.seed(200) # Random number Generation 
split<-sample.split(data$scores, SplitRatio = 0.75)
train<-subset(data, split==T)
test<-subset(data, split==F)

#fitting simple linear regression
linearr<-lm(formula = scores~hours,data = train)
summary(linearr)

#predicting test set results
y_pred<-predict(linearr, newdata = test)
y_pred
dataPred<-data.frame(y_pred)
dataPred

#visualizing train set results
library(ggplot2)
y_pred_train<-predict(linearr, newdata = train)
ggplot()+
  geom_point(aes(x=train$hours, y=train$scores),       
             colour="blue")+
  geom_line(aes(x=train$hours, y=y_pred_train),colour="red")+
  ggtitle("scores vs hours - training dataset")+
  xlab("hours")+
  ylab("scores")

#visualizing test set results
ggplot()+
  geom_point(aes(x=test$hours, y=test$score),
             colour="red")+
  geom_line(aes(x=train$hours, y=y_pred_train),colour="blue")+
  ggtitle("scors vs hours-testing dataset")+
  xlab("hours")+
  ylab("scores")


#prediction
p<-data.frame(9.25)
colnames(p) <- "hours"
Requiredout<-predict(linearr,newdata=p)
Requiredout


pred <- data.frame(cbind(actual=test$scores, predicted=dataPred)) # cbind is used to combine data frame by columns
#accuracy
accuracy <- mean(apply(pred, 1, min) / apply(pred, 1, max))
accuracy
#Error
install.packages("Metrics")
library(Metrics)
y_pred<-as.numeric(y_pred)
str(y_pred) # str is used for compact display of internatal r structure
str(test$scores)
mae(test$scores,y_pred)

