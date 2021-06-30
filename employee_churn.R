setwd("D:/dataset/DMML")
getwd()

#data manipulating 
library(dplyr)

#data Visualization
library(ggplot2)
library(ggthemes)
library(corrplot)

#data modelling 
library(MASS)
#install.packages("caTools")
library(caTools)

#data evalution 
library(caret)
churn<-read.csv("employee_churn.csv",header = T, stringsAsFactors = T)
head(churn)
summary(churn)
str(churn)
#preprocessing the data 
colSums(is.na(churn))

#count of employees who left the company - 3571
table(churn$left)
#exploring the data
ggplot(churn, aes(left)) +
  geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..), fill=left)) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("frequencies") +
  xlab("Churn") +
  geom_text(aes(label = scales::percent((count)/sum(..count..)), y=(count)/sum(..count..)), stat= "count",vjust =-.5)+
  scale_fill_brewer(palette="Set2")

# checking on satisfication levels
ggplot(churn, aes(x = satisfaction_level, fill = left)) + 
  geom_density(alpha = 0.6) +
  labs(x = "satisfaction_level", y = "") +
  ggtitle("Attrition by income satisfaction_level") +
  theme_classic()

#grouping by salary
ggplot(churn,aes(x=left,group=salary))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~salary)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Churn",y="Percentage",title="churn vs. salary")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("frequencies") +
  scale_fill_brewer(palette="Set1")

#finding the correlation
Co<-churn
Co<- Co [,!(names(Co) %in% c("left"))]
str(Co)

Co$number_project<-as.numeric(Co$number_project)
Co$average_montly_hours<-as.numeric(Co$average_montly_hours)
Co$time_spend_company<-as.numeric(Co$time_spend_company)
Co$Work_accident<-as.numeric(Co$Work_accident)
Co$promotion_last_5years<-as.numeric(Co$promotion_last_5years)
Co$Departments<-as.numeric(Co$Departments)
Co$salary<-as.numeric(Co$salary)

relation<-cor(Co)
corrplot(relation, method = "number")


#splitting the data into test and train data

my_log<-sample.split(churn$left , SplitRatio =  0.60)## considering 60% data for training rest 40% will used for testing
train<-subset(churn,my_log==T)
test<-subset(churn,my_log==F)
nrow(train)
nrow(test)

#logistic model
log_new_model<-glm(left~., data=train, family = binomial)
mypred<-predict(log_new_model, newdata = test, type = "response")
summary(log_new_model)
#finding the probabality range
range(mypred)

#evaluating using Confusion matrix
xtab<- table(test$left,mypred>0.25)
xtab

#accuracy(test$left,mypred)
#accuracy
x<-(3471+1027)/(3471+1027+1100+401)
x
str(churn)
#churn$satisfaction_level<-as.factor(churn$satisfaction_level)
#churn$last_evaluation<-as.factor(churn$last_evaluation)
#churn$number_project<-as.factor(churn$number_project)
#churn$average_montly_hours<-as.factor(churn$average_montly_hours)
#churn$time_spend_company<-as.factor(churn$time_spend_company)
#churn$Work_accident<-as.factor(churn$Work_accident)
#churn$promotion_last_5years<-as.factor(churn$promotion_last_5years)
#class(churn$left)
#confusionMatrix(xtab)

x1<-stepAIC(log_new_model, direction="both")
##############################################################################################################

#################################### Decision Tree ########################################################
#install.packages("tree")
library(tree)
library(rpart.plot)
library(rpart)
library(caret)
dtree<-tree(left~., data = train)
dtree1<-rpart(left~., data=train)
dpredict<-predict(dtree, newdata = test , type = "class") 
dpredict1<-predict(dtree1, newdata = test , type = "class")
summary(dtree)
rpart.plot(dtree1)
confusionMatrix(test$left,dpredict)
confusionMatrix(test$left,dpredict1)

###################################################################################################

