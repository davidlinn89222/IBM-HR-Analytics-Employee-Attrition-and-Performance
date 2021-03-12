library(GGally)
library(ggplot2)
library(caret)
library(tidyverse)
library(class)
library(MASS)
library(e1071)
hrdata=read.csv("C:/Users/Nick/Desktop/WA_Fn-UseC_-HR-Employee-Attrition (1).csv")
names(hrdata)[1]="Age"
#修變數型態
hrdata$Attrition=as.numeric(as.factor(hrdata$Attrition))
hrdata$Attrition=as.factor(hrdata$Attrition)
hrdata$Education=as.factor(hrdata$Education)
hrdata$EmployeeCount=as.factor(hrdata$EmployeeCount)
hrdata$EnvironmentSatisfaction=as.factor(hrdata$EnvironmentSatisfaction)
hrdata$JobInvolvement=as.factor(hrdata$JobInvolvement)
hrdata$JobLevel=as.factor(hrdata$JobLevel)
hrdata$JobSatisfaction = as.factor(hrdata$JobSatisfaction)
hrdata$JobInvolvement=as.factor(hrdata$JobInvolvement)
hrdata$JobLevel=as.factor(hrdata$JobLevel)
hrdata$JobSatisfaction =as.factor(hrdata$JobSatisfaction)
hrdata$PerformanceRating=as.factor(hrdata$PerformanceRating)
hrdata$RelationshipSatisfaction=as.factor(hrdata$RelationshipSatisfaction)
hrdata$StockOptionLevel=as.factor(hrdata$StockOptionLevel)
#刪除變數後的資料
hr=hrdata[,-c(4,9,13,20,22,27,28)]
hr$WorkLifeBalance=as.factor(hr$WorkLifeBalance)
rownames(hr)=hr [,8]
hr=hr[,-c(8)]

#看Attrition與各類別變數的table,correlation
# t1 <- table(hr$Attrition,hr$BusinessTravel);t1
# t2 <- table(hr$Attrition,hr$Department);t2 
# t3 <- table(hr$Attrition,hr$Education);t3 
# t4 <- table(hr$Attrition,hr$EducationField);t4 
# t5 <- table(hr$Attrition,hr$EnvironmentSatisfaction);t5 
# t6 <- table(hr$Attrition,hr$Gender);t6 
# t7 <- table(hr$Attrition,hr$JobInvolvement);t7
# t8 <- table(hr$Attrition,hr$JobLevel);t8
# t9 <- table(hr$Attrition,hr$JobRole);t9
# t10 <- table(hr$Attrition,hr$JobSatisfaction);t10
# t11 <- table(hr$Attrition,hr$MaritalStatus);t11
# t12 <- table(hr$Attrition,hr$OverTime);t12
# t13 <- table(hr$Attrition,hr$PerformanceRating);t13
# t14 <- table(hr$Attrition,hr$RelationshipSatisfaction);t14
# t15 <- table(hr$Attrition,hr$WorkLifeBalance);t15
# cor(hr[,c(1,5,15,16,18,21,22,24,25,26,27)])


#--------------------GGpairs---------------------------------#
# library(GGally)
# ggpairs(hr[,c(1,2,5,15,16,18,21,22,24,25,26,27)],mapping= aes(color = Attrition ,fill = Attrition, alpha = 0.6),
#         upper = list(combo = 'box'),
#         diag = list(discrete = wrap('barDiag', position = 'fill')),
#         lower = list(combo = 'dot_no_facet'))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# #--------------------------------------------------------------------#
# library(corrplot)
# corMat=cor(hr[,c(1,5,15,16,18,21,22,24,25,26,27)])
# corrplot(corMat, method="ellipse")
# corrplot.mixed(corMat, lower="ellipse", upper="number",
#                order = "hclust", tl.col="black")
#切資料

set.seed(1)
train=sample(1:nrow(hr),nrow(hr)*0.8)
training_data=hr[train,]
testing_data=hr[-train,]

#par(mfrow=c(1,4))

#Naive Bayes
#全部變數
nb.fit <- naiveBayes(Attrition~., data=training_data)
nb.pred <- predict(nb.fit, testing_data,k=10)
table(nb.pred, testing_data$Attrition)
mean(nb.pred == testing_data$Attrition)

#
# par(mfrow=c(1,2))
# hist(hr$Age[hr$Attrition==1]);hist(hr$Age[hr$Attrition==2])
# hist(hr$Department[hr$Attrition==1]);hist(hr$Department[hr$Attrition==2])
# hist(hr$DistanceFromHome[hr$Attrition==1]);hist(hr$DistanceFromHome[hr$Attrition==2])
# hist(hr$EnvironmentSatisfaction[hr$Attrition==1]);hist(hr$EnvironmentSatisfaction[hr$Attrition==2])
# hist(hr$JobInvolvement[hr$Attrition==1]);hist(hr$JobInvolvement[hr$Attrition==2])
# hist(hr$JobRole[hr$Attrition==1]);hist(hr$JobRole[hr$Attrition==2])
# hist(hr$JobSatisfaction[hr$Attrition==1]);hist(hr$JobSatisfaction[hr$Attrition==2])
# hist(hr$MonthlyIncome[hr$Attrition==1]);hist(hr$MonthlyIncome[hr$Attrition==2])
# hist(hr$PercentSalaryHike[hr$Attrition==1]);hist(hr$PercentSalaryHike[hr$Attrition==2])



#用caret package做NB( ) 

#confusion Matrix
features <- setdiff(names(training_data),"Attrition")
features=as.vector(features)
x <- training_data[,features]
y <- training_data$Attrition

train_control <- trainControl(
  method = "cv", 
  number = 10
)
# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)
# results
confusionMatrix(nb.m1) #84%
