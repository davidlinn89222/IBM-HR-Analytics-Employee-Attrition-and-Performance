hr <- read.csv("E:/資料/統學/Final proj/WA_Fn-UseC_-HR-Employee-Attrition.csv")
hr <- hr[,c(-4,-9,-13,-20,-22,-27,-28)]


set.seed(1)

hr$Attrition <- as.factor(hr$Attrition)
hr$Education <- as.factor(hr$Education)
hr$EducationField <- as.factor(hr$EducationField)
hr$EnvironmentSatisfaction <- as.factor(hr$EnvironmentSatisfaction)
hr$Gender <- as.factor(hr$Gender)
hr$JobInvolvement <- as.factor(hr$JobInvolvement)
hr$JobLevel <- as.factor(hr$JobLevel)
hr$JobRole <- as.factor(hr$JobRole)
hr$JobSatisfaction <- as.factor(hr$JobSatisfaction)
hr$MaritalStatus <- as.factor(hr$MaritalStatus)
hr$OverTime <- as.factor(hr$OverTime)
hr$PerformanceRating <- as.factor(hr$PerformanceRating)
hr$RelationshipSatisfaction <- as.factor(hr$RelationshipSatisfaction)
hr$WorkLifeBalance <- as.factor(hr$WorkLifeBalance)

train_data <- sample(1:nrow(hr), nrow(hr)*0.8)
hr_train <- hr[train_data,]
hr_test <- hr[-train_data,]

rf <- randomForest(Attrition ~., data=hr_train)

#plot(rf)

rf_pred <- predict(rf, newdata=hr_test, type = 'class')
cat('accuracy:', mean(rf_pred == hr_test$Attrition))

ntree <- which.min(rf$err.rate[, 1])
cat('best tree size:', ntree)

hyper_grid <- expand.grid(mtry = seq(2, 20, by = 1),
                          node_size = seq(3, 9, by = 2),
                          sample_size = c(0.55, 0.632, 0.7, 0.8),
                          OOB_error = 0)

for (i in 1:nrow(hyper_grid)) {
  # train model
  model <- ranger(formula = Attrition ~ ., data = hr_train, 
                  num.trees = ntree, 
                  mtry = hyper_grid$mtry[i],
                  min.node.size = hyper_grid$node_size[i], 
                  sample.fraction = hyper_grid$sample_size[i],
                  seed = 101)
  
  hyper_grid$OOB_error[i] <- model$prediction.error
}

min_OOB_error <- hyper_grid %>% 
  dplyr::arrange(OOB_error) %>% 
  head(10)


ACC_rf <- data.frame(mtry=rep(0, 10),
                     node_size=rep(0, 10),
                     sample_size=rep(0, 10),
                     OOB_error=rep(0, 10),
                     ACC=rep(0, 10),
                     AUC=rep(0, 10))

for (i in 1:10){
  rf_param <- min_OOB_error[i,]
  
  rf_ <- randomForest(formula=Attrition ~., data=hr_train,
                      ntree=ntree, 
                      mtry=rf_param$mtry,
                      nodesize=rf_param$node_size,
                      sampsize=ceiling(rf_param$sample_size * nrow(hr_train)))
  
  rf_pred <- predict(rf_, newdata=hr_test, type='class')
  rf_prop <- predict(rf_, newdata=hr_test, type='prob')
  acc <- mean(rf_pred==hr_test$Attrition)
  auc<-roc_auc(rf_prop[,2],hr_test$Attrition)
  ACC_rf[i, ] <- cbind(min_OOB_error[i,], ACC=acc,AUC=auc$auc)
}

best_rf_param <- ACC_rf %>%
  dplyr::arrange(desc(ACC)) %>%
  head(1)


set.seed(2)
rf_best <- randomForest(formula=Attrition ~., data=hr_train,
                        ntree=ntree, 
                        mtry=best_rf_param$mtry,
                        nodesize=best_rf_param$node_size,
                        sampsize=ceiling(best_rf_param$sample_size * nrow(hr_train)))

rf_prob <- predict(rf_best, hr_test, type='prob')[,2]
rf_pred <- ifelse(rf_prob > 0.5, 'Yes',
                  'No')

performance(rf_prob, as.factor(rf_pred),
            'Random Forest', as.factor(hr_test$Attrition),
            'Yes')

confusionMatrix(factor(rf_pred), factor(hr_test$Attrition))


importance <- randomForest::importance(rf_best)    #Gini
randomForest::varImpPlot(rf_best,main="variable importance plot") 
