cars <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = FALSE)
names(cars) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "evaluation")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
folds <- createFolds(cars$evaluation, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_cars <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_cars[1, 1]<-c("C5.0")
results_cars[2, 1]<-c("svm")
results_cars[3, 1]<-c("naive bayes")
results_cars[4, 1]<-c("cart")
results_cars[5, 1]<-c("antminer")

for(i in 1:loop_size) {
  trainset_cars<-cars[ folds[[i]] ,]
  testset_cars<-cars[ -folds[[i]] ,]

  model1 <- C5.0(evaluation ~ ., data=trainset_cars)
  result1 <- predict(object=model1, newdata=testset_cars, type="class")
  tab1<-conf_matrix_table(result1, testset_cars$evaluation)
  conf1 <- confusionMatrix(tab1)
  conf1$overall['Accuracy']
  results_cars[1, i+1]<-c(conf1$overall['Accuracy'])

  model3 <- svm(evaluation ~ ., data=trainset_cars)
  result3 <- predict(object=model3, newdata=testset_cars, type="class")
  tab3<-conf_matrix_table(result3, testset_cars$evaluation)
  conf3 <- confusionMatrix(tab3)
  results_cars[2, i+1]<-c(conf3$overall['Accuracy'])

  model5 <- naiveBayes(x = subset(trainset_cars, select=-evaluation), y = trainset_cars$evaluation)
  result5 <- predict(object = model5, newdata = testset_cars, type = "class")
  #result3 <- predict(object=model3, newdata=testset_cars, type="class")
  tab5<-conf_matrix_table(result5, testset_cars$evaluation)
  conf5 <- confusionMatrix(tab5)
  results_cars[3, i+1]<-c(conf5$overall['Accuracy'])

  model6 <- rpart(evaluation ~ ., data=trainset_cars)
  result6 <- predict(object=model6, newdata=testset_cars, type="class")
  tab6<-conf_matrix_table(result6, testset_cars$evaluation)
  conf6 <- confusionMatrix(tab6)
  results_cars[4, i+1]<-c(conf6$overall['Accuracy'])

  model7 <- antminer(trainset_cars, "evaluation", 10, 3000, 10, 10)
  result7 <- predict(model7, subset(testset_cars, select=-evaluation))
  tab7<-conf_matrix_table(result7$class, testset_cars$evaluation)
  conf7 <- confusionMatrix(tab7)
  results_cars[5, i+1]<-c(conf7$overall['Accuracy'])
}

results_cars
