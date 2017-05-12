cars <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = FALSE)
names(cars) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "evaluation")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
antminer6_cars_folds <- createFolds(cars$evaluation, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer6_results_cars <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer6_results_cars[1, 1]<-c("C5.0")
antminer6_results_cars[2, 1]<-c("svm")
antminer6_results_cars[3, 1]<-c("naive bayes")
antminer6_results_cars[4, 1]<-c("cart")
antminer6_results_cars[5, 1]<-c("antminer")

for(i in 1:loop_size) {
  antminer6_trainset_cars<-cars[ antminer6_cars_folds[[i]] ,]
  antminer6_testset_cars<-cars[ -antminer6_cars_folds[[i]] ,]

  antminer6_cars_model1 <- C5.0(evaluation ~ ., data=antminer6_trainset_cars)
  antminer6_cars_result1 <- predict(object=antminer6_cars_model1, newdata=antminer6_testset_cars, type="class")
  antminer6_cars_tab1<-conf_matrix_table(antminer6_cars_result1, antminer6_testset_cars$evaluation)
  antminer6_cars_conf1 <- confusionMatrix(antminer6_cars_tab1)
  antminer6_cars_conf1$overall['Accuracy']
  antminer6_results_cars[1, i+1]<-c(antminer6_cars_conf1$overall['Accuracy'])

  antminer6_cars_model3 <- svm(evaluation ~ ., data=antminer6_trainset_cars)
  antminer6_cars_result3 <- predict(object=antminer6_cars_model3, newdata=antminer6_testset_cars, type="class")
  antminer6_cars_tab3<-conf_matrix_table(antminer6_cars_result3, antminer6_testset_cars$evaluation)
  antminer6_cars_conf3 <- confusionMatrix(antminer6_cars_tab3)
  antminer6_results_cars[2, i+1]<-c(antminer6_cars_conf3$overall['Accuracy'])

  antminer6_cars_model5 <- naiveBayes(x = subset(antminer6_trainset_cars, select=-evaluation), y = antminer6_trainset_cars$evaluation)
  antminer6_cars_result5 <- predict(object = antminer6_cars_model5, newdata = antminer6_testset_cars, type = "class")
  #antminer6_cars_result3 <- predict(object=antminer6_cars_model3, newdata=antminer6_testset_cars, type="class")
  antminer6_cars_tab5<-conf_matrix_table(antminer6_cars_result5, antminer6_testset_cars$evaluation)
  antminer6_cars_conf5 <- confusionMatrix(antminer6_cars_tab5)
  antminer6_results_cars[3, i+1]<-c(antminer6_cars_conf5$overall['Accuracy'])

  antminer6_cars_model6 <- rpart(evaluation ~ ., data=antminer6_trainset_cars)
  antminer6_cars_result6 <- predict(object=antminer6_cars_model6, newdata=antminer6_testset_cars, type="class")
  antminer6_cars_tab6<-conf_matrix_table(antminer6_cars_result6, antminer6_testset_cars$evaluation)
  antminer6_cars_conf6 <- confusionMatrix(antminer6_cars_tab6)
  antminer6_results_cars[4, i+1]<-c(antminer6_cars_conf6$overall['Accuracy'])

  antminer6_cars_model7 <- antminer6(antminer6_trainset_cars, "evaluation", 10, 1000, 10, 10)
  antminer6_cars_result7 <- predict(antminer6_cars_model7, subset(antminer6_testset_cars, select=-evaluation))
  antminer6_cars_tab7<-conf_matrix_table(antminer6_cars_result7$class, antminer6_testset_cars$evaluation)
  antminer6_cars_conf7 <- confusionMatrix(antminer6_cars_tab7)
  antminer6_results_cars[5, i+1]<-c(antminer6_cars_conf7$overall['Accuracy'])
}

antminer6_results_cars
