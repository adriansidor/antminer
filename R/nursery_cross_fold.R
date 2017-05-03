nursery <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data"), header = FALSE)
names(nursery) <- c("parents", "has_nurs", "form", "children", "housing", "finance", "social", "health", "application")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
folds <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_nursery <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_nursery[1, 1]<-c("C5.0")
results_nursery[2, 1]<-c("svm")
results_nursery[3, 1]<-c("naive bayes")
results_nursery[4, 1]<-c("cart")
results_nursery[5, 1]<-c("antminer")

for(i in 1:loop_size) {
  trainset_nursery<-nursery[ folds[[i]] ,]
  testset_nursery<-nursery[ -folds[[i]] ,]

  model1 <- C5.0(application ~ ., data=trainset_nursery)
  result1 <- predict(object=model1, newdata=testset_nursery, type="class")
  tab1<-conf_matrix_table(result1, testset_nursery$application)
  conf1 <- confusionMatrix(tab1)
  conf1$overall['Accuracy']
  results_nursery[1, i+1]<-c(conf1$overall['Accuracy'])

  model3 <- svm(application ~ ., data=trainset_nursery)
  result3 <- predict(object=model3, newdata=testset_nursery, type="class")
  tab3<-conf_matrix_table(result3, testset_nursery$application)
  conf3 <- confusionMatrix(tab3)
  results_nursery[2, i+1]<-c(conf3$overall['Accuracy'])

  model5 <- naiveBayes(x = subset(trainset_nursery, select=-application), y = trainset_nursery$application)
  result5 <- predict(object = model5, newdata = testset_nursery, type = "class")
  #result3 <- predict(object=model3, newdata=testset_nursery, type="class")
  tab5<-conf_matrix_table(result5, testset_nursery$application)
  conf5 <- confusionMatrix(tab5)
  results_nursery[3, i+1]<-c(conf5$overall['Accuracy'])

  model6 <- rpart(application ~ ., data=trainset_nursery)
  result6 <- predict(object=model6, newdata=testset_nursery, type="class")
  tab6<-conf_matrix_table(result6, testset_nursery$application)
  conf6 <- confusionMatrix(tab6)
  results_nursery[4, i+1]<-c(conf6$overall['Accuracy'])

  model7 <- antminer(trainset_nursery, "application", 10, 3000, 10, 10)
  result7 <- predict(model7, subset(testset_nursery, select=-application))
  tab7<-conf_matrix_table(result7$class, testset_nursery$application)
  conf7 <- confusionMatrix(tab7)
  results_nursery[5, i+1]<-c(conf7$overall['Accuracy'])
}

results_nursery
