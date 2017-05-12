nursery <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data"), header = FALSE)
names(nursery) <- c("parents", "has_nurs", "form", "children", "housing", "finance", "social", "health", "application")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
nursery_folds <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_nursery <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_nursery[1, 1]<-c("C5.0")
results_nursery[2, 1]<-c("svm")
results_nursery[3, 1]<-c("naive bayes")
results_nursery[4, 1]<-c("cart")
results_nursery[5, 1]<-c("antminer")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_nursery<-nursery[ nursery_folds[[i]] ,]
  testset_nursery<-nursery[ -nursery_folds[[i]] ,]

  nursery_model1 <- C5.0(application ~ ., data=trainset_nursery)
  nursery_result1 <- predict(object=nursery_model1, newdata=testset_nursery, type="class")
  nursery_tab1<-conf_matrix_table(nursery_result1, testset_nursery$application)
  nursery_conf1 <- confusionMatrix(nursery_tab1)
  nursery_conf1$overall['Accuracy']
  results_nursery[1, i+1]<-c(nursery_conf1$overall['Accuracy'])

  nursery_model3 <- svm(application ~ ., data=trainset_nursery)
  nursery_result3 <- predict(object=nursery_model3, newdata=testset_nursery, type="class")
  nursery_tab3<-conf_matrix_table(nursery_result3, testset_nursery$application)
  nursery_conf3 <- confusionMatrix(nursery_tab3)
  results_nursery[2, i+1]<-c(nursery_conf3$overall['Accuracy'])

  nursery_model5 <- naiveBayes(x = subset(trainset_nursery, select=-application), y = trainset_nursery$application)
  nursery_result5 <- predict(object = nursery_model5, newdata = testset_nursery, type = "class")
  #nursery_result3 <- predict(object=model3, newdata=testset_nursery, type="class")
  nursery_tab5<-conf_matrix_table(nursery_result5, testset_nursery$application)
  nursery_conf5 <- confusionMatrix(nursery_tab5)
  results_nursery[3, i+1]<-c(nursery_conf5$overall['Accuracy'])

  nursery_model6 <- rpart(application ~ ., data=trainset_nursery)
  nursery_result6 <- predict(object=nursery_model6, newdata=testset_nursery, type="class")
  nursery_tab6<-conf_matrix_table(nursery_result6, testset_nursery$application)
  nursery_conf6 <- confusionMatrix(nursery_tab6)
  results_nursery[4, i+1]<-c(nursery_conf6$overall['Accuracy'])

  nursery_model7 <- antminer4(trainset_nursery, "application", 10, 1000, 10, 10)
  nursery_result7 <- predict(nursery_model7, subset(testset_nursery, select=-application))
  nursery_tab7<-conf_matrix_table(nursery_result7$class, testset_nursery$application)
  nursery_conf7 <- confusionMatrix(nursery_tab7)
  results_nursery[5, i+1]<-c(nursery_conf7$overall['Accuracy'])
}
print("calkowity czas")
print(starttime-Sys.time())
results_nursery
