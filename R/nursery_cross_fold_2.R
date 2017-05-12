nursery <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data"), header = FALSE)
names(nursery) <- c("parents", "has_nurs", "form", "children", "housing", "finance", "social", "health", "application")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
antminer5_nursery_folds_run1 <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_nursery_run1 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_nursery_run1[1, 1]<-c("C5.0")
antminer5_results_nursery_run1[2, 1]<-c("svm")
antminer5_results_nursery_run1[3, 1]<-c("naive bayes")
antminer5_results_nursery_run1[4, 1]<-c("cart")
antminer5_results_nursery_run1[5, 1]<-c("antminer4")
antminer5_results_nursery_run1[6, 1]<-c("antminer5")
antminer5_results_nursery_run1[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_nursery_run1<-nursery[ antminer5_nursery_folds_run1[[i]] ,]
  antminer5_testset_nursery_run1<-nursery[ -antminer5_nursery_folds_run1[[i]] ,]

  antminer5_nursery_model_c50_run1 <- C5.0(application ~ ., data=antminer5_trainset_nursery_run1)
  antminer5_nursery_result_c50_run1 <- predict(object=antminer5_nursery_model_c50_run1, newdata=antminer5_testset_nursery_run1, type="class")
  antminer5_nursery_tab_c50_run1<-conf_matrix_table(antminer5_nursery_result_c50_run1, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_c50_run1 <- confusionMatrix(antminer5_nursery_tab_c50_run1)
  antminer5_nursery_conf_c50_run1$overall['Accuracy']
  antminer5_results_nursery_run1[1, i+1]<-c(antminer5_nursery_conf_c50_run1$overall['Accuracy'])

  antminer5_nursery_model_svm_run1 <- svm(application ~ ., data=antminer5_trainset_nursery_run1)
  antminer5_nursery_result_svm_run1 <- predict(object=antminer5_nursery_model_svm_run1, newdata=antminer5_testset_nursery_run1, type="class")
  antminer5_nursery_tab_svm_run1<-conf_matrix_table(antminer5_nursery_result_svm_run1, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_svm_run1 <- confusionMatrix(antminer5_nursery_tab_svm_run1)
  antminer5_results_nursery_run1[2, i+1]<-c(antminer5_nursery_conf_svm_run1$overall['Accuracy'])

  antminer5_nursery_model_bayes_run1 <- naiveBayes(x = subset(antminer5_trainset_nursery_run1, select=-application), y = antminer5_trainset_nursery_run1$application)
  antminer5_nursery_result_bayes_run1 <- predict(object = antminer5_nursery_model_bayes_run1, newdata = antminer5_testset_nursery_run1, type = "class")
  #antminer5_nursery_result_svm_run1 <- predict(object=antminer5_nursery_model_svm_run1, newdata=antminer5_testset_nursery_run1, type="class")
  antminer5_nursery_tab_bayes_run1<-conf_matrix_table(antminer5_nursery_result_bayes_run1, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_bayes_run1 <- confusionMatrix(antminer5_nursery_tab_bayes_run1)
  antminer5_results_nursery_run1[3, i+1]<-c(antminer5_nursery_conf_bayes_run1$overall['Accuracy'])

  antminer5_nursery_model_rpart_run1 <- rpart(application ~ ., data=antminer5_trainset_nursery_run1)
  antminer5_nursery_result_rpart_run1 <- predict(object=antminer5_nursery_model_rpart_run1, newdata=antminer5_testset_nursery_run1, type="class")
  antminer5_nursery_tab6<-conf_matrix_table(antminer5_nursery_result_rpart_run1, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_rpart_run1 <- confusionMatrix(antminer5_nursery_tab6)
  antminer5_results_nursery_run1[4, i+1]<-c(antminer5_nursery_conf_rpart_run1$overall['Accuracy'])

  antminer5_nursery_model_ant4_run1 <- antminer4(antminer5_trainset_nursery_run1, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant4_run1 <- predict(antminer5_nursery_model_ant4_run1, subset(antminer5_testset_nursery_run1, select=-application))
  antminer5_nursery_tab_ant4_run1<-conf_matrix_table(antminer5_nursery_result_ant4_run1$class, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_ant4_run1 <- confusionMatrix(antminer5_nursery_tab_ant4_run1)
  antminer5_results_nursery_run1[5, i+1]<-c(antminer5_nursery_conf_ant4_run1$overall['Accuracy'])

  antminer5_nursery_model_ant5_run1 <- antminer5(antminer5_trainset_nursery_run1, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant5_run1 <- predict(antminer5_nursery_model_ant5_run1, subset(antminer5_testset_nursery_run1, select=-application))
  antminer5_nursery_tab_ant5_run1<-conf_matrix_table(antminer5_nursery_result_ant5_run1$class, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_ant5_run1 <- confusionMatrix(antminer5_nursery_tab_ant5_run1)
  antminer5_results_nursery_run1[6, i+1]<-c(antminer5_nursery_conf_ant5_run1$overall['Accuracy'])

  antminer5_nursery_model_ant6_run1 <- antminer6(antminer5_trainset_nursery_run1, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant6_run1 <- predict(antminer5_nursery_model_ant6_run1, subset(antminer5_testset_nursery_run1, select=-application))
  antminer5_nursery_tab_ant6_run1<-conf_matrix_table(antminer5_nursery_result_ant6_run1$class, antminer5_testset_nursery_run1$application)
  antminer5_nursery_conf_ant6_run1 <- confusionMatrix(antminer5_nursery_tab_ant6_run1)
  antminer5_results_nursery_run1[7, i+1]<-c(antminer5_nursery_conf_ant6_run1$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer5_results_nursery_run1

#############################
antminer5_nursery_folds_run2 <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_nursery_run2 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_nursery_run2[1, 1]<-c("C5.0")
antminer5_results_nursery_run2[2, 1]<-c("svm")
antminer5_results_nursery_run2[3, 1]<-c("naive bayes")
antminer5_results_nursery_run2[4, 1]<-c("cart")
antminer5_results_nursery_run2[5, 1]<-c("antminer4")
antminer5_results_nursery_run2[6, 1]<-c("antminer5")
antminer5_results_nursery_run2[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_nursery_run2<-nursery[ antminer5_nursery_folds_run2[[i]] ,]
  antminer5_testset_nursery_run2<-nursery[ -antminer5_nursery_folds_run2[[i]] ,]

  antminer5_nursery_model_c50_run2 <- C5.0(application ~ ., data=antminer5_trainset_nursery_run2)
  antminer5_nursery_result_c50_run2 <- predict(object=antminer5_nursery_model_c50_run2, newdata=antminer5_testset_nursery_run2, type="class")
  antminer5_nursery_tab_c50_run2<-conf_matrix_table(antminer5_nursery_result_c50_run2, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_c50_run2 <- confusionMatrix(antminer5_nursery_tab_c50_run2)
  antminer5_nursery_conf_c50_run2$overall['Accuracy']
  antminer5_results_nursery_run2[1, i+1]<-c(antminer5_nursery_conf_c50_run2$overall['Accuracy'])

  antminer5_nursery_model_svm_run2 <- svm(application ~ ., data=antminer5_trainset_nursery_run2)
  antminer5_nursery_result_svm_run2 <- predict(object=antminer5_nursery_model_svm_run2, newdata=antminer5_testset_nursery_run2, type="class")
  antminer5_nursery_tab_svm_run2<-conf_matrix_table(antminer5_nursery_result_svm_run2, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_svm_run2 <- confusionMatrix(antminer5_nursery_tab_svm_run2)
  antminer5_results_nursery_run2[2, i+1]<-c(antminer5_nursery_conf_svm_run2$overall['Accuracy'])

  antminer5_nursery_model_bayes_run2 <- naiveBayes(x = subset(antminer5_trainset_nursery_run2, select=-application), y = antminer5_trainset_nursery_run2$application)
  antminer5_nursery_result_bayes_run2 <- predict(object = antminer5_nursery_model_bayes_run2, newdata = antminer5_testset_nursery_run2, type = "class")
  #antminer5_nursery_result_svm_run2 <- predict(object=antminer5_nursery_model_svm_run2, newdata=antminer5_testset_nursery_run2, type="class")
  antminer5_nursery_tab_bayes_run2<-conf_matrix_table(antminer5_nursery_result_bayes_run2, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_bayes_run2 <- confusionMatrix(antminer5_nursery_tab_bayes_run2)
  antminer5_results_nursery_run2[3, i+1]<-c(antminer5_nursery_conf_bayes_run2$overall['Accuracy'])

  antminer5_nursery_model_rpart_run2 <- rpart(application ~ ., data=antminer5_trainset_nursery_run2)
  antminer5_nursery_result_rpart_run2 <- predict(object=antminer5_nursery_model_rpart_run2, newdata=antminer5_testset_nursery_run2, type="class")
  antminer5_nursery_tab6<-conf_matrix_table(antminer5_nursery_result_rpart_run2, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_rpart_run2 <- confusionMatrix(antminer5_nursery_tab6)
  antminer5_results_nursery_run2[4, i+1]<-c(antminer5_nursery_conf_rpart_run2$overall['Accuracy'])

  antminer5_nursery_model_ant4_run2 <- antminer4(antminer5_trainset_nursery_run2, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant4_run2 <- predict(antminer5_nursery_model_ant4_run2, subset(antminer5_testset_nursery_run2, select=-application))
  antminer5_nursery_tab_ant4_run2<-conf_matrix_table(antminer5_nursery_result_ant4_run2$class, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_ant4_run2 <- confusionMatrix(antminer5_nursery_tab_ant4_run2)
  antminer5_results_nursery_run2[5, i+1]<-c(antminer5_nursery_conf_ant4_run2$overall['Accuracy'])

  antminer5_nursery_model_ant5_run2 <- antminer5(antminer5_trainset_nursery_run2, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant5_run2 <- predict(antminer5_nursery_model_ant5_run2, subset(antminer5_testset_nursery_run2, select=-application))
  antminer5_nursery_tab_ant5_run2<-conf_matrix_table(antminer5_nursery_result_ant5_run2$class, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_ant5_run2 <- confusionMatrix(antminer5_nursery_tab_ant5_run2)
  antminer5_results_nursery_run2[6, i+1]<-c(antminer5_nursery_conf_ant5_run2$overall['Accuracy'])

  antminer5_nursery_model_ant6_run2 <- antminer6(antminer5_trainset_nursery_run2, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant6_run2 <- predict(antminer5_nursery_model_ant6_run2, subset(antminer5_testset_nursery_run2, select=-application))
  antminer5_nursery_tab_ant6_run2<-conf_matrix_table(antminer5_nursery_result_ant6_run2$class, antminer5_testset_nursery_run2$application)
  antminer5_nursery_conf_ant6_run2 <- confusionMatrix(antminer5_nursery_tab_ant6_run2)
  antminer5_results_nursery_run2[7, i+1]<-c(antminer5_nursery_conf_ant6_run2$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer5_results_nursery_run2



###########################
antminer5_nursery_folds_run3 <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_nursery_run3 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_nursery_run3[1, 1]<-c("C5.0")
antminer5_results_nursery_run3[2, 1]<-c("svm")
antminer5_results_nursery_run3[3, 1]<-c("naive bayes")
antminer5_results_nursery_run3[4, 1]<-c("cart")
antminer5_results_nursery_run3[5, 1]<-c("antminer4")
antminer5_results_nursery_run3[6, 1]<-c("antminer5")
antminer5_results_nursery_run3[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_nursery_run3<-nursery[ antminer5_nursery_folds_run3[[i]] ,]
  antminer5_testset_nursery_run3<-nursery[ -antminer5_nursery_folds_run3[[i]] ,]

  antminer5_nursery_model_c50_run3 <- C5.0(application ~ ., data=antminer5_trainset_nursery_run3)
  antminer5_nursery_result_c50_run3 <- predict(object=antminer5_nursery_model_c50_run3, newdata=antminer5_testset_nursery_run3, type="class")
  antminer5_nursery_tab_c50_run3<-conf_matrix_table(antminer5_nursery_result_c50_run3, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_c50_run3 <- confusionMatrix(antminer5_nursery_tab_c50_run3)
  antminer5_nursery_conf_c50_run3$overall['Accuracy']
  antminer5_results_nursery_run3[1, i+1]<-c(antminer5_nursery_conf_c50_run3$overall['Accuracy'])

  antminer5_nursery_model_svm_run3 <- svm(application ~ ., data=antminer5_trainset_nursery_run3)
  antminer5_nursery_result_svm_run3 <- predict(object=antminer5_nursery_model_svm_run3, newdata=antminer5_testset_nursery_run3, type="class")
  antminer5_nursery_tab_svm_run3<-conf_matrix_table(antminer5_nursery_result_svm_run3, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_svm_run3 <- confusionMatrix(antminer5_nursery_tab_svm_run3)
  antminer5_results_nursery_run3[2, i+1]<-c(antminer5_nursery_conf_svm_run3$overall['Accuracy'])

  antminer5_nursery_model_bayes_run3 <- naiveBayes(x = subset(antminer5_trainset_nursery_run3, select=-application), y = antminer5_trainset_nursery_run3$application)
  antminer5_nursery_result_bayes_run3 <- predict(object = antminer5_nursery_model_bayes_run3, newdata = antminer5_testset_nursery_run3, type = "class")
  #antminer5_nursery_result_svm_run3 <- predict(object=antminer5_nursery_model_svm_run3, newdata=antminer5_testset_nursery_run3, type="class")
  antminer5_nursery_tab_bayes_run3<-conf_matrix_table(antminer5_nursery_result_bayes_run3, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_bayes_run3 <- confusionMatrix(antminer5_nursery_tab_bayes_run3)
  antminer5_results_nursery_run3[3, i+1]<-c(antminer5_nursery_conf_bayes_run3$overall['Accuracy'])

  antminer5_nursery_model_rpart_run3 <- rpart(application ~ ., data=antminer5_trainset_nursery_run3)
  antminer5_nursery_result_rpart_run3 <- predict(object=antminer5_nursery_model_rpart_run3, newdata=antminer5_testset_nursery_run3, type="class")
  antminer5_nursery_tab6<-conf_matrix_table(antminer5_nursery_result_rpart_run3, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_rpart_run3 <- confusionMatrix(antminer5_nursery_tab6)
  antminer5_results_nursery_run3[4, i+1]<-c(antminer5_nursery_conf_rpart_run3$overall['Accuracy'])

  antminer5_nursery_model_ant4_run3 <- antminer4(antminer5_trainset_nursery_run3, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant4_run3 <- predict(antminer5_nursery_model_ant4_run3, subset(antminer5_testset_nursery_run3, select=-application))
  antminer5_nursery_tab_ant4_run3<-conf_matrix_table(antminer5_nursery_result_ant4_run3$class, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_ant4_run3 <- confusionMatrix(antminer5_nursery_tab_ant4_run3)
  antminer5_results_nursery_run3[5, i+1]<-c(antminer5_nursery_conf_ant4_run3$overall['Accuracy'])

  antminer5_nursery_model_ant5_run3 <- antminer5(antminer5_trainset_nursery_run3, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant5_run3 <- predict(antminer5_nursery_model_ant5_run3, subset(antminer5_testset_nursery_run3, select=-application))
  antminer5_nursery_tab_ant5_run3<-conf_matrix_table(antminer5_nursery_result_ant5_run3$class, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_ant5_run3 <- confusionMatrix(antminer5_nursery_tab_ant5_run3)
  antminer5_results_nursery_run3[6, i+1]<-c(antminer5_nursery_conf_ant5_run3$overall['Accuracy'])

  antminer5_nursery_model_ant6_run3 <- antminer6(antminer5_trainset_nursery_run3, "application", 10, 1000, 10, 10)
  antminer5_nursery_result_ant6_run3 <- predict(antminer5_nursery_model_ant6_run3, subset(antminer5_testset_nursery_run3, select=-application))
  antminer5_nursery_tab_ant6_run3<-conf_matrix_table(antminer5_nursery_result_ant6_run3$class, antminer5_testset_nursery_run3$application)
  antminer5_nursery_conf_ant6_run3 <- confusionMatrix(antminer5_nursery_tab_ant6_run3)
  antminer5_results_nursery_run3[7, i+1]<-c(antminer5_nursery_conf_ant6_run3$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer5_results_nursery_run3


