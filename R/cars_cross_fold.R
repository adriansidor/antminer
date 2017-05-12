cars <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = FALSE)
names(cars) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "evaluation")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
antminer4_cars_folds_run2 <- createFolds(cars$evaluation, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer4_results_cars_run2 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer4_results_cars_run2[1, 1]<-c("C5.0")
antminer4_results_cars_run2[2, 1]<-c("svm")
antminer4_results_cars_run2[3, 1]<-c("naive bayes")
antminer4_results_cars_run2[4, 1]<-c("cart")
antminer4_results_cars_run2[5, 1]<-c("antminer")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer4_trainset_cars_run2<-cars[ antminer4_cars_folds_run2[[i]] ,]
  antminer4_testset_cars_run2<-cars[ -antminer4_cars_folds_run2[[i]] ,]

  antminer4_cars_model_c50_run2 <- C5.0(evaluation ~ ., data=antminer4_trainset_cars_run2)
  antminer4_cars_result_c50_run2 <- predict(object=antminer4_cars_model_c50_run2, newdata=antminer4_testset_cars_run2, type="class")
  antminer4_cars_tab_c50_run2<-conf_matrix_table(antminer4_cars_result_c50_run2, antminer4_testset_cars_run2$evaluation)
  antminer4_cars_conf_c50_run2 <- confusionMatrix(antminer4_cars_tab_c50_run2)
  antminer4_cars_conf_c50_run2$overall['Accuracy']
  antminer4_results_cars_run2[1, i+1]<-c(antminer4_cars_conf_c50_run2$overall['Accuracy'])

  antminer4_cars_model_svm_run2 <- svm(evaluation ~ ., data=antminer4_trainset_cars_run2)
  antminer4_cars_result_svm_run2 <- predict(object=antminer4_cars_model_svm_run2, newdata=antminer4_testset_cars_run2, type="class")
  antminer4_cars_tab_svm_run2<-conf_matrix_table(antminer4_cars_result_svm_run2, antminer4_testset_cars_run2$evaluation)
  antminer4_cars_conf_svm_run2 <- confusionMatrix(antminer4_cars_tab_svm_run2)
  antminer4_results_cars_run2[2, i+1]<-c(antminer4_cars_conf_svm_run2$overall['Accuracy'])

  antminer4_cars_model_bayes_run2 <- naiveBayes(x = subset(antminer4_trainset_cars_run2, select=-evaluation), y = antminer4_trainset_cars_run2$evaluation)
  antminer4_cars_result_bayes_run2 <- predict(object = antminer4_cars_model_bayes_run2, newdata = antminer4_testset_cars_run2, type = "class")
  #antminer4_cars_result_svm_run2 <- predict(object=antminer4_cars_model_svm_run2, newdata=antminer4_testset_cars_run2, type="class")
  antminer4_cars_tab_bayes_run2<-conf_matrix_table(antminer4_cars_result_bayes_run2, antminer4_testset_cars_run2$evaluation)
  antminer4_cars_conf_bayes_run2 <- confusionMatrix(antminer4_cars_tab_bayes_run2)
  antminer4_results_cars_run2[3, i+1]<-c(antminer4_cars_conf_bayes_run2$overall['Accuracy'])

  antminer4_cars_model_rpart_run2 <- rpart(evaluation ~ ., data=antminer4_trainset_cars_run2)
  antminer4_cars_result_rpart_run2 <- predict(object=antminer4_cars_model_rpart_run2, newdata=antminer4_testset_cars_run2, type="class")
  antminer4_cars_tab6<-conf_matrix_table(antminer4_cars_result_rpart_run2, antminer4_testset_cars_run2$evaluation)
  antminer4_cars_conf_rpart_run2 <- confusionMatrix(antminer4_cars_tab6)
  antminer4_results_cars_run2[4, i+1]<-c(antminer4_cars_conf_rpart_run2$overall['Accuracy'])

  antminer4_cars_model_ant_run2 <- antminer4(antminer4_trainset_cars_run2, "evaluation", 10, 1000, 10, 10)
  antminer4_cars_result_ant_run2 <- predict(antminer4_cars_model_ant_run2, subset(antminer4_testset_cars_run2, select=-evaluation))
  antminer4_cars_tab_ant_run2<-conf_matrix_table(antminer4_cars_result_ant_run2$class, antminer4_testset_cars_run2$evaluation)
  antminer4_cars_conf_ant_run2 <- confusionMatrix(antminer4_cars_tab_ant_run2)
  antminer4_results_cars_run2[5, i+1]<-c(antminer4_cars_conf_ant_run2$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer4_results_cars_run2

loop_size<-10
antminer4_cars_folds_run3 <- createFolds(cars$evaluation, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer4_results_cars_run3 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer4_results_cars_run3[1, 1]<-c("C5.0")
antminer4_results_cars_run3[2, 1]<-c("svm")
antminer4_results_cars_run3[3, 1]<-c("naive bayes")
antminer4_results_cars_run3[4, 1]<-c("cart")
antminer4_results_cars_run3[5, 1]<-c("antminer")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer4_trainset_cars_run3<-cars[ antminer4_cars_folds_run3[[i]] ,]
  antminer4_testset_cars_run3<-cars[ -antminer4_cars_folds_run3[[i]] ,]

  antminer4_cars_model_c50_run3 <- C5.0(evaluation ~ ., data=antminer4_trainset_cars_run3)
  antminer4_cars_result_c50_run3 <- predict(object=antminer4_cars_model_c50_run3, newdata=antminer4_testset_cars_run3, type="class")
  antminer4_cars_tab_c50_run3<-conf_matrix_table(antminer4_cars_result_c50_run3, antminer4_testset_cars_run3$evaluation)
  antminer4_cars_conf_c50_run3 <- confusionMatrix(antminer4_cars_tab_c50_run3)
  antminer4_cars_conf_c50_run3$overall['Accuracy']
  antminer4_results_cars_run3[1, i+1]<-c(antminer4_cars_conf_c50_run3$overall['Accuracy'])

  antminer4_cars_model_svm_run3 <- svm(evaluation ~ ., data=antminer4_trainset_cars_run3)
  antminer4_cars_result_svm_run3 <- predict(object=antminer4_cars_model_svm_run3, newdata=antminer4_testset_cars_run3, type="class")
  antminer4_cars_tab_svm_run3<-conf_matrix_table(antminer4_cars_result_svm_run3, antminer4_testset_cars_run3$evaluation)
  antminer4_cars_conf_svm_run3 <- confusionMatrix(antminer4_cars_tab_svm_run3)
  antminer4_results_cars_run3[2, i+1]<-c(antminer4_cars_conf_svm_run3$overall['Accuracy'])

  antminer4_cars_model_bayes_run3 <- naiveBayes(x = subset(antminer4_trainset_cars_run3, select=-evaluation), y = antminer4_trainset_cars_run3$evaluation)
  antminer4_cars_result_bayes_run3 <- predict(object = antminer4_cars_model_bayes_run3, newdata = antminer4_testset_cars_run3, type = "class")
  #antminer4_cars_result_svm_run3 <- predict(object=antminer4_cars_model_svm_run3, newdata=antminer4_testset_cars_run3, type="class")
  antminer4_cars_tab_bayes_run3<-conf_matrix_table(antminer4_cars_result_bayes_run3, antminer4_testset_cars_run3$evaluation)
  antminer4_cars_conf_bayes_run3 <- confusionMatrix(antminer4_cars_tab_bayes_run3)
  antminer4_results_cars_run3[3, i+1]<-c(antminer4_cars_conf_bayes_run3$overall['Accuracy'])

  antminer4_cars_model_rpart_run3 <- rpart(evaluation ~ ., data=antminer4_trainset_cars_run3)
  antminer4_cars_result_rpart_run3 <- predict(object=antminer4_cars_model_rpart_run3, newdata=antminer4_testset_cars_run3, type="class")
  antminer4_cars_tab6<-conf_matrix_table(antminer4_cars_result_rpart_run3, antminer4_testset_cars_run3$evaluation)
  antminer4_cars_conf_rpart_run3 <- confusionMatrix(antminer4_cars_tab6)
  antminer4_results_cars_run3[4, i+1]<-c(antminer4_cars_conf_rpart_run3$overall['Accuracy'])

  antminer4_cars_model_ant_run3 <- antminer4(antminer4_trainset_cars_run3, "evaluation", 10, 1000, 10, 10)
  antminer4_cars_result_ant_run3 <- predict(antminer4_cars_model_ant_run3, subset(antminer4_testset_cars_run3, select=-evaluation))
  antminer4_cars_tab_ant_run3<-conf_matrix_table(antminer4_cars_result_ant_run3$class, antminer4_testset_cars_run3$evaluation)
  antminer4_cars_conf_ant_run3 <- confusionMatrix(antminer4_cars_tab_ant_run3)
  antminer4_results_cars_run3[5, i+1]<-c(antminer4_cars_conf_ant_run3$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer4_results_cars_run3
