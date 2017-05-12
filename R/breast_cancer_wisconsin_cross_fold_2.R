breast_cancer_wisconsin <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"), header = FALSE)
breast_cancer_wisconsin<-breast_cancer_wisconsin[-1]
names(breast_cancer_wisconsin) <- c("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion", "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses", "Class")

#przygotowanie danych
#usuniecie 16 przypadkow w ktorych brakuje danych
remove_list<-which(breast_cancer_wisconsin[,6]=='?')
breast_cancer_wisconsin<-breast_cancer_wisconsin[-remove_list,]
#kategoryzacja atrybutow ciaglych
#sa od 1 do 10 wiec nie trzeba
for(i in 1:9) {
  breast_cancer_wisconsin[, i]<-as.factor(breast_cancer_wisconsin[, i])
}
#change class data type to factor
bening<-which(breast_cancer_wisconsin$Class==2)
malignant<-which(breast_cancer_wisconsin$Class==4)
breast_cancer_wisconsin$Class[bening]<-'bening'
breast_cancer_wisconsin$Class[malignant]<-'malignant'
breast_cancer_wisconsin$Class<-as.factor(breast_cancer_wisconsin$Class)
#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
antminer5_breast_cancer_wisconsin_folds_run1 <- createFolds(breast_cancer_wisconsin$Class, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_breast_cancer_wisconsin_run1 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_breast_cancer_wisconsin_run1[1, 1]<-c("C5.0")
antminer5_results_breast_cancer_wisconsin_run1[2, 1]<-c("svm")
antminer5_results_breast_cancer_wisconsin_run1[3, 1]<-c("naive bayes")
antminer5_results_breast_cancer_wisconsin_run1[4, 1]<-c("cart")
antminer5_results_breast_cancer_wisconsin_run1[5, 1]<-c("antminer4")
antminer5_results_breast_cancer_wisconsin_run1[6, 1]<-c("antminer5")
antminer5_results_breast_cancer_wisconsin_run1[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_breast_cancer_wisconsin_run1<-breast_cancer_wisconsin[ antminer5_breast_cancer_wisconsin_folds_run1[[i]] ,]
  antminer5_testset_breast_cancer_wisconsin_run1<-breast_cancer_wisconsin[ -antminer5_breast_cancer_wisconsin_folds_run1[[i]] ,]

  antminer5_breast_cancer_wisconsin_model_c50_run1 <- C5.0(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run1)
  antminer5_breast_cancer_wisconsin_result_c50_run1 <- predict(object=antminer5_breast_cancer_wisconsin_model_c50_run1, newdata=antminer5_testset_breast_cancer_wisconsin_run1, type="class")
  antminer5_breast_cancer_wisconsin_tab_c50_run1<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_c50_run1, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_c50_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_c50_run1)
  antminer5_breast_cancer_wisconsin_conf_c50_run1$overall['Accuracy']
  antminer5_results_breast_cancer_wisconsin_run1[1, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_c50_run1$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_svm_run1 <- svm(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run1)
  antminer5_breast_cancer_wisconsin_result_svm_run1 <- predict(object=antminer5_breast_cancer_wisconsin_model_svm_run1, newdata=antminer5_testset_breast_cancer_wisconsin_run1, type="class")
  antminer5_breast_cancer_wisconsin_tab_svm_run1<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_svm_run1, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_svm_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_svm_run1)
  antminer5_results_breast_cancer_wisconsin_run1[2, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_svm_run1$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_bayes_run1 <- naiveBayes(x = subset(antminer5_trainset_breast_cancer_wisconsin_run1, select=-Class), y = antminer5_trainset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_result_bayes_run1 <- predict(object = antminer5_breast_cancer_wisconsin_model_bayes_run1, newdata = antminer5_testset_breast_cancer_wisconsin_run1, type = "class")
  #antminer5_breast_cancer_wisconsin_result_svm_run1 <- predict(object=antminer5_breast_cancer_wisconsin_model_svm_run1, newdata=antminer5_testset_breast_cancer_wisconsin_run1, type="class")
  antminer5_breast_cancer_wisconsin_tab_bayes_run1<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_bayes_run1, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_bayes_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_bayes_run1)
  antminer5_results_breast_cancer_wisconsin_run1[3, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_bayes_run1$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_rpart_run1 <- rpart(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run1)
  antminer5_breast_cancer_wisconsin_result_rpart_run1 <- predict(object=antminer5_breast_cancer_wisconsin_model_rpart_run1, newdata=antminer5_testset_breast_cancer_wisconsin_run1, type="class")
  antminer5_breast_cancer_wisconsin_tab6<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_rpart_run1, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_rpart_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab6)
  antminer5_results_breast_cancer_wisconsin_run1[4, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_rpart_run1$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant4_run1 <- antminer4(antminer5_trainset_breast_cancer_wisconsin_run1, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant4_run1 <- predict.antminer4(antminer5_breast_cancer_wisconsin_model_ant4_run1, subset(antminer5_testset_breast_cancer_wisconsin_run1, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant4_run1<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant4_run1$class, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_ant4_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant4_run1)
  antminer5_results_breast_cancer_wisconsin_run1[5, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant4_run1$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant5_run1 <- antminer5(antminer5_trainset_breast_cancer_wisconsin_run1, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant5_run1 <- predict.antminer5(antminer5_breast_cancer_wisconsin_model_ant5_run1, subset(antminer5_testset_breast_cancer_wisconsin_run1, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant5_run1<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant5_run1$class, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_ant5_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant5_run1)
  antminer5_results_breast_cancer_wisconsin_run1[6, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant5_run1$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant6_run1 <- antminer6(antminer5_trainset_breast_cancer_wisconsin_run1, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant6_run1 <- predict.antminer6(antminer5_breast_cancer_wisconsin_model_ant6_run1, subset(antminer5_testset_breast_cancer_wisconsin_run1, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant6_run1<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant6_run1$class, antminer5_testset_breast_cancer_wisconsin_run1$Class)
  antminer5_breast_cancer_wisconsin_conf_ant6_run1 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant6_run1)
  antminer5_results_breast_cancer_wisconsin_run1[7, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant6_run1$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer5_results_breast_cancer_wisconsin_run1

#############################
antminer5_breast_cancer_wisconsin_folds_run2 <- createFolds(breast_cancer_wisconsin$Class, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_breast_cancer_wisconsin_run2 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_breast_cancer_wisconsin_run2[1, 1]<-c("C5.0")
antminer5_results_breast_cancer_wisconsin_run2[2, 1]<-c("svm")
antminer5_results_breast_cancer_wisconsin_run2[3, 1]<-c("naive bayes")
antminer5_results_breast_cancer_wisconsin_run2[4, 1]<-c("cart")
antminer5_results_breast_cancer_wisconsin_run2[5, 1]<-c("antminer4")
antminer5_results_breast_cancer_wisconsin_run2[6, 1]<-c("antminer5")
antminer5_results_breast_cancer_wisconsin_run2[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_breast_cancer_wisconsin_run2<-breast_cancer_wisconsin[ antminer5_breast_cancer_wisconsin_folds_run2[[i]] ,]
  antminer5_testset_breast_cancer_wisconsin_run2<-breast_cancer_wisconsin[ -antminer5_breast_cancer_wisconsin_folds_run2[[i]] ,]

  antminer5_breast_cancer_wisconsin_model_c50_run2 <- C5.0(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run2)
  antminer5_breast_cancer_wisconsin_result_c50_run2 <- predict(object=antminer5_breast_cancer_wisconsin_model_c50_run2, newdata=antminer5_testset_breast_cancer_wisconsin_run2, type="class")
  antminer5_breast_cancer_wisconsin_tab_c50_run2<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_c50_run2, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_c50_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_c50_run2)
  antminer5_breast_cancer_wisconsin_conf_c50_run2$overall['Accuracy']
  antminer5_results_breast_cancer_wisconsin_run2[1, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_c50_run2$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_svm_run2 <- svm(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run2)
  antminer5_breast_cancer_wisconsin_result_svm_run2 <- predict(object=antminer5_breast_cancer_wisconsin_model_svm_run2, newdata=antminer5_testset_breast_cancer_wisconsin_run2, type="class")
  antminer5_breast_cancer_wisconsin_tab_svm_run2<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_svm_run2, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_svm_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_svm_run2)
  antminer5_results_breast_cancer_wisconsin_run2[2, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_svm_run2$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_bayes_run2 <- naiveBayes(x = subset(antminer5_trainset_breast_cancer_wisconsin_run2, select=-Class), y = antminer5_trainset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_result_bayes_run2 <- predict(object = antminer5_breast_cancer_wisconsin_model_bayes_run2, newdata = antminer5_testset_breast_cancer_wisconsin_run2, type = "class")
  #antminer5_breast_cancer_wisconsin_result_svm_run2 <- predict(object=antminer5_breast_cancer_wisconsin_model_svm_run2, newdata=antminer5_testset_breast_cancer_wisconsin_run2, type="class")
  antminer5_breast_cancer_wisconsin_tab_bayes_run2<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_bayes_run2, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_bayes_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_bayes_run2)
  antminer5_results_breast_cancer_wisconsin_run2[3, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_bayes_run2$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_rpart_run2 <- rpart(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run2)
  antminer5_breast_cancer_wisconsin_result_rpart_run2 <- predict(object=antminer5_breast_cancer_wisconsin_model_rpart_run2, newdata=antminer5_testset_breast_cancer_wisconsin_run2, type="class")
  antminer5_breast_cancer_wisconsin_tab6<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_rpart_run2, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_rpart_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab6)
  antminer5_results_breast_cancer_wisconsin_run2[4, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_rpart_run2$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant4_run2 <- antminer4(antminer5_trainset_breast_cancer_wisconsin_run2, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant4_run2 <- predict.antminer4(antminer5_breast_cancer_wisconsin_model_ant4_run2, subset(antminer5_testset_breast_cancer_wisconsin_run2, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant4_run2<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant4_run2$class, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_ant4_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant4_run2)
  antminer5_results_breast_cancer_wisconsin_run2[5, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant4_run2$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant5_run2 <- antminer5(antminer5_trainset_breast_cancer_wisconsin_run2, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant5_run2 <- predict.antminer5(antminer5_breast_cancer_wisconsin_model_ant5_run2, subset(antminer5_testset_breast_cancer_wisconsin_run2, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant5_run2<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant5_run2$class, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_ant5_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant5_run2)
  antminer5_results_breast_cancer_wisconsin_run2[6, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant5_run2$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant6_run2 <- antminer6(antminer5_trainset_breast_cancer_wisconsin_run2, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant6_run2 <- predict.antminer6(antminer5_breast_cancer_wisconsin_model_ant6_run2, subset(antminer5_testset_breast_cancer_wisconsin_run2, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant6_run2<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant6_run2$class, antminer5_testset_breast_cancer_wisconsin_run2$Class)
  antminer5_breast_cancer_wisconsin_conf_ant6_run2 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant6_run2)
  antminer5_results_breast_cancer_wisconsin_run2[7, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant6_run2$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer5_results_breast_cancer_wisconsin_run2



###########################
antminer5_breast_cancer_wisconsin_folds_run3 <- createFolds(breast_cancer_wisconsin$Class, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_breast_cancer_wisconsin_run3 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_breast_cancer_wisconsin_run3[1, 1]<-c("C5.0")
antminer5_results_breast_cancer_wisconsin_run3[2, 1]<-c("svm")
antminer5_results_breast_cancer_wisconsin_run3[3, 1]<-c("naive bayes")
antminer5_results_breast_cancer_wisconsin_run3[4, 1]<-c("cart")
antminer5_results_breast_cancer_wisconsin_run3[5, 1]<-c("antminer4")
antminer5_results_breast_cancer_wisconsin_run3[6, 1]<-c("antminer5")
antminer5_results_breast_cancer_wisconsin_run3[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_breast_cancer_wisconsin_run3<-breast_cancer_wisconsin[ antminer5_breast_cancer_wisconsin_folds_run3[[i]] ,]
  antminer5_testset_breast_cancer_wisconsin_run3<-breast_cancer_wisconsin[ -antminer5_breast_cancer_wisconsin_folds_run3[[i]] ,]

  antminer5_breast_cancer_wisconsin_model_c50_run3 <- C5.0(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run3)
  antminer5_breast_cancer_wisconsin_result_c50_run3 <- predict(object=antminer5_breast_cancer_wisconsin_model_c50_run3, newdata=antminer5_testset_breast_cancer_wisconsin_run3, type="class")
  antminer5_breast_cancer_wisconsin_tab_c50_run3<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_c50_run3, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_c50_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_c50_run3)
  antminer5_breast_cancer_wisconsin_conf_c50_run3$overall['Accuracy']
  antminer5_results_breast_cancer_wisconsin_run3[1, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_c50_run3$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_svm_run3 <- svm(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run3)
  antminer5_breast_cancer_wisconsin_result_svm_run3 <- predict(object=antminer5_breast_cancer_wisconsin_model_svm_run3, newdata=antminer5_testset_breast_cancer_wisconsin_run3, type="class")
  antminer5_breast_cancer_wisconsin_tab_svm_run3<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_svm_run3, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_svm_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_svm_run3)
  antminer5_results_breast_cancer_wisconsin_run3[2, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_svm_run3$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_bayes_run3 <- naiveBayes(x = subset(antminer5_trainset_breast_cancer_wisconsin_run3, select=-Class), y = antminer5_trainset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_result_bayes_run3 <- predict(object = antminer5_breast_cancer_wisconsin_model_bayes_run3, newdata = antminer5_testset_breast_cancer_wisconsin_run3, type = "class")
  #antminer5_breast_cancer_wisconsin_result_svm_run3 <- predict(object=antminer5_breast_cancer_wisconsin_model_svm_run3, newdata=antminer5_testset_breast_cancer_wisconsin_run3, type="class")
  antminer5_breast_cancer_wisconsin_tab_bayes_run3<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_bayes_run3, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_bayes_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_bayes_run3)
  antminer5_results_breast_cancer_wisconsin_run3[3, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_bayes_run3$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_rpart_run3 <- rpart(Class ~ ., data=antminer5_trainset_breast_cancer_wisconsin_run3)
  antminer5_breast_cancer_wisconsin_result_rpart_run3 <- predict(object=antminer5_breast_cancer_wisconsin_model_rpart_run3, newdata=antminer5_testset_breast_cancer_wisconsin_run3, type="class")
  antminer5_breast_cancer_wisconsin_tab6<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_rpart_run3, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_rpart_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab6)
  antminer5_results_breast_cancer_wisconsin_run3[4, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_rpart_run3$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant4_run3 <- antminer4(antminer5_trainset_breast_cancer_wisconsin_run3, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant4_run3 <- predict.antminer4(antminer5_breast_cancer_wisconsin_model_ant4_run3, subset(antminer5_testset_breast_cancer_wisconsin_run3, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant4_run3<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant4_run3$class, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_ant4_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant4_run3)
  antminer5_results_breast_cancer_wisconsin_run3[5, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant4_run3$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant5_run3 <- antminer5(antminer5_trainset_breast_cancer_wisconsin_run3, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant5_run3 <- predict.antminer5(antminer5_breast_cancer_wisconsin_model_ant5_run3, subset(antminer5_testset_breast_cancer_wisconsin_run3, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant5_run3<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant5_run3$class, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_ant5_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant5_run3)
  antminer5_results_breast_cancer_wisconsin_run3[6, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant5_run3$overall['Accuracy'])

  antminer5_breast_cancer_wisconsin_model_ant6_run3 <- antminer6(antminer5_trainset_breast_cancer_wisconsin_run3, "Class", 10, 1000, 10, 10)
  antminer5_breast_cancer_wisconsin_result_ant6_run3 <- predict.antminer6(antminer5_breast_cancer_wisconsin_model_ant6_run3, subset(antminer5_testset_breast_cancer_wisconsin_run3, select=-Class))
  antminer5_breast_cancer_wisconsin_tab_ant6_run3<-conf_matrix_table(antminer5_breast_cancer_wisconsin_result_ant6_run3$class, antminer5_testset_breast_cancer_wisconsin_run3$Class)
  antminer5_breast_cancer_wisconsin_conf_ant6_run3 <- confusionMatrix(antminer5_breast_cancer_wisconsin_tab_ant6_run3)
  antminer5_results_breast_cancer_wisconsin_run3[7, i+1]<-c(antminer5_breast_cancer_wisconsin_conf_ant6_run3$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
antminer5_results_breast_cancer_wisconsin_run3


