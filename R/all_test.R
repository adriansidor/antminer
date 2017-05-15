cars <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = FALSE)
names(cars) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "evaluation")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
cars_folds_run1 <- createFolds(cars$evaluation, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_cars_run1 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_cars_run1[1, 1]<-c("C5.0")
results_cars_run1[2, 1]<-c("svm")
results_cars_run1[3, 1]<-c("naive bayes")
results_cars_run1[4, 1]<-c("cart")
results_cars_run1[5, 1]<-c("antminer4")
results_cars_run1[6, 1]<-c("antminer5")
results_cars_run1[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_cars_run1<-cars[ cars_folds_run1[[i]] ,]
  testset_cars_run1<-cars[ -cars_folds_run1[[i]] ,]

  cars_model_c50_run1 <- C5.0(evaluation ~ ., data=trainset_cars_run1)
  cars_result_c50_run1 <- predict(object=cars_model_c50_run1, newdata=testset_cars_run1, type="class")
  cars_tab_c50_run1<-conf_matrix_table(cars_result_c50_run1, testset_cars_run1$evaluation)
  cars_conf_c50_run1 <- confusionMatrix(cars_tab_c50_run1)
  cars_conf_c50_run1$overall['Accuracy']
  results_cars_run1[1, i+1]<-c(cars_conf_c50_run1$overall['Accuracy'])

  cars_model_svm_run1 <- svm(evaluation ~ ., data=trainset_cars_run1)
  cars_result_svm_run1 <- predict(object=cars_model_svm_run1, newdata=testset_cars_run1, type="class")
  cars_tab_svm_run1<-conf_matrix_table(cars_result_svm_run1, testset_cars_run1$evaluation)
  cars_conf_svm_run1 <- confusionMatrix(cars_tab_svm_run1)
  results_cars_run1[2, i+1]<-c(cars_conf_svm_run1$overall['Accuracy'])

  cars_model_bayes_run1 <- naiveBayes(x = subset(trainset_cars_run1, select=-evaluation), y = trainset_cars_run1$evaluation)
  cars_result_bayes_run1 <- predict(object = cars_model_bayes_run1, newdata = testset_cars_run1, type = "class")
  #cars_result_svm_run1 <- predict(object=cars_model_svm_run1, newdata=testset_cars_run1, type="class")
  cars_tab_bayes_run1<-conf_matrix_table(cars_result_bayes_run1, testset_cars_run1$evaluation)
  cars_conf_bayes_run1 <- confusionMatrix(cars_tab_bayes_run1)
  results_cars_run1[3, i+1]<-c(cars_conf_bayes_run1$overall['Accuracy'])

  cars_model_rpart_run1 <- rpart(evaluation ~ ., data=trainset_cars_run1)
  cars_result_rpart_run1 <- predict(object=cars_model_rpart_run1, newdata=testset_cars_run1, type="class")
  cars_tab6<-conf_matrix_table(cars_result_rpart_run1, testset_cars_run1$evaluation)
  cars_conf_rpart_run1 <- confusionMatrix(cars_tab6)
  results_cars_run1[4, i+1]<-c(cars_conf_rpart_run1$overall['Accuracy'])

  cars_model_ant4_run1 <- antminer4(trainset_cars_run1, "evaluation", 10, 1000, 10, 10)
  cars_result_ant4_run1 <- predict.antminer4(cars_model_ant4_run1, subset(testset_cars_run1, select=-evaluation))
  cars_tab_ant4_run1<-conf_matrix_table(cars_result_ant4_run1$class, testset_cars_run1$evaluation)
  cars_conf_ant4_run1 <- confusionMatrix(cars_tab_ant4_run1)
  results_cars_run1[5, i+1]<-c(cars_conf_ant4_run1$overall['Accuracy'])

  cars_model_ant5_run1 <- antminer5(trainset_cars_run1, "evaluation", 10, 1000, 10, 10)
  cars_result_ant5_run1 <- predict.antminer5(cars_model_ant5_run1, subset(testset_cars_run1, select=-evaluation))
  cars_tab_ant5_run1<-conf_matrix_table(cars_result_ant5_run1$class, testset_cars_run1$evaluation)
  cars_conf_ant5_run1 <- confusionMatrix(cars_tab_ant5_run1)
  results_cars_run1[6, i+1]<-c(cars_conf_ant5_run1$overall['Accuracy'])

  cars_model_ant6_run1 <- antminer6(trainset_cars_run1, "evaluation", 10, 1000, 10, 10)
  cars_result_ant6_run1 <- predict.antminer6(cars_model_ant6_run1, subset(testset_cars_run1, select=-evaluation))
  cars_tab_ant6_run1<-conf_matrix_table(cars_result_ant6_run1$class, testset_cars_run1$evaluation)
  cars_conf_ant6_run1 <- confusionMatrix(cars_tab_ant6_run1)
  results_cars_run1[7, i+1]<-c(cars_conf_ant6_run1$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
results_cars_run1

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
breast_cancer_wisconsin_folds_run1 <- createFolds(breast_cancer_wisconsin$Class, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_breast_cancer_wisconsin_run1 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_breast_cancer_wisconsin_run1[1, 1]<-c("C5.0")
results_breast_cancer_wisconsin_run1[2, 1]<-c("svm")
results_breast_cancer_wisconsin_run1[3, 1]<-c("naive bayes")
results_breast_cancer_wisconsin_run1[4, 1]<-c("cart")
results_breast_cancer_wisconsin_run1[5, 1]<-c("antminer4")
results_breast_cancer_wisconsin_run1[6, 1]<-c("antminer5")
results_breast_cancer_wisconsin_run1[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_breast_cancer_wisconsin_run1<-breast_cancer_wisconsin[ breast_cancer_wisconsin_folds_run1[[i]] ,]
  testset_breast_cancer_wisconsin_run1<-breast_cancer_wisconsin[ -breast_cancer_wisconsin_folds_run1[[i]] ,]

  breast_cancer_wisconsin_model_c50_run1 <- C5.0(Class ~ ., data=trainset_breast_cancer_wisconsin_run1)
  breast_cancer_wisconsin_result_c50_run1 <- predict(object=breast_cancer_wisconsin_model_c50_run1, newdata=testset_breast_cancer_wisconsin_run1, type="class")
  breast_cancer_wisconsin_tab_c50_run1<-conf_matrix_table(breast_cancer_wisconsin_result_c50_run1, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_c50_run1 <- confusionMatrix(breast_cancer_wisconsin_tab_c50_run1)
  breast_cancer_wisconsin_conf_c50_run1$overall['Accuracy']
  results_breast_cancer_wisconsin_run1[1, i+1]<-c(breast_cancer_wisconsin_conf_c50_run1$overall['Accuracy'])

  breast_cancer_wisconsin_model_svm_run1 <- svm(Class ~ ., data=trainset_breast_cancer_wisconsin_run1)
  breast_cancer_wisconsin_result_svm_run1 <- predict(object=breast_cancer_wisconsin_model_svm_run1, newdata=testset_breast_cancer_wisconsin_run1, type="class")
  breast_cancer_wisconsin_tab_svm_run1<-conf_matrix_table(breast_cancer_wisconsin_result_svm_run1, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_svm_run1 <- confusionMatrix(breast_cancer_wisconsin_tab_svm_run1)
  results_breast_cancer_wisconsin_run1[2, i+1]<-c(breast_cancer_wisconsin_conf_svm_run1$overall['Accuracy'])

  breast_cancer_wisconsin_model_bayes_run1 <- naiveBayes(x = subset(trainset_breast_cancer_wisconsin_run1, select=-Class), y = trainset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_result_bayes_run1 <- predict(object = breast_cancer_wisconsin_model_bayes_run1, newdata = testset_breast_cancer_wisconsin_run1, type = "class")
  #breast_cancer_wisconsin_result_svm_run1 <- predict(object=breast_cancer_wisconsin_model_svm_run1, newdata=testset_breast_cancer_wisconsin_run1, type="class")
  breast_cancer_wisconsin_tab_bayes_run1<-conf_matrix_table(breast_cancer_wisconsin_result_bayes_run1, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_bayes_run1 <- confusionMatrix(breast_cancer_wisconsin_tab_bayes_run1)
  results_breast_cancer_wisconsin_run1[3, i+1]<-c(breast_cancer_wisconsin_conf_bayes_run1$overall['Accuracy'])

  breast_cancer_wisconsin_model_rpart_run1 <- rpart(Class ~ ., data=trainset_breast_cancer_wisconsin_run1)
  breast_cancer_wisconsin_result_rpart_run1 <- predict(object=breast_cancer_wisconsin_model_rpart_run1, newdata=testset_breast_cancer_wisconsin_run1, type="class")
  breast_cancer_wisconsin_tab6<-conf_matrix_table(breast_cancer_wisconsin_result_rpart_run1, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_rpart_run1 <- confusionMatrix(breast_cancer_wisconsin_tab6)
  results_breast_cancer_wisconsin_run1[4, i+1]<-c(breast_cancer_wisconsin_conf_rpart_run1$overall['Accuracy'])

  breast_cancer_wisconsin_model_ant4_run1 <- antminer4(trainset_breast_cancer_wisconsin_run1, "Class", 10, 1000, 10, 10)
  breast_cancer_wisconsin_result_ant4_run1 <- predict.antminer4(breast_cancer_wisconsin_model_ant4_run1, subset(testset_breast_cancer_wisconsin_run1, select=-Class))
  breast_cancer_wisconsin_tab_ant4_run1<-conf_matrix_table(breast_cancer_wisconsin_result_ant4_run1$class, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_ant4_run1 <- confusionMatrix(breast_cancer_wisconsin_tab_ant4_run1)
  results_breast_cancer_wisconsin_run1[5, i+1]<-c(breast_cancer_wisconsin_conf_ant4_run1$overall['Accuracy'])

  breast_cancer_wisconsin_model_ant5_run1 <- antminer5(trainset_breast_cancer_wisconsin_run1, "Class", 10, 1000, 10, 10)
  breast_cancer_wisconsin_result_ant5_run1 <- predict.antminer5(breast_cancer_wisconsin_model_ant5_run1, subset(testset_breast_cancer_wisconsin_run1, select=-Class))
  breast_cancer_wisconsin_tab_ant5_run1<-conf_matrix_table(breast_cancer_wisconsin_result_ant5_run1$class, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_ant5_run1 <- confusionMatrix(breast_cancer_wisconsin_tab_ant5_run1)
  results_breast_cancer_wisconsin_run1[6, i+1]<-c(breast_cancer_wisconsin_conf_ant5_run1$overall['Accuracy'])

  breast_cancer_wisconsin_model_ant6_run1 <- antminer6(trainset_breast_cancer_wisconsin_run1, "Class", 10, 1000, 10, 10)
  breast_cancer_wisconsin_result_ant6_run1 <- predict.antminer6(breast_cancer_wisconsin_model_ant6_run1, subset(testset_breast_cancer_wisconsin_run1, select=-Class))
  breast_cancer_wisconsin_tab_ant6_run1<-conf_matrix_table(breast_cancer_wisconsin_result_ant6_run1$class, testset_breast_cancer_wisconsin_run1$Class)
  breast_cancer_wisconsin_conf_ant6_run1 <- confusionMatrix(breast_cancer_wisconsin_tab_ant6_run1)
  results_breast_cancer_wisconsin_run1[7, i+1]<-c(breast_cancer_wisconsin_conf_ant6_run1$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
results_breast_cancer_wisconsin_run1

nursery <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data"), header = FALSE)
names(nursery) <- c("parents", "has_nurs", "form", "children", "housing", "finance", "social", "health", "application")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
nursery_folds_run1 <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_nursery_run1 <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_nursery_run1[1, 1]<-c("C5.0")
results_nursery_run1[2, 1]<-c("svm")
results_nursery_run1[3, 1]<-c("naive bayes")
results_nursery_run1[4, 1]<-c("cart")
results_nursery_run1[5, 1]<-c("antminer4")
results_nursery_run1[6, 1]<-c("antminer5")
results_nursery_run1[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_nursery_run1<-nursery[ nursery_folds_run1[[i]] ,]
  testset_nursery_run1<-nursery[ -nursery_folds_run1[[i]] ,]

  nursery_model_c50_run1 <- C5.0(application ~ ., data=trainset_nursery_run1)
  nursery_result_c50_run1 <- predict(object=nursery_model_c50_run1, newdata=testset_nursery_run1, type="class")
  nursery_tab_c50_run1<-conf_matrix_table(nursery_result_c50_run1, testset_nursery_run1$application)
  nursery_conf_c50_run1 <- confusionMatrix(nursery_tab_c50_run1)
  nursery_conf_c50_run1$overall['Accuracy']
  results_nursery_run1[1, i+1]<-c(nursery_conf_c50_run1$overall['Accuracy'])

  nursery_model_svm_run1 <- svm(application ~ ., data=trainset_nursery_run1)
  nursery_result_svm_run1 <- predict(object=nursery_model_svm_run1, newdata=testset_nursery_run1, type="class")
  nursery_tab_svm_run1<-conf_matrix_table(nursery_result_svm_run1, testset_nursery_run1$application)
  nursery_conf_svm_run1 <- confusionMatrix(nursery_tab_svm_run1)
  results_nursery_run1[2, i+1]<-c(nursery_conf_svm_run1$overall['Accuracy'])

  nursery_model_bayes_run1 <- naiveBayes(x = subset(trainset_nursery_run1, select=-application), y = trainset_nursery_run1$application)
  nursery_result_bayes_run1 <- predict(object = nursery_model_bayes_run1, newdata = testset_nursery_run1, type = "class")
  #nursery_result_svm_run1 <- predict(object=nursery_model_svm_run1, newdata=testset_nursery_run1, type="class")
  nursery_tab_bayes_run1<-conf_matrix_table(nursery_result_bayes_run1, testset_nursery_run1$application)
  nursery_conf_bayes_run1 <- confusionMatrix(nursery_tab_bayes_run1)
  results_nursery_run1[3, i+1]<-c(nursery_conf_bayes_run1$overall['Accuracy'])

  nursery_model_rpart_run1 <- rpart(application ~ ., data=trainset_nursery_run1)
  nursery_result_rpart_run1 <- predict(object=nursery_model_rpart_run1, newdata=testset_nursery_run1, type="class")
  nursery_tab6<-conf_matrix_table(nursery_result_rpart_run1, testset_nursery_run1$application)
  nursery_conf_rpart_run1 <- confusionMatrix(nursery_tab6)
  results_nursery_run1[4, i+1]<-c(nursery_conf_rpart_run1$overall['Accuracy'])

  nursery_model_ant4_run1 <- antminer4(trainset_nursery_run1, "application", 10, 1000, 10, 10)
  nursery_result_ant4_run1 <- predict(nursery_model_ant4_run1, subset(testset_nursery_run1, select=-application))
  nursery_tab_ant4_run1<-conf_matrix_table(nursery_result_ant4_run1$class, testset_nursery_run1$application)
  nursery_conf_ant4_run1 <- confusionMatrix(nursery_tab_ant4_run1)
  results_nursery_run1[5, i+1]<-c(nursery_conf_ant4_run1$overall['Accuracy'])

  nursery_model_ant5_run1 <- antminer5(trainset_nursery_run1, "application", 10, 1000, 10, 10)
  nursery_result_ant5_run1 <- predict(nursery_model_ant5_run1, subset(testset_nursery_run1, select=-application))
  nursery_tab_ant5_run1<-conf_matrix_table(nursery_result_ant5_run1$class, testset_nursery_run1$application)
  nursery_conf_ant5_run1 <- confusionMatrix(nursery_tab_ant5_run1)
  results_nursery_run1[6, i+1]<-c(nursery_conf_ant5_run1$overall['Accuracy'])

  nursery_model_ant6_run1 <- antminer6(trainset_nursery_run1, "application", 10, 1000, 10, 10)
  nursery_result_ant6_run1 <- predict(nursery_model_ant6_run1, subset(testset_nursery_run1, select=-application))
  nursery_tab_ant6_run1<-conf_matrix_table(nursery_result_ant6_run1$class, testset_nursery_run1$application)
  nursery_conf_ant6_run1 <- confusionMatrix(nursery_tab_ant6_run1)
  results_nursery_run1[7, i+1]<-c(nursery_conf_ant6_run1$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
results_nursery_run1



starttime<-Sys.time()
for(i in 2:loop_size) {
  trainset_nursery_run2<-nursery[ nursery_folds_run1[[i]] ,]
  testset_nursery_run2<-nursery[ -nursery_folds_run1[[i]] ,]

  nursery_model_ant5_run2 <- antminer5(trainset_nursery_run2, "application", 10, 1000, 10, 10)
  nursery_result_ant5_run2 <- predict(nursery_model_ant5_run2, subset(testset_nursery_run2, select=-application))
  nursery_tab_ant5_run2<-conf_matrix_table(nursery_result_ant5_run2$class, testset_nursery_run2$application)
  nursery_conf_ant5_run2 <- confusionMatrix(nursery_tab_ant5_run2)
  results_nursery_run1[6, i+1]<-c(nursery_conf_ant5_run2$overall['Accuracy'])

}

print('czas wykonania')
print(starttime-Sys.time())
results_nursery_run1
