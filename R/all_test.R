test <- 3
loop_size<-10

cars <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = FALSE)
names(cars) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "evaluation")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
cars_folds<-list()
cars_results<-list()
cars_model_time<-list()
cars_predict_time<-list()

cars_result_c50 <-list()
cars_conf_c50<-list()

cars_result_svm <-list()
cars_conf_svm<-list()

cars_result_bayes <-list()
cars_conf_bayes<-list()

cars_result_rpart <-list()
cars_conf_rpart<-list()

cars_result_antminer <-list()
cars_conf_antminer<-list()

cars_result_antminer2 <-list()
cars_conf_antminer2<-list()

cars_result_antminer3 <-list()
cars_conf_antminer3<-list()

cars_result_antminer4 <-list()
cars_conf_antminer4<-list()

for(t in 1:test) {
  folds <- createFolds(cars$evaluation, k = loop_size, returnTrain = TRUE)
  cars_folds[[t]]<-folds
  #data frame with all results
  results <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
  results[1, 1]<-c("C5.0")
  results[2, 1]<-c("svm")
  results[3, 1]<-c("naive bayes")
  results[4, 1]<-c("cart")
  results[5, 1]<-c("antminer")
  results[6, 1]<-c("antminer2")
  results[7, 1]<-c("antminer3")
  results[8, 1]<-c("antminer4")

  model_time <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
  model_time[1, 1]<-c("C5.0")
  model_time[2, 1]<-c("svm")
  model_time[3, 1]<-c("naive bayes")
  model_time[4, 1]<-c("cart")
  model_time[5, 1]<-c("antminer")
  model_time[6, 1]<-c("antminer2")
  model_time[7, 1]<-c("antminer3")
  model_time[8, 1]<-c("antminer4")

  predict_time <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
  predict_time[1, 1]<-c("C5.0")
  predict_time[2, 1]<-c("svm")
  predict_time[3, 1]<-c("naive bayes")
  predict_time[4, 1]<-c("cart")
  predict_time[5, 1]<-c("antminer")
  predict_time[6, 1]<-c("antminer2")
  predict_time[7, 1]<-c("antminer3")
  predict_time[8, 1]<-c("antminer4")

  for(i in 1:loop_size) {
    trainset<-cars[ folds[[i]] ,]
    testset<-cars[ -folds[[i]] ,]

    starttime<-Sys.time()
    model_c50 <- C5.0(evaluation ~ ., data=trainset)
    model_time[1, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_c50 <- predict(object=model_c50, newdata=testset, type="class")
    predict_time[1, i+1]<-Sys.time()-starttime
    tab_c50<-conf_matrix_table(result_c50, testset$evaluation)
    conf_c50 <- confusionMatrix(tab_c50)
    results[1, i+1]<-c(conf_c50$overall['Accuracy'])
    cars_result_c50[t][[i]]<-result_c50
    cars_conf_c50[t][[i]]<-conf_c50

    starttime<-Sys.time()
    model_svm <- svm(evaluation ~ ., data=trainset, type="C-classification")
    model_time[2, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_svm <- predict(object=model_svm, newdata=testset)
    predict_time[2, i+1]<-Sys.time()-starttime
    tab_svm<-conf_matrix_table(result_svm, testset$evaluation)
    conf_svm <- confusionMatrix(tab_svm)
    results[2, i+1]<-c(conf_svm$overall['Accuracy'])
    cars_result_svm[t][[i]]<-result_svm
    cars_conf_svm[t][[i]]<-conf_svm

    starttime<-Sys.time()
    model_bayes <- naiveBayes(x = subset(trainset, select=-evaluation), y = trainset$evaluation)
    model_time[3, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_bayes <- predict(object = model_bayes, newdata = testset, type = "class")
    predict_time[3, i+1]<-Sys.time()-starttime
    tab_bayes<-conf_matrix_table(result_bayes, testset$evaluation)
    conf_bayes <- confusionMatrix(tab_bayes)
    results[3, i+1]<-c(conf_bayes$overall['Accuracy'])
    cars_result_bayes[t][[i]]<-result_bayes
    cars_conf_bayes[t][[i]]<-conf_bayes

    starttime<-Sys.time()
    model_rpart <- rpart(evaluation ~ ., data=trainset)
    model_time[4, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_rpart <- predict(object=model_rpart, newdata=testset, type="class")
    predict_time[4, i+1]<-Sys.time()-starttime
    tab6<-conf_matrix_table(result_rpart, testset$evaluation)
    conf_rpart <- confusionMatrix(tab6)
    results[4, i+1]<-c(conf_rpart$overall['Accuracy'])
    cars_result_rpart[t][[i]]<-result_rpart
    cars_conf_rpart[t][[i]]<-conf_rpart

    starttime<-Sys.time()
    model_antminer <- antminer(trainset, "evaluation", 10, 1000, 10, 10)
    model_time[5, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer <- predict.antminer(model_antminer, subset(testset, select=-evaluation))
    predict_time[5, i+1]<-Sys.time()-starttime
    tab_antminer<-conf_matrix_table(result_antminer$class, testset$evaluation)
    conf_antminer <- confusionMatrix(tab_antminer)
    results[5, i+1]<-c(conf_antminer$overall['Accuracy'])
    cars_result_antminer[t][[i]]<-result_antminer
    cars_conf_antminer[t][[i]]<-conf_antminer

    starttime<-Sys.time()
    model_antminer2 <- antminer2(trainset, "evaluation", 10, 1000, 10, 10)
    model_time[6, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer2 <- predict.antminer2(model_antminer2, subset(testset, select=-evaluation))
    predict_time[6, i+1]<-Sys.time()-starttime
    tab_antminer2<-conf_matrix_table(result_antminer2$class, testset$evaluation)
    conf_antminer2 <- confusionMatrix(tab_antminer2)
    results[6, i+1]<-c(conf_antminer2$overall['Accuracy'])
    cars_result_antminer2[t][[i]]<-result_antminer2
    cars_conf_antminer2[t][[i]]<-conf_antminer2

    starttime<-Sys.time()
    model_antminer3 <- antminer3(trainset, "evaluation", 10, 1000, 10, 10)
    model_time[7, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer3 <- predict.antminer3(model_antminer3, subset(testset, select=-evaluation))
    predict_time[7, i+1]<-Sys.time()-starttime
    tab_antminer3<-conf_matrix_table(result_antminer3$class, testset$evaluation)
    conf_antminer3 <- confusionMatrix(tab_antminer3)
    results[7, i+1]<-c(conf_antminer3$overall['Accuracy'])
    cars_result_antminer3[t][[i]]<-result_antminer3
    cars_conf_antminer3[t][[i]]<-conf_antminer3

    starttime<-Sys.time()
    model_antminer4 <- antminer4(trainset, "evaluation", 10, 1000, 10, 10)
    model_time[8, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer4 <- predict.antminer4(model_antminer4, subset(testset, select=-evaluation))
    predict_time[8, i+1]<-Sys.time()-starttime
    tab_antminer4<-conf_matrix_table(result_antminer4$class, testset$evaluation)
    conf_antminer4 <- confusionMatrix(tab_antminer4)
    results[8, i+1]<-c(conf_antminer4$overall['Accuracy'])
    cars_result_antminer4[t][[i]]<-result_antminer4
    cars_conf_antminer4[t][[i]]<-conf_antminer4
  }
  cars_results[[t]]<-results
  cars_model_time[[t]]<-model_time
  cars_predict_time[[t]]<-predict_time
}


print('czas wykonania')
print(starttime-Sys.time())
results

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
breast_cancer_wisconsin_folds <- createFolds(breast_cancer_wisconsin$Class, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_breast_cancer_wisconsin <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_breast_cancer_wisconsin[1, 1]<-c("C5.0")
results_breast_cancer_wisconsin[2, 1]<-c("svm")
results_breast_cancer_wisconsin[3, 1]<-c("naive bayes")
results_breast_cancer_wisconsin[4, 1]<-c("cart")
results_breast_cancer_wisconsin[5, 1]<-c("antminer4")
results_breast_cancer_wisconsin[6, 1]<-c("antminer5")
results_breast_cancer_wisconsin[7, 1]<-c("antminer6")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_breast_cancer_wisconsin<-breast_cancer_wisconsin[ breast_cancer_wisconsin_folds[[i]] ,]
  testset_breast_cancer_wisconsin<-breast_cancer_wisconsin[ -breast_cancer_wisconsin_folds[[i]] ,]

  breast_cancer_wisconsin_model_c50[t][i] <- C5.0(Class ~ ., data=trainset_breast_cancer_wisconsin)
  breast_cancer_wisconsin_result_c50 <- predict(object=breast_cancer_wisconsin_model_c50[t][i], newdata=testset_breast_cancer_wisconsin, type="class")
  breast_cancer_wisconsin_tab_c50[t][i]<-conf_matrix_table(breast_cancer_wisconsin_result_c50, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_c50[t][i] <- confusionMatrix(breast_cancer_wisconsin_tab_c50[t][i])
  breast_cancer_wisconsin_conf_c50[t][i]$overall['Accuracy']
  results_breast_cancer_wisconsin[1, i+1]<-c(breast_cancer_wisconsin_conf_c50[t][i]$overall['Accuracy'])

  breast_cancer_wisconsin_model_svm <- svm(Class ~ ., data=trainset_breast_cancer_wisconsin)
  breast_cancer_wisconsin_result_svm <- predict(object=breast_cancer_wisconsin_model_svm, newdata=testset_breast_cancer_wisconsin, type="class")
  breast_cancer_wisconsin_tab_svm<-conf_matrix_table(breast_cancer_wisconsin_result_svm, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_svm <- confusionMatrix(breast_cancer_wisconsin_tab_svm)
  results_breast_cancer_wisconsin[2, i+1]<-c(breast_cancer_wisconsin_conf_svm$overall['Accuracy'])

  breast_cancer_wisconsin_model_bayes <- naiveBayes(x = subset(trainset_breast_cancer_wisconsin, select=-Class), y = trainset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_result_bayes <- predict(object = breast_cancer_wisconsin_model_bayes, newdata = testset_breast_cancer_wisconsin, type = "class")
  #breast_cancer_wisconsin_result_svm <- predict(object=breast_cancer_wisconsin_model_svm, newdata=testset_breast_cancer_wisconsin, type="class")
  breast_cancer_wisconsin_tab_bayes<-conf_matrix_table(breast_cancer_wisconsin_result_bayes, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_bayes <- confusionMatrix(breast_cancer_wisconsin_tab_bayes)
  results_breast_cancer_wisconsin[3, i+1]<-c(breast_cancer_wisconsin_conf_bayes$overall['Accuracy'])

  breast_cancer_wisconsin_model_rpart <- rpart(Class ~ ., data=trainset_breast_cancer_wisconsin)
  breast_cancer_wisconsin_result_rpart <- predict(object=breast_cancer_wisconsin_model_rpart, newdata=testset_breast_cancer_wisconsin, type="class")
  breast_cancer_wisconsin_tab6<-conf_matrix_table(breast_cancer_wisconsin_result_rpart, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_rpart <- confusionMatrix(breast_cancer_wisconsin_tab6)
  results_breast_cancer_wisconsin[4, i+1]<-c(breast_cancer_wisconsin_conf_rpart$overall['Accuracy'])

  breast_cancer_wisconsin_model_ant4 <- antminer4(trainset_breast_cancer_wisconsin, "Class", 10, 1000, 10, 10)
  breast_cancer_wisconsin_result_ant4 <- predict.antminer4(breast_cancer_wisconsin_model_ant4, subset(testset_breast_cancer_wisconsin, select=-Class))
  breast_cancer_wisconsin_tab_ant4<-conf_matrix_table(breast_cancer_wisconsin_result_ant4$class, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_ant4 <- confusionMatrix(breast_cancer_wisconsin_tab_ant4)
  results_breast_cancer_wisconsin[5, i+1]<-c(breast_cancer_wisconsin_conf_ant4$overall['Accuracy'])

  breast_cancer_wisconsin_model_ant5 <- antminer5(trainset_breast_cancer_wisconsin, "Class", 10, 1000, 10, 10)
  breast_cancer_wisconsin_result_ant5 <- predict.antminer5(breast_cancer_wisconsin_model_ant5, subset(testset_breast_cancer_wisconsin, select=-Class))
  breast_cancer_wisconsin_tab_ant5<-conf_matrix_table(breast_cancer_wisconsin_result_ant5$class, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_ant5 <- confusionMatrix(breast_cancer_wisconsin_tab_ant5)
  results_breast_cancer_wisconsin[6, i+1]<-c(breast_cancer_wisconsin_conf_ant5$overall['Accuracy'])

  breast_cancer_wisconsin_model_ant6 <- antminer6(trainset_breast_cancer_wisconsin, "Class", 10, 1000, 10, 10)
  breast_cancer_wisconsin_result_ant6 <- predict.antminer6(breast_cancer_wisconsin_model_ant6, subset(testset_breast_cancer_wisconsin, select=-Class))
  breast_cancer_wisconsin_tab_ant6<-conf_matrix_table(breast_cancer_wisconsin_result_ant6$class, testset_breast_cancer_wisconsin$Class)
  breast_cancer_wisconsin_conf_ant6 <- confusionMatrix(breast_cancer_wisconsin_tab_ant6)
  results_breast_cancer_wisconsin[7, i+1]<-c(breast_cancer_wisconsin_conf_ant6$overall['Accuracy'])


}

print('czas wykonania')
print(starttime-Sys.time())
results_breast_cancer_wisconsin

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
results_nursery[5, 1]<-c("antminer4")
results_nursery[6, 1]<-c("antminer5")
results_nursery[7, 1]<-c("antminer6")
results_nursery[8, 1]<-c("antminer7")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_nursery<-nursery[ nursery_folds[[i]] ,]
  testset_nursery<-nursery[ -nursery_folds[[i]] ,]

  nursery_model_c50[t][i] <- C5.0(application ~ ., data=trainset_nursery)
  nursery_result_c50 <- predict(object=nursery_model_c50[t][i], newdata=testset_nursery, type="class")
  nursery_tab_c50[t][i]<-conf_matrix_table(nursery_result_c50, testset_nursery$application)
  nursery_conf_c50[t][i] <- confusionMatrix(nursery_tab_c50[t][i])
  nursery_conf_c50[t][i]$overall['Accuracy']
  results_nursery[1, i+1]<-c(nursery_conf_c50[t][i]$overall['Accuracy'])

  nursery_model_svm <- svm(application ~ ., data=trainset_nursery)
  nursery_result_svm <- predict(object=nursery_model_svm, newdata=testset_nursery, type="class")
  nursery_tab_svm<-conf_matrix_table(nursery_result_svm, testset_nursery$application)
  nursery_conf_svm <- confusionMatrix(nursery_tab_svm)
  results_nursery[2, i+1]<-c(nursery_conf_svm$overall['Accuracy'])

  nursery_model_bayes <- naiveBayes(x = subset(trainset_nursery, select=-application), y = trainset_nursery$application)
  nursery_result_bayes <- predict(object = nursery_model_bayes, newdata = testset_nursery, type = "class")
  #nursery_result_svm <- predict(object=nursery_model_svm, newdata=testset_nursery, type="class")
  nursery_tab_bayes<-conf_matrix_table(nursery_result_bayes, testset_nursery$application)
  nursery_conf_bayes <- confusionMatrix(nursery_tab_bayes)
  results_nursery[3, i+1]<-c(nursery_conf_bayes$overall['Accuracy'])

  nursery_model_rpart <- rpart(application ~ ., data=trainset_nursery)
  nursery_result_rpart <- predict(object=nursery_model_rpart, newdata=testset_nursery, type="class")
  nursery_tab6<-conf_matrix_table(nursery_result_rpart, testset_nursery$application)
  nursery_conf_rpart <- confusionMatrix(nursery_tab6)
  results_nursery[4, i+1]<-c(nursery_conf_rpart$overall['Accuracy'])

  nursery_model_ant4 <- antminer4(trainset_nursery, "application", 10, 1000, 10, 10)
  nursery_result_ant4 <- predict(nursery_model_ant4, subset(testset_nursery, select=-application))
  nursery_tab_ant4<-conf_matrix_table(nursery_result_ant4$class, testset_nursery$application)
  nursery_conf_ant4 <- confusionMatrix(nursery_tab_ant4)
  results_nursery[5, i+1]<-c(nursery_conf_ant4$overall['Accuracy'])

  nursery_model_ant5 <- antminer5(trainset_nursery, "application", 10, 1000, 10, 10)
  nursery_result_ant5 <- predict(nursery_model_ant5, subset(testset_nursery, select=-application))
  nursery_tab_ant5<-conf_matrix_table(nursery_result_ant5$class, testset_nursery$application)
  nursery_conf_ant5 <- confusionMatrix(nursery_tab_ant5)
  results_nursery[6, i+1]<-c(nursery_conf_ant5$overall['Accuracy'])

  nursery_model_ant6 <- antminer6(trainset_nursery, "application", 10, 1000, 10, 10)
  nursery_result_ant6 <- predict(nursery_model_ant6, subset(testset_nursery, select=-application))
  nursery_tab_ant6<-conf_matrix_table(nursery_result_ant6$class, testset_nursery$application)
  nursery_conf_ant6 <- confusionMatrix(nursery_tab_ant6)
  results_nursery[7, i+1]<-c(nursery_conf_ant6$overall['Accuracy'])
}

print('czas wykonania')
print(starttime-Sys.time())
results_nursery



starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_nursery<-nursery[ nursery_folds[[i]] ,]
  testset_nursery<-nursery[ -nursery_folds[[i]] ,]

  nursery_model_ant7 <- antminer7(trainset_nursery, "application", 10, 1000, 10, 10)
  nursery_result_ant7 <- predict.antminer7(nursery_model_ant7, subset(testset_nursery, select=-application))
  nursery_tab_ant7<-conf_matrix_table(nursery_result_ant7$class, testset_nursery$application)
  nursery_conf_ant7 <- confusionMatrix(nursery_tab_ant7)
  results_nursery[8, i+1]<-c(nursery_conf_ant7$overall['Accuracy'])

}

print('czas wykonania')
print(starttime-Sys.time())
results

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset<-cars[ folds[[i]] ,]
  testset<-cars[ -folds[[i]] ,]

  cars_model_ant7 <- antminer7(trainset, "evaluation", 10, 1000, 10, 10)
  cars_result_ant7 <- predict.antminer7(cars_model_ant7, subset(testset, select=-evaluation))
  cars_tab_ant7<-conf_matrix_table(cars_result_ant7$class, testset$evaluation)
  cars_conf_ant7 <- confusionMatrix(cars_tab_ant7)
  results[8, i+1]<-c(cars_conf_ant7$overall['Accuracy'])

}

print('czas wykonania')
print(starttime-Sys.time())
results


starttime<-Sys.time()

  breast_cancer_wisconsin_model_ant4 <- antminer4(trainset_breast_cancer_wisconsin, "Class", 10, 1000, 10, 10)
  print('czas wykonania')
  print(starttime-Sys.time())
  breast_cancer_wisconsin_model_ant5 <- antminer5(trainset_breast_cancer_wisconsin, "Class", 10, 1000, 10, 10)
  print('czas wykonania')
  print(starttime-Sys.time())
