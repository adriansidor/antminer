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
breast_cancer_wisconsin_folds<-list()
breast_cancer_wisconsin_results<-list()
breast_cancer_wisconsin_model_time<-list()
breast_cancer_wisconsin_predict_time<-list()

breast_cancer_wisconsin_result_c50 <-list()
breast_cancer_wisconsin_conf_c50<-list()

breast_cancer_wisconsin_result_svm <-list()
breast_cancer_wisconsin_conf_svm<-list()

breast_cancer_wisconsin_result_bayes <-list()
breast_cancer_wisconsin_conf_bayes<-list()

breast_cancer_wisconsin_result_rpart <-list()
breast_cancer_wisconsin_conf_rpart<-list()

breast_cancer_wisconsin_result_antminer <-list()
breast_cancer_wisconsin_conf_antminer<-list()

breast_cancer_wisconsin_result_antminer2 <-list()
breast_cancer_wisconsin_conf_antminer2<-list()

breast_cancer_wisconsin_result_antminer3 <-list()
breast_cancer_wisconsin_conf_antminer3<-list()

breast_cancer_wisconsin_result_antminer4 <-list()
breast_cancer_wisconsin_conf_antminer4<-list()

for(t in 1:test) {
  folds <- createFolds(breast_cancer_wisconsin$Class, k = loop_size, returnTrain = TRUE)
  breast_cancer_wisconsin_folds[[t]]<-folds
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
    trainset<-breast_cancer_wisconsin[ folds[[i]] ,]
    testset<-breast_cancer_wisconsin[ -folds[[i]] ,]

    starttime<-Sys.time()
    model_c50 <- C5.0(Class ~ ., data=trainset)
    model_time[1, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_c50 <- predict(object=model_c50, newdata=testset, type="class")
    predict_time[1, i+1]<-Sys.time()-starttime
    tab_c50<-conf_matrix_table(result_c50, testset$Class)
    conf_c50 <- confusionMatrix(tab_c50)
    results[1, i+1]<-c(conf_c50$overall['Accuracy'])
    breast_cancer_wisconsin_result_c50[t][[i]]<-result_c50
    breast_cancer_wisconsin_conf_c50[t][[i]]<-conf_c50

    starttime<-Sys.time()
    model_svm <- svm(Class ~ ., data=trainset, type="C-classification")
    model_time[2, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_svm <- predict(object=model_svm, newdata=testset)
    predict_time[2, i+1]<-Sys.time()-starttime
    tab_svm<-conf_matrix_table(result_svm, testset$Class)
    conf_svm <- confusionMatrix(tab_svm)
    results[2, i+1]<-c(conf_svm$overall['Accuracy'])
    breast_cancer_wisconsin_result_svm[t][[i]]<-result_svm
    breast_cancer_wisconsin_conf_svm[t][[i]]<-conf_svm

    starttime<-Sys.time()
    model_bayes <- naiveBayes(x = subset(trainset, select=-Class), y = trainset$Class)
    model_time[3, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_bayes <- predict(object = model_bayes, newdata = testset, type = "class")
    predict_time[3, i+1]<-Sys.time()-starttime
    tab_bayes<-conf_matrix_table(result_bayes, testset$Class)
    conf_bayes <- confusionMatrix(tab_bayes)
    results[3, i+1]<-c(conf_bayes$overall['Accuracy'])
    breast_cancer_wisconsin_result_bayes[t][[i]]<-result_bayes
    breast_cancer_wisconsin_conf_bayes[t][[i]]<-conf_bayes

    starttime<-Sys.time()
    model_rpart <- rpart(Class ~ ., data=trainset)
    model_time[4, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_rpart <- predict(object=model_rpart, newdata=testset, type="class")
    predict_time[4, i+1]<-Sys.time()-starttime
    tab6<-conf_matrix_table(result_rpart, testset$Class)
    conf_rpart <- confusionMatrix(tab6)
    results[4, i+1]<-c(conf_rpart$overall['Accuracy'])
    breast_cancer_wisconsin_result_rpart[t][[i]]<-result_rpart
    breast_cancer_wisconsin_conf_rpart[t][[i]]<-conf_rpart

    starttime<-Sys.time()
    model_antminer <- antminer(trainset, "Class", 10, 1000, 10, 10)
    model_time[5, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer <- predict.antminer(model_antminer, subset(testset, select=-Class))
    predict_time[5, i+1]<-Sys.time()-starttime
    tab_antminer<-conf_matrix_table(result_antminer$class, testset$Class)
    conf_antminer <- confusionMatrix(tab_antminer)
    results[5, i+1]<-c(conf_antminer$overall['Accuracy'])
    breast_cancer_wisconsin_result_antminer[t][[i]]<-result_antminer
    breast_cancer_wisconsin_conf_antminer[t][[i]]<-conf_antminer

    starttime<-Sys.time()
    model_antminer2 <- antminer2(trainset, "Class", 10, 1000, 10, 10)
    model_time[6, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer2 <- predict.antminer2(model_antminer2, subset(testset, select=-Class))
    predict_time[6, i+1]<-Sys.time()-starttime
    tab_antminer2<-conf_matrix_table(result_antminer2$class, testset$Class)
    conf_antminer2 <- confusionMatrix(tab_antminer2)
    results[6, i+1]<-c(conf_antminer2$overall['Accuracy'])
    breast_cancer_wisconsin_result_antminer2[t][[i]]<-result_antminer2
    breast_cancer_wisconsin_conf_antminer2[t][[i]]<-conf_antminer2

    starttime<-Sys.time()
    model_antminer3 <- antminer3(trainset, "Class", 10, 1000, 10, 10)
    model_time[7, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer3 <- predict.antminer3(model_antminer3, subset(testset, select=-Class))
    predict_time[7, i+1]<-Sys.time()-starttime
    tab_antminer3<-conf_matrix_table(result_antminer3$class, testset$Class)
    conf_antminer3 <- confusionMatrix(tab_antminer3)
    results[7, i+1]<-c(conf_antminer3$overall['Accuracy'])
    breast_cancer_wisconsin_result_antminer3[t][[i]]<-result_antminer3
    breast_cancer_wisconsin_conf_antminer3[t][[i]]<-conf_antminer3

    starttime<-Sys.time()
    model_antminer4 <- antminer4(trainset, "Class", 10, 1000, 10, 10)
    model_time[8, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer4 <- predict.antminer4(model_antminer4, subset(testset, select=-Class))
    predict_time[8, i+1]<-Sys.time()-starttime
    tab_antminer4<-conf_matrix_table(result_antminer4$class, testset$Class)
    conf_antminer4 <- confusionMatrix(tab_antminer4)
    results[8, i+1]<-c(conf_antminer4$overall['Accuracy'])
    breast_cancer_wisconsin_result_antminer4[t][[i]]<-result_antminer4
    breast_cancer_wisconsin_conf_antminer4[t][[i]]<-conf_antminer4
  }
  breast_cancer_wisconsin_results[[t]]<-results
  breast_cancer_wisconsin_model_time[[t]]<-model_time
  breast_cancer_wisconsin_predict_time[[t]]<-predict_time
}

nursery <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data"), header = FALSE)
names(nursery) <- c("parents", "has_nurs", "form", "children", "housing", "finance", "social", "health", "application")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
nursery_folds<-list()
nursery_results<-list()
nursery_model_time<-list()
nursery_predict_time<-list()

nursery_result_c50 <-list()
nursery_conf_c50<-list()

nursery_result_svm <-list()
nursery_conf_svm<-list()

nursery_result_bayes <-list()
nursery_conf_bayes<-list()

nursery_result_rpart <-list()
nursery_conf_rpart<-list()

nursery_result_antminer <-list()
nursery_conf_antminer<-list()

nursery_result_antminer2 <-list()
nursery_conf_antminer2<-list()

nursery_result_antminer3 <-list()
nursery_conf_antminer3<-list()

nursery_result_antminer4 <-list()
nursery_conf_antminer4<-list()

for(t in 1:test) {
  folds <- createFolds(nursery$application, k = loop_size, returnTrain = TRUE)
  nursery_folds[[t]]<-folds
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
    trainset<-nursery[ folds[[i]] ,]
    testset<-nursery[ -folds[[i]] ,]

    starttime<-Sys.time()
    model_c50 <- C5.0(application ~ ., data=trainset)
    model_time[1, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_c50 <- predict(object=model_c50, newdata=testset, type="class")
    predict_time[1, i+1]<-Sys.time()-starttime
    tab_c50<-conf_matrix_table(result_c50, testset$application)
    conf_c50 <- confusionMatrix(tab_c50)
    results[1, i+1]<-c(conf_c50$overall['Accuracy'])
    nursery_result_c50[t][[i]]<-result_c50
    nursery_conf_c50[t][[i]]<-conf_c50

    starttime<-Sys.time()
    model_svm <- svm(application ~ ., data=trainset, type="C-classification")
    model_time[2, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_svm <- predict(object=model_svm, newdata=testset)
    predict_time[2, i+1]<-Sys.time()-starttime
    tab_svm<-conf_matrix_table(result_svm, testset$application)
    conf_svm <- confusionMatrix(tab_svm)
    results[2, i+1]<-c(conf_svm$overall['Accuracy'])
    nursery_result_svm[t][[i]]<-result_svm
    nursery_conf_svm[t][[i]]<-conf_svm

    starttime<-Sys.time()
    model_bayes <- naiveBayes(x = subset(trainset, select=-application), y = trainset$application)
    model_time[3, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_bayes <- predict(object = model_bayes, newdata = testset, type = "class")
    predict_time[3, i+1]<-Sys.time()-starttime
    tab_bayes<-conf_matrix_table(result_bayes, testset$application)
    conf_bayes <- confusionMatrix(tab_bayes)
    results[3, i+1]<-c(conf_bayes$overall['Accuracy'])
    nursery_result_bayes[t][[i]]<-result_bayes
    nursery_conf_bayes[t][[i]]<-conf_bayes

    starttime<-Sys.time()
    model_rpart <- rpart(application ~ ., data=trainset)
    model_time[4, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_rpart <- predict(object=model_rpart, newdata=testset, type="class")
    predict_time[4, i+1]<-Sys.time()-starttime
    tab6<-conf_matrix_table(result_rpart, testset$application)
    conf_rpart <- confusionMatrix(tab6)
    results[4, i+1]<-c(conf_rpart$overall['Accuracy'])
    nursery_result_rpart[t][[i]]<-result_rpart
    nursery_conf_rpart[t][[i]]<-conf_rpart

    starttime<-Sys.time()
    model_antminer <- antminer(trainset, "application", 10, 1000, 10, 10)
    model_time[5, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer <- predict.antminer(model_antminer, subset(testset, select=-application))
    predict_time[5, i+1]<-Sys.time()-starttime
    tab_antminer<-conf_matrix_table(result_antminer$class, testset$application)
    conf_antminer <- confusionMatrix(tab_antminer)
    results[5, i+1]<-c(conf_antminer$overall['Accuracy'])
    nursery_result_antminer[t][[i]]<-result_antminer
    nursery_conf_antminer[t][[i]]<-conf_antminer

    starttime<-Sys.time()
    model_antminer2 <- antminer2(trainset, "application", 10, 1000, 10, 10)
    model_time[6, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer2 <- predict.antminer2(model_antminer2, subset(testset, select=-application))
    predict_time[6, i+1]<-Sys.time()-starttime
    tab_antminer2<-conf_matrix_table(result_antminer2$class, testset$application)
    conf_antminer2 <- confusionMatrix(tab_antminer2)
    results[6, i+1]<-c(conf_antminer2$overall['Accuracy'])
    nursery_result_antminer2[t][[i]]<-result_antminer2
    nursery_conf_antminer2[t][[i]]<-conf_antminer2

    starttime<-Sys.time()
    model_antminer3 <- antminer3(trainset, "application", 10, 1000, 10, 10)
    model_time[7, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer3 <- predict.antminer3(model_antminer3, subset(testset, select=-application))
    predict_time[7, i+1]<-Sys.time()-starttime
    tab_antminer3<-conf_matrix_table(result_antminer3$class, testset$application)
    conf_antminer3 <- confusionMatrix(tab_antminer3)
    results[7, i+1]<-c(conf_antminer3$overall['Accuracy'])
    nursery_result_antminer3[t][[i]]<-result_antminer3
    nursery_conf_antminer3[t][[i]]<-conf_antminer3

    starttime<-Sys.time()
    model_antminer4 <- antminer4(trainset, "application", 10, 1000, 10, 10)
    model_time[8, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer4 <- predict.antminer4(model_antminer4, subset(testset, select=-application))
    predict_time[8, i+1]<-Sys.time()-starttime
    tab_antminer4<-conf_matrix_table(result_antminer4$class, testset$application)
    conf_antminer4 <- confusionMatrix(tab_antminer4)
    results[8, i+1]<-c(conf_antminer4$overall['Accuracy'])
    nursery_result_antminer4[t][[i]]<-result_antminer4
    nursery_conf_antminer4[t][[i]]<-conf_antminer4
  }
  nursery_results[[t]]<-results
  nursery_model_time[[t]]<-model_time
  nursery_predict_time[[t]]<-predict_time
}

mushroom <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = FALSE)
names(mushroom) <- c("edibility", "cap-shape", "cap_surface", "cap_color", "bruises?", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
mushroom <- mushroom[-17]
#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
mushroom_folds<-list()
mushroom_results<-list()
mushroom_model_time<-list()
mushroom_predict_time<-list()

mushroom_result_c50 <-list()
mushroom_conf_c50<-list()

mushroom_result_svm <-list()
mushroom_conf_svm<-list()

mushroom_result_bayes <-list()
mushroom_conf_bayes<-list()

mushroom_result_rpart <-list()
mushroom_conf_rpart<-list()

mushroom_result_antminer <-list()
mushroom_conf_antminer<-list()

mushroom_result_antminer2 <-list()
mushroom_conf_antminer2<-list()

mushroom_result_antminer3 <-list()
mushroom_conf_antminer3<-list()

mushroom_result_antminer4 <-list()
mushroom_conf_antminer4<-list()

for(t in 1:test) {
  folds <- createFolds(mushroom$edibility, k = loop_size, returnTrain = TRUE)
  mushroom_folds[[t]]<-folds
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
    trainset<-mushroom[ folds[[i]] ,]
    testset<-mushroom[ -folds[[i]] ,]

    starttime<-Sys.time()
    model_c50 <- C5.0(edibility ~ ., data=trainset)
    model_time[1, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_c50 <- predict(object=model_c50, newdata=testset, type="class")
    predict_time[1, i+1]<-Sys.time()-starttime
    tab_c50<-conf_matrix_table(result_c50, testset$edibility)
    conf_c50 <- confusionMatrix(tab_c50)
    results[1, i+1]<-c(conf_c50$overall['Accuracy'])
    mushroom_result_c50[t][[i]]<-result_c50
    mushroom_conf_c50[t][[i]]<-conf_c50

    starttime<-Sys.time()
    model_svm <- svm(edibility ~ ., data=trainset, type="C-classification")
    model_time[2, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_svm <- predict(object=model_svm, newdata=testset)
    predict_time[2, i+1]<-Sys.time()-starttime
    tab_svm<-conf_matrix_table(result_svm, testset$edibility)
    conf_svm <- confusionMatrix(tab_svm)
    results[2, i+1]<-c(conf_svm$overall['Accuracy'])
    mushroom_result_svm[t][[i]]<-result_svm
    mushroom_conf_svm[t][[i]]<-conf_svm

    starttime<-Sys.time()
    model_bayes <- naiveBayes(x = subset(trainset, select=-edibility), y = trainset$edibility)
    model_time[3, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_bayes <- predict(object = model_bayes, newdata = testset, type = "class")
    predict_time[3, i+1]<-Sys.time()-starttime
    tab_bayes<-conf_matrix_table(result_bayes, testset$edibility)
    conf_bayes <- confusionMatrix(tab_bayes)
    results[3, i+1]<-c(conf_bayes$overall['Accuracy'])
    mushroom_result_bayes[t][[i]]<-result_bayes
    mushroom_conf_bayes[t][[i]]<-conf_bayes

    starttime<-Sys.time()
    model_rpart <- rpart(edibility ~ ., data=trainset)
    model_time[4, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_rpart <- predict(object=model_rpart, newdata=testset, type="class")
    predict_time[4, i+1]<-Sys.time()-starttime
    tab6<-conf_matrix_table(result_rpart, testset$edibility)
    conf_rpart <- confusionMatrix(tab6)
    results[4, i+1]<-c(conf_rpart$overall['Accuracy'])
    mushroom_result_rpart[t][[i]]<-result_rpart
    mushroom_conf_rpart[t][[i]]<-conf_rpart

    starttime<-Sys.time()
    model_antminer <- antminer(trainset, "edibility", 10, 1000, 10, 10)
    model_time[5, i+1]<-Sys.time()-starttime
    Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer <- predict.antminer(model_antminer, subset(testset, select=-edibility))
    predict_time[5, i+1]<-Sys.time()-starttime
    tab_antminer<-conf_matrix_table(result_antminer$class, testset$edibility)
    conf_antminer <- confusionMatrix(tab_antminer)
    results[5, i+1]<-c(conf_antminer$overall['Accuracy'])
    mushroom_result_antminer[t][[i]]<-result_antminer
    mushroom_conf_antminer[t][[i]]<-conf_antminer

    starttime<-Sys.time()
    model_antminer2 <- antminer2(trainset, "edibility", 10, 1000, 10, 10)
    model_time[6, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer2 <- predict.antminer2(model_antminer2, subset(testset, select=-edibility))
    predict_time[6, i+1]<-Sys.time()-starttime
    tab_antminer2<-conf_matrix_table(result_antminer2$class, testset$edibility)
    conf_antminer2 <- confusionMatrix(tab_antminer2)
    results[6, i+1]<-c(conf_antminer2$overall['Accuracy'])
    mushroom_result_antminer2[t][[i]]<-result_antminer2
    mushroom_conf_antminer2[t][[i]]<-conf_antminer2

    starttime<-Sys.time()
    model_antminer3 <- antminer3(trainset, "edibility", 10, 1000, 10, 10)
    model_time[7, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer3 <- predict.antminer3(model_antminer3, subset(testset, select=-edibility))
    predict_time[7, i+1]<-Sys.time()-starttime
    tab_antminer3<-conf_matrix_table(result_antminer3$class, testset$edibility)
    conf_antminer3 <- confusionMatrix(tab_antminer3)
    results[7, i+1]<-c(conf_antminer3$overall['Accuracy'])
    mushroom_result_antminer3[t][[i]]<-result_antminer3
    mushroom_conf_antminer3[t][[i]]<-conf_antminer3

    starttime<-Sys.time()
    model_antminer4 <- antminer4(trainset, "edibility", 10, 1000, 10, 10)
    model_time[8, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer4 <- predict.antminer4(model_antminer4, subset(testset, select=-edibility))
    predict_time[8, i+1]<-Sys.time()-starttime
    tab_antminer4<-conf_matrix_table(result_antminer4$class, testset$edibility)
    conf_antminer4 <- confusionMatrix(tab_antminer4)
    results[8, i+1]<-c(conf_antminer4$overall['Accuracy'])
    mushroom_result_antminer4[t][[i]]<-result_antminer4
    mushroom_conf_antminer4[t][[i]]<-conf_antminer4
  }
  mushroom_results[[t]]<-results
  mushroom_model_time[[t]]<-model_time
  mushroom_predict_time[[t]]<-predict_time
}

tic_tac_toe <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data"), header = FALSE)
names(tic_tac_toe) <- c("top-left-square", "top-middle-square", "top-right-square", "middle-left-square", "middle-middle-square", "middle-right-square", "bottom-left-square", "bottom-middle-square", "bottom-right-square", "Class")

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
tic_tac_toe_folds<-list()
tic_tac_toe_results<-list()
tic_tac_toe_model_time<-list()
tic_tac_toe_predict_time<-list()

tic_tac_toe_result_c50 <-list()
tic_tac_toe_conf_c50<-list()

tic_tac_toe_result_svm <-list()
tic_tac_toe_conf_svm<-list()

tic_tac_toe_result_bayes <-list()
tic_tac_toe_conf_bayes<-list()

tic_tac_toe_result_rpart <-list()
tic_tac_toe_conf_rpart<-list()

tic_tac_toe_result_antminer <-list()
tic_tac_toe_conf_antminer<-list()

tic_tac_toe_result_antminer2 <-list()
tic_tac_toe_conf_antminer2<-list()

tic_tac_toe_result_antminer3 <-list()
tic_tac_toe_conf_antminer3<-list()

tic_tac_toe_result_antminer4 <-list()
tic_tac_toe_conf_antminer4<-list()

for(t in 1:test) {
  folds <- createFolds(tic_tac_toe$Class, k = loop_size, returnTrain = TRUE)
  tic_tac_toe_folds[[t]]<-folds
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
    trainset<-tic_tac_toe[ folds[[i]] ,]
    testset<-tic_tac_toe[ -folds[[i]] ,]

    starttime<-Sys.time()
    model_c50 <- C5.0(Class ~ ., data=trainset)
    model_time[1, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_c50 <- predict(object=model_c50, newdata=testset, type="class")
    predict_time[1, i+1]<-Sys.time()-starttime
    tab_c50<-conf_matrix_table(result_c50, testset$Class)
    conf_c50 <- confusionMatrix(tab_c50)
    results[1, i+1]<-c(conf_c50$overall['Accuracy'])
    tic_tac_toe_result_c50[t][[i]]<-result_c50
    tic_tac_toe_conf_c50[t][[i]]<-conf_c50

    starttime<-Sys.time()
    model_svm <- svm(Class ~ ., data=trainset, type="C-classification")
    model_time[2, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_svm <- predict(object=model_svm, newdata=testset)
    predict_time[2, i+1]<-Sys.time()-starttime
    tab_svm<-conf_matrix_table(result_svm, testset$Class)
    conf_svm <- confusionMatrix(tab_svm)
    results[2, i+1]<-c(conf_svm$overall['Accuracy'])
    tic_tac_toe_result_svm[t][[i]]<-result_svm
    tic_tac_toe_conf_svm[t][[i]]<-conf_svm

    starttime<-Sys.time()
    model_bayes <- naiveBayes(x = subset(trainset, select=-Class), y = trainset$Class)
    model_time[3, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_bayes <- predict(object = model_bayes, newdata = testset, type = "class")
    predict_time[3, i+1]<-Sys.time()-starttime
    tab_bayes<-conf_matrix_table(result_bayes, testset$Class)
    conf_bayes <- confusionMatrix(tab_bayes)
    results[3, i+1]<-c(conf_bayes$overall['Accuracy'])
    tic_tac_toe_result_bayes[t][[i]]<-result_bayes
    tic_tac_toe_conf_bayes[t][[i]]<-conf_bayes

    starttime<-Sys.time()
    model_rpart <- rpart(Class ~ ., data=trainset)
    model_time[4, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_rpart <- predict(object=model_rpart, newdata=testset, type="class")
    predict_time[4, i+1]<-Sys.time()-starttime
    tab6<-conf_matrix_table(result_rpart, testset$Class)
    conf_rpart <- confusionMatrix(tab6)
    results[4, i+1]<-c(conf_rpart$overall['Accuracy'])
    tic_tac_toe_result_rpart[t][[i]]<-result_rpart
    tic_tac_toe_conf_rpart[t][[i]]<-conf_rpart

    starttime<-Sys.time()
    model_antminer <- antminer(trainset, "Class", 10, 1000, 10, 10)
    model_time[5, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer <- predict.antminer(model_antminer, subset(testset, select=-Class))
    predict_time[5, i+1]<-Sys.time()-starttime
    tab_antminer<-conf_matrix_table(result_antminer$class, testset$Class)
    conf_antminer <- confusionMatrix(tab_antminer)
    results[5, i+1]<-c(conf_antminer$overall['Accuracy'])
    tic_tac_toe_result_antminer[t][[i]]<-result_antminer
    tic_tac_toe_conf_antminer[t][[i]]<-conf_antminer

    starttime<-Sys.time()
    model_antminer2 <- antminer2(trainset, "Class", 10, 1000, 10, 10)
    model_time[6, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer2 <- predict.antminer2(model_antminer2, subset(testset, select=-Class))
    predict_time[6, i+1]<-Sys.time()-starttime
    tab_antminer2<-conf_matrix_table(result_antminer2$class, testset$Class)
    conf_antminer2 <- confusionMatrix(tab_antminer2)
    results[6, i+1]<-c(conf_antminer2$overall['Accuracy'])
    tic_tac_toe_result_antminer2[t][[i]]<-result_antminer2
    tic_tac_toe_conf_antminer2[t][[i]]<-conf_antminer2

    starttime<-Sys.time()
    model_antminer3 <- antminer3(trainset, "Class", 10, 1000, 10, 10)
    model_time[7, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer3 <- predict.antminer3(model_antminer3, subset(testset, select=-Class))
    predict_time[7, i+1]<-Sys.time()-starttime
    tab_antminer3<-conf_matrix_table(result_antminer3$class, testset$Class)
    conf_antminer3 <- confusionMatrix(tab_antminer3)
    results[7, i+1]<-c(conf_antminer3$overall['Accuracy'])
    tic_tac_toe_result_antminer3[t][[i]]<-result_antminer3
    tic_tac_toe_conf_antminer3[t][[i]]<-conf_antminer3

    starttime<-Sys.time()
    model_antminer4 <- antminer4(trainset, "Class", 10, 1000, 10, 10)
    model_time[8, i+1]<-Sys.time()-starttime
    starttime<-Sys.time()
    result_antminer4 <- predict.antminer4(model_antminer4, subset(testset, select=-Class))
    predict_time[8, i+1]<-Sys.time()-starttime
    tab_antminer4<-conf_matrix_table(result_antminer4$class, testset$Class)
    conf_antminer4 <- confusionMatrix(tab_antminer4)
    results[8, i+1]<-c(conf_antminer4$overall['Accuracy'])
    tic_tac_toe_result_antminer4[t][[i]]<-result_antminer4
    tic_tac_toe_conf_antminer4[t][[i]]<-conf_antminer4
  }
  tic_tac_toe_results[[t]]<-results
  tic_tac_toe_model_time[[t]]<-model_time
  tic_tac_toe_predict_time[[t]]<-predict_time
}
