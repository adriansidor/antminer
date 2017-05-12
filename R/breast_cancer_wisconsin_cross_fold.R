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
results_breast_cancer_wisconsin[5, 1]<-c("antminer")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_breast_cancer_wisconsin<-breast_cancer_wisconsin[ breast_cancer_wisconsin_folds[[i]] ,]
  testset_breast_cancer_wisconsin<-breast_cancer_wisconsin[ -breast_cancer_wisconsin_folds[[i]] ,]

  model_breast_cancer_wisconsin1 <- C5.0(Class ~ ., data=trainset_breast_cancer_wisconsin)
  result_breast_cancer_wisconsin1 <- predict(object=model_breast_cancer_wisconsin1, newdata=testset_breast_cancer_wisconsin, type="class")
  tab_breast_cancer_wisconsin1<-conf_matrix_table(result_breast_cancer_wisconsin1, testset_breast_cancer_wisconsin$Class)
  conf_breast_cancer_wisconsin1 <- confusionMatrix(tab_breast_cancer_wisconsin1)
  conf_breast_cancer_wisconsin1$overall['Accuracy']
  results_breast_cancer_wisconsin[1, i+1]<-c(conf_breast_cancer_wisconsin1$overall['Accuracy'])

  model_breast_cancer_wisconsin3 <- svm(Class ~ ., data=trainset_breast_cancer_wisconsin)
  result_breast_cancer_wisconsin3 <- predict(object=model_breast_cancer_wisconsin3, newdata=testset_breast_cancer_wisconsin, type="class")
  tab_breast_cancer_wisconsin3<-conf_matrix_table(result_breast_cancer_wisconsin3, testset_breast_cancer_wisconsin$Class)
  conf_breast_cancer_wisconsin3 <- confusionMatrix(tab_breast_cancer_wisconsin3)
  results_breast_cancer_wisconsin[2, i+1]<-c(conf_breast_cancer_wisconsin3$overall['Accuracy'])

  model_breast_cancer_wisconsin5 <- naiveBayes(x = subset(trainset_breast_cancer_wisconsin, select=-Class), y = trainset_breast_cancer_wisconsin$Class)
  result_breast_cancer_wisconsin5 <- predict(object = model_breast_cancer_wisconsin5, newdata = testset_breast_cancer_wisconsin, type = "class")
  #result_breast_cancer_wisconsin3 <- predict(object=model_breast_cancer_wisconsin3, newdata=testset_breast_cancer_wisconsin, type="class")
  tab_breast_cancer_wisconsin5<-conf_matrix_table(result_breast_cancer_wisconsin5, testset_breast_cancer_wisconsin$Class)
  conf_breast_cancer_wisconsin5 <- confusionMatrix(tab_breast_cancer_wisconsin5)
  results_breast_cancer_wisconsin[3, i+1]<-c(conf_breast_cancer_wisconsin5$overall['Accuracy'])

  model_breast_cancer_wisconsin6 <- rpart(Class ~ ., data=trainset_breast_cancer_wisconsin)
  result_breast_cancer_wisconsin6 <- predict(object=model_breast_cancer_wisconsin6, newdata=testset_breast_cancer_wisconsin, type="class")
  tab_breast_cancer_wisconsin6<-conf_matrix_table(result_breast_cancer_wisconsin6, testset_breast_cancer_wisconsin$Class)
  conf_breast_cancer_wisconsin6 <- confusionMatrix(tab_breast_cancer_wisconsin6)
  results_breast_cancer_wisconsin[4, i+1]<-c(conf_breast_cancer_wisconsin6$overall['Accuracy'])

  model_breast_cancer_wisconsin7 <- antminer4(trainset_breast_cancer_wisconsin, "Class", 10, 1000, 10, 10)
  print("po modelu")
  result_breast_cancer_wisconsin7 <- predict(model_breast_cancer_wisconsin7, subset(testset_breast_cancer_wisconsin, select=-Class))
  tab_breast_cancer_wisconsin7<-conf_matrix_table(result_breast_cancer_wisconsin7$class, testset_breast_cancer_wisconsin$Class)
  conf_breast_cancer_wisconsin7 <- confusionMatrix(tab_breast_cancer_wisconsin7)
  results_breast_cancer_wisconsin[5, i+1]<-c(conf_breast_cancer_wisconsin7$overall['Accuracy'])
}

print("calkowity czas")
print(starttime-Sys.time())
results_breast_cancer_wisconsin
