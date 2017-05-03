conf_matrix_table <- function(predicted, actual) {
  #y <- testset_cars$evaluation
  #l <- union(result7$class, y)
  #tab7<-table(factor(result7$class, l), factor(testset_cars$evaluation, l))
  levels <- union(predicted, actual)
  table(factor(predicted, levels), factor(actual, levels))
}

run_antminer <- function(trainset, testset, column, results, loop) {
  for (i in 1:loop) {
    model <- antminer(trainset, column, 10, 3000, 10, 10)
    result <- predict(model, testset[ , -which(names(testset) %in% c(column))])
    tab<-conf_matrix_table(result$class, testset[ , column])
    tab
    conf <- confusionMatrix(tab)
    results[nrow(results)+1,]<-c("antminer", conf$overall['Accuracy'])
  }
  results
}

run_C50 <- function(trainset, testset, column, results, loop) {
  for (i in 1:loop) {
    model <- C5.0(edibility ~ ., data=trainset_mushroom)
    result1 <- predict(object=model1, newdata=testset_mushroom, type="class")
    tab1<-conf_matrix_table(result1, testset_mushroom$edibility)
    conf1 <- confusionMatrix(tab1)
    conf1$overall['Accuracy']
    results_mushroom[nrow(results_mushroom)+1,]<-c("C5.0", conf1$overall['Accuracy'])
  }
  results
}

load_libraries <- function() {
  library(e1071)
  library(rpart)
  library(antminer)
  library(entropy)
  library(caret)
  library(C50)
  library(data.table)
  #library(matlab)
  library(dplyr)
}

install <- function() {
  install.packages("dplyr")
  install.packages("data.table")
}
