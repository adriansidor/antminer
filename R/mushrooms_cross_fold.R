mushroom <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = FALSE)
names(mushroom) <- c("edibility", "cap-shape", "cap_surface", "cap_color", "bruises?", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
mushroom <- mushroom[-17] #usuwanie atrybutu z jednym przypadkiem

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
folds <- createFolds(mushroom$edibility, k = loop_size, returnTrain = TRUE)

#data frame with all results
results_mushroom <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
results_mushroom[1, 1]<-c("C5.0")
results_mushroom[2, 1]<-c("svm")
results_mushroom[3, 1]<-c("naive bayes")
results_mushroom[4, 1]<-c("cart")
results_mushroom[5, 1]<-c("antminer")

starttime<-Sys.time()
for(i in 1:loop_size) {
  trainset_mushroom<-mushroom[ folds[[i]] ,]
  testset_mushroom<-mushroom[ -folds[[i]] ,]

  model1 <- C5.0(edibility ~ ., data=trainset_mushroom)
  result1 <- predict(object=model1, newdata=testset_mushroom, type="class")
  tab1<-conf_matrix_table(result1, testset_mushroom$edibility)
  conf1 <- confusionMatrix(tab1)
  conf1$overall['Accuracy']
  results_mushroom[1, i+1]<-c(conf1$overall['Accuracy'])

  model3 <- svm(edibility ~ ., data=trainset_mushroom)
  result3 <- predict(object=model3, newdata=testset_mushroom, type="class")
  tab3<-conf_matrix_table(result3, testset_mushroom$edibility)
  conf3 <- confusionMatrix(tab3)
  results_mushroom[2, i+1]<-c(conf3$overall['Accuracy'])

  model5 <- naiveBayes(x = subset(trainset_mushroom, select=-edibility), y = trainset_mushroom$edibility)
  result5 <- predict(object = model5, newdata = testset_mushroom, type = "class")
  #result3 <- predict(object=model3, newdata=testset_mushroom, type="class")
  tab5<-conf_matrix_table(result5, testset_mushroom$edibility)
  conf5 <- confusionMatrix(tab5)
  results_mushroom[3, i+1]<-c(conf5$overall['Accuracy'])

  model6 <- rpart(edibility ~ ., data=trainset_mushroom)
  result6 <- predict(object=model6, newdata=testset_mushroom, type="class")
  tab6<-conf_matrix_table(result6, testset_mushroom$edibility)
  conf6 <- confusionMatrix(tab6)
  results_mushroom[4, i+1]<-c(conf6$overall['Accuracy'])

  model7 <- antminer4(trainset_mushroom, "edibility", 10, 100, 10, 10)
  #model7 <- antminer3(mushroom, "edibility", 10, 3000, 10, 10)
  result7 <- predict(model7, subset(testset_mushroom, select=-edibility))
  tab7<-conf_matrix_table(result7$class, testset_mushroom$edibility)
  conf7 <- confusionMatrix(tab7)
  results_mushroom[5, i+1]<-c(conf7$overall['Accuracy'])
}
print("calkowity czas")
print(starttime-Sys.time())
results_mushroom
