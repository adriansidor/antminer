mushroom <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = FALSE)
names(mushroom) <- c("edibility", "cap-shape", "cap_surface", "cap_color", "bruises?", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
mushroom <- mushroom[-17] #usuwanie atrybutu z jednym przypadkiem

#10-fold-cross-validation
#sprawdzian krzyzowy (10 krotna walidacja)
loop_size<-10
antminer5_mushroom_folds <- createFolds(mushroom$edibility, k = loop_size, returnTrain = TRUE)

#data frame with all results
antminer5_results_mushroom <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))
antminer5_results_mushroom[1, 1]<-c("C5.0")
antminer5_results_mushroom[2, 1]<-c("svm")
antminer5_results_mushroom[3, 1]<-c("naive bayes")
antminer5_results_mushroom[4, 1]<-c("cart")
antminer5_results_mushroom[5, 1]<-c("antminer")

starttime<-Sys.time()
for(i in 1:loop_size) {
  antminer5_trainset_mushroom<-mushroom[ antminer5_mushroom_folds[[i]] ,]
  antminer5_testset_mushroom<-mushroom[ -antminer5_mushroom_folds[[i]] ,]

  antminer5_mushroom_model1 <- C5.0(edibility ~ ., data=antminer5_trainset_mushroom)
  antminer5_mushroom_result1 <- predict(object=antminer5_mushroom_model1, newdata=antminer5_testset_mushroom, type="class")
  antminer5_mushroom_tab1<-conf_matrix_table(antminer5_mushroom_result1, antminer5_testset_mushroom$edibility)
  antminer5_mushroom_conf1 <- confusionMatrix(antminer5_mushroom_tab1)
  antminer5_mushroom_conf1$overall['Accuracy']
  antminer5_results_mushroom[1, i+1]<-c(antminer5_mushroom_conf1$overall['Accuracy'])

  antminer5_mushroom_model3 <- svm(edibility ~ ., data=antminer5_trainset_mushroom)
  antminer5_mushroom_result3 <- predict(object=antminer5_mushroom_model3, newdata=antminer5_testset_mushroom, type="class")
  antminer5_mushroom_tab3<-conf_matrix_table(antminer5_mushroom_result3, antminer5_testset_mushroom$edibility)
  antminer5_mushroom_conf3 <- confusionMatrix(antminer5_mushroom_tab3)
  antminer5_results_mushroom[2, i+1]<-c(antminer5_mushroom_conf3$overall['Accuracy'])

  antminer5_mushroom_model5 <- naiveBayes(x = subset(antminer5_trainset_mushroom, select=-edibility), y = antminer5_trainset_mushroom$edibility)
  antminer5_mushroom_result5 <- predict(object = antminer5_mushroom_model5, newdata = antminer5_testset_mushroom, type = "class")
  #antminer5_mushroom_result3 <- predict(object=antminer5_mushroom_model3, newdata=antminer5_testset_mushroom, type="class")
  antminer5_mushroom_tab5<-conf_matrix_table(antminer5_mushroom_result5, antminer5_testset_mushroom$edibility)
  antminer5_mushroom_conf5 <- confusionMatrix(antminer5_mushroom_tab5)
  antminer5_results_mushroom[3, i+1]<-c(antminer5_mushroom_conf5$overall['Accuracy'])

  antminer5_mushroom_model6 <- rpart(edibility ~ ., data=antminer5_trainset_mushroom)
  antminer5_mushroom_result6 <- predict(object=antminer5_mushroom_model6, newdata=antminer5_testset_mushroom, type="class")
  antminer5_mushroom_tab6<-conf_matrix_table(antminer5_mushroom_result6, antminer5_testset_mushroom$edibility)
  antminer5_mushroom_conf6 <- confusionMatrix(antminer5_mushroom_tab6)
  antminer5_results_mushroom[4, i+1]<-c(antminer5_mushroom_conf6$overall['Accuracy'])

  antminer5_mushroom_model7 <- antminer5(antminer5_trainset_mushroom, "edibility", 10, 100, 10, 10)
  #antminer5_mushroom_model7 <- antminer3(mushroom, "edibility", 10, 3000, 10, 10)
  antminer5_mushroom_result7 <- predict(antminer5_mushroom_model7, subset(antminer5_testset_mushroom, select=-edibility))
  antminer5_mushroom_tab7<-conf_matrix_table(antminer5_mushroom_result7$class, antminer5_testset_mushroom$edibility)
  antminer5_mushroom_conf7 <- confusionMatrix(antminer5_mushroom_tab7)
  antminer5_results_mushroom[5, i+1]<-c(antminer5_mushroom_conf7$overall['Accuracy'])
}
print("calkowity czas")
print(starttime-Sys.time())
antminer5_results_mushroom
