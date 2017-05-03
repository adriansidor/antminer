mushroom <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = FALSE)
names(mushroom) <- c("edibility", "cap-shape", "cap_surface", "cap_color", "bruises?", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
mushroom <- mushroom[-17] #usuwanie atrybutu z jednym przypadkiem
#70% dane treningowe, 30% dane testowe
sam<-sample(2,nrow(mushroom),replace = TRUE, prob=c(0.7,0.3))
trainset_mushroom<-mushroom[sam==1,]
testset_mushroom<-mushroom[sam==2,]

#data frame with all results
loop_size<-10
results_mushroom <- as.data.frame(setNames(replicate(loop_size+1,numeric(0), simplify = F), c("algorithm", "accuracy")))

#library for confusion matrix
#install.packages(c("caret", "e1071"))
library(caret)
#C5.0
#install.packages("C50")
library(C50)
results_mushroom[nrow(results_mushroom)+1, 1]<-c("C5.0")
for(i in 1:loop_size) {
  model1 <- C5.0(edibility ~ ., data=trainset_mushroom)
  result1 <- predict(object=model1, newdata=testset_mushroom, type="class")
  tab1<-conf_matrix_table(result1, testset_mushroom$edibility)
  conf1 <- confusionMatrix(tab1)
  conf1$overall['Accuracy']
  results_mushroom[nrow(results_mushroom), i+1]<-c(conf1$overall['Accuracy'])
}
#k-means (cos nie dziala!)
#install.packages("stats")
# library(stats)
# train.indeces <- sample(1:nrow(mushroom), 6000)
# iris.train <- mushroom[train.indeces, ]
# iris.test <- mushroom[-train.indeces, ]
# testset_mushroom_subset <- subset(testset_mushroom, select=-edibility)
# model2 <- kmeans(x=subset(iris.train, select=-edibility), centers=2)
# tab2<-conf_matrix_table(model2$cluster, testset_mushroom$edibility)
# conf2 <- confusionMatrix(tab1)
# results_mushroom[nrow(results_mushroom)+1,]<-c("k-means", conf2$overall['Accuracy'])

#svm
#install.packages("e1071")
library(e1071)
results_mushroom[nrow(results_mushroom)+1, 1]<-c("svm")
for(i in 1:loop_size) {
  model3 <- svm(edibility ~ ., data=trainset_mushroom)
  result3 <- predict(object=model3, newdata=testset_mushroom, type="class")
  tab3<-conf_matrix_table(result3, testset_mushroom$edibility)
  conf3 <- confusionMatrix(tab3)
  results_mushroom[nrow(results_mushroom), i+1]<-c(conf3$overall['Accuracy'])
}


#knn
#install.packages("e1071")
# library(class)
# model4 <- knn(train = subset(trainset_mushroom, select = -edibility),
#                test = subset(testset_mushroom, select = -edibility),
#                cl = trainset_mushroom$edibility)
# tab4<-conf_matrix_table(model4, testset_mushroom$edibility)
# conf4 <- confusionMatrix(tab4)
# results_mushroom[nrow(results_mushroom)+1,]<-c("knn", conf4$overall['Accuracy'])

#Naive Bayes
#install.packages("e1071")
library(e1071)
results_mushroom[nrow(results_mushroom)+1, 1]<-c("naive bayes")
for(i in 1:loop_size) {
  model5 <- naiveBayes(x = subset(trainset_mushroom, select=-edibility), y = trainset_mushroom$edibility)
  result5 <- predict(object = model5, newdata = testset_mushroom, type = "class")
  #result3 <- predict(object=model3, newdata=testset_mushroom, type="class")
  tab5<-conf_matrix_table(result5, testset_mushroom$edibility)
  conf5 <- confusionMatrix(tab5)
  results_mushroom[nrow(results_mushroom), i+1]<-c(conf5$overall['Accuracy'])
}


#cart
library(rpart)
results_mushroom[nrow(results_mushroom)+1, 1]<-c("cart")
for(i in 1:loop_size) {
  model6 <- rpart(edibility ~ ., data=trainset_mushroom)
  result6 <- predict(object=model6, newdata=testset_mushroom, type="class")
  tab6<-conf_matrix_table(result6, testset_mushroom$edibility)
  conf6 <- confusionMatrix(tab6)
  results_mushroom[nrow(results_mushroom), i+1]<-c(conf6$overall['Accuracy'])
}


#antminer
library(antminer)
library(entropy)
results_mushroom[nrow(results_mushroom)+1, 1]<-c("antminer")
for(i in 1:loop_size) {
  model7 <- antminer(trainset_mushroom, "edibility", 10, 3000, 10, 10)
  result7 <- predict(model7, subset(testset_mushroom, select=-edibility))
  tab7<-conf_matrix_table(result7$class, testset_mushroom$edibility)
  conf7 <- confusionMatrix(tab7)
  results_mushroom[nrow(results_mushroom), i+1]<-c(conf7$overall['Accuracy'])
}

results_mushroom
