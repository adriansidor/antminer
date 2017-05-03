cars <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = FALSE)
names(cars) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "evaluation")
#cars <- cars[-17] #usuwanie atrybutu z jednym przypadkiem
#70% dane treningowe, 30% dane testowe
sam<-sample(2,nrow(cars),replace = TRUE, prob=c(0.7,0.3))
trainset_cars<-cars[sam==1,]
testset_cars<-cars[sam==2,]

#data frame with all results
results_cars <- as.data.frame(setNames(replicate(2,numeric(0), simplify = F), c("algorithm", "accuracy")))

#library for confusion matrix
#install.packages(c("caret", "e1071"))
library(caret)
#C5.0
#install.packages("C50")
library(C50)
model1 <- C5.0(evaluation ~ ., data=trainset_cars)
result1 <- predict(object=model1, newdata=testset_cars, type="class")
tab1<-conf_matrix_table(result1, testset_cars$evaluation)
conf1 <- confusionMatrix(tab1)
conf1$overall['Accuracy']
results_cars[nrow(results_cars)+1,]<-c("C5.0", conf1$overall['Accuracy'])

#k-means (cos nie dziala!)
#install.packages("stats")
# library(stats)
# train.indeces <- sample(1:nrow(cars), 6000)
# iris.train <- cars[train.indeces, ]
# iris.test <- cars[-train.indeces, ]
# testset_cars_subset <- subset(testset_cars, select=-evaluation)
# model2 <- kmeans(x=subset(iris.train, select=-evaluation), centers=2)
# tab2<-conf_matrix_table(model2$cluster, testset_cars$evaluation)
# conf2 <- confusionMatrix(tab1)
# results_cars[nrow(results_cars)+1,]<-c("k-means", conf2$overall['Accuracy'])

#svm
#install.packages("e1071")
library(e1071)
model3 <- svm(evaluation ~ ., data=trainset_cars)
result3 <- predict(object=model3, newdata=testset_cars, type="class")
tab3<-conf_matrix_table(result3, testset_cars$evaluation)
conf3 <- confusionMatrix(tab3)
results_cars[nrow(results_cars)+1,]<-c("svm", conf3$overall['Accuracy'])

#knn
#install.packages("e1071")
# library(class)
# model4 <- knn(train = subset(trainset_cars, select = -evaluation),
#                test = subset(testset_cars, select = -evaluation),
#                cl = trainset_cars$evaluation)
# tab4<-conf_matrix_table(model4, testset_cars$evaluation)
# conf4 <- confusionMatrix(tab4)
# results_cars[nrow(results_cars)+1,]<-c("knn", conf4$overall['Accuracy'])

#Naive Bayes
#install.packages("e1071")
library(e1071)
model5 <- naiveBayes(x = subset(trainset_cars, select=-evaluation), y = trainset_cars$evaluation)
result5 <- predict(object = model5, newdata = testset_cars, type = "class")
#result3 <- predict(object=model3, newdata=testset_cars, type="class")
tab5<-conf_matrix_table(result5, testset_cars$evaluation)
conf5 <- confusionMatrix(tab5)
results_cars[nrow(results_cars)+1,]<-c("naive bayes", conf5$overall['Accuracy'])

#cart
library(rpart)
model6 <- rpart(evaluation ~ ., data=trainset_cars)
result6 <- predict(object=model6, newdata=testset_cars, type="class")
tab6<-conf_matrix_table(result6, testset_cars$evaluation)
conf6 <- confusionMatrix(tab6)
results_cars[nrow(results_cars)+1,]<-c("cart", conf6$overall['Accuracy'])

#antminer
library(antminer)
library(entropy);
model7 <- antminer(trainset_cars, "evaluation", 10, 3000, 10, 10)
result7 <- predict(model7, subset(testset_cars, select=-evaluation))
tab7<-conf_matrix_table(result7$class, testset_cars$evaluation)
conf7 <- confusionMatrix(tab7)
results_cars[nrow(results_cars)+1,]<-c("antminer", conf7$overall['Accuracy'])

results_cars
