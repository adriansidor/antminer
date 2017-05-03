nursery <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data"), header = FALSE)
names(nursery) <- c("parents", "has_nurs", "form", "children", "housing", "finance", "social", "health", "application")
#mushroom <- mushroom[-17] #usuwanie atrybutu z jednym przypadkiem
#70% dane treningowe, 30% dane testowe
sam<-sample(2,nrow(nursery),replace = TRUE, prob=c(0.7,0.3))
trainset_nursery<-nursery[sam==1,]
testset_nursery<-nursery[sam==2,]

#data frame with all results
results_nursery <- as.data.frame(setNames(replicate(2,numeric(0), simplify = F), c("algorithm", "accuracy")))

#library for confusion matrix
#install.packages(c("caret", "e1071"))
library(caret)
#C5.0
#install.packages("C50")
library(C50)
model1 <- C5.0(application ~ ., data=trainset_nursery)
result1 <- predict(object=model1, newdata=testset_nursery, type="class")
tab1<-conf_matrix_table(result1, testset_nursery$application)
conf1 <- confusionMatrix(tab1)
conf1$overall['Accuracy']
results_nursery[nrow(results_nursery)+1,]<-c("C5.0", conf1$overall['Accuracy'])

#k-means (cos nie dziala!)
#install.packages("stats")
# library(stats)
# train.indeces <- sample(1:nrow(nursery), 6000)
# iris.train <- nursery[train.indeces, ]
# iris.test <- nursery[-train.indeces, ]
# testset_nursery_subset <- subset(testset_nursery, select=-application)
# model2 <- kmeans(x=subset(iris.train, select=-application), centers=2)
# tab2<-conf_matrix_table(model2$cluster, testset_nursery$application)
# conf2 <- confusionMatrix(tab1)
# results_nursery[nrow(results_nursery)+1,]<-c("k-means", conf2$overall['Accuracy'])

#svm
#install.packages("e1071")
library(e1071)
model3 <- svm(application ~ ., data=trainset_nursery)
result3 <- predict(object=model3, newdata=testset_nursery, type="class")
tab3<-conf_matrix_table(result3, testset_nursery$application)
conf3 <- confusionMatrix(tab3)
results_nursery[nrow(results_nursery)+1,]<-c("svm", conf3$overall['Accuracy'])

#knn
#install.packages("e1071")
# library(class)
# model4 <- knn(train = subset(trainset_nursery, select = -application),
#                test = subset(testset_nursery, select = -application),
#                cl = trainset_nursery$application)
# tab4<-conf_matrix_table(model4, testset_nursery$application)
# conf4 <- confusionMatrix(tab4)
# results_nursery[nrow(results_nursery)+1,]<-c("knn", conf4$overall['Accuracy'])

#Naive Bayes
#install.packages("e1071")
library(e1071)
model5 <- naiveBayes(x = subset(trainset_nursery, select=-application), y = trainset_nursery$application)
result5 <- predict(object = model5, newdata = testset_nursery, type = "class")
#result3 <- predict(object=model3, newdata=testset_nursery, type="class")
tab5<-conf_matrix_table(result5, testset_nursery$application)
conf5 <- confusionMatrix(tab5)
results_nursery[nrow(results_nursery)+1,]<-c("naive bayes", conf5$overall['Accuracy'])

#cart
library(rpart)
model6 <- rpart(application ~ ., data=trainset_nursery)
result6 <- predict(object=model6, newdata=testset_nursery, type="class")
tab6<-conf_matrix_table(result6, testset_nursery$application)
conf6 <- confusionMatrix(tab6)
results_nursery[nrow(results_nursery)+1,]<-c("cart", conf6$overall['Accuracy'])

#antminer
library(antminer)
library(entropy);
model7 <- antminer(trainset_nursery, "application", 10, 3000, 10, 10)
result7 <- predict(model7, subset(testset_nursery, select=-application))
tab7<-conf_matrix_table(result7$class, testset_nursery$application)
conf7 <- confusionMatrix(tab7)
results_nursery[nrow(results_nursery)+1,]<-c("antminer", conf7$overall['Accuracy'])

results_nursery
