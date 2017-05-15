bar_names<-c("C5.0","svm","naive bayes","cart","antminer","antminer2","antminer3")
bar_width<-rep(0.2,7)

results_cars_run1_mean<-apply(results_cars_run1[-1], 1, mean)
results_cars_run1_mean
#barplot(as.matrix(results_cars_run1[-1]), beside = TRUE, width = bar_width, names.arg=bar_names, col=rainbow(7), space=1)
barplot(as.matrix(results_cars_run1[-1]),ylim=c(0,1), beside = TRUE, col=rainbow(7))

results_nursery_run1_mean<-apply(results_nursery_run1[-1], 1, mean)
results_nursery_run1_mean

results_breast_cancer_wisconsin_run1_mean<-apply(results_breast_cancer_wisconsin_run1[-1], 1, mean)
results_breast_cancer_wisconsin_run1_mean

results_mushroom_run1_mean<-apply(results_mushroom_run1[-1], 1, mean)
results_mushroom_run1_mean
