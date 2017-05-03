time<-function(size) {
  trainingSet<-mushroom[-1]
  trainingSet[length(trainingSet)+1]<-mushroom[1]
  terms<-getTerms(mushroom[-1])
  class<-"edibility"
  nr_of_class<-length(unique(trainingSet[,class]))
  entropies<-computeEntropy(terms, trainingSet, class)
  pheromones <- initPheromone(terms)
  rule<-build_rule(trainingSet, terms, 10, class, nr_of_class, entropies, pheromones)
  #system.time( replicate(size, build_rule(trainingSet, terms, 10, class, nr_of_class, entropies, pheromones) ) )
  #system.time( replicate(size, prune(rule, trainingSet, class) ) )
  #system.time( replicate(size, quality(rule, trainingSet, class) ) )
  system.time( replicate(size, as.data.table(trainingSet) ) )
  #system.time( replicate(size, TN(rule, trainingSet, class)))
  #system.time( replicate(size, TP(rule, trainingSet, class)))
  #system.time( replicate(size, TP2(rule, trainingSet, class)))
  #system.time( replicate(size, TP3(rule, trainingSet, class)))
  #TP4(rule, trainingSet, class)
}

time<-function(size) {

  trainingSet<-mushroom[-1]
  trainingSet[length(trainingSet)+1]<-mushroom[1]
  trainingSet<-as.data.table(trainingSet)
  terms<-getTerms(mushroom[-1])
  class<-"edibility"
  nr_of_class<-length(unique(trainingSet[,class, with=FALSE]))
  entropies<-computeEntropy(terms, trainingSet, class)
  pheromones <- initPheromone(terms)
  rule<-build_rule(trainingSet, terms, 10, class, nr_of_class, entropies, pheromones)
  #system.time( replicate(size, build_rule(trainingSet, terms, 10, class, nr_of_class, entropies, pheromones) ) )
  #system.time( replicate(size, prune(rule, trainingSet, class) ) )
  #system.time( replicate(size, quality(rule, trainingSet, class) ) )
  #system.time( replicate(size, as.data.table(trainingSet) ) )
  #system.time( replicate(size, TN(rule, trainingSet, class)))
  #system.time( replicate(size, TP(rule, trainingSet, class)))
  #system.time( replicate(size, TP2(rule, trainingSet, class)))
  #system.time( replicate(size, TP3(rule, trainingSet, class)))
  #TP4(rule, trainingSet, class)
}

time<-function(size) {
  trainingSet<-mushroom[-17]
  system.time( replicate(size, antminer3(trainingSet, "edibility", 10, 2, 10, 10) ) )
}

time<-function(size) {
  trainingSet<-kredyt
  #terms<-getTerms(trainingSet[-5])
  #class<-"ryzyko"
  #nr_of_class<-length

  #b<-as.data.table(trainingSet)
  #entropies<-computeEntropy(terms, trainingSet, class)
  #entropies<-computeEntropy(terms, b, class)
  #pheromones <- initPheromone(terms)
  #rule<-build_rule(trainingSet, terms, 2, class, nr_of_class, entropies, pheromones)
  #print(rule)
  #print(FN2(rule, trainingSet, class))
  #print(FN(rule, trainingSet, class))
  #system.time( replicate(size, quality2(rule, trainingSet, class) ) )
  #system.time( replicate(size, prune(rule, trainingSet, class) ) )
  #system.time( replicate(size, TP2(rule[[1]],rule[[2]], trainingSet, class) ) )
  #system.time( replicate(size, TP(rule, trainingSet, class) ) )
  #system.time( replicate(size, quality(rule, trainingSet, class)))
  #system.time( replicate(size, TP2(rule[[1]],rule[[2]], trainingSet, class)))
  #system.time( replicate(size, TP3(rule, trainingSet, class)))
  #FP(rule, trainingSet, class)
  system.time( replicate(size, antminer3(trainingSet, "ryzyko", 2, 100, 2, 2) ) )
}

time<-function(size) {
  model <- antminer3(kredyt, "ryzyko", 2, 100, 10, 2)
  #system.time( replicate(size, prune(rule, trainingSet, class) ) )
  #system.time( replicate(size, TP2(rule[[1]],rule[[2]], trainingSet, class) ) )
  #system.time( replicate(size, TP(rule, trainingSet, class) ) )
  #system.time( replicate(size, quality(rule, trainingSet, class)))
  #system.time( replicate(size, TP2(rule[[1]],rule[[2]], trainingSet, class)))
  #system.time( replicate(size, TP3(rule, trainingSet, class)))
  #FP(rule, trainingSet, class)
}

time2<-function() {
  trainingSet<-mushroom[-1]
  trainingSet[length(trainingSet)+1]<-mushroom[1]
  terms<-getTerms(mushroom[-1])
  class<-"edibility"
  entropy<-computeEntropy(terms, trainingSet, class)
  pheromone <- initPheromone(terms)
  k<- nrow(unique(trainingSet[class]));
  #number of attributes
  n<- ncol(mushroom[-1]);
  system.time( replicate(1, build.rule(trainingSet, terms, 10, class, k, n, entropy, pheromone) ) )

}

time3<-function() {
  system.time(replicate(100, mushroom[which(mushroom[1]=="p"),]))
  system.time(replicate(100,dt[edibility=="p"]))
}

time4<-function() {
  system.time(replicate(100,dt[edibility=="p"]))
}
