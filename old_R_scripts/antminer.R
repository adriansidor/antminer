#install.packages("entropy", repos = "http://cran.us.r-project.org");
library(entropy);


#' @title Antminer
#' @description Algorytm do budowy modelu klasyfikacji oparty na algorytmie mrowkowym optymalizacji
#' @param data zbior danych do budowy modelu
#' @param class nazwa kolumny ze zbioru danych z wartosciami klas.
#' Algorytm zbuduje model do predykcji tych klas
#' @param maxUncoveredCases liczba niepokrytych przypadkow ze zbioru danych.
#' Algorytm bedzie pracowal dopoki liczba niepokrytych przypadkow jest wieksza od tej liczby
#' @param NumberOfAnts liczba tworzonych regul podczas jednej iteracji.
#' @param NumberOfRulesConverge liczba stworzonych regul podczas iteracji, ktore sie powtarzaja.
#' Algorytm konczy iteracje powyzej tej liczby.
#' @param MinCasesPerRule liczba pokrytych przypadklw przez zbudowana regule.
#' Jesli regula pokrywa mniej to jest odrzucana.
#' @details
#' Algorytm w kazdej iteracji tworzy maksymalnie NumberOfAnts regul na podstawie aktualnego zbioru danych.
#' Regula jest dodawana do zbioru regul jesli pokrywa co najmniej MinCasesPerRule przypadkow.
#' Ze zbioru stworzonych regul wybierana jest jedna regula, najlepsza pod wzgledem jakosci. Przypadki pokrywane przez ta regule sa usuwane ze zbioru danych.
#' Algorytm moze wczesniej zakonczyc iteracje jesli liczba zduplikowanych regul ktore stworzyl przekroczy wartosc NumberOfRulesConverge.
#' Jesli algorytm ciagle tworzy zduplikowane reguly to znaczy ze tak juz sie dostosowal i nie ma sensu tworzyc kolejnych regul, zatem konczymy iteracje.
#' Algorytm bedzie wykonywal kolejne iteracje jesli liczba przypadkow ze zbioru danych jest wieksza niz maxUncoveredCases.
#' Stworzony model ma forme listy uporzadkowanych regul. Od pierwszej znalezionej reguly do ostatniej znalezionej reguly.
#' Wnioskowanie klasy na podstawie modelu polega na znalezieniu regyly ktora pokrywa przypadek i ustawieniu klasy z tego przypadku.
#' Wazne jest to, ze pierwsza regula ktora pokrywa przyklad jest wybierana, nie sa przegladane inne reguly ktore moga lepiej pokrywac przyklad.
#' W modelu ustawiana jest takze klasa domyslna, jest to dominujaca klasa z niepokrytych przypadkow.
#' @return Model skladajacy sie z listy abudowanych regul
#' @examples
#' model <- antminer(trainingSet, "Class", 10, 100, 15, 1)
#' pred <- predict(model, testSet)
antminer <- function(data,class, maxUncoveredCases, NumberOfAnts, NumberOfRulesConverge, MinCasesPerRule) {
  trainingSet <- data;
  c <- trainingSet[class]
  trainingSet[class]<-NULL
  trainingSet[class]<-c;
  trainingSetWithoutClass <- trainingSet;
  trainingSetWithoutClass[class]<-NULL;
  discoveredRules <- list();
  str(trainingSetWithoutClass)
  terms <- getTerms(trainingSetWithoutClass);
  entropy<-computeEntropy(terms, trainingSet, class)
  #number of classes
  k<- nrow(unique(trainingSet[class]));
  #number of attributes
  n<- ncol(trainingSetWithoutClass);
  while(nrow(trainingSet) > maxUncoveredCases) {
    #ant index
    i <- 1;
    #convergence test index
    j <- 1;
    #initialize pheromone
    pheromone <- initPheromone(terms)
    rules <- list();
    als <- list();
    qualities <- NULL;
    while( (i<NumberOfAnts) & (j<NumberOfRulesConverge)) {
      print("mrowka")
      print(i)
      list<- build.rule(trainingSet, terms, MinCasesPerRule, class, k, n, entropy, pheromone);
      #Oprocz numberOfRulesConverge ktora sprawdza tworzenie podobnych regul
      #mozna dodac parametr ktory wylacza algorytm jesli nie utworzy reguly
      #w n krokach, moze sie tak zdarzyc gdy przypadkow jest tak malo ze
      #nie spelniaja np progu pokrytych regul
      if(is.null(list)) {
        i <- i+1;
        next
      }
      list<- prune(list[[1]], list[[2]], trainingSet, class);
      rule <- list[[1]]
      print("regula")
      print(rule)
      al <- list[[2]]
      #update pheromone
      quality <- quality(rule, al, trainingSet, class);
      pheromone <- increasePheromone(terms, rule, al, pheromone, quality);
      pheromone <- decreasePheromone(pheromone);
      print("ostatnia regula")
      print(tail(rules,1))
      #if(isEqualRule(rule, tail(rules, (NumberOfRulesConverge-1)))) {
      if(isEqualRule(rule, tail(rules, 1))) {
        j <- j+1;
      } else {
        j <- 1;
        rules[[length(rules)+1]]<-rule;
        als[[length(als)+1]]<-al;
        qualities <- append(qualities,quality);
      }
      print("convergence")
      print(j)
      i<-i+1;
    }
    #jesli lista regul jest pusta to znaczy ze algorytm nie potrafil podczas
    #NumberOfAnts prób stworzyc reguly
    #dzieje sie tak zwykle gdy liczba przypadkow jest zbyt mala dla pewnych parametrow
    #algorytmu, w takim przypadku nalezy zakonczyc algorytm
    if(length(rules)!=0) {
      bestRule <- bestRule(rules, als, qualities);
      discoveredRules[[length(discoveredRules)+1]]<-bestRule
      trainingSet<-uncoveredCases(bestRule[[1]],bestRule[[2]], trainingSet, class)
    } else {
      print("przerywam algorytm")
      break;
    }
    #print(nrow(trainingSet))
  }
  default <- majorClass(trainingSet, class)
  model <- list(discoveredRules, default)
  class(model)<-"antminer"
  return(model)

}

#sprawdza czy w zbiorze regul jest juz taka regula
isEqualRule <-function(rule, rules) {
  n<-length(rules)
  e<-sapply(rules, function(x) {isEqual(rule,x)})
  if(length(e)==0) {
    return(FALSE)
  }
  if(sum(e) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#przypadki ze zbioru ktorych regula nie pokrywa
uncoveredCases <- function(rule, al, trainingSet, class) {
  n <- length(rule)
  temp <- NULL;
  for(i in 1:n) {
    if(i < 2) {
      temp<-(trainingSet[al[i]]!=rule[i])
    } else if(i > 1 & i < n) {
      temp<-temp | (trainingSet[al[i]]!=rule[i])
    } else {
      temp<-temp | (trainingSet[class]!=rule[i])
    }
  }
  return(trainingSet[temp,])
}

#zwraca najlepsza regule ze wzgledu na jakosc
bestRule <- function(rules, als, qualities) {
  id <- which.max(qualities);
  print("najlepsza regula")
  print(id)
  print(rules)
  list<-list(rules[[id]], als[[id]])
  return(list)
}

#sprawdza czy reguly sa takie same
isEqual <- function(rule1, rule2) {
  n1 <- length(rule1);
  n2 <- length(rule2);
  if(n1 != n2) {
    return(FALSE)
  }
  for(i in 1:n1) {
    if(!is.element(rule1[i],rule2)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#zwieksza feromon dla term'ow ktore znalazly sie w regule
increasePheromone <- function(terms, rule, al, pheromone, quality) {
  n<-length(pheromone)
  for(i in 1:n) {
    t <- terms[[i]];
    id <- which(al == i);
    term <- NULL;
    if(length(id)!=0) {
      term <- rule[id]
    }
    tempPheromone <- NULL;
    for(j in 1:length(t)) {
      term2 <- t[j]
      if(!is.null(term)) {
        oldPheromone <- pheromone[[i]][j]
        tempPheromone[j] <- ifelse(term == term2, oldPheromone + (oldPheromone*quality), oldPheromone)
      } else {
        tempPheromone[j] <- pheromone[[i]][j]
      }
    }
    pheromone[[i]] <- tempPheromone;
  }
  return(pheromone)
}

#zmiejsza feromon dla wszystkich term'ow
#symuluje wyparowywanie feromonu
decreasePheromone <- function(pheromone) {
  pheromoneSum <- pheromoneSum(pheromone)
  for(i in 1:length(pheromone)) {
    pheromone[[i]] <- sapply(pheromone[[i]], function(x) {x/pheromoneSum})
  }
  return(pheromone)
}

#sumuje skladowe wektora feromonu
pheromoneSum <- function(pheromone) {
  sum <- 0;
  for(i in 1:length(pheromone)) {
    sum <- sum + sum(pheromone[[i]])
  }
  return(sum)
}
#przycina regule
prune <- function(rule, al, trainingSet, class) {
  isBetterQuality <- TRUE;

  bestQuality <- quality(rule,al,trainingSet, class)
  while( (length(rule) > 2) & isBetterQuality) {
    qualities <- NULL;
    n<-length(rule)
    for(i in 1:(n-1)) {
      tempRule <- rule[-i];
      tempAl <- al[-i]
      qualities[i] <- quality(tempRule, tempAl, trainingSet, class)
    }
    id<- which.max(qualities)
    maxquality <- qualities[id]
    if(maxquality > bestQuality) {
      rule<-rule[-id]
      al <- al[-id]
      bestQuality <- maxquality
    } else {
      isBetterQuality <- FALSE;
    }
  }
  rule<-rule[-(length(rule))]
  cc <- coveredCases(rule, al, trainingSet)
  rule["class"]<-majorClass(cc, class)
  list<-list(rule,al)

  return(list)
}
#mierzy jakosc reguly
quality <- function(rule, al, trainingSet, class) {
  TP<-TP(rule, al, trainingSet, class)
  FP<-FP(rule, al, trainingSet, class)
  FN<-FN(rule, al, trainingSet, class)
  TN<-TN(rule, al, trainingSet, class)
  counter<- nrow(TP)*nrow(TN)
  dominator <- (nrow(TP)+nrow(FN))*(nrow(FP)+nrow(TN))
  quality <- counter/dominator;
  #wzor na jakosc jest nie najlepszy bo mozna uzyskac dzielenie przez 0
  #jak spada nam liczba przypadkow treningowych to moze sie zdarzyc
  #ze choc jedna z powyzszych 4 wartosci bedzie 0
  #w sumie to nie wiem co z tym robic, implementuje nie swoje zalozenia
  #dziwne ze nie ujeli tego w artykule
  #na razie jakosc ustawie na 0
  #jakosc wplywa na zmiane feromonu wiec jest to dosc wazne
  if(is.nan(quality)) {
    return(0)
  }
  return(quality)
}

#True Positive
TP <- function(rule, al, trainingSet, class) {
  n <- length(rule)
  temp <- NULL;
  for(i in 1:n) {
    if(i < 2) {
      temp<-(trainingSet[al[i]]==rule[i])
    } else if(i > 1 & i < n) {
      temp<-temp & (trainingSet[al[i]]==rule[i])
    } else {
      temp<-temp & (trainingSet[class]==rule[i])
    }
  }
  return(trainingSet[temp,])
}

#False Positive
FP <- function(rule, al, trainingSet, class) {
  n <- length(rule)
  temp <- NULL;
  for(i in 1:n) {
    if(i < 2) {
      temp<-(trainingSet[al[i]]==rule[i])
    } else if(i > 1 & i < n) {
      temp<-temp & (trainingSet[al[i]]==rule[i])
    } else {
      temp<-temp & (trainingSet[class]!=rule[i])
    }
  }
  return(trainingSet[temp,])
}

#False Negative
FN <- function(rule, al, trainingSet, class) {
  n <- length(rule)
  temp <- NULL;
  for(i in 1:n) {
    if(i < 2) {
      temp<-(trainingSet[al[i]]!=rule[i])
    } else if(i > 1 & i < n) {
      temp<-temp | (trainingSet[al[i]]!=rule[i])
    } else {
      temp<-temp & (trainingSet[class]==rule[i])
    }
  }
  return(trainingSet[temp,])
}

#True Negative
TN <- function(rule, al, trainingSet, class) {
  n <- length(rule)
  temp <- NULL;
  for(i in 1:n) {
    if(i < 2) {
      temp<-(trainingSet[al[i]]!=rule[i])
    } else if(i > 1 & i < n) {
      temp<-temp | (trainingSet[al[i]]!=rule[i])
    } else {
      temp<-temp & (trainingSet[class]!=rule[i])
    }
  }
  return(trainingSet[temp,])
}
#oblicza prawdopodobienstwo kazdego term'a
probability <- function(terms, pheromone, eta, a) {
  probability <- list();
  for(i in 1:length(terms)) {
    part <- sapply(eta[[i]], function(x) {x/(sum(a)*sum(pheromone[[i]]*eta[[i]]))})
    probability[[i]] <- part*pheromone[[i]];
  }
  return(probability)
}
#inicjuje feromon kazdego term'a poczatkowa wartoscia
initPheromone <- function(terms) {
  pheromone <- list();
  for(i in 1:length(terms)) {
    pheromone[[i]] <- sapply(terms[[i]], function(x) {initialAmountOfPheromone(terms)})
  }
  return(pheromone)
}
#poczatkowa wartosc feromonu
initialAmountOfPheromone <- function(terms) {
  numberOfValues <- NULL;
  for(i in 1:length(terms)) {
    numberOfValues[i]<-length(terms[[i]]);
  }
  return(1/sum(numberOfValues))
}
#wektor wskazujacy ktore atrybuty znajduja sie w tymczasowej regule
a <- function(cpr, n) {
  if(is.null(cpr)) {
    return(rep(1,n));
  } else {
    a<-NULL;
    for(i in 1:n) {
      if(is.element(i,cpr)) {
        a<-append(a,0);
      } else {
        a<-append(a,1);
      }
    }
    return(a)
  }
}
#lista wartosci NA o rozmiarze jak lista term'ow
NAList <- function(terms) {
  list<-list();
  for(i in 1:length(terms)) {
    list[[i]]<-rep(NA, length(terms[[i]]))
  }
  return(list)
}
#buduje regule
build.rule <- function(trainingSet, terms, MinCasesPerRule, class, k, n, entropy, pheromone) {
  start.time <- Sys.time()
  maxAttributes <- length(terms)
  #current partial rule
  #empty at the beggining
  cpr <- NULL;
  #attribute list
  #what attributes are in cpr
  al <- NULL;
  #any term to be added to the rule would make the rule cover a number
  #of cases smaller then Min_cases_per_rule
  covered_cases_flag = TRUE;
  #All attributes have already been used by ant
  attributes_flag = TRUE;
  #variables <- unlist(terms)
  print("inicjalizacja argumentow - przed while")
  print(start.time-Sys.time())
  while(covered_cases_flag && attributes_flag) {
    addedTerm_flag <- FALSE;
    #draw term based on probabilities
    eta <- computeEta(k, entropy, a(cpr,n))
    print("obliczone eta")
    print(start.time-Sys.time())
    probability <- probability(terms, pheromone, eta, a(cpr,n))
    print("obliczone probabilities")
    print(start.time-Sys.time())
    prob <- unlist(removeUsedTerms(probability,al))
    size <- length(prob)
    browser()
    variables<-unlist(removeUsedTerms(terms, al))
    drawTerms <- sample(variables, size , FALSE, prob)
    print("wylosowane termy")
    print(start.time-Sys.time())
    for(i in 1:size) {
      print(size)
      print(i)
      id <-getAttributeId(terms, drawTerms[i])
      print("wybrany term")
      print(start.time-Sys.time())

      print("przed pokryciem przypadkow")
      print(start.time-Sys.time())
      coveredCases <- coveredCases2(drawTerms[i], id, trainingSet)
      print("po pokryciu przypadkow")
      print(start.time-Sys.time())
      if(nrow(coveredCases) >= MinCasesPerRule) {
        cpr <- append(cpr,drawTerms[i])
        al <- append(al, id)
        addedTerm_flag <- TRUE;
        trainingSet<-coveredCases
        break
      }
    }
    if(length(al) == maxAttributes) {
      attributes_flag <- FALSE;
    }
    if(!addedTerm_flag) {
      covered_cases_flag <- FALSE;
    }
  }
  if(is.null(cpr)) {
    return(NULL)
  }
  cpr["class"]<-majorClass(coveredCases,class)
  list<-list(cpr,al)
  return(list)
}

removeUsedTerms<-function(terms, al) {
  terms[al]<-NULL
  return (terms)
}

#zwraca klase dominujaca dla podanych przypadkow
majorClass <- function(coveredCases, class) {
  sub <- coveredCases[, class]
  tt <- table(sub)
  major <- names(tt[tt==max(tt)])
  return(major[1])
}
#pokryte przez regule przypadki
coveredCases <- function(rule, al,data) {
  sub <- data;
  for(i in 1:length(rule)) {
    sub <- subset(sub, sub[al[i]] == rule[i]);
  }
  return(sub)
}

coveredCases2<-function(drawnTerm, attribute_id, trainingSet) {
  trainingSet[which(trainingSet[attribute_id]==drawnTerm),]
}
#losowanie term'a
chooseTerm <- function(terms, probability) {
  print("losowanie termu z listy")
  print(unlist(terms))
  print("prawdopodobienstwo termow")
  print(probability)
  term <- sample(unlist(terms), 1, FALSE, unlist(probability))
  return(term)
}
#zwraca id atrybutu ktorego wartoscia jest dany term
getAttributeId <- function(terms, term) {
  for(i in 1:length(terms)) {
    if(is.element(term, terms[[i]])) {
      return(i)
    }
  }
}
#lista wszystkich mozliwych term'ow na podstawie zbioru danych
getTerms <- function(data) {
  allterms <- apply(data, 2, unique);
}
#heurystyka
eta <- function(k, entropies, entropy, a) {
  counter <- log2(k)-entropy;
  dominator <- sum(a)*sum((log2(k)-entropies))
  eta<- counter/dominator;
  return(eta)
}
#oblicza heurystyke dla kazdego terma
computeEta <- function(k, entropy, a) {
  n<-length(entropy);
  eta <- list();
  for(i in 1:n) {
    e <- sapply(entropy[[i]], function(x) {eta(k,entropy[[i]],x, a)})
    eta[[i]] <- e;
  }
  return(eta)
}
#oblicza entropie dla kazdego terma
computeEntropy <- function(terms, data, class) {
  #print("terms")
  #print(terms)
  E <- list();
  m <- vector("list", length=14);
  n<-length(terms);
  #print("N")
  #print(n)
  for(i in 1:n) {
    #browser()
    a <- terms[1]
    b <- terms[[1]]
    entropies <- sapply(terms[[i]], function(x) {entropy(x,i,data, class)})
    #browser()
    E[[i]] <- entropies;
    m[i] <- entropies;
    #browser()
  }
  return(E)
}
#entropia
entropy <- function(term, col, data, class) {
  #print("term")
  #print(term)
  #print("col")
  #print(col)
  cases <- data[data[col]==term,class]
  #print("cases")
  #print(cases)
  freqs <- table(cases)/length(cases)
  #print("freqs")
  #print(freqs)
  entropy <- entropy.empirical(freqs, unit="log2");
  #browser()
  return(entropy)
}
#wnioskowanie klas danych na podstawie modelu
predict.antminer <- function(model, data) {
  discoveredRules <- model[[1]]
  defaultClass <- model[[2]]
  test<-apply(data,1, function(x) {
    result <- sapply(discoveredRules, function(y) {isCoveredByRule(y,x)})
    coveredRules<-which(result == TRUE)
    if(length(coveredRules)!=0) {
      id<-coveredRules[1]
      rule <- discoveredRules[[id]][[1]];
      class <- tail(rule, 1)
      x["class"]<-class
    } else {
      x["class"]<-defaultClass
    }
    return(x)
  })
  n<-ncol(data)
  names <- names(data)
  names<-append(names,"class")
  df<-NULL;
  for(i in 1:(n+1)) {
    if(i == 1) {
      df<-data.frame(test[seq(i,length(test),n+1)])
    } else {
      df[i]<-test[seq(i,length(test),n+1)]
    }
  }
  names(df)<-names
  return(df)
}
#sprawdza czy przypadek jest pokrywane przez regule
isCoveredByRule <- function(rule, case) {
  al <- rule[[2]]
  rule <- rule[[1]]
  n<-length(rule)
  for(i in 1:(n-1)) {
    id<-al[i]
    if(!(case[id] == rule[i])) {
      return(FALSE)
    }
  }
  return(TRUE)
}
