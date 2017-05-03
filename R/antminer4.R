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
antminer4 <- function(trainingSet,class, maxUncoveredCases, NumberOfAnts, NumberOfRulesConverge, MinCasesPerRule) {
  #zamien data frame na data table
  trainingSet<-as.data.table(trainingSet)
  #przenies kolumne z atrybutami decyzyjnymi na sam koniec data table
  setcolorder(trainingSet, c(setdiff(names(trainingSet), class), class))
  #liczba klas atrybutu decyzyjnego
  nr_of_class<-nrow(unique(trainingSet[,class, with=FALSE]))

  #####wyznacz wszystkie termy (pary atrybut-wartosc)
  terms <- getTerms(trainingSet[,!class, with=FALSE])
  #####koniec
  initialPheromone <- 1/length(unlist(terms))
  nr_of_columns<-length(terms)

  #wyznacz entropie dla kazdego termu
  entropies<-computeEntropy(terms, trainingSet, class)

  #na poczatku lista regul jest pusta
  discoveredRules <- list()
  while(nrow(trainingSet) > maxUncoveredCases) {
    print('mrowka numer')
    print(nrow(trainingSet))
    #numer mrowki
    i <- 1;
    #indeks testu zbieznosci
    j <- 1;

    #lista utworzonych regul, na poczatku pusta
    rules<-list()

    #wektor jakosci regul, zeby wiedziec ktora regula jest najlepsze
    #sposrod wszystkich stworzonych
    rules_qualities<-list()

    #inicjalizacja sciezek ta sama iloscia feromonu
    pheromones <- lapply(terms, function(x) {sapply(x, function(y) {namedPheromone(y,initialPheromone)})})

    columnNames<-names(trainingSet)
    #powtarzamy az wykorzystamy wszystkie mrowki
    #albo gdy przekroczymy indeks zbierznosci
    while( (i<NumberOfAnts) & (j<NumberOfRulesConverge) ) {
      print("bla")
      print(i)
      #regula to lista skladajaca sie z dwoch list
      #pierwsza lista to zbior termow tworzacych regule
      #druga lista przechowuje atrybuty (w kolejnosci) wykorzystane w regule

      #####budowa reguly##########################################
      trainingSet2<-trainingSet
      #liczba atrybutow
      maxAttributes <- length(terms)

      #budowana regula
      #pusta na poczatku
      #pierwszy element to vektor termow reguly
      #drugi element to vektor numerow atrybutow, aby wiedziec o kolejnosci atrybutow w regule
      #powie nam ktory atrybut byl dodany kiedy tylko
      #czy byl w ogole wykorzystany
      rule <- list( character(maxAttributes), numeric(maxAttributes) )
      #informacja o tym ktory atrybut zostal juz uzyty do stworzenia reguly
      #vector sklada sie z tylu elementow ile jest atrybutow
      #element moze przyjac 0 co oznacza ze atrybut zostal juz wykorzystany
      #element moze przyjac 1 co oznacza ze atrybut nie zostal jeszcze wykorzystany
      #posortowane od pierwszego atrybutu do ostatniego
      #na poczatku zaden atrybut nie zostal wykorzystany (regula jest pusta)
      rule.used_attributes <- rep(1, maxAttributes)
      sum.used_attributes<-sum(rule.used_attributes)

      isMinCasesPerRule<-TRUE
      addedTermIndex<-1
      while(sum.used_attributes > 0 & isMinCasesPerRule) {
        #######compute eta####################################
        etas <- lapply(entropies ,function(x) {sapply(x, function(entropy) {eta(nr_of_class, x, entropy, rule.used_attributes)})})
        #######koniec compute eta
        #######compute probabilities##########################
        probabilities <- mapply(function(x, y) {mapply(function(eta, pheromone) {(eta*pheromone)/(sum.used_attributes*sum(x*y))}, x, y)}, etas, pheromones)
        #######koniec compute probabilities
        #wylosowane termy na podstawie prawdopodobienstwa
        unlistTerms <- unlist(removeUsedTerms(terms, rule.used_attributes))
        size <- length(unlistTerms)
        drawnTerms <- sample(unlistTerms, size, FALSE, unlist(removeUsedTerms(probabilities, rule.used_attributes)))
        isMinCasesPerRule<-FALSE
        for(q in 1:size) {
          #wybierz z wylosowanych term'ow term ktory nie nalezy do
          #atrybutow juz wybranych
          attribute_id <- getAttributeId(terms, drawnTerms[q])

          coveredCases<-trainingSet2[get(columnNames[attribute_id]) == drawnTerms[q]]

          #jesli tymczasowa regula spelnia wymagania liczby pokrytych przypadkow
          #to powiekszamy regule o wybrany term, przerywamy petle wybierania
          #wylosowanych termow
          if(nrow(coveredCases) >= MinCasesPerRule) {
            rule[[1]][addedTermIndex]<-drawnTerms[q]
            rule.used_attributes[attribute_id]<-0
            sum.used_attributes<-sum(rule.used_attributes)
            rule[[2]][addedTermIndex]<-attribute_id
            isMinCasesPerRule<-TRUE
            trainingSet2<-coveredCases
            addedTermIndex<-addedTermIndex+1
            break
          }
        }
        #moze sie zdarzyc ze po wyjsciu z petli for dodajacej termy
        #nie zostanie dodany zaden term (bo nie spelni warunku pokrycia
        #wystarczajacej liczby przypadkow)
      }

      #nie udalo sie dodac termu, za malo pokrytych przypadkow
      if(addedTermIndex == 1) {
        return (NULL)
      }

      rule[[1]][addedTermIndex]<-majorClass2(trainingSet2, class)
      rule[[1]]<-rule[[1]][rule[[1]] != ""]
      rule[[2]][addedTermIndex]<-maxAttributes+1
      rule[[2]]<-rule[[2]][rule[[2]] != 0]
      trainingSet2<-NULL
      #####koniec budowy reguly

      print("zbudowana regula")
      #jesli regula jest null to znaczy ze nie spelnila warunku MinCasesPerRule
      #w takim przypadku juz wiecej regul nie powstanie
      if(is.null(rule)) {
        break
      }

      ########przycinanie reguly##############################
      isBetterQuality <- TRUE;
      bestQuality <- quality(rule, trainingSet, class, columnNames)
      n<-length(rule[[2]])
      while( n > 2 & isBetterQuality) {
        qualities<-sapply(1:(n-1), function(k) {quality(removeTerm(rule,k), trainingSet, class, columnNames)})
        id<- which.max(qualities)
        maxquality <- qualities[id]
        if(maxquality > bestQuality) {
          rule[[1]]<-rule[[1]][-id]
          rule[[2]]<-rule[[2]][-id]
          bestQuality <- maxquality
          n<-length(rule[[2]])
        } else {
          isBetterQuality <- FALSE;
        }

      }


      rule[[1]][n]<-majorClass(coveredCases(rule[[1]], rule[[2]], trainingSet, columnNames), class, trainingSet)
      ########koniec przycinanie reguly

      #########zwiekszamy feromon####################
      for(q in 1:(n-1)) {
        attributeId<-rule[[2]][q]
        term<-rule[[1]][q]
        termid<-which(names(pheromones[[attributeId]])==term)
        pheromones[[attributeId]][termid]<-pheromones[[attributeId]][termid] + (pheromones[[attributeId]][termid]*bestQuality)
      }
      #########koniec zwieksz feromon
      #########zmniejsz feromon######################
      pheromones_sum<-sum(sapply(pheromones, function(x) {sum(x)}))
      pheromones<-lapply(pheromones, function(x) {sapply(x, function(pheromone){pheromone/pheromones_sum})})
      #########koniec zmniejsz feromon

      #Lepiej zmianic na wykrywanie czy stworzyl jakas nowa regule
      #w n poprzednich krokach niz to
      if(isEqualRule(rule, rules)) {
        j <- j+1;
      } else {
        j <- 1;
        size_rules<-length(rules)+1
        rules[[size_rules]]<-rule
        rules_qualities[[size_rules]]<-bestQuality
      }
      i<-i+1;
    }

    #jesli lista regul jest pusta to znaczy ze algorytm nie potrafil podczas
    #NumberOfAnts prób stworzyc reguly
    #dzieje sie tak zwykle gdy liczba przypadkow jest zbyt mala dla pewnych parametrow
    #algorytmu, w takim przypadku nalezy zakonczyc algorytm
    if(length(rules)>0) {
      max_quality_id<-which.max(rules_qualities)
      bestRule <- rules[[max_quality_id]]
      discoveredRules[[length(discoveredRules)+1]]<-bestRule
      trainingSet<-trainingSet[-coveredCases(bestRule[[1]], bestRule[[2]], trainingSet, columnNames)]
    } else {
      print("przerywam algorytm")
      break;
    }
  }
  defaultClass <- majorClass2(trainingSet, class)
  model <- list(discoveredRules, defaultClass)
  class(model)<-"antminer"
  return(model)
}

#zwraca wszystkie termy na podstawie danych treningowych
#dane treningowe musza byc bez kolumny z atrybutem decyzyjnym
#bo w innym razie zwrocilby takze termy w postaci atrybutow decyzyjnych
getTerms<-function(trainingSet) {
  #zwraca unikalne wartości z każdej kolumny danych treningowych
  #2 oznacza ze operuje na kolumnach, 1 by oznaczala ze na wierszach
  terms<-apply(trainingSet, 2, unique)
  #lapply(terms, function(x) {sapply(x, function(term) {namedTerm(term, x)})})
  #lapply(terms, function(x) {sapply(x, function(term) {term}, USE.NAMES = FALSE)})
  lapply(1:length(terms), function(x) {lapply(terms[[x]], function(term) {namedTerm(term, terms[x])})})
}

namedTerm<-function(term, terms) {
  names(term)<-names(terms)
  term
}

#Zwraca obliczona entropie dla kazdej pary atrybut-wartosc
#pierwszy terms oznacza liste list wszystkich atrybutow
#drugie terms oznacza liste atrybutu (jednego), czyli wartosci tego atrybutu
computeEntropy <- function(terms, data, class) {
  mapply(function(terms, index) {sapply(terms, function(x) {entropy(x, index, data, class)})}, terms, 1:length(terms))
}

#oblicza entropie danego terma, czyli entropie pary atrybut-wartosc
entropy <- function(term, col, data, class) {
  cases <- data[get(names(data)[col])==term, class, with=FALSE]
  #data[which(data[col]==term),class]
  freqs <- table(cases)/nrow(cases)
  entropy.empirical(freqs, unit="log2");
}

#poczatkowy feromon to 1/liczba_wszystkich_wartosci (liczba wszystkich termow)
#liczba odwrotnie proporcjonalna do liczby wartości wszystkich atrybutow
initPheromone <- function(terms) {
  initialPheromone <- 1/length(unlist(terms))
  #lapply(terms, function(x) {sapply(x, function(y) {initialPheromone})})
  lapply(terms, function(x) {sapply(x, function(y) {namedPheromone(y,initialPheromone)})})

  #lapply(1:length(terms), function(i) {lapply(terms[[i]], function(y) {initialPheromone})})
}

namedPheromone<-function(term, pheromone) {
  names(pheromone)<-term
  return (pheromone)
}

build_rule <- function(trainingSet, terms, MinCasesPerRule, class, nr_of_class, entropies, pheromones) {
  #liczba atrybutow
  maxAttributes <- length(terms)

  #budowana regula
  #pusta na poczatku
  rule <- character(maxAttributes)#NULL

  #informacja o tym ktory atrybut zostal juz uzyty do stworzenia reguly
  #vector sklada sie z tylu elementow ile jest atrybutow
  #element moze przyjac 0 co oznacza ze atrybut zostal juz wykorzystany
  #element moze przyjac 1 co oznacza ze atrybut nie zostal jeszcze wykorzystany
  #posortowane od pierwszego atrybutu do ostatniego
  #na poczatku zaden atrybut nie zostal wykorzystany (regula jest pusta)
  rule.used_attributes <- rep(1, maxAttributes)
  #vektor numerow atrybutow, aby wiedziec o kolejnosci atrybutow w regule
  #rule.used_attributes nie powie nam ktory atrybut byl dodany kiedy tylko
  #czy byl w ogole wykorzystany
  rule.attributes <- numeric(maxAttributes)#NULL

  isMinCasesPerRule<-TRUE
  j<-1
  while(sum(rule.used_attributes) > 0 & isMinCasesPerRule) {
    etas <- computeEta(nr_of_class, entropies, rule.used_attributes);
    probabilities <- computeProbabilities(etas, pheromones, rule.used_attributes);
    #wylosowane termy na podstawie prawdopodobienstwa
    unlistTerms <- unlist(removeUsedTerms(terms, rule.used_attributes))
    size <- length(unlistTerms)
    drawnTerms <- sample(unlistTerms, size, FALSE, unlist(removeUsedTerms(probabilities, rule.used_attributes)))
    isMinCasesPerRule<-FALSE
    for(i in 1:size) {
      #wybierz z wylosowanych term'ow term ktory nie nalezy do
      #atrybutow juz wybranych
      attribute_id <- getAttributeId(terms, drawnTerms[i])
      #sprawdzamy czy atrybut juz byl wykorzystany
      #jestli tak to wybieramy kolejny term z listy wylosowanych term'ow
      #if(rule.used_attributes[attribute_id] == 0) {
      #  next
      #}
      #tempRule<-append(rule, drawnTerms[i])
      #tempRule.used_attributes<-rule.used_attributes
      #tempRule.used_attributes[attribute_id]<-0
      #tempRule.attributes <- append(rule.attributes, attribute_id)

      #coveredCases<-coveredCases(tempRule, tempRule.attributes, trainingSet)trainingSet[paste(names(trainingSet[rule[[2]][-n]]), rule[[1]][-n], sep="==", collapse='&')]
      coveredCases<-trainingSet[get(names(trainingSet)[attribute_id]) == drawnTerms[i]]

      #jesli tymczasowa regula spelnia wymagania liczby pokrytych przypadkow
      #to powiekszamy regule o wybrany term, przerywamy petle wybierania
      #wylosowanych termow
      if(nrow(coveredCases) >= MinCasesPerRule) {
        rule[j]<-drawnTerms[i]#rule<-append(rule, drawnTerms[i])
        rule.used_attributes[attribute_id]<-0
        rule.attributes[j]<-attribute_id#rule.attributes <- append(rule.attributes, attribute_id)
        isMinCasesPerRule<-TRUE
        trainingSet<-coveredCases
        j<-j+1
        break
      }
    }
    #moze sie zdarzyc ze po wyjsciu z petli for dodajacej termy
    #nie zostanie dodany zaden term (bo nie spelni warunku pokrycia
    #wystarczajacej liczby przypadkow)
  }

  # if(is.null(rule)){
  #   return (rule)
  # }
  #nie udalo sie dodac termu, za malo pokrytych przypadkow
  if(j == 1) {
    return (NULL)
  }

  rule[j]<-majorClass2(trainingSet, class)#rule<-append(rule, majorClass2(coveredCases, class))
  rule<-rule[rule != ""]
  rule.attributes[j]<-maxAttributes+1#rule.attributes<-append(rule.attributes, maxAttributes+1)
  rule.attributes<-rule.attributes[rule.attributes != 0]
  list(rule, rule.attributes)
}

coveredCasesWithDrawnTerm<-function(drawnTerm, terms, rule, rule.attributes, trainingSet) {
  attribute_id <- getAttributeId(terms, drawnTerm)

  rule<-append(rule, drawnTerm)
  rule.attributes <- append(rule.attributes, attribute_id)

  result<-length(coveredCases(rule, rule.attributes, trainingSet))
  names(result)<-attribute_id

  return (result)
}

removeUsedTerms<-function(terms, used_attributes) {
  terms[which(used_attributes==0)]<-NULL
  return (terms)
}


#oblicza znormalizowana entropie dla wszystkich term'ow
#eta wszystkich term'ow sumuje sie do 1
computeEta <- function(nr_of_class, entropies, used_attributes) {
  lapply(entropies ,function(x) {sapply(x, function(entropy) {eta(nr_of_class, x, entropy, used_attributes)})})
}

#liczy znormalizowana entropie dla pary atrybut-wartosc (term)
#mianownik (dominator) tej funkcji jest staly dla wszystkich termow
#mozna by go liczyc tylko raz i podawac do funkcji???
eta <- function(nr_of_class, entropies, entropy, used_attributes) {
  counter <- log2(nr_of_class)-entropy;
  dominator <- sum(used_attributes)*sum((log2(nr_of_class)-unlist(entropies)))
  counter/dominator;
}

#liczy prawdopodobienstwo wszystkich term'ow na podstawie
#znormalizowanych entropii i feromonu
computeProbabilities <- function(etas, pheromones, used_attributes) {
  mapply(function(x, y) {mapply(function(eta, pheromone) {(eta*pheromone)/(sum(used_attributes)*sum(x*y))}, x, y)}, etas, pheromones)
}

#zwraca id atrybutu ktorego wartoscia jest dany term
getAttributeId <- function(terms, term) {
  #min(which(sapply(terms, function(x) {is.element(term, x)}) == TRUE))
  which(sapply(sapply(terms, function(x) {sapply(x, function(y) {checkEqualTerm(term, y)})}), function(row) {is.element(TRUE, row)}) == TRUE)
}

#sprawdza czy termy ma taka sama wartosc i nazwe
checkEqualTerm<-function(term1, term2) {
  term1 == term2 & names(term1) == names(term2)
}

#pokryte przez regule przypadki
#zwraca liste wierszy z pokrytymi przypadkami
coveredCases <- function(rule, rule.attributes, trainingSet, columnNames) {
  Reduce(intersect, mapply(function(attribute, value) {trainingSet[get(columnNames[attribute]) == value, which=TRUE]}, rule.attributes, rule))
}

coveredCases2 <- function(drawnTerm, attribute_id, trainingSet) {
  trainingSet[get(columnNames[attribute_id]) == drawnTerm]
}

#Zwraca klase dominujaca w pokrytych przypadkach
majorClass <- function(coveredCases, class, trainingSet) {
  tt <- table(trainingSet[coveredCases, class, with=FALSE])
  major <- names(tt[tt==max(tt)])
  return(major[1])
}

#zwraca klase domunujaca w danym zbiorze danych treningowych
majorClass2<-function(trainingSet, class) {
  names(which.max(table(trainingSet[,class, with=FALSE])))
}

#Przycina regule
prune <- function(rule, trainingSet, class) {
  isBetterQuality <- TRUE;
  bestQuality <- quality(rule, trainingSet, class)
  n<-length(rule[[2]])
  while( n > 2 & isBetterQuality) {
    qualities<-sapply(1:(n-1), function(i) {quality(removeTerm(rule,i), trainingSet, class)})
    id<- which.max(qualities)
    maxquality <- qualities[id]
    if(maxquality > bestQuality) {
      rule[[1]]<-rule[[1]][-id]
      rule[[2]]<-rule[[2]][-id]
      bestQuality <- maxquality
    } else {
      isBetterQuality <- FALSE;
    }
    n<-length(rule[[2]])
  }

  rule[[1]][n]<-majorClass(coveredCases(rule[[1]], rule[[2]], trainingSet), class, trainingSet)
  return (rule)
}

removeTerm<-function(rule, index) {
  rule[[1]]<-rule[[1]][-index]
  rule[[2]]<-rule[[2]][-index]
  rule
}

getFilter<-function(rule) {
  mapply(function(x,y) {})
}

#mierzy jakosc reguly
quality <- function(rule, trainingSet, class, columnNames) {
  n<-length(rule[[1]])
  predictedClass<-rule[[1]][n]
  coveredIndex<-Reduce(intersect, mapply(function(x,y) {trainingSet[get(columnNames[y]) == x, which=TRUE]}, rule[[1]][-n], rule[[2]][-n]))
  #pokryte przypadki przez regule
  cases<-trainingSet[coveredIndex]
  tp<-nrow(cases[get(class) == predictedClass])
  fp<-nrow(cases[get(class) != predictedClass])
  #nie pokryte przypadki przez regule
  cases<-trainingSet[-coveredIndex]
  fn<-nrow(cases[get(class) == predictedClass])
  tn<-nrow(cases[get(class) != predictedClass])

  quality <- (tp/(tp+fn))*(tn/(fp+tn))
  #wzor na jakosc jest nie najlepszy bo mozna uzyskac dzielenie przez 0
  #jak spada nam liczba przypadkow treningowych to moze sie zdarzyc
  #ze choc jedna z powyzszych 4 wartosci bedzie 0
  #w sumie to nie wiem co z tym robic, implementuje nie swoje zalozenia
  #dziwne ze nie ujeli tego w artykule
  #na razie jakosc ustawie na 0
  #jakosc wplywa na zmiane feromonu wiec jest to dosc wazne
  #Poprawka
  #TP nie moze byc zero
  #TN moze byc zero
  #powinien byc tylko pierwszy czlon, drugi z TN do usuniecia w mojej wersji
  #mysle tez ze nie da sie uzyskac na koniec innej klasy
  #bo caly czas badamy jakosc reguly na podstawie klasy ktora wywnioskowalismy
  #nie wybierzemy gorszej jakosci czyli reguly w ktorej inna klasa bedzie domuniujaca!!
  if(is.nan(quality)) {
    return(0)
  }

  return(quality)
}


#zwieksza feromon dla term'ow ktore znalazly sie w regule
increasePheromone <- function(rule, pheromones, quality) {
  n<-length(rule[[2]])
  for(i in 1:(n-1)) {
    attributeId<-rule[[2]][i]
    term<-rule[[1]][i]
    pheromones[[attributeId]][which(names(pheromones[[attributeId]])==term)]<-updated_pheromone(pheromones, attributeId, term, quality)
  }
  #mapply(function(term, attributeId) {pheromones[[attributeId]][which(names(pheromones[[attributeId]])==term)]=updated_pheromone(pheromones, attributeId, term, quality)}, rule[[1]][-n], rule[[2]][-n])
  return (pheromones)
}

updated_pheromone<-function(pheromones, attributeId, term, quality) {
  pheromone<-pheromones[[attributeId]][which(names(pheromones[[attributeId]])==term)]
  pheromone+pheromone*quality
}

#zmiejsza feromon dla wszystkich term'ow
#symuluje wyparowywanie feromonu
decreasePheromone <- function(pheromones) {
  n<-length(pheromones)
  sum<-sum(sapply(pheromones, function(x) {sum(x)}))
  lapply(pheromones, function(x) {sapply(x, function(pheromone){pheromone/sum})})
}

#sprawdza czy reguly sa takie same
isEqualRule <- function(rule1, rules) {
  if(length(rules)==0) {
    return (FALSE)
  }
  rule2<-rules[[length(rules)]]
  #funkcja all sprawdza czy wszystkie wartosci sa TRUE, jestli tak to zwraca TRUE
  all(mapply(function(term, attributeId) {is.element(term, rule2[[1]]) & is.element(attributeId, rule2[[2]])}, rule1[[1]], rule1[[2]]))
}

#przypadki ze zbioru ktorych regula nie pokrywa
#zastepuje dotychczasowy trainigSet eliminujac przypadki pokryte przez regule
uncoveredCases <- function(rule, trainingSet, class) {
  trainingSet[-coveredCases(rule[[1]], rule[[2]], trainingSet)]
}

#wnioskowanie klas danych na podstawie modelu
predict.antminer3 <- function(model, data) {
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
