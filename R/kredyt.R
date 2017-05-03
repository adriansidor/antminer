createKredyt<-function() {
  wiek<-c("(0,30]","(30,40]","(30,40]","(40,60]","(0,30]","(30,40]","(30,40]","(40,60]","(40,60]","(30,40]","(0,30]","(40,60]","(40,60]","(40,60]")
  status<-c("kawaler","zonaty","rozwiedziony","zonaty","zonaty","rozwiedziony","kawaler","zonaty","zonaty","rozwiedziony","kawaler","rozwiedziony","zonaty","rozwiedziony")
  dochod<-c("niski","sredni","wysoki","sredni","niski","wysoki","niski","wysoki","sredni","niski","wysoki","sredni","sredni","wysoki")
  dzieci<-c(0,1,2,2,1,0,0,2,1,2,0,4,1,2)
  ryzyko<-c("wysokie","niskie","niskie","niskie","wysokie","wysokie","wysokie","niskie","niskie","wysokie","niskie","wysokie","niskie","niskie")
  data.frame(wiek,status,dochod,dzieci,ryzyko)
}
