dotest<-function(i) {
  for(a in 1:i) {
    print("fdskfs")
    for(b in 1:i) {
      print("fdsf")
    }
  }
  print("end")
}

run<-function() {
  system.time(antminer3(mushroom, "edibility"))
}
