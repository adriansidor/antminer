library(antminer)
install.packages("entropy", repos = "http://cran.us.r-project.org");
library(entropy);
mushroom <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = FALSE)
#view(mushroom)
names(mushroom) <- c("edibility", "cap-shape", "cap_surface", "cap_color", "bruises?", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
mushroom <- mushroom[-17] #usuwanie atrybutu z jednym przypadkiem
sam<-sample(2,nrow(mushroom),replace = TRUE, prob=c(0.7,0.3))
trainset_mushroom<-mushroom[sam==1,]
testset_mushroom<-mushroom[sam==2,]
testset_mushroom<-testset_mushroom
model_ant_mushroom <- antminer2(trainset_mushroom, "edibility", 10, 10, 5, 2)
prediction_mushroom_ant <- predict(model_ant_mushroom, testset_mushroom[,-1])
tab_mushroom_ant <- table(pred = prediction_mushroom_ant, true = testset_mushroom[,1])
table(testset_mushroom$edibility,prediction_mushroom_ant$class)
mean(testset_mushroom$edibility == prediction_mushroom_ant$class)
tk_mushroom_ant <- (tab_mushroom_ant[1,1]+tab_mushroom_ant[2,2])/nrow(testset_mushroom)
bk_mushroom_ant <- (tab_mushroom_ant[2,1]+tab_mushroom_ant[1,2])/nrow(testset_mushroom)
b<-mushroom[sam==2,]
prediction_mushroom_ant["realclass"]<-b[1]
check <- trainset_mushroom[trainset_mushroom[2]=="x",1]
test <- antminer(df2, "Ryzyko", 10, 10, 5, 2)
