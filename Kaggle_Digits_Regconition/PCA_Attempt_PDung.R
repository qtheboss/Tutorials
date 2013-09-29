# Digital Recognizer with Neural network
# 23/9/2013
# 

library(neuralnet)
library(psych)
library(labdsv)
library(FactoMineR)
library(ade4)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- train[1:5000,1]
traininginput <- train[1:5000,-1]

# try with PCA

pca3  <- PCA(traininginput, ncp =5, graph=FALSE)

# convert data to matrix
traininginput <- as.matrix(traininginput)
eigvec <- as.matrix(pca3$eig)

trainingd <- traininginput %*% eigvec
colnames(trainingd) <- c("e", "p1","p2")
trainingdata <- cbind(trainingd, labels)

# train model with neural net

ptm <- proc.time()
net.digit <- neuralnet(labels~e+p1+p2, trainingdata, hidden = 50, threshold = 0.01)
proc.time()


testdata <- train[5001: 6000, -1]
testlabels <- train[5001: 6000, 1]
# PCA with data test
pca4  <- PCA(test, ncp = 5, graph=FALSE)

# convert data test to matrix
test1 <-as.matrix(testdata) 
eigvec2 <- as.matrix(pca4$eig)
test2 <-test1 %*% eigvec2
colnames(test2) <- c("e", "p1","p2")

# results test
net.results <- compute(net.digit,test2 )

ls(net.results)
write(floor(net.results$net.result+0.5), file="Neuralnet_benchmark.csv", ncolumns=1)

cleanoutput <- cbind( testlabels, 
                      as.data.frame(floor(net.results$net.result+0.5)))
colnames(cleanoutput) <- c("Expected Output", "Neural Net Output")

print (cleanoutput)
# checking accuracy


#c1 <- 0
#for (i in 1:100)
#{
# a1 <- floor(net.results$net.result[i]+0.5) - labelstest[i]

#if( a1!=0) 
#{ c1 <- c1}
#else
#  {c1 <- c1 + 1}

#}
#print(c1)

