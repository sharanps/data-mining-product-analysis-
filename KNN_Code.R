require(magrittr)
require(data.table)
require(methods)
require(xgboost)
require(DiagrammeR)
require(Ckmeans.1d.dp)
require(caret)
require(class)

setwd("/Users/sharan/Desktop/ITM/Mining/Project")
train <- fread('train.csv', header = T, stringsAsFactors = F)

train.ind <- sample(2, 
                    nrow(train),
                    replace = T,
                    prob = c(0.6,0.4))

data.train <- train[train.ind==1,]
data.test <- train[train.ind==2,]

data.test[, id:= NULL]
data.train[, id:= NULL]

train.lastCol<- names(data.train)[ncol(data.train)]
test.lastCol<- names(data.test)[ncol(data.test)]

train.y <- data.train[, train.lastCol, with = F][[1]] 
test.y <- data.test[, test.lastCol, with = F][[1]] 

data.train[, train.lastCol:=NULL, with = F]
data.test[, test.lastCol:=NULL, with = F]

otto_pred = knn(train = data.train, test = data.test, cl= train.y, k=5)

confusionMatrix(data = test.y, reference = otto_pred, positive = "yes")
