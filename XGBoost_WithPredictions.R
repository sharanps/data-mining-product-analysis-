require(magrittr)
require(data.table)
require(methods)
require(xgboost)
require(DiagrammeR)
require(Ckmeans.1d.dp)
require(caret)

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

train.y <- data.train[, train.lastCol, with = F][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}
test.y <- data.test[, test.lastCol, with = F][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}

data.train[, train.lastCol:=NULL, with = F]
data.test[, test.lastCol:=NULL, with = F]

trainMatrix <- data.train[,lapply(.SD,as.numeric)] %>% as.matrix

testMatrix <- data.test[,lapply(.SD,as.numeric)] %>% as.matrix

numberOfClasses <- max(train.y) + 1

param <- list("objective" = "multi:softmax",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

nround = 50
bst = xgboost(param=param, data = trainMatrix, label = train.y, nrounds=nround)

xgpredict <- predict(bst, testMatrix)

confusionMatrix(data = test.y, reference = xgpredict, positive = "yes")
