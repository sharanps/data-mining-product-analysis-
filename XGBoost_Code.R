require(magrittr)
require(data.table)
require(methods)
require(xgboost)
require(DiagrammeR)
require(Ckmeans.1d.dp)

setwd("/Users/sharan/Desktop/ITM/Mining/Project")
train <- fread('train.csv', header = T, stringsAsFactors = F)

dim(train)train[1:6,1:5, with=F]
train[, id:= NULL]
train[1:6, ncol(train), with = F]
nameLastCol <- names(train)[ncol(train)]
#Converting label to number
y <- train[, nameLastCol, with = F][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}
y[1:5]

train[, nameLastCol:=NULL, with = F]
trainMatrix <- train[,lapply(.SD,as.numeric)] %>% as.matrix
numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
                nfold = cv.nfold, nrounds = cv.nround)

nround = 50
bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround)

# Get the feature real names
names <- dimnames(trainMatrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

#Tree Plot
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)