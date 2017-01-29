library(randomForest)
require(class)
require(xgboost)


# Random Forest
set.seed(1234)
train<-read.csv('train.csv',header=TRUE)
drop <- c("id")
traind=train[,!(names(train) %in% drop)]
train.ind <- sample(2,
+ nrow(traind),
+ replace = T,
+ prob = c(0.6,0.4))
data.train <- traind[train.ind==1,]
data.test <- traind[train.ind==2,]
rf <- randomForest(data.train[,c(-1,-94)], as.factor(data.train$target), ntree=25, importance=TRUE,do.trace = TRUE)
rf_pred = predict(rf, data.test, type = "prob")

#KNN Prediction
train.y <- data.train[, train.lastCol]
test.y <- data.test[, test.lastCol]
data.test.knn = data.test[,1:93]
data.train.knn = data.train[,1:93]
knn_pred = knn(train = data.train.knn, test = data.test.knn, cl = train.y, k=5, prob = TRUE)

#ADABoost
adaboost<-boosting(target~., data=data.train, boos=TRUE, mfinal=20)
adapred <- predict.boosting(adaboost,newdata = data.test,newmfinal = length(adaboost$trees))
ada_pred = adapred$prob

