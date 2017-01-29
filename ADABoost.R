library(adabag);

adadata<-read.csv('train.csv',header=TRUE)
drop <- c("id")
adata=adadata[,!(names(adadata) %in% drop)]

adaboost<-boosting(target~., data=adata, boos=TRUE, mfinal=20)

adaboost$importance
errorevol(adaboost, adata)
importanceplot(adaboost)



train.ind <- sample(2, 
                    nrow(adata),
                    replace = T,
                    prob = c(0.6,0.4))

data.train <- adata[train.ind==1,]
data.test <- adata[train.ind==2,]


adaboost<-boosting(target~., data=data.train, boos=TRUE, mfinal=20)
adapred <- predict.boosting(adaboost,newdata = data.test,newmfinal = length(adaboost$trees))
adapred$confusion
adapred$error

                            