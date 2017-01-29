library(ggplot2)
library(randomForest)
library(readr)
library(e1071)
library(caret)

set.seed(1)

# Load the data
train <- read_csv("train.csv")

# Splitting the data 
train.ind <- sample(2,nrow(train),replace = T,prob = c(0.6,0.4))
train.train <- train[train.ind==1,]
train.validate <- train[train.ind==2,]

rf <- randomForest(train.train[,c(-1,-95)], as.factor(train.train$target), ntree=25, importance=TRUE,do.trace = TRUE)

# Get the predictions on training data set
train.train$predicted.value = predict(rf,train.train)

# Get the predictions on validation data set
train.validate$predicted.value = predict(rf,train.validate)

# Confusion Matrix for Validating Data
confusionMatrix(data = train.validate$predicted.value,reference = train.validate$target,positive = "yes")

# Confusion Matrix for Training Data
confusionMatrix(data = train.train$predicted.value,reference = train.train$target,positive = "yes")

# Write output files to csv
write.csv(train.train, file = "train_train.csv")
write.csv(train.validate, file = "train_validate.csv")

# Run algorithm again to get the Feature Importance
rf <- randomForest(train[,c(-1,-95)], as.factor(train$target), ntree=25, importance=TRUE,do.trace = TRUE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p, height=20, width=8, units="in")

#Evaluate variable importance
importance(rf)
varImpPlot(rf)


