library(caret)
library(caTools)
library(ggplot2)
library(randomForest)


#read the dataset
dataset <- read.csv(file = "cardio.csv", header = T, sep = ",")

#factor target variable MSP
dataset$NSP <- as.factor(dataset$NSP)
str(dataset)
# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$NSP, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
str(training_set)

#run the forest
set.seed(345)
forest <- randomForest(NSP~.,
                       data = training_set,
                       ntree = 350,
                       replace = T,
                       sampsize = 150)

p1 <- predict(forest, training_set[,-22])
cm1 <- table (p1, training_set$NSP)
cm1
p2 <- predict(forest, test_set[,-22])
cm2 <- table(p2, test_set$NSP)
cm2

accuracy1 <- (sum(diag(cm1))/nrow(training_set)) *100
accuracy2 <- (sum(diag(cm2))/nrow(test_set)) *100

accuracy1
accuracy2

plot(forest)


# In the plot black solid line for overall OOB error and the colour lines, one for each class' error.
# Tuning mtry
library(caret)
str(training_set)
tuneRF(training_set[ ,-22], training_set$NSP,
      stepFactor=0.5,
      plot = TRUE,
      ntreeTry  = 400,
      trace = TRUE,
      improve = 0.05)

rf <- randomForest(NSP~.,data = training_set,
                   ntreeTry = 400,
                   mtry=2,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)

p1 <- predict(rf, training_set[,-22])
cm1 <- table (p1, training_set$NSP)
cm1
p2 <- predict(rf, test_set[,-22])
cm2 <- table(p2, test_set$NSP)
cm2

# Number of nodes for trees
hist(treesize(rf),
     main = "No. of nodes for trees",
     col = "green")

varImpPlot(rf)
importance(rf)
varUsed(rf)


