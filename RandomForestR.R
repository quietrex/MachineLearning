# This is About Random Forest - Model 3
# Please ensure to load kickDataset_4_Transformed.csv from R.Script - amlAssignment.R before running 
#                                                                                                   //    TP034717-RandomForestR.R         script.       //
setwd("D:/Master Semester 2/Sem 2")
kickDataset_4_Transformed = read.csv('kickDataset_4_Transformed.csv', header = T, stringsAsFactors = FALSE)

str(kickDataset_4_Transformed)
kickDataset_4_Transformed = kickDataset_4_Transformed[, -1]

selectedColumns_all <- c("IsBadBuy","Auction","VehYear","VehicleAge", "Color",
                         "Transmission","WheelType","VehOdo","Nationality",
                         "Size", "TopThreeAmericanName", 
                         "MMRAcquisitionAuctionAveragePrice", "MMRAcquisitionAuctionCleanPrice", 
                         "MMRAcquisitionRetailAveragePrice", "MMRAcquisitonRetailCleanPrice", 
                         "MMRCurrentAuctionAveragePrice", "MMRCurrentAuctionCleanPrice", 
                         "MMRCurrentRetailAveragePrice", "MMRCurrentRetailCleanPrice",
                         "BYRNO","VNZIP1", "VehBCost", "IsOnlineSale",
                         "WarrantyCost","SubModel_Type","SubModel_Door", 
                         "Model_WheelDrive", "Model_I4_Engine", "Model_I6_Engine", "Model_Cylinder") 

selectedColumns_importanceVar <- c("IsBadBuy","Auction","VehYear","VehicleAge", "Color",
                                   "Transmission","WheelType","VehOdo","Nationality",
                                   "Size", "TopThreeAmericanName", 
                                   "MMRAcquisitionAuctionCleanPrice", 
                                   "BYRNO","VNZIP1", "VehBCost", "IsOnlineSale",
                                   "WarrantyCost","SubModel_Type","SubModel_Door", 
                                   "Model_WheelDrive", "Model_I4_Engine", "Model_I6_Engine", "Model_Cylinder") 

library(plyr)
library(stringr)
library(caret)
library(caTools)
#library(Deducer)
library(ggplot2)
library(psych)  # pairs.panels
library(aod)    #wald.test
library(visreg)
library(ROSE)

kickDataset_4_Transformed$IsBadBuy <- factor(kickDataset_4_Transformed$IsBadBuy)
kickDataset_4_Transformed$Auction <- factor(kickDataset_4_Transformed$Auction)
kickDataset_4_Transformed$Color <- factor(kickDataset_4_Transformed$Color)
kickDataset_4_Transformed$Transmission <- factor(kickDataset_4_Transformed$Transmission)
kickDataset_4_Transformed$WheelType <- factor(kickDataset_4_Transformed$WheelType)
kickDataset_4_Transformed$Nationality <- factor(kickDataset_4_Transformed$Nationality)
kickDataset_4_Transformed$Size <- factor(kickDataset_4_Transformed$Size)
kickDataset_4_Transformed$TopThreeAmericanName <- factor(kickDataset_4_Transformed$TopThreeAmericanName)
kickDataset_4_Transformed$IsOnlineSale <- factor(kickDataset_4_Transformed$IsOnlineSale)
kickDataset_4_Transformed$SubModel_Type <- factor(kickDataset_4_Transformed$SubModel_Type)
kickDataset_4_Transformed$SubModel_Door <- factor(kickDataset_4_Transformed$SubModel_Door)
kickDataset_4_Transformed$Model_WheelDrive <- factor(kickDataset_4_Transformed$Model_WheelDrive)
kickDataset_4_Transformed$Model_Cylinder <- factor(kickDataset_4_Transformed$Model_Cylinder)
kickDataset_4_Transformed$Model_I4_Engine <- factor(kickDataset_4_Transformed$Model_I4_Engine)
kickDataset_4_Transformed$Model_I6_Engine <- factor(kickDataset_4_Transformed$Model_I6_Engine)

str(kickDataset_4_Transformed)

# Split dataset into training and 2 test sets by 0.7 split ratio~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(123)
splitting_Criteria = sample.split(kickDataset_4_Transformed$IsBadBuy, SplitRatio = 0.7)
training_set <- subset(kickDataset_4_Transformed, splitting_Criteria == TRUE)
testing_set <- subset(kickDataset_4_Transformed, splitting_Criteria == FALSE)

str(training_set)
str(testing_set)

# Sampling 1 - Original data
# kickDataset_4_Transformed 
table(training_set$IsBadBuy)
prop.table(table(training_set$IsBadBuy))

# Sampling 2 - Over Sampling
training_set_overSampling <- ovun.sample(IsBadBuy ~ ., 
                                         data = training_set, method = "over")$data 
table(training_set_overSampling$IsBadBuy)
prop.table(table(training_set_overSampling$IsBadBuy))

# Sampling 3 - Under sampling
training_set_underSampling <- ovun.sample(IsBadBuy ~ ., 
                                          data = training_set, method = "under", seed = 1)$data 
table(training_set_underSampling$IsBadBuy)
prop.table(table(training_set_underSampling$IsBadBuy))

# Sampling 4 - Both sampling (Combination of Over and under sampling)
training_set_bothSampling<- ovun.sample(IsBadBuy ~ ., 
                                        data = training_set, method = "both", p=0.5, seed = 1)$data 
table(training_set_bothSampling$IsBadBuy)
prop.table(table(training_set_bothSampling$IsBadBuy))

# Sampling 5 - Rose sampling
training_set_roseSampling <- ROSE(IsBadBuy ~ ., 
                                  data = training_set, seed = 1)$data
table(training_set_roseSampling$IsBadBuy)
prop.table(table(training_set_roseSampling$IsBadBuy))

# Each sampling will be run into experiment 1, 2, 3, 4, and 5, which experiment 1 will have normal sampling, experiment 2 will have random sampling, and etc.
# Splitting sequence will follows as above.
# Also, each sampling will be used repeatedly on fitting into each model.

####################################################################################################################################################### 1 training_set

# Before building the classifier and fitting into model, we will have to create empty list that store accuracy
# It will have to show the result in the list.

model15_all <- c()
model16 <- c()
model17 <- c()
model18 <- c()
model19 <- c()
model20 <- c()

####################################################################################################################################################### 6A

# Build RandomForest on imbalanced dataset with all variable selected (selectedColumns_all)
# Each variation will only be considered to run into selectedColumns_all or selectedColumns_importanceVar,
# which selectedColumns_importanceVar selected only 23 variables, while selectedColumns_all selected 30 variables.

table(training_set$IsBadBuy)
prop.table(table(training_set$IsBadBuy))

# Building the classifier from package randomForest.
library(randomForest)

randomForest_classifier = randomForest(x = training_set[complete.cases(training_set), (selectedColumns_all)][,-1],
                                       y = training_set$IsBadBuy )

randomForest_classifier

# Train the model on testing set
randomForest_pred = predict(randomForest_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_all)][,-1])
randomForest_pred

# Prediction result will be saved into model15_all.

cm_randomForest = table(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred)

model15_all <- c( confusionMatrix(cm_randomForest), roc$auc ) # Model will be saved into (model15_all)
accuracy_train <- sum( diag(cm_randomForest) ) / sum(cm_randomForest) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model15_all$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

####################################################################################################################################################### 6B

# Build RandomForest on imbalanced dataset with all variable selected (selectedColumns_importanceVar)
# Each variation will only be considered to run into selectedColumns_all or selectedColumns_importanceVar,
# which selectedColumns_importanceVar selected only 23 variables, while selectedColumns_all selected 30 variables.

# Building model on imbalanced target variable with selectedColumns_importanceVar
table(training_set$IsBadBuy)
prop.table(table(training_set$IsBadBuy))

# Building the classifier from package randomForest.
library(randomForest)

randomForest_classifier = randomForest(x = training_set[complete.cases(training_set), (selectedColumns_importanceVar)][,-1],
                                       y = training_set$IsBadBuy )

randomForest_classifier

# Train the model on testing set
randomForest_pred = predict(randomForest_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
randomForest_pred

# Prediction result will be saved into model16.

cm_randomForest = table(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred)

model16 <- c( confusionMatrix(cm_randomForest), roc$auc ) # Model will be saved into (model16)
accuracy_train <- sum( diag(cm_randomForest) ) / sum(cm_randomForest) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model16$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

####################################################################################################################################################### 6C

# Build RandomForest on oversampling with all variable selected (selectedColumns_importanceVar)

# Building model with oversampling alongside selectedColumns_importanceVar
table(training_set_overSampling$IsBadBuy)
prop.table(table(training_set_overSampling$IsBadBuy))

# Building the classifier from package randomForest.
library(randomForest)

randomForest_classifier = randomForest(x = training_set_overSampling[complete.cases(training_set_overSampling), (selectedColumns_importanceVar)][,-1],
                                       y = training_set_overSampling$IsBadBuy )

randomForest_classifier

# Train the model on testing set
randomForest_pred = predict(randomForest_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
randomForest_pred

# Prediction result will be saved into model17.

cm_randomForest = table(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred)

model17 <- c( confusionMatrix(cm_randomForest), roc$auc ) # Model will be saved into (model17)
accuracy_train <- sum( diag(cm_randomForest) ) / sum(cm_randomForest) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model17$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

####################################################################################################################################################### 6D

# Build RandomForest on undersampling with all variable selected (selectedColumns_importanceVar)

# Building model with undersampling alongside selectedColumns_importanceVar
table(training_set_underSampling$IsBadBuy)
prop.table(table(training_set_underSampling$IsBadBuy))

# Building the classifier from package randomForest.
library(randomForest)

randomForest_classifier = randomForest(x = training_set_underSampling[complete.cases(training_set_underSampling), (selectedColumns_importanceVar)][,-1],
                                       y = training_set_underSampling$IsBadBuy )

randomForest_classifier

# Train the model on testing set
randomForest_pred = predict(randomForest_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
randomForest_pred

# Prediction result will be saved into model18

cm_randomForest = table(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred)

model18 <- c( confusionMatrix(cm_randomForest), roc$auc ) # Model will be saved into (model18)
accuracy_train <- sum( diag(cm_randomForest) ) / sum(cm_randomForest) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model18$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

####################################################################################################################################################### 6E

# Build RandomForest on both sampling with all variable selected (selectedColumns_importanceVar)

# Building model with both sampling alongside selectedColumns_importanceVar
table(training_set_bothSampling$IsBadBuy)
prop.table(table(training_set_bothSampling$IsBadBuy))

# Building the classifier from package randomForest.
library(randomForest)

randomForest_classifier = randomForest(x = training_set_bothSampling[complete.cases(training_set_bothSampling), (selectedColumns_importanceVar)][,-1],
                                       y = training_set_bothSampling$IsBadBuy )

randomForest_classifier

# Train the model on testing set
randomForest_pred = predict(randomForest_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
randomForest_pred

# Prediction result will be saved into model19

cm_randomForest = table(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred)

model19 <- c( confusionMatrix(cm_randomForest), roc$auc ) # Model will be saved into (model19)
accuracy_train <- sum( diag(cm_randomForest) ) / sum(cm_randomForest) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model19$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

####################################################################################################################################################### 6F

# Build RandomForest on rose sampling with all variable selected (selectedColumns_importanceVar)

# Building model with rose sampling alongside selectedColumns_importanceVar
table(training_set_roseSampling$IsBadBuy)
prop.table(table(training_set_roseSampling$IsBadBuy))

# Building the classifier from package randomForest.
library(randomForest)

randomForest_classifier = randomForest(x = training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)][,-1],
                                       y = training_set_roseSampling$IsBadBuy )

randomForest_classifier

# Train the model on testing set
randomForest_pred = predict(randomForest_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
randomForest_pred

# Prediction result will be saved into model20

cm_randomForest = table(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred)

model20 <- c( confusionMatrix(cm_randomForest), roc$auc ) # Model will be saved into (model20)
accuracy_train <- sum( diag(cm_randomForest) ) / sum(cm_randomForest) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model20$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred)
print(confusionMatrix(cm_randomForest))

####################################################################################################################################################### Output

model15_all # 6A - imbalanced target on selected all columns
model16 # 6B - imbalanced target variable on selectedColumns_importanceVar
model17 # 6C - over sampling
model18 # 6D - under sampling
model19 # 6E - Both sampling
model20 # 6F - # Building RandomForest on ROSE sampling dataset with few variables selected during feature selection (selectedColumns_importanceVar)

####################################################################################################################################################### Model Tuning

# Result will save into variable model21_randomForest_10fold and model22_randomForest_tools
model21_randomForest_10fold <- c()
model22_randomForest_tools <- c()

####################################################################################################################################################### Model Tuning

# Variation 1 - model tuning - random Forest with 10-fold cross-validation  - 7A
# will build alongside rose sampling

library(caret)
library(e1071)
library(rpart)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
randomForest_classifier_kFold = train(IsBadBuy ~ ., 
                                      data =  training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)],
                                      method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
randomForest_classifier_kFold

randomForest_classifier_kFold_2 <- rpart(IsBadBuy ~ ., 
                                         data =  training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)], 
                                         method = "class", cp = 0.01) # Build the model which produce highest accuracy

randomForest_pred_kFold <- predict(randomForest_classifier_kFold_2, 
                                   newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][, -1], type = "class")
cm_randomForest_kFold = table(testing_set$IsBadBuy, randomForest_pred_kFold)
print(confusionMatrix(cm_randomForest_kFold))

library(gmodels)
CrossTable(randomForest_pred_kFold, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred_kFold)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred_kFold)

model21_randomForest_10fold <- c( confusionMatrix(cm_randomForest_kFold), roc$auc ) # Model will be saved into (model21_randomForest_10fold)
accuracy_train <- sum( diag(cm_randomForest_kFold) ) / sum(cm_randomForest_kFold) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model21_randomForest_10fold$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred_kFold)
print(confusionMatrix(cm_randomForest_kFold))

# Produce Result

print(model21_randomForest_10fold)

####################################################################################################################################################### Model Tuning

# Variation 2 - model tuning - with algorithm Tune (tuneRF) - Tune Using Algorithm Tools------------7B
# will build alongside rose sampling

set.seed(123)
bestmtry <- tuneRF(training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)][, -1], 
                   training_set_roseSampling$IsBadBuy, 
                   stepFactor=1.5, improve=1e-5, ntree=500)

# Select best mtry that produce least error.
print(bestmtry)
bestmtry

# set mtry as 4.
tunegrid_bestmtry <- expand.grid(.mtry=4)
rf <- randomForest(IsBadBuy ~ ., 
                   data =  training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)],
                   tuneGrid = tunegrid_bestmtry)

randomForest_pred2 = predict(rf, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
randomForest_pred2

cm_randomForest2 = table(testing_set$IsBadBuy, randomForest_pred2)
print(confusionMatrix(cm_randomForest2))

library(gmodels)
CrossTable(randomForest_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc.curve(testing_set$IsBadBuy, randomForest_pred2)

roc = roc.curve(testing_set$IsBadBuy, randomForest_pred2)

model22_randomForest_tools <- c( confusionMatrix(cm_randomForest2), roc$auc ) # Model will be saved into (model22_randomForest_tools)
accuracy_train <- sum( diag(cm_randomForest2) ) / sum(cm_randomForest2) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

# Reproduce result
print(accuracy_train) # Accuracy
print(model22_randomForest_tools$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy, randomForest_pred2)
print(confusionMatrix(cm_randomForest2))

####################################################################################################################################################### Model Tuning

# List of model result.
# Model Tuning
model21_randomForest_10fold # Variation 1 7A
model22_randomForest_tools # Variation 2 7B
