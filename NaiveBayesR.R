# This is About Naive Bayes - Model 2
# Please ensure to load kickDataset_4_Transformed.csv from R.Script - amlAssignment.R before running 
#                                                                                                   //    TP034717-NaiveBayesR.R         script.       //
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

model7_all <- c()
model8 <- c()
model9 <- c()
model10 <- c()
model11 <- c()
model12 <- c()

####################################################################################################################################################### 4A

# Build Naive Bayes on imbalanced dataset with all variable selected (selectedColumns_all)

library(e1071)

classifier = naiveBayes(x = training_set[complete.cases(training_set), (selectedColumns_all)][,-1],
                        y = training_set$IsBadBuy )
classifier

# Evaluate model performance using the classifier to test the predictions
y_pred_test = predict(classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_all)][,-1])
y_pred_test

cm = table(testing_set$IsBadBuy, y_pred_test)
print(confusionMatrix(cm))

library(gmodels)
CrossTable(y_pred_test, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, y_pred_test)

model7_all <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model7_all$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred_test)
print(confusionMatrix(cm))

####################################################################################################################################################### 4B

# Build Naive Bayes on imbalanced dataset with few variables selected during feature selection (selectedColumns_importanceVar)

library(e1071)

classifier = naiveBayes(x = training_set_overSampling[complete.cases(training_set_overSampling), (selectedColumns_importanceVar)][,-1],
                        y = training_set_overSampling$IsBadBuy )
classifier

# Evaluate model performance using the classifier to test the predictions
y_pred_test = predict(classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
y_pred_test

cm = table(testing_set$IsBadBuy, y_pred_test)
print(confusionMatrix(cm))

library(gmodels)
CrossTable(y_pred_test, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, y_pred_test)

model8 <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model8$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred_test)
print(confusionMatrix(cm))

####################################################################################################################################################### 4C

# Build Naive Bayes on oversampling dataset with few variables selected during feature selection (selectedColumns_importanceVar)

library(e1071)

classifier = naiveBayes(x = training_set[complete.cases(training_set), (selectedColumns_importanceVar)][,-1],
                        y = training_set$IsBadBuy )
classifier

# Evaluate model performance using the classifier to test the predictions
y_pred_test = predict(classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
y_pred_test

cm = table(testing_set$IsBadBuy, y_pred_test)
print(confusionMatrix(cm))

library(gmodels)
CrossTable(y_pred_test, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, y_pred_test)

model9 <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model9$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred_test)
print(confusionMatrix(cm))

####################################################################################################################################################### 4D

# Build Naive Bayes on undersampling dataset with few variables selected during feature selection (selectedColumns_importanceVar)

library(e1071)

classifier = naiveBayes(x = training_set_underSampling[complete.cases(training_set_underSampling), (selectedColumns_importanceVar)][,-1],
                        y = training_set_underSampling$IsBadBuy )
classifier

# Evaluate model performance using the classifier to test the predictions
y_pred_test = predict(classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
y_pred_test

cm = table(testing_set$IsBadBuy, y_pred_test)
print(confusionMatrix(cm))

library(gmodels)
CrossTable(y_pred_test, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, y_pred_test)

model10 <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model9$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred_test)
print(confusionMatrix(cm))

####################################################################################################################################################### 4E

# Build Naive Bayes on bothsampling dataset with few variables selected during feature selection (selectedColumns_importanceVar)

library(e1071)

classifier = naiveBayes(x = training_set_bothSampling[complete.cases(training_set_bothSampling), (selectedColumns_importanceVar)][,-1],
                        y = training_set_bothSampling$IsBadBuy )
classifier

# Evaluate model performance using the classifier to test the predictions
y_pred_test = predict(classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
y_pred_test

cm = table(testing_set$IsBadBuy, y_pred_test)
print(confusionMatrix(cm))

library(gmodels)
CrossTable(y_pred_test, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, y_pred_test)

model11 <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model9$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred_test)
print(confusionMatrix(cm))

####################################################################################################################################################### 4F

# Build Naive Bayes on ROSE sampling dataset with few variables selected during feature selection (selectedColumns_importanceVar)

library(e1071)

classifier = naiveBayes(x = training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)][,-1],
                        y = training_set_roseSampling$IsBadBuy )
classifier

# Evaluate model performance using the classifier to test the predictions
y_pred_test = predict(classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
y_pred_test

cm = table(testing_set$IsBadBuy, y_pred_test)
print(confusionMatrix(cm))

library(gmodels)
CrossTable(y_pred_test, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, y_pred_test)

model12 <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model9$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred_test)
print(confusionMatrix(cm))

####################################################################################################################################################### Output

model7_all # 4A - imbalanced target on selected all columns
model8 # 4B - imbalanced target variable on selectedColumns_importanceVar
model9 # 4C - over sampling
model10 # 4D - under sampling
model11 # 4E - Both sampling
model12 # 4F - # Build Naive Bayes on ROSE sampling dataset with few variables selected during feature selection (selectedColumns_importanceVar)
  
####################################################################################################################################################### Model Tuning
  
# Model Tuning

model13_MT <- c()
model14_MT <- c()

####################################################################################################################################################### 5A

# Variation 1: Improve model performance with Laplace Smoothing

nb_laplace_classifier = naiveBayes(x = training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)][,-1],
                                   y = training_set_roseSampling$IsBadBuy, laplace = 1)
nb_laplace_classifier

nb_laplace_pred = predict(nb_laplace_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
nb_laplace_pred

cm_laplace = table(testing_set$IsBadBuy, nb_laplace_pred)
print(confusionMatrix(cm_laplace))

library(gmodels)
CrossTable(nb_laplace_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, nb_laplace_pred)

model13_MT <- c( confusionMatrix(cm_laplace), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm_laplace) ) / sum(cm_laplace) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model13_MT$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,nb_laplace_pred)
print(confusionMatrix(cm_laplace))

####################################################################################################################################################### 5B

# Variation 2: Kernel Density Estimation for continuous features
library(naivebayes)
nb_kernelSelection_classifier = naive_bayes(x = training_set_roseSampling[complete.cases(training_set_roseSampling), (selectedColumns_importanceVar)][,-1],
                                            y = training_set_roseSampling$IsBadBuy, usekernel = TRUE, laplace = 1 )
nb_kernelSelection_classifier

nb_kernelSelection_pred = predict(nb_kernelSelection_classifier, newdata = testing_set[complete.cases(testing_set), (selectedColumns_importanceVar)][,-1])
nb_kernelSelection_pred

cm_kernelSelection = table(testing_set$IsBadBuy, nb_kernelSelection_pred)
print(confusionMatrix(cm_kernelSelection))

library(gmodels)
CrossTable(nb_kernelSelection_pred, testing_set$IsBadBuy,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

roc = roc.curve(testing_set$IsBadBuy, nb_kernelSelection_pred)

model14_MT <- c( confusionMatrix(cm_kernelSelection), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm_kernelSelection) ) / sum(cm_kernelSelection) * 100 # equivalent to mean(y_pred_test == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model14_MT$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,nb_kernelSelection_pred)
print(confusionMatrix(cm_kernelSelection))

####################################################################################################################################################### Output

model13_MT # Naive Bayes with laplace
model14_MT# Naive Bayes with kernel density
