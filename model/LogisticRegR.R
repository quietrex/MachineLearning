# This is About Logistic Regression
# Please ensure to load kickDataset_4_Transformed.csv from R.Script - amlAssignment.R before running 
#                                                                                                   //    TP034717-LogisticRegR.R         script.       //
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

# Before building the classifier and fitting into model, we will have to create empty list that store accuracy, and prediction of F Score.
# It will have to show the result in the list.

model1 <- c()
model2 <- c()
model3 <- c()
model4 <- c()
model5 <- c()
model6_all <- c()

####################################################################################################################################################### 1A

# 1A) Building the classifier model with all variable (selectedColumns_all) that has been declared in code line 10. 

glm_model_classifier <- glm(formula = IsBadBuy ~ ., data = training_set[complete.cases(training_set),(selectedColumns_all)], 
                           family=binomial(logit))

summary_glm_classifier <- summary(glm_model_classifier)
summary_glm_classifier # the classifer is been built.

# Fit the model into testing set.
prod_prediction <- predict(glm_model_classifier, testing_set[complete.cases(testing_set),(selectedColumns_all)], type='response', se.fit = FALSE)
y_pred = ifelse(prod_prediction > 0.50, 1, 0)
y_pred

cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy, y_pred)
model6_all <- c( confusionMatrix(cm), roc$auc ) # Model will be saved into (model6_all)
accuracy_train <- sum( diag(cm) ) / sum(cm) * 100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

print(accuracy_train) # Accuracy
print(model6_all$byClass) # Model recall / Precision / FScore
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

####################################################################################################################################################### 1B


# 1B) Building the classifier model with specific variable (selectedColumns_importanceVar) after feature selection. 

# Imbalanced target variable (training_set)
table(training_set$IsBadBuy)
prop.table(table(training_set$IsBadBuy))

glm_model_classifier <- glm(formula = IsBadBuy ~ ., data = training_set[complete.cases(training_set),(selectedColumns_importanceVar)], 
                        family=binomial(logit))

summary_glm_classifier <- summary(glm_model_classifier)
summary_glm_classifier

# Fit the model / classifier into testing set.
prod_prediction <- predict(glm_model_classifier, testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)], type='response', se.fit = FALSE)
y_pred = ifelse(prod_prediction > 0.50, 1, 0)
y_pred

cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model1 <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model1)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

# Result accuracy
print(accuracy_train)
print(model1$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

####################################################################################################################################################### 1C

# 1C) Building the classifier model with specific variable (selectedColumns_importanceVar) after feature selection. 

# training set with over sampling
table(training_set_overSampling$IsBadBuy)
prop.table(table(training_set_overSampling$IsBadBuy))

# Building the classifier model - Approach 2 - Over Sampling
glm_model_classifier <- glm(formula = IsBadBuy ~ ., data = training_set_overSampling[complete.cases(training_set_overSampling),(selectedColumns_importanceVar)], 
                            family=binomial(logit))
summary_glm_classifier <- summary(glm_model_classifier)
summary_glm_classifier

# Fit the model / classifier to predict the accuracy.
prod_prediction <- predict(glm_model_classifier, testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)], type='response', se.fit = FALSE)
y_pred = ifelse(prod_prediction > 0.50, 1, 0)
y_pred

cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model2 <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model2)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

# Result accuracy
print(accuracy_train)
print(model2$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

####################################################################################################################################################### 1D

# 1D) Building the classifier model with specific variable (selectedColumns_importanceVar) after feature selection. 

# training set with under sampling
table(training_set_underSampling$IsBadBuy)
prop.table(table(training_set_underSampling$IsBadBuy))

# Building the classifier model - Approach 3 - Under Sampling
glm_model_classifier <- glm(formula = IsBadBuy ~ ., data = training_set_underSampling[complete.cases(training_set_underSampling),(selectedColumns_importanceVar)], 
                            family=binomial(logit))
summary_glm_classifier <- summary(glm_model_classifier)
summary_glm_classifier

# Fit the model / classifier to predict the accuracy.
prod_prediction <- predict(glm_model_classifier, testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)], type='response', se.fit = FALSE)
y_pred = ifelse(prod_prediction > 0.50, 1, 0)
y_pred

cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,prod_prediction)
model3 <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model3)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

# Result accuracy
print(accuracy_train)
print(model3$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

####################################################################################################################################################### 1E

# 1E) Building the classifier model with specific variable (selectedColumns_importanceVar) after feature selection. 

# training set with both sampling
table(training_set_bothSampling$IsBadBuy)
prop.table(table(training_set_bothSampling$IsBadBuy))

# Building the classifier model - Approach 4 - Both Sampling
glm_model_classifier <- glm(formula = IsBadBuy ~ ., data = training_set_bothSampling[complete.cases(training_set_bothSampling),(selectedColumns_importanceVar)], 
                            family=binomial(logit))
summary_glm_classifier <- summary(glm_model_classifier)
summary_glm_classifier

# Fit the model / classifier to predict the accuracy.
prod_prediction <- predict(glm_model_classifier, testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)], type='response', se.fit = FALSE)
y_pred = ifelse(prod_prediction > 0.50, 1, 0)
y_pred

cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model4 <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model4)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

# Result accuracy
print(accuracy_train)
print(model4$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))


####################################################################################################################################################### 1F

# 1F) Building the classifier model with specific variable (selectedColumns_importanceVar) after feature selection. 

# training set with rose sampling
table(training_set_roseSampling$IsBadBuy)
prop.table(table(training_set_roseSampling$IsBadBuy))

# Building the classifier model - Approach 5 - ROSE Sampling
glm_model_classifier <- glm(formula = IsBadBuy ~ ., data = training_set_roseSampling[complete.cases(training_set_roseSampling),(selectedColumns_importanceVar)], 
                            family=binomial(logit))
summary_glm_classifier <- summary(glm_model_classifier)
summary_glm_classifier

# Fit the model / classifier to predict the accuracy.
prod_prediction <- predict(glm_model_classifier, testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)], type='response', se.fit = FALSE)
y_pred = ifelse(prod_prediction > 0.50, 1, 0)
y_pred

# Model accuracy
cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model5 <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model5)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

print(accuracy_train)
print(model5$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))


####################################################################################################################################################### Getting Result
# Result with all variable
model6_all

# Logistic Regression without tuning.
model1 # train the classifer with Imbalanced target variable
model2 # train the classifer with over sampling
model3 # train the classifer with under sampling
model4 # train the classifer with both sampling
model5 # train the classifer with rose sampling

############################################################################################################################################  Model Tuning

# Model Tuning with lasso regression, ridge regression and elastic net regression

#alpha: the elasticnet mixing parameter. Allowed values include:
# "1": for lasso regression
# "0": for ridge regression

############################################################################################################################################################ 

# Compare the result for both normal model 1 (normal sampling) vs model 5 (rose sampling)

# Result will be stored in both model.
model_Exp1_Lasso_NormalSampling <- c() # normal sampling

model_Exp1_Lasso_RoseSampling_min <- c()
model_Exp1_Lasso_RoseSampling_1se <- c()

############################################################################################################################################################ 2A

# Model Tuning

# 2A) Running on a lasso penalized regression model - Specify alpha as 1 with (rose sampling) - with lambda.min as optimal lambda

library(glmnet)

# Theoratically, minimum ten variables can cause overfitting problem
x2 <- model.matrix(IsBadBuy~., training_set_roseSampling[complete.cases(training_set_roseSampling),(selectedColumns_importanceVar)])[,-1]
y2 <- training_set_roseSampling$IsBadBuy

grid = 10^seq(10, -2, length = 100)
lambda_seq <- 10 ^ seq(2, -2, by = -.1)

cv.lasso_roseSampling <- cv.glmnet(x2, y2, alpha = 1, family = "binomial")
plot(cv.lasso_roseSampling)

# 1. Using lambda.min as the best lambda, gives the following regression coefficients.
coef(cv.lasso_roseSampling, cv.lasso_roseSampling$lambda.min)
coef(cv.lasso_roseSampling, cv.lasso_roseSampling$lambda.1se)

# Fit the model on lasso regression with lambda.min.
lasso_model_lambda_min_rose <- glmnet(x2, y2, alpha = 1, family = "binomial",
                                 lambda = cv.lasso_roseSampling$lambda.min)
coef(lasso_model_lambda_min_rose)

x2.test <- model.matrix(IsBadBuy ~., testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)])[,-1]

# Fit the model / classifier to predict the accuracy.
prod_prediction <- lasso_model_lambda_min_rose %>% predict(newx = x2.test)
y_pred <- ifelse(prod_prediction > 0.5, 1, 0)
y_pred

# Model accuracy
cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model_Exp1_Lasso_RoseSampling_min <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model_Exp1_Lasso_RoseSampling_min)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

print(accuracy_train)
print(model_Exp1_Lasso_RoseSampling_min$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

############################################################################################################################################################ 2C

# 2B) Running on a lasso penalized regression model - Specify alpha as 1 with (rose sampling) - with lambda.1se as optimal lambda

library(glmnet)

# Theoratically, minimum ten variables can cause overfitting problem
x2 <- model.matrix(IsBadBuy~., training_set_roseSampling[complete.cases(training_set_roseSampling),(selectedColumns_importanceVar)])[,-1]
y2 <- training_set_roseSampling$IsBadBuy

grid = 10^seq(10, -2, length = 100)
lambda_seq <- 10 ^ seq(2, -2, by = -.1)

cv.lasso_roseSampling <- cv.glmnet(x2, y2, alpha = 1, family = "binomial")
plot(cv.lasso_roseSampling)

# 1. Using lambda.1se as the best lambda, gives the following regression coefficients.
coef(cv.lasso_roseSampling, cv.lasso_roseSampling$lambda.1se)

# Fit the model on lasso regression with lambda.min.
lasso_model_lambda_1se_rose <- glmnet(x2, y2, alpha = 1, family = "binomial",
                                      lambda = cv.lasso_roseSampling$lambda.1se)
coef(lasso_model_lambda_1se_rose)

x2.test <- model.matrix(IsBadBuy ~., testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)])[,-1]

# Fit the model / classifier to predict the accuracy.
prod_prediction <- lasso_model_lambda_1se_rose %>% predict(newx = x2.test)
y_pred <- ifelse(prod_prediction > 0.5, 1, 0)
y_pred

# Model accuracy
cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model_Exp1_Lasso_RoseSampling_1se <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model_Exp1_Lasso_RoseSampling_1se)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

print(accuracy_train)
print(model_Exp1_Lasso_RoseSampling_1se$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

############################################################################################################################################################ 

# Compare the result for both normal model 1 (normal sampling) vs model 5 (rose sampling)

# Result will be stored in both model.
model_Exp1_Ridge_RoseSampling_min <- c()
model_Exp1_Lasso_RoseSampling_1se <- c()

############################################################################################################################################################ 3A

# 3A) Running on a ridge regression model - Specify alpha as 0 with (training_set_roseSampling) with optimal lambda (lambda.min)

x3 <- model.matrix(IsBadBuy~., training_set_roseSampling[complete.cases(training_set_roseSampling),(selectedColumns_importanceVar)])[,-1]
y3 <- training_set_roseSampling$IsBadBuy
lambda_seq <- 10^seq(2, -2, by = -.1)

cv.ridge <- cv.glmnet(x3, y3, alpha = 0, lambda = lambda_seq, family = "binomial")
plot(cv.ridge)
summary(cv.ridge)

# 1. Choosing Optimal Lambda Value with lambda min
best_lambda <- cv.ridge$lambda.min
best_lambda

# Tuning parameter with optimal lambda
ridge_model_optimal_lambda_rose <- glmnet(x3, y3, alpha = 0, family = "binomial",
                                     lambda = best_lambda)

coef(ridge_model_optimal_lambda_rose)

x3.test <- model.matrix(IsBadBuy ~., testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)])[,-1]

# Fit the model / classifier to predict the accuracy.
prod_prediction <- ridge_model_optimal_lambda_rose %>% predict(newx = x3.test)
y_pred <- ifelse(prod_prediction > 0.5, 1, 0)
y_pred

# Model accuracy
cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model_Exp1_Ridge_RoseSampling_min <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model_Exp1_Ridge_RoseSampling_min)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

print(accuracy_train)
print(model_Exp1_Ridge_RoseSampling_min$byClass)

# Model 3A
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

############################################################################################################################################################ 3C
# 3C) Running on a ridge regression model - Specify alpha as 0 with (training_set_roseSampling) by Extracting the best model using K-cross validation

x3 <- model.matrix(IsBadBuy~., training_set_roseSampling[complete.cases(training_set_roseSampling),(selectedColumns_importanceVar)])[,-1]
y3 <- training_set_roseSampling$IsBadBuy
lambda_seq <- 10^seq(2, -2, by = -.1)

cv.ridge <- cv.glmnet(x3, y3, alpha = 0, lambda = lambda_seq, family = "binomial")
plot(cv.ridge)
summary(cv.ridge)

# 2. Extracting the best model using K-cross validation
best_fit <- cv.ridge$glmnet.fit
best_fit

best_fit_val <- 7.94300

ridge_model_optimal_lambda_KCross <- glmnet(x3, y3, alpha = 0, family = "binomial",
                                            lambda =  19.95000)
coef(ridge_model_optimal_lambda_KCross)

x4.test <- model.matrix(IsBadBuy ~., testing_set[complete.cases(testing_set),(selectedColumns_importanceVar)])[,-1]

# Fit the model / classifier to predict the accuracy.
prod_prediction <- ridge_model_optimal_lambda_KCross %>% predict(newx = x4.test)
y_pred <- ifelse(prod_prediction > 0.5, 1, 0)
y_pred

# Model accuracy
cm = table(testing_set$IsBadBuy, y_pred)
roc = roc.curve(testing_set$IsBadBuy,y_pred)
model_Exp1_Ridge_RoseSampling_min <- c(confusionMatrix(cm), roc$auc) # Model will be saved into (model_Exp1_Ridge_RoseSampling_min)
accuracy_train <- sum(diag(cm))/sum(cm)*100 # equivalent to mean(y_pred == testing_set$IsBadBuy)

print(accuracy_train)
print(model_Exp1_Ridge_RoseSampling_min$byClass)
roc.curve(testing_set$IsBadBuy,y_pred)
print(confusionMatrix(cm))

# Output Result
model_Exp1_Lasso_RoseSampling_min # Lasso with lambda min as optimal lambda
model_Exp1_Lasso_RoseSampling_1se # Lasso with lambda min as optimal 1se

model_Exp1_Ridge_RoseSampling_min # Ridge with lambda min as optimal lambda