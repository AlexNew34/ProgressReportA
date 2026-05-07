# This code creates the logistic regression model for the duration of the COVID-19 pandemic
# for face mask behaviour and protective behaviour binary in Australia.

# Load the packages.
library(caret)
library(tidymodels)
library(tidyverse)
library(vip)
library(pROC)

# Set the seed for reproducibility.
set.seed(1826179)
# Read the new data created and set as factor main response variables.
df<-read.csv('./face_mask_ausmand.csv')
df$face_mask_behaviour_binary <- as.factor(df$face_mask_behaviour_binary)
df$protective_behaviour_binary <- as.factor(df$protective_behaviour_binary)

### Model 1 ###
# Define 10-fold cross-validation for model metrics to be calculated.
cv_control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)
# Fit a logistic regression model using cross-validation (removing unwanted explanatory variables from model). 
# Google AI was used to help with 'method', 'family' and 'trControl' in this model to define a logistic regression.
model1 <- train(
  face_mask_behaviour_binary ~ .- (RecordNo + endtime + face_mask_behaviour_scale
                                   + protective_behaviour_scale + face_mask_behaviour_binary 
                                   + protective_behaviour_binary + protective_behaviour_nomask_scale
                                   + protective_behaviour_nomask_binary),
  data = df,
  method = "glm",
  family = "binomial",
  trControl = cv_control
)

# VIP plot for 20 best predictors.
vip(model1, num_features = 20)

# Calculate the model metrics.
preds <- predict(model1, df)
conf_matrix <- confusionMatrix(preds, df$face_mask_behaviour_binary)
print(conf_matrix$overall['Accuracy'])
print(conf_matrix$byClass['Sensitivity']) # This is Recall
accuracy <- conf_matrix$overall['Accuracy']
recall <- conf_matrix$byClass['Sensitivity']
precision <- conf_matrix$byClass['Pos Pred Value']
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)
probs <- predict(model1, df, type = "prob")
roc_obj <- roc(df$face_mask_behaviour_binary, probs$Yes)
plot(roc_obj, main = "ROC Curve Model 1",legacy.axes = TRUE)
print(auc(roc_obj))

### Model 2 ###
# Define 10-fold cross-validation for model metrics to be calculated.
cv_control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE)
# Fit a logistic regression model using cross-validation (removing unwanted explanatory variables from model). 
# Google AI was used to help with 'method', 'family' and 'trControl' in this model to define a logistic regression.
model2 <- train(
  protective_behaviour_binary ~ .- (RecordNo + endtime + face_mask_behaviour_scale
                                    + protective_behaviour_scale + face_mask_behaviour_binary 
                                    + protective_behaviour_binary + protective_behaviour_nomask_scale
                                    + protective_behaviour_nomask_binary),
  data = df,
  method = "glm",
  family = "binomial",
  trControl = cv_control
)
# VIP plot for 20 best predictors.
vip(model2, num_features = 20)

# Calculate the model metrics.
preds <- predict(model2, df)
conf_matrix <- confusionMatrix(preds, df$protective_behaviour_binary)
print(conf_matrix$overall['Accuracy'])
print(conf_matrix$byClass['Sensitivity']) # This is Recall
accuracy <- conf_matrix$overall['Accuracy']
recall <- conf_matrix$byClass['Sensitivity']
precision <- conf_matrix$byClass['Pos Pred Value']
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)
probs <- predict(model2, df, type = "prob")
roc_obj <- roc(df$protective_behaviour_binary, probs$Yes)
plot(roc_obj, main = "ROC Curve Model 2", legacy.axes = TRUE)
print(auc(roc_obj))
