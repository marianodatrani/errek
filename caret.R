library(tidyverse)
library(caret)
library(caretEnsemble)

# datasets: errek/caret dbs

#####################
##### regression ####



data("diamonds")

# Fit lm model using 10-fold CV: model
model <- train(
  price~., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
model

#predict(model, newdata = )

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  price ~ ., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)

# Fit random forest: model
model <- train(
  quality~.,
  tuneLength = 1,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

###tunelength: You can adjust the tuneLength variable to make a trade-off between 
#runtime and how deep you want to grid-search the model.Try a tuneLength of 3, 
#rather than 1, to explore some more potential models, and plot the resulting 
#model using the plot function. tuneLength is used to tell train to explore more models 
#along its default tuning grid and after plotted we can visually inspect the model's
#accuracy for different values of mtry, the most important hyperparameter of RF

# Fit random forest: model
model <- train(
  quality~.,
  tuneLength = 3,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model

# Plot model
plot(model)



# Now that you've explored the default tuning grids provided by the train() 
# function, let's customize your models a bit more.Define the tuning grid: tuneGrid

tuneGrid <- data.frame(
  .mtry = c(2, 3, 7),
  .splitrule = "variance",
  .min.node.size = 5
)

# Fit random forest: model
model <- train(
  quality~.,
  tuneGrid = tuneGrid,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

##########################
##### classification #####

#Classification problems are a little more complicated than regression problems 
#because you have to provide a custom summaryFunction to the train() function to
#use the AUC metric to rank your models (instead of accuracy). Start by making a 
#custom trainControl, as you did in the previous chapter. Be sure to set 
#classProbs = TRUE, otherwise the twoClassSummary for summaryFunction will break. 
#Binary classification!

myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model<-train(Class~.,
             method="glm",
             data=Sonar,
             trControl=myControl)


# Print model to console
model


#glmnet is an extension of the generalized linear regression model (or glm) that 
#places constraints on the magnitude of the coefficients to prevent overfitting. 
#This is more commonly known as "penalized" regression modeling and is a very 
#useful technique on datasets with many predictors and few values.
#glmnet is capable of fitting two different kinds of penalized models, controlled
#by the alpha parameter:
  
#Ridge regression (or alpha = 0)
#Lasso regression (or alpha = 1)

# lambda 0-infinity: size of penalty

#data(overfit)

# Fit glmnet model: model
model <- train(
  y~., 
  overfit,
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic ----- ?? roc statistics ??
max(model[["results"]]$ROC)

##
# Create confusion matrix from predicted classes, after predict()
confusionMatrix(p_class, test[["Class"]])

# Predict on test: p
p<-predict(model, test, type="response")

# Make ROC curve
caTools::colAUC(p, test$Class, plotROC=T)
##


#the glmnet model actually fits many models at once (one of the great things about 
#the package). You can exploit this by passing a large number of lambda values, which
#control the amount of penalization in the model. train() is smart enough to only fit
#one model per alpha value and pass all of the lambda values at once for simultaneous 
#fitting
#You also look at the two forms of penalized models with this tuneGrid: ridge regression
#and lasso regression. alpha = 0 is pure ridge regression, and alpha = 1 is pure lasso
#regression. You can fit a mixture of the two models (i.e. an elastic net) using an 
#alpha between 0 and 1. For example, alpha = 0.05 would be 95% ridge regression and 5%
#lasso regression.

#In this problem you'll just explore the 2 extremes – pure ridge and pure lasso 
#regression – for the purpose of illustrating their differences.

# Train glmnet with custom trainControl and tuning: model
model <- train(
  y~., 
  overfit,
  tuneGrid = expand.grid(
    alpha=0:1,
    lambda=seq(0.0001, 1, length=20)
  ),
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic ----- ?? roc statistics ??
max(model[["results"]]$ROC)


########################
##### preprocessing ####


# Apply median imputation: median_model
median_model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print median_model to console
median_model


#An alternative to median imputation is k-nearest neighbors, or KNN, imputation. 
#This is a more advanced form of imputation where missing values are replaced with 
#values from other rows that are similar to the current row. Better when NAs are 
#not missing at random!

# Apply KNN imputation: knn_model
knn_model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print knn_model to console
knn_model


# train() also includes a wide variety of other preProcess techniques to make 
#your life as a data scientist much easier. You can read a full list of them 
#by typing ?preProcess


# Update model with standardization and other combined preproc methods
model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("zv", "medianImpute", "center", "scale", "pca")
)

# "spatialSign" instead of "pca" useful when there are many outliers and 
#high dimensionality

# "zv" to remove features with zero- and "nzv" to remove features with near-zero variance:
#When a dataset contains many variables some of them may have extremely low variances. 
#This means that there is very little information in these variables because they 
#mostly consist of a single value (e.g. zero).

#caret also contains a utility function called nearZeroVar() for removing such variables
#to save time during modeling. Furthermore, zero variance variables can cause problems
#with cross-validation (e.g. if one fold ends up with only a single unique value for 
#that variable), so removing them prior to modeling means you are less likely to 
#get errors during the fitting process.
#nearZeroVar() takes in data x, then looks at the 
#ratio of the most common value to the second most common value, freqCut, and the 
#percentage of distinct values out of the number of total samples, uniqueCut. 
#If the frequency ratio is greater than a pre-specified threshold and the unique
#value percentage is less than a threshold, we might consider a predictor to be
#near zero-variance.
#By default, caret uses freqCut = 19 and uniqueCut = 10, which is fairly conservative.
#I like to be a little more aggressive and use freqCut = 2 and uniqueCut = 20 when
#calling nearZeroVar().
#

nearZeroVar(df_x, names = TRUE, 
            freqCut = 2, uniqueCut = 20, saveMetrics=T)


# PCA is generally a better method for handling low-information predictors 
#than throwing them out entirely!



########################################
#### Make custom train/test indices ####


#With predefined cv folds you can use the same summaryFunction and tuning parameters 
#for multiple models, you don't have to repeat code when fitting multiple models and
#you can compare models on the exact same training and test data.

# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# best first try usually glmnet: simple and often gives accurate results and easy to interpret

# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)

# next try should be rf: usually gives better prediction, but needs more time to run

# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl)



#You can compare models in caret using the resamples() function, provided they 
#have the same training data and use the same trainControl object with preset 
#cross-validation folds. resamples() takes as input a list of models and can be
#used to compare dozens of models at once

# Create model_list, without names there will be model1, model2,...
model_list <- list(glmnet = model_glmnet, rf = model_rf)

# Pass model_list to resamples(): resamples
resamples<-resamples(model_list)

# Summarize the results
summary(resamples)


#caret provides a variety of methods to use for comparing models. All of these 
#methods are based on the resamples() function.

# Create bwplot = box and whisker plot
bwplot(resamples, metric="ROC")

# Create xyplot = scatterplot with abline
xyplot(resamples, metric="ROC")


#caretEnsemble provides the caretList() function for creating multiple caret 
#models at once on the same dataset, using the same resampling folds. You can 
#also create your own lists of caret models.  Use the caretStack() function to
#make a stack of caret models, with the two sub-models (glmnet and ranger) feeding
#into another (hopefully more accurate!) caret model.

# Create ensemble model: stack
stack <- caretStack(model_list, method="glm")

# Look at summary
summary(stack)
