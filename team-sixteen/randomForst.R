# ============================================ #
#                                              #
# Title: Newcrest challenge 2 import           #
# Author: Daniel Booth                         #
#                                              #
# ============================================ #

# ================ HEADER ================ #
# Set wd
setwd("~/Dropbox/unearthed2016/")

# Load packages
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(caret)
library(corrplot)
library(parallel)
library(doMC)
# Set parallel
registerDoMC(7)

# ======================================== #

# ================ Read in the data ================ #
# Load data
data_path <- "data_in/processed/grinding.df.rds"
grinding.df <- readRDS(data_path)

# = Cleaning = #
# Remove the redundant fields
grinding.df <- grinding.df[,-c(4:5,22:23,40:41)]

# Remove rows where surge is NA
grinding.df <- grinding.df[!is.na(grinding.df$`3311WI671.PV`),]

# ================ Feature Engineering ================ #

# Created a basic surge classifier
grinding.df$surge <- as.factor(grinding.df$`3311WI671.PV` >= 250)

# Fix levels
levels(grinding.df$surge) <- c("N","Y")

# Date as int
grinding.df$date_int <- as.integer(grinding.df$measurement_date)

# ======================================== #

# ================ Modeling ================ #
# Split training and testing sets
# By default, createDataPartition does a stratified random split of the data
set.seed(1123)
grinding.df <- grinding.df[grinding.df$measurement_date > "2016-05-01",]
inTrain <- createDataPartition(y = grinding.df$surge, # The outcome/response data
                               p = 0.75, # The proportion of data in training
                               list = FALSE) # The format of the results
# Note: inTrain is a set of integers for the rows of
# your.df that belong in the training set.

# Remove any variables not required for modeling (if required)
training <- grinding.df[inTrain, -c(1,14)]
testing <- grinding.df[-inTrain, -c(1,14)]

# Sanity check consistency in the distibutions of the original and partitioned training data
# Training
prop.table(table(training$surge)) * 100
# Original
prop.table(table(grinding.df$surge)) * 100

# = Pre-process Testing (if required) = #
# If your algorithm requires variables to be normalized or scaled
# Before we train the model let's test that the preprocessing works
# Get the predictors
# trainX <- training[,names(training) != "response"]
# Normalize
# preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
# Check
# preProcValues

# = Train the model = #
### Control our resampling method and other settings in the training
# Explanation of options:
# - method: Controls the type of resampling to use. Defaults to "boot" (simple bootstrap)
#           Other options include "cv" and "repeatedcv" (repeat K-fold cross-validation)
# - number: The number of folds in k-fold CV. Defaults to 10
# - repeats: The number of repeats in repeated CV
# - search: The parameters to tune on. "grid" sets this to the default grid
# - classProbs: For classification prediction, this sets the model to return
#               the predicted class probabilities (which are not computed automatically)
# - summaryFunction: Used to pass in a function that takes the observed and
#                    predicted values and estimates some measure of performance.
#                    twoClassSummary computes the ROC/AUC value                    
# - verboseIter: Print status updates so we know the model is running
ctrl <- trainControl(method = "cv",
                     number = 10,
                     search = "random",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

### Fit/train our model
# Explanation of options:
# - formula: The equation to first. Usually 'response ~ .'
#            Use 'response ~ . + .*.' for all variables and interactions
# - method: The type of model
# - metric: The performance metric to evaluate on when tuning
# - trControl: Passing in the training controls from above
# - na.action: Define how to handle missing values
# - tuneLength: The train fuction will generate a candidate set of
#               parameter values and the tuneLength argument controls
#               how many are evaluated. Eg. tune on 1:tuneLength
# - tuneGrid: If you want to set multiple tuning parameter settings.
#             Here input a data frame where each row is a tuning parameter
#             setting and each column is a tuning parameter
# - allowPalallel: If you have registed cores in the header and your model
#                  supports parallel processing, set this option true

rfFit <- train(surge ~ .,
                data = training,
                method = "rf",
                metric = "ROC",
                trControl = ctrl,
                tuneLength = 10,
                preProcess = c("scale","center","medianImpute", "nzv"),
                na.action = na.pass,
                allowParallel = TRUE)

### Inspect the output
rfFit
# The note at the bottom should tell us the optimal parameters (based on tuning)
# That were used to fit the final (and best performing) model

# Visualise the results
# Show the relationship between the resampled performance values
plot(rfFit)

# We can inspect the final model
rfFit$finalModel

# Variable importance
data.frame(predictor = row.names(importance(rfFit$finalModel)),
           MeanDecreaseGini = importance(rfFit$finalModel)) %>%
    arrange(desc(MeanDecreaseGini))

# == Save/Load the model == #
# Save
saveRDS(rfFit, "models/rfFit.rds")
# Load in model
# logreg <- readRDS("models/logregFit.rds")

# ==== Evaluate - testing set from caret ==== #
# Predict
rfPredict <- predict(rfFit, newdata = testing)
# Compute class probabilities for classification models
rfPredict_probs <- predict(rfFit, newdata = testing, type = "prob")

# Confusion matrix and assoicated statistics for the model fit
confusionMatrix(rfPredict, testing$surge)
# Two Class Summary with probs (for ROC)
twoClassSummary(data.frame(obs = testing$surge,
                           pred = rfPredict,
                           rfPredict_probs), lev = c("N","Y"))

# === Comparing multiple Caret models === #
# After you've fit more than one Caret model you can
# compare them in terms of their resampling results.
# The resamples function can be used to collect, summarize
# and contract the resampling results.
# resamps <- resamples(lists(mod1 = mod1Fit, mod2 = mod2Fit))
# summary(resamps)

# You can visualize these with a Bland-Altman plot
# xyplot(resamps, what = "BlandAltman")
# Here we want to see a large number of points on either the top or bottom

# Also can test whether there is significant difference between
# each model with a paired t-test
# diffs <- diff(resamps)
# summary(diffs)