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
library(forecast)
library(vars)
library(coefplot)

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

# Subset data to just be from May (3 months) and the core fields
grinding.df <- grinding.df[grinding.df$measurement_date >= "2016-05-01", c("measurement_date",
                                                                           "3311WI671.PV",
                                                                           "FI22302.PV",
                                                                           "FI22963",
                                                                           "WIC22026.SV",
                                                                           "JIC22366",
                                                                           "YIC22001",
                                                                           "1101047CL1.CPV2",
                                                                           "3311HS181A.PV")]
# Only complete cases
grinding.df <- grinding.df[complete.cases(grinding.df),]

# Hold out set for measuring quality of forecasts (last 5 days)
inTrain <- 1:(nrow(grinding.df)-((60/5)*60*24))
training <- grinding.df[inTrain,]
testing <- grinding.df[-inTrain,]

# ====== Time Series ====== #
# Convert to Time Series
grinding_surgeTS <- ts(data = training$`3311WI671.PV`)

# == Try just forecasting the surge variable == #
# Number of diffs
numDiffs <- ndiffs(grinding_surgeTS)

# Plot
plot(diff(grinding_surgeTS,2))

# Fit an ARIMA
surgeForecastBest <- auto.arima(x = grinding_surgeTS)

# Save
saveRDS(surgeForecastBest, "models/surgeARIMAForecast.rds")

# Inspect
surgeForecastBest

# Residuals
acf(surgeForecastBest$residuals)
pacf(surgeForecastBest$residuals)

# Predict 5 days ahead
surgeForecastPrediction <- forecast(object = surgeForecastBest, h = 17280 * 5)

# Plot
plot(surgeForecastPrediction)

# ===== VAR ===== #
# Convert to Time Series
grinding_surgeTS <- ts(data = grinding.df[,c("3311WI671.PV",
                                             "FI22302.PV",
                                             "FI22963",
                                             "WIC22026.SV",
                                             "JIC22366",
                                             "YIC22001",
                                             "1101047CL1.CPV2",
                                             "3311HS181A.PV")])

# Plot
# plot(grinding_surgeTS, plot.type="single")
# legend("topleft", legend = colnames(grinding_surgeTS), ncol = 2, lty=1, cex =.9)

# == Fit the Vector Autoregressive Model == #

# Get max diffs
numDiffs <- ndiffs(grinding_surgeTS)
grinding_surgeTS_diffed <- diff(grinding_surgeTS, differences = numDiffs)

# Hold out set for measuring quality of forecasts (last 5 days)
inTrain <- 1:(nrow(grinding_surgeTS_diffed)-((60/5)*60*24))
training <- grinding_surgeTS_diffed[inTrain,]
testing <- grinding_surgeTS_diffed[-inTrain,]

# Plot
plot(grinding_surgeTS_diffed, plot.type="single")
legend("bottomleft", legend = colnames(grinding_surgeTS), ncol = 2, lty=1, cex =.9)

# Fit the VAR
surgeVAR <- VAR(training, lag.max = 24)
# Chosen order
surgeVAR$p

# Inspect the results
coefplot(surgeVAR$varresult$X3311WI671.PV)

# Plot just the influential variables
VARcoefs.df <- data.frame(predictor = names(surgeVAR$varresult$X3311WI671.PV$coefficients),
                          coefficient = surgeVAR$varresult$X3311WI671.PV$coefficients)
rownames(VARcoefs.df) <- NULL
YIC22001.df <- data.frame(VARcoefs.df[str_detect(VARcoefs.df$predictor,"YIC22001"),],
                          time_before_surge = seq(from = 5, to = 24*5,by=5))

# Plot just 'YIC22001' lags
ggplot(YIC22001.df, aes(x = time_before_surge, y = -1*coefficient)) + 
    geom_line(color = "#2CB5C0") +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_point() +
    labs(title = "YIC22001 (T9ML001 Noise) influence on Surge",
         x = "Time before surge (secs)",
         y = "Relative influence")

# Plot the YIC22001 vs actual values
YIC22001_vs_3311WI671.df <- data.frame(value = surgeVAR$varresult$X3311WI671.PV$model$y,
                                  measurement = "3311WI671.PV",
                                  index = 1:length(surgeVAR$varresult$YIC22001$model$y))
YIC22001_vs_3311WI671.df <- bind_rows(YIC22001_vs_3311WI671.df,
                                 data.frame(value = surgeVAR$varresult$YIC22001$model$y,
                                            measurement = "YIC22001",
                                            index = 1:length(surgeVAR$varresult$YIC22001$model$y)))

# Plot
ggplot(YIC22001_vs_3311WI671.df, aes(x = index, y = value, colour = measurement)) +
    geom_line() +
    facet_grid(measurement ~ ., scales = "free_y") +
    labs(title = "YIC22001 vs 3311WI671",
         x = "Time index",
         y = "1st Diffed value") +
    guides(fill=FALSE)

# Plot the actual vs fitted values
actual_vs_fitted.df <- data.frame(value = surgeVAR$varresult$X3311WI671.PV$fitted.values,
                                  measurement = "fitted_value",
                                  index = 1:length(surgeVAR$varresult$X3311WI671.PV$fitted.values))
actual_vs_fitted.df <- bind_rows(actual_vs_fitted.df,
                                 data.frame(value = surgeVAR$varresult$X3311WI671.PV$model$y,
                                            measurement = "actual_value",
                                            index = 1:length(surgeVAR$varresult$X3311WI671.PV$model$y)))

# Plot
ggplot(actual_vs_fitted.df, aes(x = index, y = value, colour = measurement)) +
    geom_line() +
    labs(title = "VAR timeseries forecast",
         x = "Time index",
         y = "1st Diffed value")

# Root mean squared error
sqrt(mean(surgeVAR$varresult$X3311WI671.PV$residuals^2))


# Prediction
surgeVARprediction <- predict(surgeVAR, n.ahead = 30)

# Root mean squared error
sqrt(mean((testing$`3311WI671.PV`[1:1000]-surgeVARprediction$fcst$X3311WI671.PV[,1])^2))

# Plot the quality of the prediction
prediction_vs_holdout <- data.frame(value = surgeVARprediction$fcst$X3311WI671.PV[,1],
                                    measurement = "prediction",
                                    index = 1:30)
prediction_vs_holdout <- bind_rows(prediction_vs_holdout,
                                   data.frame(value = testing[1:30,1],
                                              measurement = "actual",
                                              index = 1:30))

# Plot
ggplot(prediction_vs_holdout, aes(x = index, y = value, colour = measurement)) +
    geom_line() +
    labs(title = "Performance of prediction",
         x = "Time index",
         y = "1st Diffed value")

# Save
saveRDS(surgeVAR, "models/surgeVAR.rds")
# Load
# surgeVAR <- readRDS("models/surgeVAR.rds")
# See the prediction of surges

# Plot those two variables, facet by surge
# Prepare the data for the plot
plotData <- grinding.df[grinding.df$measurement_date >= "2016-06-01",c("3311WI671.PV",
                                                                       "measurement_date",
                                                                       "WIC22026.SV")]
# Created a basic surge classifier
plotData$surge <- as.factor(plotData$`3311WI671.PV` >= 250)

# Fix levels
levels(plotData$surge) <- c("N","Y")

ggplot(plotData, aes(x = measurement_date, y = `3311WI671.PV`, colour = surge)) + 
    geom_line() +
    labs(title = "Surge Timeline",
         x = "date",
         y = "3311WI671.PV")

ggplot(surgeVAR, aes(x = , y = `3311WI671.PV`, colour = surge)) + 
    geom_line() +
    labs(title = "Surge Timeline",
         x = "date",
         y = "3311WI671.PV")