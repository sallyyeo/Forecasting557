# Singapore Temperature Forecasting using Time Series Models

**Author**: Sally Marcellina Yeo  



## Overview

This project provides a detailed analysis and forecasting of Singapore's maximum daily temperatures using several time series models. The analysis spans from 1960 to 2022, and forecasts are made for the next 10 years (2023-2032).
The analysis is contained in the file `forecastingECA.Rmd`
The data used can be found in the link <a href="https://github.com/sallyyeo/Forecasting557/blob/main/Submission/SG_Enviroment_TemperatureRainfall.csv"> SG Environment Temperature and Rainfall </a>.


Key models used include:
- **Holt's Linear Trend Method**: Captures linear trends in temperature over time.
- **Linear Regression**: Predicts future temperatures based on historical trends.
- **ARIMA**: Used to handle non-stationary data through differencing and log transformations.
- **ARIMAX**: A dynamic regression model used to incorporate exogenous variables such as rainfall for more accurate predictions.

## Key Features

- **Data Cleaning**: Reshaping and transforming the raw data to prepare it for time series analysis.
- **Time Series Exploration**: Analyzing the trends, autocorrelations, and stationarity of the time series data.
- **Model Evaluation**: Evaluating the performance of different models using metrics such as MAPE, MAD, MSD, and AIC.
- **Forecasting**: Forecasting future temperatures using the best-performing models.

## Results

The project evaluates the following models:

| Model                 | MAPE     | MAD      | MSD      | AIC     |
|-----------------------|----------|----------|----------|---------|
| Holt’s Linear Trend    | 0.846    | 0.265    | 0.107    | 130.05  |
| Linear Regression      | 0.812    | 0.254    | 0.108    | 44.37   |
| ARIMA MA(1)            | 0.843    | 0.265    | 0.109    | 44.15   |
| ARIMAX(0,1,1)          | -        | -        | -        | 24.62   |

Among the evaluated models, **Linear Regression** and **ARIMA MA(1)** demonstrated the best overall performance in terms of accuracy. The dynamic regression model (ARIMAX) also provided good results when incorporating rainfall as an exogenous variable.

# R Code Explanation

Following are the required library:

# Requirements

```{r}
library(tidyverse)
library(forecast)
library(tsibble)
library(ggplot2)
library(tseries)
library(car)
library(TSA)
library(forecast)
```

# Loading Data

```{r}
# Clear all variables
rm(list=ls())  

# Set your working directory
setwd("/Users/sallyyeo/Desktop/557ECA")

# Read the dataset
raw <- read.csv("SG_Enviroment_TemperatureRainfall.csv")
```

# Shaping Data

```{r}
# Reshape the data using pivot_longer and pivot_wider
long_data <- raw %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value")

wide_data <- long_data %>%
  pivot_wider(names_from = Data.Series, values_from = Value)

# Remove 'X' from the Year column and convert to numeric
wide_data$Year <- sub("X", "", wide_data$Year)
wide_data <- wide_data %>%
  mutate_all(function(x) {
    x <- gsub(",", "", x)  # Remove commas
    parse_number(x, na = "na")  # Set empty values to NA
  })

# Convert year to date type
wide_data$Year <- as.Date(paste(wide_data$Year, "-01-01", sep=""), format="%Y-%m-%d")

# Ensure the data is in ascending chronological order
wide_data <- wide_data[order(wide_data$Year), ]

# Display the data
print(wide_data, n=63)
```

# Time-Series Preparation

```{r}
# Load additional required libraries
library(forecast)
library(tsibble)
library(ggplot2)

# Convert 'Air Temperature Means Daily Maximum' to a ts object
temp_max_ts <- ts(wide_data$`Air Temperature Means Daily Maximum (Degree Celsius)`, 
                  start = min(year(wide_data$Year)), 
                  end = max(year(wide_data$Year)), 
                  frequency = 1)  # Yearly data

# Display the time-series
print(temp_max_ts)

# Create a data frame with years and temperature values
years <- seq(1960, 2022)  # Sequence from start year to end year
temp_max_df <- data.frame(year = years, temp = temp_max_ts)
```

Output:

![](assets/img/time_series.jpg)

# Time-Series Plotting

``` {r}
# Plot the time series
plot(temp_max_ts, xlab = "Year", ylab = "Temperature (°C)", 
     main = "Annual Average Maximum Temperature in Singapore")

# Linear regression model
time_index <- time(temp_max_ts)
temp_max_trend_model <- lm(temp_max_ts ~ time_index)
summary(temp_max_trend_model)

# Plot the original time series and the trend
plot(temp_max_ts, xlab = "Year", ylab = "Temperature (°C)",
     main = "Annual Average Maximum Temperature and Trend in Singapore")
abline(a = temp_max_trend_model$coefficients[1], b = temp_max_trend_model$coefficients[2], col = "red")
legend("topleft", legend = c("Original", "Trend"), col = c("black", "red"), lty = 1)
```

Output:

![](assets/img/lm.jpg) | ![](assets/img/trend.png)

# Holt's Linear Trend Method

``` {r}
# Set forecast length
fc_length <- 10

# Apply Holt's linear trend method
temp_max_ts.desm <- holt(temp_max_ts, h = fc_length)

# Create a combined time series with forecasted values
temp_max_ts.desm_fc <- ts(c(temp_max_ts.desm$fitted, temp_max_ts.desm$mean), 
                          start = c(1960, 1), frequency = 1)

# Plot the original data and Holt's forecast
x_val <- time(temp_max_ts.desm_fc)
y_val <- c(wide_data$`Air Temperature Means Daily Maximum (Degree Celsius)`, rep(NA, fc_length))

plot(x = x_val, y = y_val, type = "l", xlab = "Year", ylab = "Temperature (°C)", lwd = 2, 
     main = "Annual Average Maximum Temperature and Trend in Singapore")
lines(x = x_val, y = temp_max_ts.desm_fc, col = "red", lwd = 3, type = "l")
legend("topleft", legend = c("Original", "Holt's"), col = c("black", "red"), lty = 1)
```

Output:

![](assets/img/holt.png)

# Model Evaluation - Holt's Method

``` {r}
# Compute evaluation metrics for Holt's model
MAPE_holt <- mean(abs(temp_max_ts.desm$residuals) / wide_data$`Air Temperature Means Daily Maximum (Degree Celsius)`) * 100
MAD_holt <- mean(abs(temp_max_ts.desm$residuals))
MSD_holt <- mean(temp_max_ts.desm$residuals ^ 2)
AIC_holt <- temp_max_ts.desm$model$aic

# Display metrics
print(paste("MAPE_holt:", MAPE_holt))
print(paste("MAD_holt:", MAD_holt))
print(paste("MSD_holt:", MSD_holt))
print(paste("AIC_holt:", AIC_holt))
```

Output:

```plaintext
[1] "MAPE_holt: 0.845606672437578"
[1] "MAD_holt: 0.265236023309835"
[1] "MSD_holt: 0.10671831495861"
[1] "AIC_holt: 130.051051125273"
```

# Linear Regression Analysis

``` {r}
# Linear regression model
linear_model <- lm(temp ~ year, data = temp_max_df)
summary(linear_model)

# Forecast future values
all_years <- data.frame(year = c(1960:2022, 2023:2032))
temp_max_df.linear_fc <- predict(linear_model, newdata = all_years)
all_years$temp <- temp_max_df.linear_fc

# Convert to time series object
temp_max_ts.linear_fc <- ts(temp_max_df.linear_fc, start = 1960, end = 2032, frequency = 1)
# Print the forecasted values
print(temp_max_ts.linear_fc)

# Get the extended time periods & append the exact number of NA as the forecast length
x_val <- time(temp_max_ts.linear_fc)
y_val <- c(temp_max_df$temp, rep(NA, fc_length))

# Plot the forecasted and original data
plot(x = time(temp_max_ts.linear_fc), y = c(temp_max_df$temp, rep(NA, fc_length)), type = "l", 
     xlab = "Year", ylab = "Temperature (°C)", lwd = 2, 
     main = "Annual Average Maximum Temperature and Trend in Singapore")
abline(linear_model, col = "green")
lines(x = time(temp_max_ts.linear_fc), y = temp_max_ts.linear_fc, col = "red", lty = 1, type = "l")
legend("topleft", legend = c("Original", "Linear Regression", "Forecasted Value"), 
       col = c("black", "green", "red"), lty = 1)
```

Linear Regression forecast:

![](assets/img/lm_forecast.png)

# Linear Model Evaluation

```{r}
################# Linear Model Evaluation #######################
anova(linear_model)

# Extract the residuals of the linear regression
linear_model.res <- residuals(linear_model)

# Create a qq-plot for the model residuals
qqnorm(y = linear_model.res)

# Add a 45-degree line to it. If the residuals are lying closely on the line, the residuals
# of the linear regression are most likely normally distributed.
qqline(linear_model.res, col = "red")

# Generate a histogram to have another check on the distribution of the residuals
hist(linear_model.res, breaks = 8)

# The result should be supported by a numerical test. 
# The Shapiro-Wilk test is most common for it.
shapiro.test(linear_model.res)

# Check on the independence of the residuals by looking at the ACF
acf(linear_model.res, lag.max = 63)

#install.packages("car")
library(car)
durbinWatsonTest(linear_model)

# Calculating predicted values for the actual period (1960-2022)
temp_max_df$predicted_temp <- predict(linear_model, newdata = temp_max_df)

# Calculating MAPE
MAPE_lm <- mean(abs((temp_max_df$temp - temp_max_df$predicted_temp) / temp_max_df$temp)) * 100

# Calculating MAD
MAD_lm <- mean(abs(temp_max_df$temp - temp_max_df$predicted_temp))

# Calculating MSD
MSD_lm <- mean((temp_max_df$temp - temp_max_df$predicted_temp)^2)

# Extracting AIC
AIC_lm <- AIC(linear_model)

# Print the metrics
print(paste("MAPE:", MAPE_lm))
print(paste("MAD:", MAD_lm))
print(paste("MSD:", MSD_lm))
print(paste("AIC:", AIC_lm))
```

Output:

```plaintext
Analysis of Variance Table

Response: temp
          Df Sum Sq Mean Sq F value    Pr(>F)    
year       1 6.6214  6.6214   59.55 1.351e-10 ***
Residuals 61 6.7827  0.1112                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	Shapiro-Wilk normality test

data:  linear_model.res
W = 0.97428, p-value = 0.2093

 lag Autocorrelation D-W Statistic p-value
   1        0.312369      1.362993   0.006
 Alternative hypothesis: rho != 0
[1] "MAPE: 0.811967714365999"
[1] "MAD: 0.254445846438931"
[1] "MSD: 0.107661605263372"
[1] "AIC: 44.3742330891586"
```

Residual Analysis:

![](assets/img/qq.png) | ![](assets/img/lm_resid.png) | ![](assets/img/acf_lm.png)


# Data Preparation for ARIMA

``` {r}
# First differencing
temp_max_ts_diff1 <- diff(temp_max_df$temp, lag = 1)

# Check variance
plot(temp_max_ts_diff1)
plot(x = 1:62, y = temp_max_ts_diff1)

# Check stationarity with ACF
acf(temp_max_ts_diff1, lag.max=62)
pacf(temp_max_ts_diff1, lag.max=62)
```

Variance, ACF and PACF of first differencing:

![](assets/img/diff1_variance.png) | ![](assets/img/diff1_acf.png) | ![](assets/img/diff1_pacf.png)

Performing Log Transformation:

```{r}
# Log transformation
temp_max_ts_log <- log(temp_max_df$temp, base = exp(1))

# Check variance
plot(x = wide_data$Year, y = temp_max_ts_log)

# Check stationarity with ACF
acf(temp_max_ts_log)
pacf(temp_max_ts_log)
```

Variance, ACF and PACF of log transformed:

![](assets/img/log_variance.png) | ![](assets/img/log_acf.png) | ![](assets/img/log_pacf.png)


```{r}
# Differencing the log transformed
temp_max_ts_log_diff1 <- diff(temp_max_ts_log, lag = 1)

# Check variance
plot(temp_max_ts_log_diff1)
plot(x = 1:62, y = temp_max_ts_log_diff1)

# Check stationarity with ACF
acf(temp_max_ts_log_diff1, lag.max = 63)
pacf(temp_max_ts_log_diff1, lag.max = 63)
```

Variance, ACF and PACF of log and diff1 transformed:

![](assets/img/log_diff1_variance.png) | ![](assets/img/log_diff1_acf.png) | ![](assets/img/log_diff1_pacf.png)

```{r}
# MA(1) after diff the log transformed
ma_process.ma1 <- arima(temp_max_ts_log_diff1, order = c(0, 0, 1))
print(ma_process.ma1)

# Find p-value of the MA(1)
param_ma1 <- ma_process.ma1$coef
covmat_ma1 <- ma_process.ma1$var.coef
se_ma1 <- sqrt(diag(covmat_ma1))
ttest_ma1 <- abs(param_ma1) / se_ma1
pt(ttest_ma1, df = length(ma_process.ma1) - length(param_ma1), lower.tail = FALSE) * 2
summary(ma_process.ma1)

# MA(2) after diff the log transformed
ma_process.ma2 <- arima(temp_max_ts_log_diff1, order = c(0, 0, 2))
print(ma_process.ma2)

# Find p-value of the MA(2)
param_ma2 <- ma_process.ma2$coef
covmat_ma2 <- ma_process.ma2$var.coef
se_ma2 <- sqrt(diag(covmat_ma2))
ttest_ma2 <- abs(param_ma2) / se_ma2
pt(ttest_ma2, df = length(ma_process.ma2) - length(param_ma2), lower.tail = FALSE) * 2
summary(ma_process.ma2)
```

Model evaluation:

```plaintext
Shapiro-Wilk normality test

data:  ma_process.ma1.res
W = 0.98073, p-value = 0.4378


Call:
arima(x = temp_max_df$temp, order = c(0, 1, 1))

Coefficients:
         ma1
      -0.712
s.e.   0.089

sigma^2 estimated as 0.1106:  log likelihood = -20.07,  aic = 42.15
```

```{r}
########## Model Diagnostic ##########
# Extract the residuals of the model MA(1)
ma_process.ma1.res <- residuals(ma_process.ma1)
acf(ma_process.ma1.res)
pacf(ma_process.ma1.res)

# Check model's normality
hist(ma_process.ma1.res)
qqnorm(ma_process.ma1.res)
qqline(ma_process.ma1.res, col = "red")
shapiro.test(ma_process.ma1.res)

# Accuracy metrics
arima011 <- arima(temp_max_df$temp, order = c(0, 1, 1))
summary(arima011)
```

ACF, PACF and QQ of MA1:

![](assets/img/ma1_acf.png) | ![](assets/img/ma1_pacf.png) | ![](assets/img/ma1_resid.png) | ![](assets/img/ma1_qq.png)


# ARIMA MODEL

``` {r}
# Forecast using MA(1)
fc_length = 10
ma1_fc = predict(ma_process.ma1, n.ahead = fc_length)

# Reverse the differencing
fcval <- c(temp_max_ts_log_diff1, ma1_fc$pred) # Combine historical & forecasted
fcval <- ts(fcval, start = c(1960), frequency = 1)

# Reverse the log transformation
newval <- c(temp_max_ts_log[1], temp_max_ts_log[1] + cumsum(fcval))
newval <- ts(newval, start = c(1960), frequency = 1)
ma1_original_scale_fc = exp(newval)

# Print forecast in original scale
print(ma1_original_scale_fc)

# Compare forecast with mean of historical data
mean(temp_max_df$temp)

# Plot the forecast values
plot(ma1_original_scale_fc, type = "l", col = "red")
lines(temp_max_df$temp, col = "black")
legend("topleft", legend = c("Original", "ARIMA(0,0,1)"), col = c("black", "red"), lty = 1)
```

ARIMA forecast:

![assets/img/arima.png]

