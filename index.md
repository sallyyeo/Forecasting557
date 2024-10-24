# Singapore Temperature Forecasting using Time Series Models

**Author**: Sally Marcellina Yeo  

The analysis is contained in the file `ANL501_ECA_Sallyyeo001_SallyMarcellinaYeo.Rmd`

## Overview

This project provides a detailed analysis and forecasting of Singapore's maximum daily temperatures using several time series models. The analysis spans from 1960 to 2022, and forecasts are made for the next 10 years (2023-2032).
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

