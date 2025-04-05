Time Series Analysis Application using R Shiny
This repository contains an interactive R Shiny web application designed for the exploration and modeling of time series data. Developed as part of an academic project, the application implements a comprehensive suite of techniques including decomposition, transformation, ARIMA-based models, and volatility modeling through ARCH and GARCH frameworks. It also leverages Python integration via the reticulate package to extend analytical capabilities.

Key Features
Time Series Decomposition (Additive and Multiplicative)

Moving Average Smoothing

Differencing and Seasonal Differencing

Autocorrelation and Partial Autocorrelation (ACF and PACF) Plots

Logarithmic Transformation

AR, MA, ARMA, and ARIMA Modeling

Seasonal ARIMA (SARIMA) Modeling

ARCH and GARCH Models for Volatility Estimation

Forecasting with Confidence Intervals

Interactive Controls for Model Parameters

Python Integration via reticulate for Enhanced Functionality

Libraries Used
R Packages
shiny — Interactive web application framework

forecast — Time series forecasting methods

tseries — Statistical tools for time series analysis, including ARCH/GARCH

rugarch — Advanced GARCH modeling

ggplot2 — Data visualization

TTR — Technical trading rules and smoothing

zoo — Infrastructure for regular and irregular time series

reticulate — Integration between R and Python

xts — Extension of zoo for uniform time series handling

gridExtra — Arranging multiple ggplot2 visualizations

Python Packages (via reticulate)
pandas

numpy

Note: Ensure Python is installed and properly configured with the required packages for full functionality.

How to Run the App
1. Clone the Repository
git clone https://github.com/your-username/time-series-shiny-app.git
cd time-series-shiny-app
2. Install Required R Packages
install.packages(c("shiny", "forecast", "tseries", "rugarch", 
                   "ggplot2", "TTR", "zoo", "reticulate", "xts", "gridExtra"))
3. Run the App in R
shiny::runApp("RSHINY.R")
Ensure Python is configured properly with reticulate.
Credits
This application was developed as part of the Time Series Analysis coursework by a group of five MSc Statistics students. The project involved collaborative design, development, and implementation of time series analysis techniques using R Shiny, contributing to both academic understanding and practical experience.
