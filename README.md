📊 Time Series Analysis App using R Shiny
This repository contains an interactive R Shiny web application designed for the exploration and modeling of time series data. Developed as part of an academic project, the application implements a comprehensive suite of techniques including decomposition, transformation, ARIMA-based models, and volatility modeling through ARCH and GARCH frameworks. It also leverages Python integration via the reticulate package to extend analytical capabilities.

🔍 Key Features
📈 Time Series Decomposition (Additive/Multiplicative)
🔁 Moving Average Smoothing
🔂 Differencing and Seasonal Differencing
🔍 ACF and PACF plots
⚙ Log Transformation
🧠 AR, MA, ARMA, ARIMA Modeling
📆 SARIMA (Seasonal ARIMA) Modeling
📊 ARCH and GARCH Models for volatility
📉 Forecasting with confidence intervals
🔧 Interactive input controls for parameters
🐍 Python Integration via reticulate for extended functionality
📦 Libraries Used
R Packages:
shiny — Interactive web framework
forecast — Time series forecasting
tseries — Time series tools and ARCH/GARCH modeling
rugarch — Advanced GARCH models
ggplot2 — Data visualization
TTR — Technical Trading Rules for smoothing
zoo — Time series objects
reticulate — Integrate Python with R
xts — Extensible time series
gridExtra — Combine multiple ggplots
Python Packages (via reticulate):
pandas
numpy
(Ensure Python is installed and configured with these packages)
🚀 How to Run the App
1. Clone the Repository
git clone https://github.com/your-username/time-series-shiny-app.git
cd time-series-shiny-app
2. Install Required R Packages
install.packages(c("shiny", "forecast", "tseries", "rugarch", 
                   "ggplot2", "TTR", "zoo", "reticulate", "xts", "gridExtra"))
3. Run the App in R
shiny::runApp("RSHINY.R")
✅ Ensure Python is configured properly with reticulate.

👥 Credits
This app was developed as part of our Time Series Analysis coursework by a team of five MSc Statistics students.
We collaboratively designed and implemented this Shiny app as part of our academic project.
