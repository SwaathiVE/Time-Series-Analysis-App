library(shiny)
library(shinydashboard)
library(tseries)
library(ggplot2)
library(forecast)
library(TTR)
library(lmtest)
library(readxl)
library(rugarch)
library(urca)

ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Column Info", tabName = "columns", icon = icon("table")),
      menuItem("Decomposition", tabName = "decomposition", icon = icon("project-diagram")),
      menuItem("Data Visualization", tabName = "viz", icon = icon("chart-line")),
      menuItem("Stationarity Test", tabName = "stationarity", icon = icon("check-circle")),
      menuItem("Modeling", tabName = "model", icon = icon("cogs")),
      menuItem("Residual Analysis", tabName = "residuals", icon = icon("chart-bar")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-area")),
      menuItem("Final Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(fileInput("file", "Upload Excel File", accept = ".xlsx"), width = 6),
                box(uiOutput("columnSelector"), width = 6),
                box(numericInput("frequency", "Enter Data Frequency (e.g., 4 for Quarterly)", value = 4), width = 6)
              )
      ),
      tabItem(tabName = "columns",
              fluidRow(box(tableOutput("columnDisplay"), width = 12))
      ),
      tabItem(tabName = "decomposition",
              fluidRow(
                box(selectInput("decompType", "Choose Decomposition Type", choices = c("Additive", "Multiplicative")), width = 6),
                box(plotOutput("baseDecompPlot"), width = 12)
              )
      ),
      tabItem(tabName = "viz",
              fluidRow(box(plotOutput("tsPlot"), width = 12)),
              fluidRow(
                box(selectInput("transform", "Choose Transformation", choices = c("None", "Log", "Differencing", "Seasonal Differencing", "Moving Average")), width = 6),
                box(numericInput("maWindow", "Moving Average Window:", value = 3), width = 6),
                box(actionButton("applyTransform", "Apply Transformation"), width = 6)
              )
      ),
      tabItem(tabName = "stationarity",
              fluidRow(
                box(plotOutput("decompPlot"), width = 12),
                box(verbatimTextOutput("adfTest"), width = 12)
              )
      ),
      tabItem(tabName = "model",
              fluidRow(box(plotOutput("acfPacfPlot"), width = 12)),
              fluidRow(
                box(selectInput("modelType", "Select Model Type:",
                                choices = c("AR", "MA", "ARMA", "ARIMA", "SARIMA", "ARCH", "GARCH")), width = 4),
                box(numericInput("p_order", "AR (p):", value = 1), width = 2),
                box(numericInput("d_order", "I (d):", value = 0), width = 2),
                box(numericInput("q_order", "MA (q):", value = 1), width = 2),
                box(numericInput("P_order", "SAR (P):", value = 0), width = 2),
                box(numericInput("D_order", "SI (D):", value = 0), width = 2),
                box(numericInput("Q_order", "SMA (Q):", value = 0), width = 2),
                box(numericInput("seasonality", "Seasonality (s):", value = 0), width = 2),
                box(actionButton("fitModel", "Fit Model"), width = 2)
              ),
              fluidRow(box(verbatimTextOutput("modelSummary"), width = 12))
      ),
      tabItem(tabName = "residuals",
              fluidRow(
                box(plotOutput("residualsPlot"), width = 12),
                box(plotOutput("acfPlot"), width = 12),
                box(plotOutput("qqPlot"), width = 12),
                box(verbatimTextOutput("shapiroTest"), width = 6),
                box(verbatimTextOutput("ljungBoxTest"), width = 6),
                box(verbatimTextOutput("whiteTest"), width = 12)
              )
      ),
      tabItem(tabName = "forecasting",
              fluidRow(
                box(selectInput("forecastMethod", "Forecast Method:",
                                choices = c("Exponential Smoothing", "Holt-Winters", "ARIMA")), width = 4),
                box(actionButton("runForecast", "Forecast"), width = 2)
              ),
              fluidRow(
                box(plotOutput("forecastPlot"), width = 6),
                box(tableOutput("forecastTable"), width = 6)
              ),
              fluidRow(
                box(verbatimTextOutput("forecastMAE"), width = 6),
                box(verbatimTextOutput("forecastAICBIC"), width = 6)
              )
      ),
      tabItem(tabName = "report",
              fluidRow(
                box(downloadButton("downloadReport", "Download Report"), width = 12),
                box(verbatimTextOutput("finalReport"), width = 12)
              )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {
  
  # Reactive for data upload
  dataset <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  # Show column names
  output$columnDisplay <- renderTable({
    req(dataset())
    head(dataset())
  })
  
  output$columnSelector <- renderUI({
    req(dataset())
    selectInput("selectedColumn", "Select Column for Time Series", choices = names(dataset()))
  })
  
  # Reactive for ts object
  ts_data <- reactive({
    req(input$selectedColumn, input$frequency)
    ts(dataset()[[input$selectedColumn]], frequency = input$frequency)
  })
  
  # Decomposition plot
  output$baseDecompPlot <- renderPlot({
    req(ts_data())
    type <- ifelse(input$decompType == "Additive", "additive", "multiplicative")
    plot(decompose(ts_data(), type = type))
  })
  
  # Time series visualization
  output$tsPlot <- renderPlot({
    req(ts_data())
    autoplot(ts_data()) + ggtitle("Original Time Series")
  })
  
  # Transformation
  transformed_data <- reactiveVal()
  
  observeEvent(input$applyTransform, {
    ts_val <- ts_data()
    transform_type <- input$transform
    new_data <- switch(transform_type,
                       "None" = ts_val,
                       "Log" = log(ts_val),
                       "Differencing" = diff(ts_val),
                       "Seasonal Differencing" = diff(ts_val, lag = input$frequency),
                       "Moving Average" = SMA(ts_val, n = input$maWindow))
    transformed_data(new_data)
  })
  
  # Stationarity
  output$decompPlot <- renderPlot({
    req(transformed_data())
    autoplot(transformed_data()) + ggtitle("Transformed Time Series")
  })
  
  output$adfTest <- renderPrint({
    req(transformed_data())
    adf.test(na.omit(transformed_data()))
  })
  
  # ACF/PACF plot
  output$acfPacfPlot <- renderPlot({
    req(transformed_data())
    par(mfrow = c(1, 2))
    acf(na.omit(transformed_data()), main = "ACF")
    pacf(na.omit(transformed_data()), main = "PACF")
  })
  
  # Model fitting
  fitted_model <- reactiveVal()
  
  observeEvent(input$fitModel, {
    ts_val <- na.omit(transformed_data())
    model <- switch(input$modelType,
                    "AR" = arima(ts_val, order = c(input$p_order, 0, 0)),
                    "MA" = arima(ts_val, order = c(0, 0, input$q_order)),
                    "ARMA" = arima(ts_val, order = c(input$p_order, 0, input$q_order)),
                    "ARIMA" = arima(ts_val, order = c(input$p_order, input$d_order, input$q_order)),
                    "SARIMA" = Arima(ts_val, order = c(input$p_order, input$d_order, input$q_order), seasonal = list(order = c(input$P_order, input$D_order, input$Q_order), period = input$seasonality)),
                    "ARCH" = garch(ts_val, order = c(1, 0)),
                    "GARCH" = ugarchfit(spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(input$p_order, input$q_order), include.mean = TRUE)), data = ts_val)
    )
    fitted_model(model)
  })
  
  output$modelSummary <- renderPrint({
    req(fitted_model())
    summary(fitted_model())
  })
  
  # Residual analysis
  output$residualsPlot <- renderPlot({ req(fitted_model()); plot(residuals(fitted_model()), main = "Residuals") })
  output$acfPlot <- renderPlot({ req(fitted_model()); acf(residuals(fitted_model())) })
  output$qqPlot <- renderPlot({ req(fitted_model()); qqnorm(residuals(fitted_model())); qqline(residuals(fitted_model())) })
  output$shapiroTest <- renderPrint({ req(fitted_model()); shapiro.test(residuals(fitted_model())) })
  output$ljungBoxTest <- renderPrint({ req(fitted_model()); Box.test(residuals(fitted_model()), type = "Ljung-Box") })
  output$whiteTest <- renderPrint({ req(fitted_model()); bptest(fitted_model()) })
  
  # Forecasting
  output$forecastPlot <- renderPlot({
    req(fitted_model())
    forecasted <- forecast(fitted_model(), h = 10)
    autoplot(forecasted)
  })
  output$forecastTable <- renderTable({ forecast(fitted_model(), h = 10) })
  output$forecastMAE <- renderPrint({ req(fitted_model()); mean(abs(residuals(fitted_model())), na.rm = TRUE) })
  output$forecastAICBIC <- renderPrint({ req(fitted_model()); c(AIC = AIC(fitted_model()), BIC = BIC(fitted_model())) })
  
  # Report placeholder
  output$finalReport <- renderPrint({ "Analysis complete. Export logic can be integrated with rmarkdown::render() if needed." })
}

shinyApp(ui, server)
