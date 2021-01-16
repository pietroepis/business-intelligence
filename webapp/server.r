server <- function(input, output) {
  # Merge selected stocks return series to show plot
  multiple_dataset <- reactive({
    ds = c();
    
    if (!is.null(input$chk_stocks))
      for (i in 1:length(input$chk_stocks)){
        if (input$chk_stocks[i] == "SBUX")
          ds <- cbind(ds, stocks_cc_returns$SBUX)
        else if (input$chk_stocks[i] == "MCD")
          ds <- cbind(ds, stocks_cc_returns$MCD)
        else if (input$chk_stocks[i] == "AXP")
          ds <- cbind(ds, stocks_cc_returns$AXP)
        else if (input$chk_stocks[i] == "AON")
          ds <- cbind(ds, stocks_cc_returns$AON)
        else if (input$chk_stocks[i] == "KO")
          ds <- cbind(ds, stocks_cc_returns$KO)
        else if (input$chk_stocks[i] == "PEP")
          ds <- cbind(ds, stocks_cc_returns$PEP)
      }
    
      ds <- window(ds, start = input$date_range[1], end = input$date_range[2])
  })
  
  # Return data related to the selected stock
  single_dataset <- reactive({
    if (input$radio_stocks == "SBUX")
      ds <- stocks_cc_returns$SBUX
    else if (input$radio_stocks == "MCD")
      ds <- stocks_cc_returns$MCD
    else if (input$radio_stocks == "AXP")
      ds <- stocks_cc_returns$AXP
    else if (input$radio_stocks == "AON")
      ds <- stocks_cc_returns$AON
    else if (input$radio_stocks == "KO")
      ds <- stocks_cc_returns$KO
    else if (input$radio_stocks == "PEP")
      ds <- stocks_cc_returns$PEP
    
    ds <- window(ds, start = input$date_range[1], end = input$date_range[2])
  })
  
  # Return predicted returns
  predicted_dataset <- reactive({
    if (input$select_stock_forecast == "SBUX")
      ds <- SBUX_pred_returns
    else if (input$select_stock_forecast == "MCD")
      ds <- MCD_pred_returns
    else if (input$select_stock_forecast == "AXP")
      ds <- AXP_pred_returns
    else if (input$select_stock_forecast == "AON")
      ds <- AON_pred_returns
    else if (input$select_stock_forecast == "KO")
      ds <- KO_pred_returns
    else if (input$select_stock_forecast == "PEP")
      ds <- PEP_pred_returns
  })
  
  # Calculate number of stocks to buy in relation to selected budget and optimal weights
  n_stocks_reactive <- reactive({
    budget = input$budget
    
    SBUX_nshares <- floor((budget * MOP$pw[1]) / (as.numeric(stocks_initial_prices$SBUX) + as.numeric(stocks_initial_prices$SBUX) * transaction_cost))
    MCD_nshares <- floor((budget * MOP$pw[2]) / (as.numeric(stocks_initial_prices$MCD) + as.numeric(stocks_initial_prices$MCD) * transaction_cost))
    AXP_nshares <- floor((budget * MOP$pw[3]) / (as.numeric(stocks_initial_prices$AXP) + as.numeric(stocks_initial_prices$AXP) * transaction_cost))
    AON_nshares <- floor((budget * MOP$pw[4]) / (as.numeric(stocks_initial_prices$AON) + as.numeric(stocks_initial_prices$AON) * transaction_cost))
    KO_nshares <- floor((budget * MOP$pw[5]) / (as.numeric(stocks_initial_prices$KO) + as.numeric(stocks_initial_prices$KO) * transaction_cost))
    PEP_nshares <- floor((budget * MOP$pw[6]) / (as.numeric(stocks_initial_prices$PEP) + as.numeric(stocks_initial_prices$PEP) * transaction_cost))
    
    cbind(SBUX_nshares, MCD_nshares, AXP_nshares, AON_nshares, KO_nshares, PEP_nshares)
  })
  
  # Calculate actual invested wealth (in relation to selected budget)
  budget_reactive <- reactive({
    budget = input$budget
    
    SBUX_nshares <- floor((budget * MOP$pw[1]) / (as.numeric(stocks_initial_prices$SBUX) + as.numeric(stocks_initial_prices$SBUX) * transaction_cost))
    MCD_nshares <- floor((budget * MOP$pw[2]) / (as.numeric(stocks_initial_prices$MCD) + as.numeric(stocks_initial_prices$MCD) * transaction_cost))
    AXP_nshares <- floor((budget * MOP$pw[3]) / (as.numeric(stocks_initial_prices$AXP) + as.numeric(stocks_initial_prices$AXP) * transaction_cost))
    AON_nshares <- floor((budget * MOP$pw[4]) / (as.numeric(stocks_initial_prices$AON) + as.numeric(stocks_initial_prices$AON) * transaction_cost))
    KO_nshares <- floor((budget * MOP$pw[5]) / (as.numeric(stocks_initial_prices$KO) + as.numeric(stocks_initial_prices$KO) * transaction_cost))
    PEP_nshares <- floor((budget * MOP$pw[6]) / (as.numeric(stocks_initial_prices$PEP) + as.numeric(stocks_initial_prices$PEP) * transaction_cost))

    investment <- round(SBUX_nshares * as.numeric(stocks_initial_prices$SBUX), 2) +
      round(MCD_nshares * as.numeric(stocks_initial_prices$MCD), 2) +
      round(AXP_nshares * as.numeric(stocks_initial_prices$AXP), 2) +
      round(AON_nshares * as.numeric(stocks_initial_prices$AON), 2) +
      round(KO_nshares * as.numeric(stocks_initial_prices$KO), 2) +
      round(PEP_nshares * as.numeric(stocks_initial_prices$PEP), 2)
  })
  
  # Statistical Indices table
  output$descriptive_statistics <- renderTable({
    table <- data.frame(mean(single_dataset()), var(single_dataset()), sd(single_dataset()),
                        skewness(single_dataset()), kurtosis(single_dataset()))
    colnames(table) <- c("MEAN", "VARIANCE", "STANDARD DEVIATION", "SKEWNESS", "KURTOSIS")
    table
  })
  
  # Histogram
  output$plot_hist <- renderPlot({
    hist(
      single_dataset(),
      xlab = "Return",
      main = paste("CC Returns Histogram -", input$radio_stocks)
    )
    
    if (input$chk_density)
      lines(density(single_dataset()), col = "cadetblue", lwd = 2)
  })
  
  # Probability Density Function
  output$plot_pdf <- renderPlot({
    plot(density(single_dataset()), type="l", lwd = 2, col = "cadetblue", xlab = "Return", ylab = "Probability", 
         main = paste("Probability Density -", input$radio_stocks))
  })
  
  # Boxplot
  output$plot_boxplot <- renderPlot({
    boxplot(as.numeric(single_dataset()), horizontal = T, col = "cadetblue", 
            main = paste("Boxplot -", input$radio_stocks))
  })
  
  # Q-Q Plot
  output$plot_qqplot <- renderPlot({
    qqnorm(single_dataset(), main = paste("Boxplot -", input$radio_stocks))
    qqline(single_dataset(), lwd = 2, col = "cadetblue")
  })
  
  # Merged stocks plot
  output$plot_returns <- renderPlot({
    plot(multiple_dataset(), xlab = "Date", ylab = "Return", main = "CC Returns")
  })
  
  # MOP weights Pie Chart
  output$plot_stocks_pie <- renderPlot({
    pie(round(MOP$pw, 2), main = "Pesi degli Stock", labels = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"))
  })
  
  # Set total invested wealth text label
  output$tot_investito<-renderText({
    paste0(round(budget_reactive(), 2), " EUR")
  })
  
  # Set budget residual text label
  output$residuo<-renderText({
    paste0(round(input$budget - budget_reactive(), 2), " EUR")
  })
  
  # Number of stocks BarPlot
  output$plot_n_stocks<-renderPlot({
    barplot(n_stocks_reactive(), names.arg = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"))
  })
  
  # Predicted Returns Plot
  output$plot_prediction<-renderPlot({
    plot(predicted_dataset())
  })
}