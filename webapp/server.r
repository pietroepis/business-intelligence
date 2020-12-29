server <- function(input, output) {
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
    
    cat(paste(input$date_range[1], input$date_range[2]))
    ds
    #ds <- window(ds, input$date_range[1], input$date_range[2])
  })
  
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
    
    #ds <- window(ds, input$date_range[1], input$date_range[2])
  })
  
  output$descriptive_statistics <- renderTable({
    table <- data.frame(mean(single_dataset()), var(single_dataset()), sd(single_dataset()),
                        skewness(single_dataset()), kurtosis(single_dataset()))
    colnames(table) <- c("MEAN", "VARIANCE", "STANDARD DEVIATION", "SKEWNESS", "KURTOSIS")
    table
  })
  
  output$plot_hist <- renderPlot({
    hist(
      single_dataset(),
      xlab = "Return",
      main = paste("CC Returns Histogram -", input$radio_stocks)
    )
    
    if (input$chk_density)
      lines(density(single_dataset()), col = "cadetblue", lwd = 2)
  })
  
  output$plot_pdf <- renderPlot({
    plot(density(single_dataset()), type="l", lwd = 2, col = "cadetblue", xlab = "Return", ylab = "Probability", 
         main = paste("Probability Density -", input$radio_stocks))
  })
  
  output$plot_boxplot <- renderPlot({
    boxplot(as.numeric(single_dataset()), horizontal = T, col = "cadetblue", 
            main = paste("Boxplot -", input$radio_stocks))
  })
    
  output$plot_qqplot <- renderPlot({
    qqnorm(single_dataset(), main = paste("Boxplot -", input$radio_stocks))
    qqline(single_dataset(), lwd = 2, col = "cadetblue")
  })
  
  output$plot_returns <- renderPlot({
    plot(multiple_dataset(), xlab = "Date", ylab = "Return", main = "CC Returns")
  
  })
}