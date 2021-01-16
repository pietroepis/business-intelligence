ui <- fluidPage(
  #### DESCRIPTIVE ANALYTICS ####
  titlePanel("Descriptive Analytics"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        inputId = "date_range", 
        label = "Period Range", 
        start = index(stocks_cc_returns[1,]), 
        end = index(stocks_cc_returns[dim(stocks_cc_returns)[1],])
      ),
      
      radioButtons(
        inputId = "radio_stocks", 
        label="Stock",
        choices = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
      ),
      
      selectInput(
        inputId = "select_plot_type",
        label="Plot Type",
        choices = c("Histogram", "PDF", "Boxplot", "QQPlot")
      ),
      
      conditionalPanel(
        condition = "input.select_plot_type == 'Histogram'",
        checkboxInput(
          inputId = "chk_density",
          label = strong("Show Probability Density"),
          value = FALSE
        )
      )
    ),
    mainPanel(
      tableOutput("descriptive_statistics"),
      conditionalPanel(
        condition = "input.select_plot_type == 'Histogram'",
        plotOutput(outputId = "plot_hist", height = "400px")
      ),
      conditionalPanel(
        condition = "input.select_plot_type == 'PDF'",
        plotOutput(outputId = "plot_pdf", height = "400px")
      ),
      conditionalPanel(
        condition = "input.select_plot_type == 'Boxplot'",
        plotOutput(outputId = "plot_boxplot", height = "400px")
      ),
      conditionalPanel(
        condition = "input.select_plot_type == 'QQPlot'",
        plotOutput(outputId = "plot_qqplot", height = "400px")
      ),
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "chk_stocks",
        label = "Stocks", 
        selected = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP"),
        choices = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "plot_returns", height = "400px")
    )
  ),
  
  #### PORTFOLIO MANAGEMENT ####
  titlePanel("Portfolio Management"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "budget", label="Budget", 
                  min = 0, max = 100000,
                  value = c(50000), pre = "EUR "),
      plotOutput(outputId = "plot_stocks_pie"),
      br(), strong(paste0("Prezzi degli stock al ", format(date_to %m+% months(-10), "%d/%m/%Y"))), br(),
      "SBUX: ", round(stocks_initial_prices$SBUX, 2), " EUR", br(),
      "MCD: ", round(stocks_initial_prices$MCD, 2), " EUR", br(),
      "AXP: ", round(stocks_initial_prices$AXP, 2), " EUR", br(),
      "AON: ", round(stocks_initial_prices$AON, 2), " EUR", br(),
      "KO: ", round(stocks_initial_prices$KO, 2), " EUR", br(),
      "PEP: ", round(stocks_initial_prices$PEP, 2), " EUR", br(),
    ),
    
    mainPanel(
      "Totale Investito: ", textOutput("tot_investito"), br(),
      "Residuo: ", textOutput("residuo"), br(),
      
      plotOutput(outputId = "plot_n_stocks", height = "400px")
    )
  ),
  
  #### PREDICTIVE ANALYTICS ####
  titlePanel("Predictive Analytics"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "select_stock_forecast",
        label="Select Stock",
        choices = c("SBUX", "MCD", "AXP", "AON", "KO", "PEP")
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot_prediction", height = "400px")
    )
  )
)