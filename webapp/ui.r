ui <- fluidPage(
  titlePanel("Descriptive Analytics"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        inputId = "date_range", 
        label = "Period Range", 
        start = index(stocks_cc_returns[1,]), 
        end = Sys.Date()
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
  )
)