library("openxlsx")
library("data.table")
library("purrr")
library("Rcpp")
library("magrittr")
library("lubridate")
library("shiny")
library("shinythemes")
library("DT")
library("plotly")
library("ggthemes")
library("scales")

# Static Data -------------------------------------------------------------

loan_dt <- readWorkbook("data/alexis-interest-accrual.xlsx", sheet = "Loans Summary") %>%
  as.data.table()

loan_dt[, share_of_payment := amount / sum(amount)]

# https://www.ifa.com/articles/with_stock_returns_normally_distributed/
volatility_params <- list(
    "IFA-5"   = list(mean = 0.32, sd = 0.53)
  , "IFA-50"  = list(mean = 0.60, sd = 2.24)
  , "IFA-100" = list(mean = 0.90, sd = 4.53)
  , "S&P"     = list(mean = 0.91, sd = 4.10)
)

set.seed(1)

# Functions ---------------------------------------------------------------

# https://studentaid.ed.gov/sa/types/loans/interest-rates
sourceCpp("src/calc-balance.cpp")
# sourceCpp("src/calc-compound-interest.cpp")

compoundIntWithDeposits <- function(rates, deposits) {
  results <- numeric(length(rates))
  ix <- (seq_along(rates) - 1L)[-1L]
  for (i in ix) {
    results[i + 1] = deposits[i] + results[i] * (1.0 + rates[i])
  }
  return(results)
}

vline <- function(x = 0, color = "gray") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 100E6, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

# App ---------------------------------------------------------------------

ui <- navbarPage(
    title = "Loan Explorer"
  , theme = shinytheme("united")
  , tabPanel(
      title = "Summary"
    , dataTableOutput("summary")
  )
  , tabPanel(
    title = "Explore",
    sidebarLayout(
      sidebarPanel(
          h4("Loans")
        , numericInput("n_years", "# Years to Consider", min = 5L, max = 50L, step = 5L, value = 20L)
        , numericInput("loan_percent", "% of Salary to Loan", min = 1L, max = 50L, value = 10L, step = 1L)
        , hr()
        , h4("Salary")
        , sliderInput("start_salary", "Starting Salary (in $10K)", min = 50L, max = 150L, value = 80L, step = 5L)
        , numericInput("yearly_raise", "Average Yearly Raise (%)", min = 1L, max = 20L, value = 3L, step = 1L)
        , hr()
        , h4("Investment")
        , numericInput("invest_percent", "% of Salary to Invest", min = 1L, max = 50L, value = 10L, step = 1L)
        , numericInput("return_percent", "Avg. Rate of Return", min = -10, max = 50L, value = 10L, step = 0.5)
        , radioButtons("volatility", "Volatility Profile", choices = c("High" = 5, "Med" = 2, "Low" = 0.5), inline = TRUE)
        , checkboxInput("subtract_discharge", "Subtract Discharge from Worth")
      ) #/ sidebarPanel
      , mainPanel(
        tabsetPanel(
          tabPanel("Summary", uiOutput("summary_ui"))
          , tabPanel("Payments", uiOutput("payments_ui"))
          , tabPanel("Salary", uiOutput("salary_ui"))
        )
      ) #/ mainPanel
    ) #/ sidebarLayout
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  today <- Sys.Date()
  
  loans <- reactive({
    loan_dt
  })
  
  payment_months <- reactive({
    seq(today, today + years(input$n_years), by = "month")
  })
  
  month_dt <- reactive({
    data.table(date = payment_months())
  })

  days_in_payment_months <- reactive({
    days_in_month(payment_months())
  })
  
  days_in_payment_years  <- reactive({
    map_int(payment_months(), ~ifelse(leap_year(.), 366L, 365L))
  })
  
  monthly_interest_rates <- reactive({
    tcrossprod(loans()$interest / 100, days_in_payment_months() / days_in_payment_years())
  })
  
  salary_estimates <- reactive({
    k_sal <- input$start_salary * (1L + input$yearly_raise / 100L) ^ (0:(input$n_years - 1L))
    plyr::round_any(k_sal, accuracy = 10, f = floor) * 1E3
  })
  
  monthly_sched_payments <- reactive({
    rep(salary_estimates() * input$loan_percent / 100L / 12L, each = 12L)
  })
  
  balances_and_payment <- reactive({
    getIntervalBalance(loans()$amount, monthly_interest_rates(), monthly_sched_payments())
  })
  
  monthly_balances <- reactive({
    balances_and_payment()[['balances']]
  })
  
  monthly_actual_payments <- reactive({
    balances_and_payment()[['payments']]
  })
  
  return_rates <- reactive({
    custom <- list(mean = input$return_percent / 12, sd = as.numeric(input$volatility))
    c(volatility_params, custom = list(custom)) %>% 
      map(~append(., list(n = length(payment_months())))) %>%
      map(~do.call(rnorm, args = .)) %>%
      map(~ . / 100)
  })
  
  invested <- reactive({
    rep(salary_estimates() * input$invest_percent / 100L / 12L, each = 12L)
  })
  
  invested_worth <- reactive({
    map(return_rates(), ~compoundIntWithDeposits(rates = ., deposits = invested())) %>%
      map(~cbind(month_dt(), data.table(value = .))) %>%
      rbindlist(idcol = "variable")
  })
  
  output$summary         <- renderDataTable({
    loans()
  })
  
  output$salary_plot <- renderPlotly({
    m <- data.table(
        date   = seq(today, today + years(input$n_years), by = "year")
      , salary = c(1E3 * input$start_salary, salary_estimates())
    )
    
    p <- ggplot(m, aes(x = date, y = salary)) +
      geom_line() + 
      geom_point(aes(text = dollar(salary))) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(breaks = pretty_breaks(10), labels = dollar) +
      theme_tufte() +
      theme(axis.ticks = element_blank(), legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "", y = "", title = "Projected Salary")
    
    ggplotly(p, tooltip = "text")
    
  })
  
  output$salary_ui <- renderUI({
    plotlyOutput("salary_plot", height = "85vh")
  })
  
  loans_balance <- reactive({
    colSums(monthly_balances())
  })
  
  loans_paid <- reactive({
    c(0, colSums(monthly_actual_payments()) %>% cumsum())
  })
  
  summary_dt <- reactive({
    
    discharge_dt <- data.table(
        date          = payment_months()
      , discharge     = round(0.37 * loans_balance())
    )
    
    summary <- data.table(
      date          = payment_months()
      , invested      = c(0, cumsum(invested()))
      , loans_balance = loans_balance()
      , loans_paid    = loans_paid()
    )
    
    m_summary <- rbind(
      melt.data.table(summary, id.vars = "date")
      , invested_worth()
    ) %>%
      merge(discharge_dt, by = "date")
    
    m_summary[, label_text := sprintf("%s: %s", variable, dollar(value))]
    m_summary[variable == "loans_paid", `:=`(
      label_text = sprintf("%s: %s + %s = %s<br>(if discharged in %d)", variable, dollar(value), dollar(discharge), dollar(discharge + value), year(date))
      , value = value + discharge
    )]
    
    return(m_summary)
    
  })
  
  output$summary_plot <- renderPlotly({
    
    m_summary <- copy(summary_dt())
    
    return_names <- c(names(volatility_params), "custom")
    if (input$subtract_discharge) {
      m_summary[variable %in% return_names, value := value - discharge]
    }
    
    m_summary[variable %in% return_names, label_text := sprintf("%s: %s", variable, dollar(value))]
    
    # vline_pos <- seq(today, today + years(input$n_years), by = "5 year")[-1L]
    p <- ggplot(
        data    = m_summary
      , aes(
          x      = date
        , y      = value
        , colour = variable
      )
    ) +
      geom_step(alpha = 0.75) + 
      geom_point(aes(text = label_text), size = 0.01) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(breaks = pretty_breaks(10), labels = dollar) +
      theme_tufte() +
      theme(axis.ticks = element_blank(), legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "", y = "", title = "Cumulative Total Loan Balance, Total Paid and Investments")
      # geom_vline(xintercept = vline_pos, colour = "grey")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summary_ui <- renderUI({
    plotlyOutput("summary_plot", height = "85vh")
  })
  
  output$payments_plot <- renderPlotly({
    
    summary <- data.table(
        date          = payment_months()
      , loan    = c(0, colSums(monthly_actual_payments()))
      , invest  = invested()
    ) %>% melt.data.table(id.vars = "date")
    
    p <- ggplot(summary, aes(x = date, y = value, fill = variable)) +
      geom_bar(aes(text = dollar(value)), stat = "identity") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(breaks = pretty_breaks(10), labels = dollar) +
      theme_tufte() +
      theme(axis.ticks = element_blank(), legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "", y = "", title = "Projected Monthly Payment")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$payments_ui <- renderUI({
    plotlyOutput("payments_plot", height = "85vh")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
