library(lubridate)
n_months <- 30
last_date <- Sys.Date() %m-% months(1)
day(last_date) <- days_in_month(month(last_date))
initial_date <- last_date %m-% months(n_months - 1)
day(initial_date) <- 1
monthly_dates <- seq.Date(from = initial_date, 
                               to = last_date,
                               by = "months")
monthly_dates <- c(monthly_dates, monthly_dates)
stocks <- c("apple", "orange")
stocks <- rep(stocks, each = n_months)
prices <- runif(n_months * 2, 1, 2) %>% round(digits = 2)
expected_returns <- runif(n_months * 2, 1.2, 1.5) %>% round(digits = 2)
dividend_per_shares <- runif(n_months * 2, 0.5, 1) %>% round(digits = 2)
dividend_growth_rates <- runif(n_months * 2, 0.9, 1.1) %>% round(digits = 2)

gordon_data <- tibble::tibble(
  date = monthly_dates,
  name = stocks,
  price = prices,
  expected_return = expected_returns,
  dividend_per_share = dividend_per_shares,
  dividend_growth_rate = dividend_growth_rates
) %>% 
  reshape2::melt(id.vars = c("date", "name"))

names(gordon_data)[3] <- "data_type"
