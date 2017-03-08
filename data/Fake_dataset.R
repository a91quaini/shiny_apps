# 2 factor model
# daily data
# random betas
# 200 assets
# 300 months
# compute monthly returns
# monthly realized variance
# monthly realized betas
# name the stocks from A to Z then AA AZ etc
# put everything in a df with columns name, date, 
# value_type (return, realized_beta, realized_variance), frequency, value
# so that it's tidy data
library(combinat)
library(magrittr)
library(tidyverse)
library(lubridate)
library(doParallel)
# set parallel backend
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)

# set date
n_months <- 300
last_date <- Sys.Date() %m-% months(1)
day(last_date) <- days_in_month(month(last_date))
initial_date <- last_date %m-% months(n_months - 1)
day(initial_date) <- 1
fake_daily_dates <- seq.Date(from = initial_date, 
                       to = last_date,
                       by = "days")
fake_monthly_dates <- seq.Date(from = initial_date, 
                               to = last_date,
                               by = "months")
n_days <- length(fake_daily_dates)
sequence_days_in_months <- as.numeric(days_in_month(fake_monthly_dates))
sequence_cumulative_days_in_months <- as.numeric(cumsum(sequence_days_in_months))

# set model
n_assets <- 20
n_factors <- 2
mean_factors <- c(2.9e-4, 3.5e-4)
sd_factors <- c(1.3e-3, 1.9e-3)
mean_epsilon <- mean_factors[2] - mean_factors[1]
sd_epsilon <- max( sqrt(sd_factors[2]^2 - sd_factors[1]^2), 0)
sd_errors <- 0.4e-3
set_seed <- 127
set.seed(set_seed)

factor1 <- rnorm(n_days, mean_factors[1], sd_factors[1])
factor2 <-  factor1 + rnorm(n_days, mean_epsilon, sd_epsilon)
Factors <- cbind(factor1, factor2)

min_beta <- 0
max_beta1 <- 2
max_beta2 <- 3
betas1 <- runif(n_assets, min_beta, max_beta1)
betas2 <- runif(n_assets, min_beta, max_beta2)
Betas <- cbind(betas1, betas2)

Errors <- foreach(i=1:n_days, .combine=rbind) %dopar% rnorm(n_assets, 0, sd_errors)

Daily_assets_returns <- Factors %*% t(Betas) + Errors

# compute monthly quantities
monthly_assets_returns <- foreach(i=1:n_months, .combine=rbind) %dopar% {
  j <- sequence_cumulative_days_in_months[i]
  z <- sequence_days_in_months[i]
  apply(Daily_assets_returns[(j-z+1):j, ] + 1, 2, prod) - 1
}
Monthly_assets_returns <- matrix(NA, nrow = n_days, ncol = n_assets)
Monthly_assets_returns[sequence_cumulative_days_in_months, ] <- monthly_assets_returns

monthly_assets_realized_variance <- foreach(i=1:n_months, .combine=rbind) %dopar% {
  j <- sequence_cumulative_days_in_months[i]
  z <- sequence_days_in_months[i]  
  apply(Daily_assets_returns[(j-z+1):j, ]^2, 2, sum)
}
Monthly_assets_realized_variance <- matrix(NA, nrow = n_days, ncol = n_assets)
Monthly_assets_realized_variance[sequence_cumulative_days_in_months, ] <- monthly_assets_realized_variance

monthly_factor1_realized_variance <- foreach(i=1:n_months, .combine=rbind) %dopar% {
  j <- sequence_cumulative_days_in_months[i]
  z <- sequence_days_in_months[i]  
  sum(factor1[(j-z+1):j]^2)  
}
monthly_realized_assets_factor1_covariance <- foreach(i=1:n_months, .combine=rbind) %dopar% {
  j <- sequence_cumulative_days_in_months[i]
  z <- sequence_days_in_months[i]
  apply(Daily_assets_returns[(j-z+1):j, ] * factor1[(j-z+1):j], 2, sum)
}
monthly_realized_betas1 <- foreach(j=1:n_assets, .combine=cbind) %dopar% {
  monthly_realized_assets_factor1_covariance[, j] / monthly_factor1_realized_variance
}
Monthly_realized_beta1 <- matrix(NA, nrow = n_days, ncol = n_assets)
Monthly_realized_beta1[sequence_cumulative_days_in_months, ] <- monthly_realized_betas1

stopCluster(cl)

# set tidy data
assets_chr_vector <- permn(c(LETTERS[1:4], letters[1:4]))[1:n_assets] %>%
  unlist() %>%
  matrix(nrow = n_assets) %>%
  apply(1, paste, collapse="")
Assets_chr_vector <- rep(assets_chr_vector, each = n_days)
id_vector <- seq(from = 11111, to = 99999, by = 1)[1:n_assets]
Id_vector <- rep(id_vector, each = n_days)
currency_vector <- sample(c("USD", "EUR", "YEN"), n_assets, replace = TRUE)
Currency_vector <- rep(currency_vector, each = n_days)
Fake_daily_dates <- rep(fake_daily_dates, n_assets)

time_series_data <- tibble(
  id = Id_vector,
  name = Assets_chr_vector, 
  currency = Currency_vector,
  date = Fake_daily_dates,
  daily_return = c(Daily_assets_returns),
  monthly_return = c(Monthly_assets_returns),
  monthly_RV = c(Monthly_assets_realized_variance),
  monthly_RB = c(Monthly_realized_beta1)
  ) %>% 
    gather(daily_return, monthly_return, monthly_RV, monthly_RB, key = "data_type", value = "value") %>%
    drop_na() %>%
    separate(col = data_type, into = c("frequency", "value_type"), sep="_")

write_csv(time_series_data, "time_series_data.csv")


country_tag_vector <- foreach(i = 1:n_assets, .combine = c) %do% {
  if (currency_vector[i] == "USD") {"US_stocks"}
  else if (currency_vector[i] == "EUR") {"EUR_stocks"}
  else {"CHN_stocks"}
}
type_tag <- paste(c("SMALL", "BIG", "VALUE"), "stocks", sep = "_")
type_tag_vector <- sample(type_tag, n_assets, replace = TRUE)
new_id_vector <- rep(id_vector, 2)
tag_vector <- c(country_tag_vector, type_tag_vector)

tagging_data <- tibble(
  id = new_id_vector,
  country_tag = tag_vector
)

write_csv(tagging_data, "tagging_data.csv")
