library(fable)
library(tsibble)
library(feasts)
library(tsibbledata)
library(lubridate)
library(dplyr)

raw_data <- read_csv(file = "data/retail_store_inventory.csv")

raw_data %>% 
  janitor::clean_names() -> raw_data

raw_data %>% 
  group_by(date) %>% 
  summarise(total_sales = sum(units_sold)) %>% 
  ungroup() %>% 
  as_tsibble() -> total_sales_data


total_sales_data %>% 
  autoplot()

total_sales_data %>% 
  filter(date >= "2023-07-01") %>% 
  autoplot()

total_sales_data %>% 
  filter(date >= "2023-01-01") %>% 
  model(
    naive = SNAIVE(total_sales ~ lag("14 days"))
  ) %>% 
  forecast(
    h = "3 weeks"
  ) %>%
  autoplot(
    filter(total_sales_data, date >= "2023-07-01"),
    level = NULL
  )

do_forecast <- function(raw_data) {
  raw_data %>% 
    as_tsibble() -> ts_data
  
  ts_data %>% 
    model(
      naive = SNAIVE(total_sales ~ lag("14 days"))
    ) %>% 
    forecast(
      h = "2 weeks"
    ) %>% 
    as_tibble() -> forecast_result
  
  raw_data %>% 
    mutate(type = "real_data") -> raw_data
  
  forecast_result %>% 
    select(date, .mean) %>% 
    rename(total_sales = .mean) %>% 
    mutate(type = "forecast") -> forecast_result
  
  raw_data %>% 
    bind_rows(forecast_result) -> all_result
  
  return(all_result)
}

raw_data %>% 
  group_by(date) %>% 
  summarise(total_sales = sum(units_sold)) %>% 
  ungroup() -> temp_result

do_forecast(temp_result) %>% 
  group_by(type) %>% 
  e_charts(date) %>% 
  e_line(total_sales)
