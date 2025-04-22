

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
