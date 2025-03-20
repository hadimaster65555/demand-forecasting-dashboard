library(shiny)
library(tidyverse)
library(bslib)
library(echarts4r)

raw_data <- read_csv(file = "data/retail_store_inventory.csv")

raw_data %>% 
  janitor::clean_names() -> raw_data

ui <- page_navbar(
  title = "ImajinEyes",
  sidebar = NULL,
  theme = bs_theme(bootswatch = "cosmo"),
  header = waiter::use_waiter(),
  nav_panel(
    title = "Dashboard",
    h1("Demand Analysis and Forecast"),
    layout_column_wrap(
      dateRangeInput(
        inputId = "dashboard_daterange",
        label = "Select Date Range",
        start = raw_data$date %>% max() - 30,
        end = raw_data$date %>% max()
      ),
      selectInput(
        inputId = "dashboard_storefilter",
        label = "Select Store",
        choices = raw_data$store_id %>% unique(),
        multiple = T,
        selected = raw_data$store_id %>% unique()
      ),
      selectInput(
        inputId = "dashboard_categoryfilter",
        label = "Select Category",
        choices = raw_data$category %>% unique(),
        selected = raw_data$category %>% unique(),
        multiple = T
      )
    ),
    h2("Overview"),
    layout_column_wrap(
      width = 1/3,
      uiOutput(outputId = "dashboard_totalsoldall"),
      uiOutput(outputId = "dashboard_totalsoldpercategory"),
      uiOutput(outputId = "dashboard_totalsoldperregion")
    ),
    layout_column_wrap(
      width = 1,
      uiOutput(outputId = "dashboard_totalsoldperstore")
    )
  )
)

server <- function(input, output, session) {
  
  dashboard_state <- reactiveValues(
    sales_data_used = NULL
  )
  
  observe({
    dashboard_state$sales_data_used <- raw_data %>% 
      filter(date >= input$dashboard_daterange[1], date <= input$dashboard_daterange[2]) %>% 
      filter(store_id %in% input$dashboard_storefilter) %>% 
      filter(category %in% input$dashboard_categoryfilter)
    
    print(dashboard_state$sales_data_used)
  })
  
  output$dashboard_totalsoldall <- renderUI({
    card(
      card_title("Total Product Sold"),
      card_body_fill(
        echarts4rOutput(outputId = "dashboard_totalSoldAllLineChart")
      )
    )
  })
  
  output$dashboard_totalsoldpercategory <- renderUI({
    card(
      card_title("Total Product Sold per Category"),
      card_body_fill(
        echarts4rOutput(outputId = "dashboard_totalSoldPerCategoryBarChart")
      )
    )
  })
  
  output$dashboard_totalsoldperregion <- renderUI({
    card(
      card_title("Total Product Sold per Region"),
      card_body_fill(
        echarts4rOutput(outputId = "dashboard_totalSoldPerRegionBarChart")
      )
    )
  })
  
  output$dashboard_totalsoldperstore <- renderUI({
    card(
      card_title("Total Product Sold per Store"),
      card_body_fill(
        echarts4rOutput(outputId = "dashboard_totalSoldPerStoreLineChart")
      )
    )
  })
  
  output$dashboard_totalSoldAllLineChart <- renderEcharts4r({
    dashboard_state$sales_data_used %>% 
      group_by(date) %>% 
      summarise(total_sold = sum(units_sold)) %>% 
      ungroup() %>% 
      e_chart(date) %>% 
      e_line(total_sold) %>% 
      e_tooltip(trigger = "axis")
  })
  
  output$dashboard_totalSoldPerCategoryBarChart <- renderEcharts4r({
    dashboard_state$sales_data_used %>% 
      group_by(category) %>% 
      summarise(total_sold = sum(units_sold)) %>% 
      ungroup() %>% 
      arrange(total_sold) %>% 
      e_charts(category) %>% 
      e_bar(total_sold) %>% 
      e_flip_coords() %>% 
      e_tooltip(trigger = "item")
  })
  
  output$dashboard_totalSoldPerRegionBarChart <- renderEcharts4r({
    dashboard_state$sales_data_used %>% 
      group_by(region) %>% 
      summarise(total_sold = sum(units_sold)) %>% 
      ungroup() %>% 
      arrange(total_sold) %>% 
      e_charts(region) %>% 
      e_bar(total_sold) %>% 
      e_flip_coords() %>% 
      e_tooltip(trigger = "item")
  })
  
  output$dashboard_totalSoldPerStoreLineChart <- renderEcharts4r({
    dashboard_state$sales_data_used %>% 
      group_by(date, store_id) %>% 
      summarise(total_sold = sum(units_sold)) %>% 
      ungroup() %>% 
      group_by(store_id) %>% 
      e_chart(date) %>% 
      e_line(total_sold) %>% 
      e_tooltip(trigger = "axis")
  })
  
}

shinyApp(ui, server)