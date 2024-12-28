# 讀取指定欄位
data <- readr::read_csv("1317043558712086528.csv", 
                        col_select = c(`期別/國道`, `平均每一旅客運距（公里）/國道`))

# 查看資料
print(data)

data <- readr::read_csv("1317043558712086528.csv", 
                        col_select = c(`期別/國道`, `平均每一旅客運距（公里）/國道`), 
                        locale = readr::locale(encoding = "UTF-8"))

# 確認資料範圍
range(data$`期別/國道`)

# 讀取完整檔案
full_data <- readr::read_csv("1317043558712086528.csv", locale = readr::locale(encoding = "UTF-8"))

# 檢查資料筆數及範圍
print(dim(full_data))  # 資料筆數與欄位
print(head(full_data)) # 前幾筆資料
print(range(full_data$`期別/國道`, na.rm = TRUE)) # 期別範圍

# 讀取完整檔案
data <- readr::read_csv("1317043558712086528.csv", locale = readr::locale(encoding = "UTF-8"))

# 列出所有資料
print(data)

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# Prepare UI
ui <- fluidPage(
  titlePanel("月份成長折線圖"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", 
                  label = "選擇年度",
                  choices = NULL, # 動態更新年份
                  selected = NULL)
    ),
    
    mainPanel(
      plotOutput("growth_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # 預處理資料
  processed_data <- reactive({
    data |>
      mutate(年 = as.numeric(str_extract(`期別/國道`, "^[0-9]+")),
             月 = str_extract(`期別/國道`, "[0-9]+月") |> str_remove("月") |> as.numeric()) |>
      group_by(年, 月) |>
      summarise(`平均每一旅客運距（公里）` = mean(`平均每一旅客運距（公里）/國道`, na.rm = TRUE), .groups = "drop")
  })
  
  # 更新年份選項
  observe({
    updateSelectInput(session, 
                      "selected_year", 
                      choices = unique(processed_data()$年))
  })
  
  # 繪製折線圖
  output$growth_plot <- renderPlot({
    req(input$selected_year)  # 確保已選擇年份
    
    # 篩選所選年度資料
    plot_data <- processed_data() |>
      filter(年 == input$selected_year)
    
    # 繪製折線圖
    ggplot(plot_data, aes(x = 月, y = `平均每一旅客運距（公里）`)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(
        title = paste("月份成長折線圖 -", input$selected_year, "年"),
        x = "月份",
        y = "平均每一旅客運距（公里）"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# Prepare UI
ui <- fluidPage(
  titlePanel("月份成長折線圖"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", 
                  label = "選擇年度",
                  choices = NULL, # 動態更新年份
                  selected = NULL)
    ),
    
    mainPanel(
      plotOutput("growth_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # 預處理資料
  processed_data <- reactive({
    data |>
      mutate(年 = as.numeric(str_extract(`期別/國道`, "^[0-9]+")),
             月 = str_extract(`期別/國道`, "[0-9]+月") |> str_remove("月") |> as.numeric()) |>
      group_by(年, 月) |>
      summarise(`平均每一旅客運距（公里）` = mean(`平均每一旅客運距（公里）/國道`, na.rm = TRUE), .groups = "drop")
  })
  
  # 更新年份選項
  observe({
    updateSelectInput(session, 
                      "selected_year", 
                      choices = unique(processed_data()$年))
  })
  
  # 繪製折線圖
  output$growth_plot <- renderPlot({
    req(input$selected_year)  # 確保已選擇年份
    
    # 篩選所選年度資料
    plot_data <- processed_data() |>
      filter(年 == input$selected_year)
    
    # 繪製折線圖
    ggplot(plot_data, aes(x = 月, y = `平均每一旅客運距（公里）`)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      scale_x_continuous(breaks = 1:12, labels = paste0(1:12, "月")) + # 設定每個月份為刻度
      labs(
        title = paste("月份成長折線圖 -", input$selected_year, "年"),
        x = "月份",
        y = "平均每一旅客運距（公里）"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
