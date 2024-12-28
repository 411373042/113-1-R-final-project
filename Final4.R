# 讀取檔案
data <- readr::read_csv("1317043558712086528.csv")

# 篩選所需欄位
selected_data <- data |> 
  dplyr::select(`期別/國道`, `行車次數（班次）/國道`, `客運收入（元）/國道`)

# 查看前幾列
head(selected_data)

library(lubridate)
library(ggplot2)

# 轉換民國年到西元年，並新增 `year` 欄位
data <- data |> 
  dplyr::mutate(
    `西元年` = as.numeric(stringr::str_sub(`期別/國道`, 1, 3)) + 1911
  )

# 按年份匯總數據
yearly_data <- data |> 
  dplyr::group_by(`西元年`) |> 
  dplyr::summarise(
    total_trips = sum(`行車次數（班次）/國道`, na.rm = TRUE),
    total_income = sum(`客運收入（元）/國道`, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

# 計算相關係數
correlation <- cor(yearly_data$total_trips, yearly_data$total_income)
print(paste("相關係數:", correlation))

# 繪製散佈圖與回歸線
ggplot(yearly_data, aes(x = total_trips, y = total_income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "行車次數與客運收入的相關性",
    x = "行車次數（班次）",
    y = "客運收入（元）"
  ) +
  theme_minimal()

library(lubridate)
library(ggplot2)
library(scales)

# 轉換民國年到西元年，並新增 `year` 欄位
data <- data |> 
  dplyr::mutate(
    `西元年` = as.numeric(stringr::str_sub(`期別/國道`, 1, 3)) + 1911
  )

# 按年份匯總數據
yearly_data <- data |> 
  dplyr::group_by(`西元年`) |> 
  dplyr::summarise(
    total_trips = sum(`行車次數（班次）/國道`, na.rm = TRUE),
    total_income = sum(`客運收入（元）/國道`, na.rm = TRUE)
  ) |> 
  dplyr::ungroup()

# 計算相關係數
correlation <- cor(yearly_data$total_trips, yearly_data$total_income)
print(paste("相關係數:", correlation))

# 繪製散佈圖與回歸線
ggplot(yearly_data, aes(x = total_trips, y = total_income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "行車次數與客運收入的相關性",
    x = "行車次數（班次）",
    y = "客運收入（元）"
  ) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal()

library(scales)

ggplot(yearly_data, aes(x = total_trips, y = total_income, color = factor(`西元年`))) +
  geom_point(size = 3) + # 调整点的大小
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "行车次数与客运收入的相关性",
    x = "行车次数（班次）",
    y = "客运收入（元）",
    color = "年份" # 图例标题
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

