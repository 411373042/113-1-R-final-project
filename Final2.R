# 讀取資料
library(readr)
library(dplyr)
library(ggplot2)

# 載入 CSV 檔案
data <- read_csv("1317043558712086528.csv")

# 轉換年份
data <- data |>
  mutate(年份 = as.numeric(stringr::str_extract(`期別/國道`, "\\d+")) + 1911)

# 按年份計算客運收入總和
yearly_income <- data |>
  group_by(年份) |>
  summarize(總收入 = sum(`客運收入（元）/國道`, na.rm = TRUE)) |>
  ungroup()

# 繪製折線圖
ggplot(yearly_income, aes(x = 年份, y = 總收入)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "每年客運收入（元）/國道的變化",
    x = "年份",
    y = "客運收入（元）"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# 讀取資料
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# 載入 CSV 檔案
data <- read_csv("1317043558712086528.csv")

# 轉換民國年到西元年，並新增年份欄位
data <- data |>
  mutate(
    年份 = as.numeric(str_extract(`期別/國道`, "\\d+")) + 1911
  )

# 按年份計算客運收入總和
yearly_income <- data |>
  group_by(年份) |>
  summarize(總收入 = sum(`客運收入（元）/國道`, na.rm = TRUE)) |>
  ungroup()

# 繪製折線圖
ggplot(yearly_income, aes(x = 年份, y = 總收入)) +
  geom_line(color = "blue", linewidth = 1) +  # 使用 linewidth 調整線條寬度
  geom_point(color = "red", size = 2) +
  labs(
    title = "每年客運收入（元）/國道的變化",
    x = "年份",
    y = "客運收入（元）"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 讀取資料
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales) # 用於格式化縱軸數字

# 載入 CSV 檔案
data <- read_csv("1317043558712086528.csv")

# 轉換民國年到西元年，並新增年份欄位
data <- data |>
  mutate(
    年份 = as.numeric(str_extract(`期別/國道`, "\\d+")) + 1911
  )

# 按年份計算客運收入總和
yearly_income <- data |>
  group_by(年份) |>
  summarize(總收入 = sum(`客運收入（元）/國道`, na.rm = TRUE)) |>
  ungroup()

# 繪製折線圖
ggplot(yearly_income, aes(x = 年份, y = 總收入)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "每年客運收入（元）/國道的變化",
    x = "年份",
    y = "客運收入（元）"
  ) +
  scale_y_continuous(labels = scales::comma) + # 縱軸顯示千分位格式
  scale_x_continuous(breaks = yearly_income$年份) + # 顯示所有年份
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 按年份計算平均每一旅客運距總和
yearly_distance <- data |>
  group_by(年份) |>
  summarize(平均每一旅客運距 = mean(`平均每一旅客運距（公里）/國道`, na.rm = TRUE)) |>
  ungroup()

# 繪製長條圖
ggplot(yearly_distance, aes(x = 年份, y = 平均每一旅客運距)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "每年平均每一旅客運距（公里）/國道的變化",
    x = "年份",
    y = "平均每一旅客運距（公里）"
  ) +
  scale_y_continuous(labels = scales::comma) + # 縱軸顯示千分位格式
  scale_x_continuous(breaks = seq(min(yearly_distance$年份), max(yearly_distance$年份), by = 1)) + # 顯示每一年
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

