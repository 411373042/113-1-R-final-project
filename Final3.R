# 讀取CSV檔案並選擇指定的欄位
data <- readr::read_csv("1317043558712086528.csv") 

# 檢查資料的欄位名稱
colnames(data)

# 提取指定欄位
selected_data <- data |>
  dplyr::select(`期別/國道`, `行車次數（班次）/國道`)

# 顯示前幾行資料
head(selected_data)

# 1. 讀取CSV檔案並檢查資料的欄位名稱
data <- readr::read_csv("1317043558712086528.csv")

# 查看解析問題
problems(data)

# 讀取檔案的前幾行
lines <- readLines("1317043558712086528.csv", n = 10)
lines

# 讀取檔案
data <- readr::read_csv("1317043558712086528.csv")

# 檢查解析問題
problems(data)

# 提取指定欄位
selected_data <- data |>
  dplyr::select(`期別/國道`, `行車次數（班次）/國道`)

# 顯示前幾行資料，檢查是否成功提取
head(selected_data)

# 將"期別/國道"欄位中的年份轉換為西元年份
data <- data |>
  dplyr::mutate(western_year = as.numeric(str_sub(`期別/國道`, 1, 3)) + 1911)

# 替換"期別/國道"中的台灣年份，並解析為日期
data <- data |>
  dplyr::mutate(`期別/國道` = stringr::str_replace(`期別/國道`, "^\\d{3}", as.character(western_year)))

# 檢查轉換後的資料
head(data)

# 提取這兩個指定欄位
selected_data <- data |>
  dplyr::select(`期別/國道`, `行車次數（班次）/國道`)

# 顯示前幾行資料
head(selected_data)

# 1. 轉換期別/國道為年份和月份，並提取年份
data <- data |>
  dplyr::mutate(
    # 假設 '期別/國道' 欄位已經是 "100年1月" 這樣的格式，轉換為日期格式
    date = lubridate::ym(`期別/國道`),  # 使用ym函數處理年份和月份
    year = lubridate::year(date),  # 提取年份
    month = lubridate::month(date)  # 提取月份
  )

# 2. 計算每年每月的行車次數（班次）/國道的平均值
data_avg <- data |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(
    avg_trips = mean(`行車次數（班次）/國道`, na.rm = TRUE)  # 計算每月的平均
  ) |>
  dplyr::ungroup()

# 3. 使用 ggplot2 繪製折線圖
library(ggplot2)

ggplot(data_avg, aes(x = month, y = avg_trips, group = year, color = as.factor(year))) +
  geom_line() +  # 折線圖
  labs(
    title = "每年每月行車次數（班次）/國道平均",
    x = "月份",
    y = "行車次數（班次）/國道的平均",
    color = "年份"
  ) +
  theme_minimal() +  # 簡潔風格
  scale_x_continuous(breaks = 1:12, labels = month.name)  # 顯示月份名稱

# 1. 轉換期別/國道為年份
data <- data |>
  dplyr::mutate(
    # 假設 '期別/國道' 欄位已經是 "100年1月" 這樣的格式，轉換為日期格式
    date = lubridate::ym(`期別/國道`),  # 使用ym函數處理年份和月份
    year = lubridate::year(date)  # 提取年份
  )

# 2. 計算每年的行車次數（班次）/國道的平均值
data_avg <- data |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    avg_trips = mean(`行車次數（班次）/國道`, na.rm = TRUE)  # 計算每年的平均
  ) |>
  dplyr::ungroup()

# 3. 使用 ggplot2 繪製折線圖
library(ggplot2)

ggplot(data_avg, aes(x = year, y = avg_trips)) +
  geom_line() +  # 折線圖
  labs(
    title = "每年行車次數（班次）/國道平均",
    x = "年份",
    y = "行車次數（班次）/國道的平均"
  ) +
  theme_minimal() +  # 簡潔風格
  scale_y_continuous(labels = scales::comma)  # 顯示數字，避免科學記號

# 1. 轉換期別/國道為年份
data <- data |>
  dplyr::mutate(
    # 假設 '期別/國道' 欄位已經是 "100年1月" 這樣的格式，轉換為日期格式
    date = lubridate::ym(`期別/國道`),  # 使用ym函數處理年份和月份
    year = lubridate::year(date)  # 提取年份
  )

# 2. 計算每年的行車次數（班次）/國道的平均值
data_avg <- data |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    avg_trips = mean(`行車次數（班次）/國道`, na.rm = TRUE)  # 計算每年的平均
  ) |>
  dplyr::ungroup()

# 3. 使用 ggplot2 繪製折線圖
library(ggplot2)

ggplot(data_avg, aes(x = year, y = avg_trips)) +
  geom_line() +  # 折線圖
  labs(
    title = "每年行車次數（班次）/國道平均",
    x = "年份",
    y = "行車次數（班次）/國道的平均"
  ) +
  theme_minimal() +  # 簡潔風格
  scale_y_continuous(labels = scales::comma) +  # 顯示數字，避免科學記號
  scale_x_continuous(breaks = seq(min(data_avg$year), max(data_avg$year), by = 1))  # 顯示每一個年份

