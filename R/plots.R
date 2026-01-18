# R/plots.R
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. 画 3D 曲面图的函数
plot_3d_surface <- function(df) {
  # 确保数据里有我们需要的三列。这里假设第三列是数值列(Rate)
  # 我们先标准化列名方便处理，假设数据清洗步骤已经保证了有 Age 和 Period
  
  # 找到数值列（排除 Age 和 Period 之外的第一列数值）
  val_col <- setdiff(names(df), c("Age", "Period", "Disease", "Sex", "Race"))[1]
  
  if (is.na(val_col)) return(NULL) # 没找到数值列
  
  # 将长数据转为宽矩阵 (Matrix)，这是 plot_ly surface 需要的格式
  # 行是 Age，列是 Period，值是 Rate
  mat_data <- df %>%
    select(Age, Period, Value = all_of(val_col)) %>%
    pivot_wider(names_from = Period, values_from = Value) %>%
    arrange(Age)
  
  # 提取轴标签
  x_axis <- as.numeric(colnames(mat_data)[-1]) # Period
  y_axis <- mat_data$Age                       # Age
  z_matrix <- as.matrix(mat_data[, -1])        # Rate Matrix
  
  # 画图
  plot_ly(x = x_axis, y = y_axis, z = z_matrix) %>%
    add_surface() %>%
    layout(
      title = "APC Descriptive Surface",
      scene = list(
        xaxis = list(title = "Period"),
        yaxis = list(title = "Age"),
        zaxis = list(title = "Rate")
      )
    )
}

# 2. 画 2D 切面图的函数 (Period Slice)
plot_period_slice <- function(df, selected_period) {
  # 同样找到数值列
  val_col <- setdiff(names(df), c("Age", "Period", "Disease", "Sex", "Race"))[1]
  
  # 筛选数据
  plot_data <- df %>% 
    filter(Period == selected_period)
  
  if (nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(x = Age, y = .data[[val_col]])) +
    geom_line(color = "#007bc2", size = 1.2) +
    geom_point(color = "#007bc2", size = 2) +
    theme_minimal() +
    labs(
      title = paste("Section at Period =", selected_period),
      y = "Rate",
      x = "Age"
    )
}