# R/config.R
# 全局配置：默认参数和共享 CSS 样式

# 默认参数
DEFAULTS <- list(
  period_start = 2000,
  period_end = 2025,

  age_start = 0,
  age_end = 105,
  intervals = 5
)

# 全局 CSS 样式（卡片、按钮等）
APP_CSS <- "
/* 卡片样式 */
.card-style {
  background-color: #ffffff;
  border: 2px solid #333;
  border-radius: 5px;
  box-shadow: 5px 5px 0px rgba(0,0,0,0.2);
  padding: 20px;
  margin-bottom: 20px;
  height: 100%;
}

/* 运行按钮样式 */
#run_btn {
  width: 100%;
  font-size: 18px;
  font-weight: bold;
  margin-top: 20px;
  height: 50px;
}

h3 {
  margin-top: 0px;
  border-bottom: 1px solid #eee;
  padding-bottom: 10px;
  margin-bottom: 20px;
}

/* 弹窗按钮样式：红色确认，蓝色取消 */
#confirm_age { background-color: #d9534f; color: white; }
#cancel_age { background-color: #5bc0de; color: white; }
"
