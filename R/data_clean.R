library(haven)
library(readxl)
library(bruceR) 
library(dplyr)

REQUIRED_COLS <- c("Disease", "Age", "Period")

read_and_validate_data <- function(file_path, ext) {
  
  df <- tryCatch({
    # 1. 读取数据
    raw_data <- if (ext == "rdata") {
      e <- new.env()
      load(file_path, envir = e)
      e[[ls(e)[1]]]
    } else if (ext == "rds") {
      readRDS(file_path)
    } else {
      bruceR::import(file_path, as = "data.frame")
    }
    
    # 2. 初步清洗：处理 haven_labelled 标签
    clean_data <- raw_data
    
    # 只有当它是 dta/sav 这种带标签的格式时，尝试转换因子
    # 这样能保留 Sex="Male" 这种可读性
    if (ext %in% c("dta", "sav")) {
      clean_data <- haven::as_factor(clean_data)
    }
    
    clean_data <- as.data.frame(clean_data)
    
    # 兜底：去除残留的 haven_labelled 属性
    clean_data[] <- lapply(clean_data, function(x) {
      if (inherits(x, "haven_labelled")) as.vector(x) else x
    })
    
    clean_data
    
  }, error = function(e) {
    print(paste("Error reading file:", e$message))
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  # 3. 列名匹配与重命名
  current_cols <- colnames(df)
  lower_current <- tolower(current_cols)
  lower_required <- tolower(REQUIRED_COLS)
  
  missing_idx <- which(!lower_required %in% lower_current)
  missing_cols_names <- REQUIRED_COLS[missing_idx]
  
  if (length(missing_cols_names) > 0) {
    attr(df, "error_msg") <- paste0("Missing columns: ", paste(missing_cols_names, collapse = ", "))
    return(df)
  }
  
  # 标准化核心列名 (Age, Period, Disease)
  for (req_col in REQUIRED_COLS) {
    match_idx <- which(lower_current == tolower(req_col))
    colnames(df)[match_idx] <- req_col
  }
  
  # =========================================================
  # 【核心修复】强制类型转换 (Fix Factor Error)
  # =========================================================
  # 无论之前发生了什么，我们必须保证这三列是纯数字
  
  # 辅助函数：安全地转为数字
  # 如果本来是 Factor (例如 "2000"), 直接 as.numeric 会变成索引值 (1)，
  # 必须先 as.character 转回文本 ("2000") 再转数字 (2000)。
  to_numeric_safe <- function(x) {
    if (is.factor(x)) {
      as.numeric(as.character(x))
    } else {
      as.numeric(x)
    }
  }
  
  # 批量转换核心列
  df$Age     <- to_numeric_safe(df$Age)
  df$Period  <- to_numeric_safe(df$Period)
  df$Disease <- to_numeric_safe(df$Disease)
  
  # 移除转换过程中产生的 NA 行 (防止脏数据导致计算报错)
  # 比如 Age 只有一半有值，剩下的计算也没意义
  df <- df %>% filter(!is.na(Age) & !is.na(Period) & !is.na(Disease))
  
  return(df)
}