library(haven)
library(readxl)
library(bruceR) 
library(dplyr) # 确保加载 dplyr 用于数据处理

REQUIRED_COLS <- c("Disease", "Age", "Period")

read_and_validate_data <- function(file_path, ext) {
  
  df <- tryCatch({
    # 1. 根据不同格式读取
    raw_data <- if (ext == "rdata") {
      e <- new.env()
      load(file_path, envir = e)
      e[[ls(e)[1]]]
    } else if (ext == "rds") {
      readRDS(file_path)
    } else {
      # 对于 dta, sav, xlsx, csv，使用 bruceR
      bruceR::import(file_path, as = "data.frame")
    }
    
    # 2. 【核心修复】强力清洗数据类型
    # haven::as_factor 会把所有带标签的数字 (例如 Sex=1) 
    # 强制转换为对应的标签文本 (例如 "Male")
    # 这样 DT 包就能识别了
    clean_data <- raw_data
    
    # 如果是 dta/sav，通常会有 label 属性，强制转为因子
    if (ext %in% c("dta", "sav")) {
      clean_data <- haven::as_factor(clean_data)
    }
    
    # 再次确保它是纯粹的 data.frame (去除 tibble 等复杂属性)
    clean_data <- as.data.frame(clean_data)
    
    # 3. 兜底清洗：如果还有列是 haven_labelled 类型，暴力转为向量
    # 这步是为了防止 haven::as_factor 漏网
    clean_data[] <- lapply(clean_data, function(x) {
      if (inherits(x, "haven_labelled")) {
        return(as.vector(x)) # 丢弃标签，只保留数值
      }
      return(x)
    })
    
    clean_data
    
  }, error = function(e) {
    print(paste("Error reading file:", e$message))
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  # 4. 列名匹配 (保持不变)
  current_cols <- colnames(df)
  lower_current <- tolower(current_cols)
  lower_required <- tolower(REQUIRED_COLS)
  
  missing_idx <- which(!lower_required %in% lower_current)
  missing_cols_names <- REQUIRED_COLS[missing_idx]
  
  if (length(missing_cols_names) > 0) {
    attr(df, "error_msg") <- paste0("Missing columns: ", paste(missing_cols_names, collapse = ", "))
    return(df)
  }
  
  # 5. 标准化列名 (保持不变)
  for (req_col in REQUIRED_COLS) {
    match_idx <- which(lower_current == tolower(req_col))
    colnames(df)[match_idx] <- req_col
  }
  
  return(df)
}