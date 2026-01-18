# 定义常量
REQUIRED_COLS <- c("Disease", "Age", "Period")

# 函数：读取并清洗数据
# 输入：file_path (字符串), ext (字符串)
# 输出：Dataframe (成功) 或 NULL (失败)
read_and_validate_data <- function(file_path, ext) {
  # 1. 读取文件
  df <- tryCatch(
    {
      switch(
        ext,
        "csv" = read.csv(file_path, stringsAsFactors = FALSE),
        "dta" = haven::read_dta(file_path),
        "sav" = haven::read_sav(file_path),
        "xlsx" = readxl::read_xlsx(file_path),
        "xls" = readxl::read_xls(file_path),
        "rdata" = {
          e <- new.env()
          load(file_path, envir = e)
          e[[ls(e)[1]]]
        },
        "rds" = readRDS(file_path),
        NULL
      )
    },
    error = function(e) NULL
  )

  if (is.null(df)) {
    return(NULL)
  }

  df <- as.data.frame(df)

  # 2. 列名匹配
  current_cols <- colnames(df)
  lower_current <- tolower(current_cols)
  lower_required <- tolower(REQUIRED_COLS)

  missing_idx <- which(!lower_required %in% lower_current)
  missing_cols_names <- REQUIRED_COLS[missing_idx]

  if (length(missing_cols_names) > 0) {
    # 如果有错，把错误信息作为属性附带在对象上，或者返回特定结构
    attr(df, "error_msg") <- paste0(
      "Missing columns: ",
      paste(missing_cols_names, collapse = ", ")
    )
    return(df)
  }

  # 3. 标准化列名
  for (req_col in REQUIRED_COLS) {
    match_idx <- which(lower_current == tolower(req_col))
    colnames(df)[match_idx] <- req_col
  }

  return(df)
}
