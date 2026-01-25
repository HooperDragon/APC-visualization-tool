# R/server_logic.R
# 核心业务逻辑：运行分析、协变量解析等

#' 解析公式中的协变量
#' @param formula_str 固定效应公式字符串
#' @param period_slopes Period 随机斜率变量
#' @param cohort_slopes Cohort 随机斜率变量
#' @return 所有协变量名的向量
parse_covariates <- function(
  formula_str,
  period_slopes = NULL,
  cohort_slopes = NULL
) {
  formula_vars <- unique(unlist(strsplit(formula_str, "[\\+\\*\\:]|\\s+")))
  formula_vars <- setdiff(formula_vars, c("age_c", "age_c2", "", "1", "0"))
  unique(c(formula_vars, period_slopes, cohort_slopes))
}

#' 执行 HAPC 分析流程
#' @param raw_df 原始数据
#' @param params 参数列表（包含 intervals）
#' @param fixed_formula 固定效应公式
#' @param period_slopes Period 随机斜率
#' @param cohort_slopes Cohort 随机斜率
#' @return list(model, data_for_model, summary_table, success, message)
run_analysis <- function(
  raw_df,
  params,
  fixed_formula,
  period_slopes,
  cohort_slopes
) {
  # 1. 解析协变量
  all_covariates <- parse_covariates(
    fixed_formula,
    period_slopes,
    cohort_slopes
  )

  # 2. 数据预处理
  data_for_model <- prepare_hapc_data(
    raw_df,
    list(intervals = params$intervals),
    all_covariates
  )

  # 3. 构建模型配置
  model_config <- list(
    fixed_formula = fixed_formula,
    period_slopes = period_slopes,
    cohort_slopes = cohort_slopes
  )

  # 4. 运行模型
  model <- tryCatch(
    run_dynamic_hapc(data_for_model, model_config),
    error = function(e) NULL
  )

  # 5. 检查结果
  if (is.null(model)) {
    return(list(
      model = NULL,
      data_for_model = data_for_model,
      summary_table = NULL,
      success = FALSE,
      message = "Model failed to run. Please check your formula or data."
    ))
  }

  # 6. 收敛性检查
  converged <- !isTRUE(attr(model, "convergence_warning"))
  msg <- if (converged) {
    "Model converged successfully!"
  } else {
    "Warning: Model failed to converge. Please simplify interactions."
  }

  # 7. 生成摘要表
  summary_table <- tryCatch(
    get_fixed_effects_bruceR(model),
    error = function(e) NULL
  )

  list(
    model = model,
    data_for_model = data_for_model,
    summary_table = summary_table,
    success = TRUE,
    converged = converged,
    message = msg
  )
}
