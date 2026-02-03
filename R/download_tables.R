# 将模型的参数整理为表格（适合导出/显示）
# 返回一个 list 包含：fixed (data.frame), random (data.frame), bic (numeric)
get_model_results_table <- function(model, digits = 2, z = 1.96) {
  if (is.null(model)) {
    return(NULL)
  }

  # 固定效应系数与协方差
  coefs <- fixef(model)$cond
  vc <- tryCatch(vcov(model)$cond, error = function(e) NULL)

  if (is.null(vc)) {
    se <- rep(NA_real_, length(coefs))
  } else {
    se <- sqrt(diag(vc))
  }

  # 计算 OR 和 CI
  est <- as.numeric(coefs)
  lower <- est - z * se
  upper <- est + z * se

  or <- exp(est)
  or_l <- exp(lower)
  or_u <- exp(upper)

  fmt_num <- function(x, d) formatC(x, format = "f", digits = d)

  fixed_tbl <- data.frame(
    term = names(coefs),
    OR = fmt_num(or, digits),
    lower = ifelse(is.na(or_l), NA, fmt_num(or_l, digits)),
    upper = ifelse(is.na(or_u), NA, fmt_num(or_u, digits)),
    stringsAsFactors = FALSE
  )

  fixed_tbl <- fixed_tbl %>%
    mutate(
      value = ifelse(
        is.na(lower),
        paste0(OR),
        paste0(OR, " (", lower, ",", upper, ")")
      )
    ) %>%
    select(term, value)

  # 美化 term 名称：替换 ':' -> ' × ', '_' -> ' '
  fixed_tbl$term <- gsub(":", " × ", fixed_tbl$term)
  fixed_tbl$term <- gsub("_", " ", fixed_tbl$term)

  # 随机效应方差：基于 ranef 的 (Intercept) 方差作为方差分量估计
  re_full <- tryCatch(ranef(model, condVar = TRUE)$cond, error = function(e) {
    NULL
  })
  random_tbl <- NULL
  if (!is.null(re_full)) {
    re_names <- names(re_full)
    rand_list <- list()
    for (nm in re_names) {
      re_df <- re_full[[nm]]
      if ("(Intercept)" %in% colnames(re_df)) {
        var_comp <- var(re_df$`(Intercept)`, na.rm = TRUE)
        rand_list[[nm]] <- data.frame(
          effect = nm,
          variance = var_comp,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rand_list) > 0) random_tbl <- bind_rows(rand_list)
  }

  # BIC
  bic_val <- tryCatch(BIC(model), error = function(e) NA_real_)

  return(list(fixed = fixed_tbl, random = random_tbl, bic = bic_val))
}
