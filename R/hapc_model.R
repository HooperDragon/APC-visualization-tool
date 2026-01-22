library(dplyr)
library(glmmTMB)
library(emmeans)
library(bruceR)
library(sjPlot)

# ==============================================================================
# 1. 数据预处理 (Dynamic Data Preparation)
# 根据用户的 intervals 动态划分队列，根据上传的数据中心化年龄
# ==============================================================================
prepare_hapc_data <- function(df, params, covariates) {
  # 提取用户设定的参数
  intv <- params$intervals

  # 动态确定年龄基准值 (Centering base)
  # 取用户数据中的最小年龄
  min_age <- min(df$Age, na.rm = TRUE)

  data_model <- df %>%
    mutate(
      # --- A. 计算队列并按用户输入的 Interval 分组 ---
      cohort = Period - Age,

      # 动态计算 Cohort Group：(当前队列 / 间隔) 向下取整 * 间隔
      # 例如：1943年，间隔5年 -> floor(1943/5)*5 = 1940
      cohort_group = floor(cohort / intv) * intv,

      # --- B. 年龄中心化 (Age Centering) ---
      # 减去最小年龄并除以 10 (以 10 年为单位)
      age_c = (Age - min_age) / 10,
      age_c2 = age_c^2,

      # --- C. 核心变量因子化 ---
      period_factor = as.factor(Period),
      cohort_group_factor = as.factor(cohort_group)
    )

  # --- D. 动态处理协变量 (因子化) ---
  # 用户选择的协变量通常是分类变量（如 Sex, Urban_Rural）
  # 我们需要确保它们在建模前被转为因子
  for (cov in covariates) {
    data_model[[cov]] <- as.factor(data_model[[cov]])
  }

  # 记录一下最小年龄，方便后面画图还原
  attr(data_model, "min_age") <- min_age

  return(data_model)
}


# ==============================================================================
# 2. 运行动态 HAPC 模型 (Dynamic Formula - User Defined)
# ==============================================================================
# R/hapc_model.R 中的 run_dynamic_hapc 函数

run_dynamic_hapc <- function(data_model, config) {
  fixed_part <- config$fixed_formula
  if (is.null(fixed_part) || !nzchar(trimws(fixed_part))) {
    fixed_part <- "age_c + age_c2"
  }

  period_slopes <- config$period_slopes
  cohort_slopes <- config$cohort_slopes
  if (is.null(period_slopes)) {
    period_slopes <- character(0)
  }
  if (is.null(cohort_slopes)) {
    cohort_slopes <- character(0)
  }

  re_period <- "(1 | period_factor)"
  if (length(period_slopes) > 0) {
    slope_terms <- paste(period_slopes, collapse = " + ")
    re_period <- paste0("(1 + ", slope_terms, " | period_factor)")
  }

  re_cohort <- "(1 | cohort_group_factor)"
  if (length(cohort_slopes) > 0) {
    slope_terms <- paste(cohort_slopes, collapse = " + ")
    re_cohort <- paste0("(1 + ", slope_terms, " | cohort_group_factor)")
  }

  # 组装最终公式
  final_formula_str <- paste(
    "Disease ~",
    fixed_part,
    "+",
    re_period,
    "+",
    re_cohort
  )

  message("Running HAPC with formula: ", final_formula_str)

  # 5. 运行模型
  model <- tryCatch(
    {
      glmmTMB(
        formula = as.formula(final_formula_str),
        data = data_model,
        family = binomial(link = "logit")
      )
    },
    error = function(e) {
      return(NULL) # 如果直接崩了，返回 NULL
    }
  )

  if (is.null(model)) {
    return(NULL)
  }

  # =========================================================
  # 【新增】检测收敛性 (Check Convergence)
  # =========================================================
  # glmmTMB 的收敛信息在 sdr/fit 等对象里；这里做一个尽量稳健的软判定
  conv_warn <- FALSE
  diag_obj <- tryCatch(glmmTMB::diagnose(model), error = function(e) NULL)
  if (!is.null(diag_obj)) {
    txt <- paste(capture.output(diag_obj), collapse = "\n")
    if (
      grepl(
        "non-positive-definite|cannot|failed|converge",
        txt,
        ignore.case = TRUE
      )
    ) {
      conv_warn <- TRUE
    }
  }
  attr(model, "convergence_warning") <- conv_warn

  return(model)
}


# ==============================================================================
# 3. 提取结果：固定效应 (使用 bruceR)
# 返回的是可以直接在 Shiny 的 HTML() 中渲染的代码
# ==============================================================================
get_fixed_effects_bruceR <- function(model) {
  # 统一使用 sjPlot::tab_model 输出 HTML

  tab <- sjPlot::tab_model(
    model,
    show.se = TRUE,
    show.ci = TRUE,
    show.p = TRUE,
    digits = 3
  )

  return(as.character(tab))
}


# ==============================================================================
# 5. 通用趋势提取函数 (Get Trend Data)
# 用于 Output Module 3
# ==============================================================================
get_model_trend_data <- function(
  model,
  x_axis = "age",
  group_by = "null",
  data_model
) {
  # 1. 提取基础信息
  avg_intercept <- fixef(model)$cond["(Intercept)"]

  # 获取 interval 参数用于生成 cohort 标签
  intv <- 5 # 默认值
  if ("cohort_group" %in% names(data_model)) {
    cohort_vals <- sort(unique(data_model$cohort_group))
    if (length(cohort_vals) > 1) {
      intv <- cohort_vals[2] - cohort_vals[1]
    }
  }

  # glmmTMB 的 ranef 结构：ranef(model, condVar=TRUE) 返回的是一个列表
  # postVar 存储在每个分组的 attr(..., "postVar") 中
  # 注意：glmmTMB 与 lme4 不同，需要直接从 ranef 对象获取
  rr_full <- ranef(model, condVar = TRUE)
  rr <- rr_full$cond # 提取条件随机效应部分

  res_df <- NULL

  # === 情况 A: X 轴是 Age (固定效应) ===
  if (x_axis == "age") {
    # 构造年龄网格
    min_age <- attr(data_model, "min_age")
    max_c <- max(data_model$age_c, na.rm = TRUE)

    # 用 for 循环逐点调用 emmeans，避免笛卡尔积导致参考网格爆炸
    res_list <- list()

    # 如果需要分层，构造带分组的 specs
    if (group_by != "null") {
      specs <- as.formula(paste("~ age_c + age_c2 |", group_by))
    } else {
      specs <- ~ age_c + age_c2
    }

    # 遍历 age_c 从 0 到 max_c，步长 1（即每 10 岁一个点）
    for (ac in seq(0, floor(max_c * 2), by = 1)) {
      emm <- emmeans(
        model,
        specs,
        at = list(age_c = ac / 2, age_c2 = (ac / 2)^2),
        type = "response"
      )
      res_list[[length(res_list) + 1]] <- as.data.frame(summary(emm))
    }

    res_df <- bind_rows(res_list) %>%
      rename(prob = prob, lower = asymp.LCL, upper = asymp.UCL) %>%
      mutate(x_val = age_c * 10 + min_age) # 还原真实年龄

    # 统一分组列名
    if (group_by != "null") {
      res_df$group <- res_df[[group_by]]
    } else {
      res_df$group <- "Overall"
    }

    # === 情况 B: X 轴是 Period 或 Cohort (随机效应) ===
  } else {
    # 确定目标随机效应名称（兼容不同命名方式）
    re_name <- if (x_axis == "period") {
      # 尝试两种可能的命名
      if ("period_factor" %in% names(rr)) "period_factor" else "period"
    } else {
      if ("cohort_group_factor" %in% names(rr)) {
        "cohort_group_factor"
      } else {
        "cohort_group"
      }
    }

    # 提取该组的随机效应数据框
    re_df <- rr[[re_name]]

    if (is.null(re_df)) {
      # 如果找不到对应的随机效应，返回 NULL
      return(NULL)
    }

    # 提取行名作为 X 轴的值
    x_values <- as.numeric(rownames(re_df))

    # glmmTMB 获取随机效应标准误：从 model$sdr$diag.cov.random
    # 需要确定当前随机效应在 sdr$par.random 中的位置
    sdr <- model$sdr

    # 获取随机效应的标准误
    if (!is.null(sdr) && !is.null(sdr$diag.cov.random)) {
      all_se <- sqrt(sdr$diag.cov.random)

      # 确定当前分组的随机效应在总随机效应中的位置
      # glmmTMB 的随机效应按照 ranef 的顺序排列
      re_names <- names(rr)
      start_idx <- 1
      for (nm in re_names) {
        if (nm == re_name) {
          break
        }
        start_idx <- start_idx + nrow(rr[[nm]])
      }
      end_idx <- start_idx + nrow(re_df) - 1

      u_se <- all_se[start_idx:end_idx]
    } else {
      # 无法获取标准误，设为 0
      u_se <- rep(0, nrow(re_df))
    }

    # --- B1. 总体趋势 (Null Stratification) ---
    if (group_by == "null") {
      # 提取截距的随机效应 (Intercept)
      u_est <- re_df$`(Intercept)`

      # 计算概率
      est_total <- avg_intercept + u_est
      res_df <- data.frame(
        x_val = x_values,
        prob = plogis(est_total),
        lower = plogis(est_total - 1.96 * u_se),
        upper = plogis(est_total + 1.96 * u_se),
        group = "Overall"
      )

      # --- B2. 分层趋势 (Stratified) ---
    } else {
      # 检查该变量是否存在于随机斜率中
      # 列名通常是 "sex1", "urban_rural1" 等。需要模糊匹配
      col_names <- colnames(re_df)
      target_col <- grep(group_by, col_names, value = TRUE)

      if (length(target_col) == 0) {
        # 如果模型里没有设这个变量的随机斜率，返回空，提示用户
        return(NULL)
      }

      # 提取截距 (Intercept) 和 斜率 (Slope)
      u_intercept <- re_df$`(Intercept)`
      u_slope <- re_df[[target_col[1]]] # 暂时只处理第一个匹配到的 level

      # 注意：这里需要加上固定效应的主效应
      # 比如 Group=1 (Reference), Total = Fixed_Int + Rand_Int
      # Group=2, Total = Fixed_Int + Fixed_Slope + Rand_Int + Rand_Slope

      # 为了简化，我们假设 group_by 是二分类变量 (0/1 或 1/2)
      # 这是一个简化的可视化逻辑，严谨计算需要对照 model matrix

      # 提取该变量的固定效应系数
      fix_slope_name <- grep(group_by, names(fixef(model)$cond), value = TRUE)
      fix_slope <- if (length(fix_slope_name) > 0) {
        fixef(model)$cond[fix_slope_name]
      } else {
        0
      }

      # 组1 (Ref): 仅截距
      df_ref <- data.frame(
        x_val = x_values,
        est = avg_intercept + u_intercept,
        group = "Reference" # 通常是 0 或 1
      )

      # 组2 (Level): 截距 + 斜率
      df_level <- data.frame(
        x_val = x_values,
        est = (avg_intercept + fix_slope) + (u_intercept + u_slope),
        group = "Level 2" # 通常是 1 或 2
      )

      # 合并计算概率
      res_df <- rbind(df_ref, df_level) %>%
        mutate(
          prob = plogis(est),
          # 简化的 CI 计算 (略过协方差，仅作示意)
          lower = prob,
          upper = prob
        )
    }

    # --- 为 Cohort 添加 x_label (格式: "xxxx-xxxx") ---
    if (x_axis == "cohort") {
      res_df <- res_df %>%
        mutate(x_label = paste0(x_val, "-", x_val + intv - 1))
    }
  }

  return(res_df)
}
