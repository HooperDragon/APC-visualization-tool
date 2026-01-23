# 函数：获取描述性分析数据
# 输入：
#   - df: 清洗后的原始数据框 (必须包含 Disease, Age, Period)
#   - params: 一个包含 intervals, age_start, age_end, period_start, period_end 的列表
# 输出：
#   - 一个聚合后的数据框，包含 age_group, cohort_group, period, rate 等列

get_descriptive_data <- function(df, params) {
  # 1. 提取参数
  intv <- params$intervals
  age_s <- params$age_start
  age_e <- params$age_end
  pd_s <- params$period_start
  pd_e <- params$period_end

  # 2. 基础过滤：根据用户设置的范围筛选数据
  # 这一步已经在 app.R 的 reactive 中做过一部分，但在核心计算函数里再做一次保险
  df_filtered <- df %>%
    filter(
      Age >= age_s,
      Age <= age_e,
      Period >= pd_s,
      Period <= pd_e
    )

  if (nrow(df_filtered) == 0) {
    return(NULL)
  }

  # 3. 分组计算核心逻辑
  df_grouped <- df_filtered %>%
    mutate(
      # --- A. 划分 Age Group ---
      # 逻辑：(当前年龄 - 起始年龄) / 间隔，取整，再乘回去。
      # 例如：Start=0, Int=5. Age=2 -> (2-0)/5=0.4 -> floor=0 -> 0*5+0 = 0 (组的起点)
      Age_Start = floor((Age - age_s) / intv) * intv + age_s,

      # 创建易读的标签，例如 "0-4"
      # 注意：如果间隔是1，标签就是 "0"
      Age_Label = if (intv == 1) {
        as.character(Age_Start)
      } else {
        paste0(Age_Start, "-", Age_Start + intv - 1)
      }
    ) %>%
    # --- B. 聚合计算 (Group by Age_Group & Period) ---
    group_by(Age_Start, Age_Label, Period) %>%
    summarise(
      # 计算发病率：Disease 列中 1 是阳性，0 是阴性
      # mean(Disease) 即为发病率 (Rate)
      Rate = 100 * mean(Disease, na.rm = TRUE),

      # 同时也计算一下样本量，万一以后要算置信区间
      # 生成跨期的基础特征表（按列百分比）
      # 参数:
      #  - df: 原始数据框，必须包含 period 列 (默认 'Period') 和疾病列 (默认 'Disease')
      #  - vars: 要汇总的变量名向量，例如 c('sex','residence','age_group',...)
      #  - period_col: 期次列名，默认 'Period'
      #  - disease_col: 疾病列名，默认 'Disease'（期待 1=发生, 0=未发生）
      #  - age_breaks: 若 vars 包含 'age_group'，用于生成区间边界，默认 c(65,80,90,105)
      #  - digits: 百分比保留位数，默认 1
      # 返回: 一个宽表 data.frame，列为 Period 1..K（依数据顺序），每个单元为 "n(%.1f)"，最后一行为 Total
      Count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # --- C. 计算 Cohort Group ---
      # 在聚合后计算 Cohort。
      # 逻辑：Cohort = Period - Age。
      # 这里使用 Age_Start (组的起点) 来代表该组年龄，计算出的 Cohort 代表该组的"最晚出生年份"
      # 为了方便画图和理解，我们通常用：Cohort = Period - Age_Start
      Cohort_Raw = Period - Age_Start,

      # 对 Cohort 也进行同样的间隔划分
      # 这里的 Cohort_Start 是指队列组的起始年份
      # 我们不设 cohort_start 参数，通常以 1900 或数据中的最小值为基准，
      # 但为了对齐，我们直接对 Cohort_Raw 进行取整
      Cohort_Start = floor(Cohort_Raw / intv) * intv,

      # 创建 Cohort 标签，例如 "1980-1984"
      Cohort_Label = if (intv == 1) {
        as.character(Cohort_Start)
      } else {
        paste0(Cohort_Start, "-", Cohort_Start + intv - 1)
      }
    ) %>%
    # --- D. 整理列名和顺序 ---
    select(
      age_group = Age_Label, # 字符标签列 (用于显示)
      cohort_group = Cohort_Label, # 字符标签列 (用于显示)
      period = Period, # 原始时期 (数字)
      rate = Rate, # 结果 (数字)

      # 保留辅助列，方便画图排序 (Plotly 画图喜欢用数字轴)
      age_start = Age_Start,
      cohort_start = Cohort_Start,
      N = Count
    ) %>%
    arrange(age_start, period)

  return(df_grouped)
}


# 生成基础特征表（用于下载）
# 参数:
#  - df: 原始数据框，必须包含疾病列 (默认 'Disease')
#  - vars: 要汇总的变量名向量，例如 c('sex','residence','age_group',...)
#  - disease_col: 疾病列名，默认 'Disease'（期待 1=发生, 0=未发生）
#  - age_breaks: 若需要生成 age_group，传入分组边界向量例如 c(65,80,90,105)
# 返回: 一个 data.frame 包含列: variable, level, N, disease (格式 "n(%.2f)")
get_basic_characteristics <- function(
  df,
  vars,
  disease_col = "Disease",
  age_breaks = c(65, 80, 90, 105)
) {
  if (missing(vars) || length(vars) == 0) {
    stop("Please provide variable names in 'vars'.")
  }
  if (!disease_col %in% names(df)) {
    stop(paste0("disease column not found in data: ", disease_col))
  }

  results <- list()

  for (v in vars) {
    # 处理 age_group 专用逻辑
    if (v == "age_group" && "Age" %in% names(df)) {
      br <- age_breaks
      if (length(br) < 2) {
        stop("age_breaks must contain at least two values")
      }
      labels <- vapply(
        seq_len(length(br) - 1),
        function(i) paste0(br[i], "-", br[i + 1]),
        FUN.VALUE = ""
      )

      # 向量化映射：根据 Age 返回对应标签或 NA
      map_age_level <- function(age_vec) {
        sapply(
          age_vec,
          function(a) {
            found <- NA_character_
            for (i in seq_len(length(br) - 1)) {
              if (!is.na(a) && a >= br[i] && a <= br[i + 1]) {
                found <- labels[i]
                break
              }
            }
            found
          },
          USE.NAMES = FALSE
        )
      }

      df_use <- df %>% mutate(.level = map_age_level(Age))
      lvl_col <- ".level"
    } else {
      if (!v %in% names(df)) {
        warning(paste0("Variable not found in data: ", v, " - skipped"))
        next
      }
      df_use <- df
      lvl_col <- v
    }

    # 计算每个水平的样本量与疾病数
    tab <- df_use %>%
      filter(!is.na(.data[[lvl_col]])) %>%
      group_by_at(lvl_col) %>%
      summarise(
        N = n(),
        disease_n = sum(.data[[disease_col]] == 1, na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(tab) == 0) {
      next
    }

    # 计算百分比并构造字符串
    tab <- tab %>%
      mutate(
        pct = ifelse(N > 0, 100 * disease_n / N, NA_real_),
        disease = ifelse(
          is.na(pct),
          paste0(disease_n, "(NA)"),
          paste0(disease_n, "(", formatC(pct, format = "f", digits = 2), ")")
        )
      )

    # 将分组列改名为 level
    names(tab)[1] <- "level"

    # 添加变量名列并选择顺序
    tab <- tab %>%
      mutate(variable = v) %>%
      select(variable, level, N, disease)

    results[[length(results) + 1]] <- tab
  }

  res_df <- bind_rows(results)
  return(res_df)
}


# 生成跨期的基础特征表（按列百分比）
# 参数:
#  - df: 原始数据框，必须包含 period 列 (默认 'Period') 和疾病列 (默认 'Disease')
#  - vars: 要汇总的变量名向量，例如 c('sex','residence','age_group',...)
#  - period_col: 期次列名，默认 'Period'
#  - disease_col: 疾病列名，默认 'Disease'（期待 1=发生, 0=未发生）
#  - age_breaks: 若 vars 包含 'age_group'，用于生成区间边界，默认 c(65,80,90,105)
#  - digits: 百分比保留位数，默认 1
# 返回: 一个宽表 data.frame，列为 Period 1..K（依数据顺序），每个单元为 "n(%.1f)"，最后一行为 Total
get_basic_characteristics_by_period <- function(
  df,
  vars,
  period_col = "Period",
  disease_col = "Disease",
  age_breaks = c(65, 80, 90, 105),
  digits = 1
) {
  if (missing(vars) || length(vars) == 0) {
    stop("Please provide variable names in 'vars'.")
  }
  if (!period_col %in% names(df)) {
    stop(paste0("period column not found in data: ", period_col))
  }
  if (!disease_col %in% names(df)) {
    stop(paste0("disease column not found in data: ", disease_col))
  }

  periods <- sort(unique(df[[period_col]]))
  if (length(periods) == 0) {
    return(NULL)
  }
  period_names <- paste0("Period ", seq_along(periods))

  # 每期总样本量，用于列百分比
  totals <- df %>%
    group_by(.data[[period_col]]) %>%
    summarise(total_n = sum(!is.na(.data[[period_col]])), .groups = "drop") %>%
    arrange(match(.data[[period_col]], periods))

  results <- list()

  for (v in vars) {
    # 处理 age_group 专用逻辑
    if (v == "age_group" && "Age" %in% names(df)) {
      br <- age_breaks
      if (length(br) < 2) {
        stop("age_breaks must contain at least two values")
      }
      labels <- vapply(
        seq_len(length(br) - 1),
        function(i) paste0(br[i], "-", br[i + 1]),
        FUN.VALUE = ""
      )
      map_age_level <- function(a) {
        found <- NA_character_
        for (i in seq_len(length(br) - 1)) {
          if (!is.na(a) && a >= br[i] && a <= br[i + 1]) {
            found <- labels[i]
            break
          }
        }
        found
      }
      df_use <- df %>%
        mutate(.level = vapply(Age, map_age_level, FUN.VALUE = ""))
      lvl_col <- ".level"
    } else {
      if (!v %in% names(df)) {
        warning(paste0("Variable not found in data: ", v, " - skipped"))
        next
      }
      df_use <- df
      lvl_col <- v
    }

    # 计算每期每个水平的计数
    tab_long <- df_use %>%
      filter(!is.na(.data[[lvl_col]])) %>%
      group_by_at(c(period_col, lvl_col)) %>%
      summarise(n = n(), .groups = "drop")

    if (nrow(tab_long) == 0) {
      next
    }

    # 合并总量并计算列百分比
    tab_long <- tab_long %>%
      mutate(period_idx = match(.data[[period_col]], periods)) %>%
      left_join(totals, by = setNames(period_col, period_col)) %>%
      mutate(pct = ifelse(total_n > 0, 100 * n / total_n, NA_real_)) %>%
      arrange(period_idx)

    # 格式化为 "n(%.1f)"
    tab_long <- tab_long %>%
      mutate(
        cell = ifelse(
          is.na(pct),
          as.character(n),
          paste0(n, "(", formatC(pct, format = "f", digits = digits), ")")
        )
      )

    # 转为宽表
    tab_wide <- tab_long %>%
      dplyr::select(level = !!rlang::sym(lvl_col), period_idx, cell) %>%
      tidyr::pivot_wider(names_from = period_idx, values_from = cell)

    # 补足缺失期列
    for (i in seq_along(periods)) {
      colname <- as.character(i)
      if (!colname %in% names(tab_wide)) tab_wide[[colname]] <- ""
    }
    tab_wide <- tab_wide %>%
      dplyr::select(level, as.character(seq_along(periods)))
    names(tab_wide)[-1] <- period_names

    tab_wide <- tab_wide %>%
      dplyr::mutate(variable = v) %>%
      dplyr::select(variable, dplyr::everything())

    results[[length(results) + 1]] <- tab_wide
  }

  if (length(results) == 0) {
    return(NULL)
  }

  res_all <- dplyr::bind_rows(results)

  # 添加 Total 行
  total_row <- tibble::tibble(variable = "Total", level = "")
  for (i in seq_along(periods)) {
    total_row[[period_names[i]]] <- as.character(totals$total_n[i])
  }

  res_final <- dplyr::bind_rows(res_all, total_row)
  return(res_final)
}
