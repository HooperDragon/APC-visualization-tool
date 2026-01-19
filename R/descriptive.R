library(dplyr)

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
      Age >= age_s, Age <= age_e,
      Period >= pd_s, Period <= pd_e
    )
  
  if (nrow(df_filtered) == 0) return(NULL)
  
  # 3. 分组计算核心逻辑
  df_grouped <- df_filtered %>%
    mutate(
      # --- A. 划分 Age Group ---
      # 逻辑：(当前年龄 - 起始年龄) / 间隔，取整，再乘回去。
      # 例如：Start=0, Int=5. Age=2 -> (2-0)/5=0.4 -> floor=0 -> 0*5+0 = 0 (组的起点)
      Age_Start = floor((Age - age_s) / intv) * intv + age_s,
      
      # 创建易读的标签，例如 "0-4"
      # 注意：如果间隔是1，标签就是 "0"
      Age_Label = if(intv == 1) {
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
      Rate = mean(Disease, na.rm = TRUE),
      
      # 同时也计算一下样本量，万一以后要算置信区间
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
      Cohort_Label = if(intv == 1) {
        as.character(Cohort_Start)
      } else {
        paste0(Cohort_Start, "-", Cohort_Start + intv - 1)
      }
    ) %>%
    # --- D. 整理列名和顺序 ---
    select(
      age_group = Age_Label,       # 字符标签列 (用于显示)
      cohort_group = Cohort_Label, # 字符标签列 (用于显示)
      period = Period,             # 原始时期 (数字)
      rate = Rate,                 # 结果 (数字)
      
      # 保留辅助列，方便画图排序 (Plotly 画图喜欢用数字轴)
      age_start = Age_Start,
      cohort_start = Cohort_Start,
      N = Count
    ) %>%
    arrange(age_start, period)
  
  return(df_grouped)
}