#### descriptive figures data ####
get_descriptive_fig <- function(df, params) {
  ## get parameter inputs
  intv <- params$intervals
  age_s <- params$age_start
  age_e <- params$age_end
  pd_s <- params$period_start
  pd_e <- params$period_end

  if (nrow(df) == 0) {
    return(NULL)
  }

  ## cluster a/p/c groups and calculate rates
  # ensure age/cohort grouping columns exist (either computed earlier or compute now)
  if (!"Age_Start" %in% colnames(df)) {
    df <- add_age_cohort_groups(df, params)
  }

  df_grouped <- df %>%
    # rates grouped by age and period (cohort cols are deterministic given Age_Start/Period)
    group_by(Age_Start, Age_Label, Period, Cohort_Start, Cohort_Label) %>%
    summarise(
      Rate = 100 * mean(Disease, na.rm = TRUE),
      Count = n(), #sample size
      .groups = "drop"
    ) %>%

    # list and arrange cols
    select(
      age_group = Age_Label, # for display
      cohort_group = Cohort_Label, # for display
      period = Period,
      rate = Rate,

      # assittance cols
      age_start = Age_Start,
      cohort_start = Cohort_Start,
      N = Count
    ) %>%
    arrange(age_start, period)

  return(df_grouped)
}
