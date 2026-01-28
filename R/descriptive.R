## get descriptive data
get_descriptive_data <- function(df, params) {
  # get parameter inputs
  intv <- params$intervals
  age_s <- params$age_start
  age_e <- params$age_end
  pd_s <- params$period_start
  pd_e <- params$period_end

  # filter data based on parameters
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

  # cluster a/p/c groups and calculate rates
  df_grouped <- df_filtered %>%
    mutate(
      # age groups （fine for now, can be improved!!）
      Age_Start = floor((Age - age_s) / intv) * intv + age_s,
      Age_Label = if (intv == 1) {
        as.character(Age_Start)
      } else {
        paste0(Age_Start, "-", Age_Start + intv - 1)
      }
    ) %>%

    # rates grouped by age and period
    group_by(Age_Start, Age_Label, Period) %>%
    summarise(
      Rate = 100 * mean(Disease, na.rm = TRUE),
      Count = n(), #sample size
      .groups = "drop"
    ) %>%
    
    # cohort groups （有问题！！继续修改）
    mutate(
      Cohort_Raw = Period - Age_Start,
      Cohort_Start = floor(Cohort_Raw / intv) * intv,
      Cohort_Label = if (intv == 1) {
        as.character(Cohort_Start)
      } else {
        paste0(Cohort_Start, "-", Cohort_Start + intv - 1)
      }
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

## basic characteristics table (for download)
get_basic_characteristics <- function(
  df,
  vars,
  disease_col = "Disease",
  age_breaks = c(5, 18, 44, 65, 75, 90)
) {
  if (missing(vars) || length(vars) == 0) {
    stop("Please provide variable names in 'vars'.")
  }
  if (!disease_col %in% names(df)) {
    stop(paste0("disease column not found in data: ", disease_col))
  }

  results <- list()

  for (v in vars) {
    if (v == "age_group" && "Age" %in% names(df)) {
      age_range <- range(df$Age, na.rm = TRUE)
      if (any(!is.finite(age_range))) {
        stop("Age contains no finite values")
      }
      age_s <- age_range[1]
      age_e <- age_range[2]
      br <- sort(unique(c(
        age_s,
        age_breaks[age_breaks > age_s & age_breaks < age_e],
        age_e
      )))
      if (length(br) < 2) {
        stop("age_breaks must contain at least two values")
      }
      labels <- vapply(
        seq_len(length(br) - 1),
        function(i) paste0(br[i], "-", br[i + 1]),
        FUN.VALUE = ""
      )

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

    # count by levels
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

    # calculate percentage and format as "n(%.2f)"
    tab <- tab %>%
      mutate(
        pct = ifelse(N > 0, 100 * disease_n / N, NA_real_),
        disease = ifelse(
          is.na(pct),
          paste0(disease_n, "(NA)"),
          paste0(disease_n, "(", formatC(pct, format = "f", digits = 2), ")")
        )
      )

    # rename level column
    names(tab)[1] <- "level"

    # select and arrange cols
    tab <- tab %>%
      mutate(variable = v) %>%
      select(variable, level, N, disease)

    results[[length(results) + 1]] <- tab
  }

  res_df <- bind_rows(results)
  return(res_df)
}


## basic characteristics by period table (for download)
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

  # count totals per period
  totals <- df %>%
    group_by(.data[[period_col]]) %>%
    summarise(total_n = sum(!is.na(.data[[period_col]])), .groups = "drop") %>%
    arrange(match(.data[[period_col]], periods))

  results <- list()

  # process each variable
  for (v in vars) {
    if (v == "age_group" && "Age" %in% names(df)) {
      age_range <- range(df$Age, na.rm = TRUE)
      if (any(!is.finite(age_range))) {
        stop("Age contains no finite values")
      }
      age_s <- age_range[1]
      age_e <- age_range[2]
      br <- sort(unique(c(
        age_s,
        age_breaks[age_breaks > age_s & age_breaks < age_e],
        age_e
      )))
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

    # count by levels and periods
    tab_long <- df_use %>%
      filter(!is.na(.data[[lvl_col]])) %>%
      group_by_at(c(period_col, lvl_col)) %>%
      summarise(n = n(), .groups = "drop")

    if (nrow(tab_long) == 0) {
      next
    }

    # calculate percentages within periods and format as "n(%.1f)"
    tab_long <- tab_long %>%
      mutate(period_idx = match(.data[[period_col]], periods)) %>%
      left_join(totals, by = setNames(period_col, period_col)) %>%
      mutate(pct = ifelse(total_n > 0, 100 * n / total_n, NA_real_)) %>%
      arrange(period_idx)

    tab_long <- tab_long %>%
      mutate(
        cell = ifelse(
          is.na(pct),
          as.character(n),
          paste0(n, "(", formatC(pct, format = "f", digits = digits), ")")
        )
      )

    # pivot to wide format
    tab_wide <- tab_long %>%
      dplyr::select(level = !!rlang::sym(lvl_col), period_idx, cell) %>%
      tidyr::pivot_wider(names_from = period_idx, values_from = cell)

    # ensure all period columns exist
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

  # add total row
  total_row <- tibble::tibble(variable = "Total", level = "")
  for (i in seq_along(periods)) {
    total_row[[period_names[i]]] <- as.character(totals$total_n[i])
  }

  res_final <- dplyr::bind_rows(res_all, total_row)
  return(res_final)
}
