#### descriptive tables data ####
get_basic_characteristics <- function(
  df,
  vars,
  disease_col = "Disease"
) {
  results <- list()

  for (v in vars) {
    if (v == "age_group") {
      df_use <- df %>% mutate(.level = as.character(Age_Label))
      lvl_col <- ".level"
    } else {
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

#### basic characteristics by period table (for download) ####
get_basic_characteristics_by_period <- function(
  df,
  vars,
  period_col = "Period",
  disease_col = "Disease",
  digits = 1
) {
  periods <- sort(unique(df[[period_col]]))
  period_names <- paste0("Period ", seq_along(periods))

  # count totals per period
  totals <- df %>%
    group_by(.data[[period_col]]) %>%
    summarise(total_n = sum(!is.na(.data[[period_col]])), .groups = "drop") %>%
    arrange(match(.data[[period_col]], periods))

  results <- list()

  # process each variable
  for (v in vars) {
    if (v == "age_group") {
      df_use <- df %>% mutate(.level = as.character(Age_Label))
      lvl_col <- ".level"
    } else {
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
