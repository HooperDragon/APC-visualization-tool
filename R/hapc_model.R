#### packages ####
library(glmmTMB)
library(emmeans)
library(bruceR)
library(sjPlot)

#### data pre-process ####
prepare_hapc_data <- function(df, params, covariates) {
  ## get parameter inputs
  intv <- params$intervals

  ## get min age for age centering
  min_age <- min(df$Age, na.rm = TRUE)

  ## centering and factorizing
  data_model <- df %>%
    mutate(
      cohort_group = Cohort_Start,

      # age centering（！！这里后续要改一下，10还是2*Interval，以及报错）
      age_c = (Age - min_age) / 10,
      age_c2 = age_c^2,

      # factorize period and cohort variables
      period_factor = as.factor(Period),
      cohort_group_factor = as.factor(cohort_group)
    ) %>%

    # drop internal grouping assistant columns to keep model data lean
    select(
      -any_of(c(
        "Age_Label",
        "Age_Start",
        "Cohort_Raw",
        "Cohort_Start",
        "Cohort_Label"
      ))
    )

  # factorize covariates
  for (cov in covariates) {
    data_model[[cov]] <- as.factor(data_model[[cov]])
  }

  # keep useful parameters as attributes for later use
  attr(data_model, "min_age") <- min_age
  attr(data_model, "intv") <- intv

  return(data_model)
}

#### run hapc model ####
build_hapc_formula <- function(config) {
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

  final_formula_str <- paste(
    "Disease ~",
    fixed_part,
    "+",
    re_period,
    "+",
    re_cohort
  )

  as.formula(final_formula_str)
}

#### this model cited from Yang Y, Land KC. Age-Period-Cohort Analysis: New Models, Methods, and Empirical Applications. Chapman and Hall/CRC; 2013.
run_dynamic_hapc <- function(data_model, config) {
  formula_obj <- build_hapc_formula(config)

  message("Running HAPC with formula: ", deparse(formula_obj))

  ## run model
  model <- tryCatch(
    {
      glmmTMB(
        formula = formula_obj,
        data = data_model,
        family = binomial(link = "logit")
      )
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (is.null(model)) {
    return(NULL)
  }

  ## check convergence
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

#### model results extraction ####
## get fixed effects
get_fixed_effects <- function(model) {
  tab <- sjPlot::tab_model(
    model,
    show.se = TRUE,
    show.ci = TRUE,
    show.p = TRUE,
    digits = 3
  )

  return(as.character(tab))
}

## get random effects
get_random_effects <- function(model) {
  rr_full <- ranef(model, condVar = TRUE)
  rr_full$cond
}

## get model trend data for plotting
get_model_trend_data <- function(
  model,
  x_axis = "age",
  group_by = "null",
  data_model
) {
  # get average intercept from fixed effects
  avg_intercept <- fixef(model)$cond["(Intercept)"]

  # get cohort interval: prefer user-specified attribute on data_model
  intv <- attr(data_model, "intv")
  if (is.null(intv)) {
    intv <- 5 # defaults
    if ("cohort_group" %in% names(data_model)) {
      cohort_vals <- sort(unique(data_model$cohort_group))
      if (length(cohort_vals) > 1) {
        intv <- cohort_vals[2] - cohort_vals[1]
      }
    }
  }

  # get random effects (and conditional variances if available)
  rr_full <- tryCatch(ranef(model, condVar = TRUE), error = function(e) NULL)
  if (is.null(rr_full)) {
    return(NULL)
  }
  rr <- rr_full$cond

  res_df <- NULL
  note <- NULL

  # when: age as x axis
  if (x_axis == "age") {
    min_age <- attr(data_model, "min_age")
    max_c <- max(data_model$age_c, na.rm = TRUE)

    # use a 'for' loop to get emmeans at each age_c point
    res_list <- list()

    if (group_by != "null") {
      specs <- as.formula(paste("~ age_c + age_c2 |", group_by))
    } else {
      specs <- ~ age_c + age_c2
    }

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
      mutate(x_val = age_c * 10 + min_age)

    # add overall row
    if (group_by != "null") {
      res_df$group <- res_df[[group_by]]
    } else {
      res_df$group <- "Overall"
    }

    # add mean line (overall mean prob)
    mean_prob <- mean(res_df$prob, na.rm = TRUE)
    res_df <- res_df %>% mutate(mean_prob = mean_prob)

    # when: period or cohort as x axis
  } else {
    re_name <- if (x_axis == "period") {
      if ("period_factor" %in% names(rr)) "period_factor" else "period"
    } else {
      if ("cohort_group_factor" %in% names(rr)) {
        "cohort_group_factor"
      } else {
        "cohort_group"
      }
    }

    # extract the relevant random effects data frame
    re_df <- rr[[re_name]]

    if (is.null(re_df)) {
      return(NULL)
    }

    x_values <- as.numeric(rownames(re_df))

    # glmmTMB: get random effects standard deviations
    sdr <- model$sdr

    if (!is.null(sdr) && !is.null(sdr$diag.cov.random)) {
      all_se <- sqrt(sdr$diag.cov.random)

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
      if (length(u_se) != nrow(re_df)) {
        u_se <- rep(0, nrow(re_df))
      }
    } else {
      # set as 0 if not available
      u_se <- rep(0, nrow(re_df))
    }

    # extract random intercepts (handle renamed column from data.frame)
    intercept_col <- if ("(Intercept)" %in% colnames(re_df)) {
      "(Intercept)"
    } else if ("X.Intercept." %in% colnames(re_df)) {
      "X.Intercept."
    } else {
      NULL
    }
    u_intercept <- if (!is.null(intercept_col)) {
      re_df[[intercept_col]]
    } else {
      numeric(0)
    }
    if (length(u_intercept) != length(x_values)) {
      u_intercept <- rep(0, length(x_values))
    }
    if (length(u_se) != length(x_values)) {
      u_se <- rep(0, length(x_values))
    }

    # general trends
    if (group_by == "null") {
      # get probabilities and CIs
      est_total <- avg_intercept + u_intercept
      res_df <- data.frame(
        x_val = x_values,
        prob = plogis(est_total),
        lower = plogis(est_total - 1.96 * u_se),
        upper = plogis(est_total + 1.96 * u_se),
        group = "Overall"
      )

      # stratified trends
    } else {
      if (!group_by %in% names(data_model)) {
        return(NULL)
      }

      level_labels <- levels(as.factor(data_model[[group_by]]))
      if (length(level_labels) == 0) {
        return(NULL)
      }

      col_names <- colnames(re_df)
      slope_cols <- setdiff(col_names, "(Intercept)")

      level_keys <- make.names(level_labels)
      slope_levels <- sub(paste0("^", group_by), "", slope_cols)
      slope_levels <- ifelse(slope_levels == "", slope_cols, slope_levels)
      slope_levels_keys <- make.names(slope_levels)
      slope_cols_by_level <- setNames(slope_cols, slope_levels_keys)

      fixef_names <- names(fixef(model)$cond)

      vc_fixed <- tryCatch(vcov(model)$cond, error = function(e) NULL)
      re_condvar <- if (!is.null(rr_full$condVar)) {
        rr_full$condVar[[re_name]]
      } else {
        NULL
      }
      vc_re <- tryCatch(VarCorr(model)$cond[[re_name]], error = function(e) {
        NULL
      })

      df_list <- lapply(seq_along(level_labels), function(i) {
        lvl <- level_labels[i]
        lvl_key <- level_keys[i]

        coef_name <- paste0(group_by, lvl_key)
        fix_slope <- if (coef_name %in% fixef_names) {
          fixef(model)$cond[coef_name]
        } else {
          0
        }

        slope_col <- slope_cols_by_level[[lvl_key]]
        u_slope <- if (!is.null(slope_col)) re_df[[slope_col]] else 0

        # fixed-effects variance (intercept + slope, with covariance)
        fix_var <- 0
        if (!is.null(vc_fixed)) {
          idx_int <- match("(Intercept)", colnames(vc_fixed))
          idx_slope <- match(coef_name, colnames(vc_fixed))
          if (!is.na(idx_int)) {
            fix_var <- fix_var + vc_fixed[idx_int, idx_int]
          }
          if (!is.na(idx_slope)) {
            fix_var <- fix_var + vc_fixed[idx_slope, idx_slope]
            if (!is.na(idx_int)) {
              fix_var <- fix_var + 2 * vc_fixed[idx_int, idx_slope]
            }
          }
        }

        # random-effects variance (intercept + slope, with covariance)
        rand_var <- rep(0, length(x_values))
        idx_re_int <- match("(Intercept)", colnames(re_df))
        idx_re_slope <- if (!is.null(slope_col)) {
          match(slope_col, colnames(re_df))
        } else {
          NA_integer_
        }

        if (!is.null(re_condvar) && length(dim(re_condvar)) == 3) {
          for (j in seq_along(x_values)) {
            mat <- re_condvar[,, j]
            if (!is.na(idx_re_int)) {
              rand_var[j] <- rand_var[j] + mat[idx_re_int, idx_re_int]
            }
            if (!is.na(idx_re_slope)) {
              rand_var[j] <- rand_var[j] + mat[idx_re_slope, idx_re_slope]
              if (!is.na(idx_re_int)) {
                rand_var[j] <- rand_var[j] + 2 * mat[idx_re_int, idx_re_slope]
              }
            }
          }
        } else if (!is.null(vc_re)) {
          if (!is.na(idx_re_int)) {
            rand_var <- rand_var + vc_re[idx_re_int, idx_re_int]
          }
          if (!is.na(idx_re_slope)) {
            rand_var <- rand_var + vc_re[idx_re_slope, idx_re_slope]
            if (!is.na(idx_re_int)) {
              rand_var <- rand_var + 2 * vc_re[idx_re_int, idx_re_slope]
            }
          }
        }

        var_total <- fix_var + rand_var
        var_total[is.na(var_total)] <- 0
        se_total <- sqrt(pmax(var_total, 0))

        data.frame(
          x_val = x_values,
          est = (avg_intercept + fix_slope) + (u_intercept + u_slope),
          se = se_total,
          group = lvl,
          stringsAsFactors = FALSE
        )
      })

      res_df <- bind_rows(df_list) %>%
        mutate(
          prob = plogis(est),
          lower = plogis(est - 1.96 * se),
          upper = plogis(est + 1.96 * se)
        )
    }

    # add x axis labels for period/cohort
    if (x_axis == "cohort") {
      res_df <- res_df %>%
        mutate(x_label = paste0(x_val, "-", x_val + intv - 1))
    }

    # add note if random slope variance is ~0 when grouped
    if (group_by != "null") {
      tol <- 1e-6
      vc_obj <- tryCatch(VarCorr(model)$cond[[re_name]], error = function(e) {
        NULL
      })
      if (!is.null(vc_obj)) {
        slope_idx <- which(colnames(vc_obj) != "(Intercept)")
        if (length(slope_idx) > 0) {
          slope_vars <- diag(vc_obj)[slope_idx]
          if (all(is.na(slope_vars) | slope_vars <= tol)) {
            note <- paste0(
              "Random slope for ",
              x_axis,
              " by ",
              group_by,
              " not significant (variance ~ 0)."
            )
          }
        }
      }
    }
  }

  # add mean line (overall mean prob) if not already added
  if (!"mean_prob" %in% names(res_df) && !is.null(res_df)) {
    mean_prob <- mean(res_df$prob, na.rm = TRUE)
    res_df <- res_df %>% mutate(mean_prob = mean_prob)
  }

  if (!is.null(note)) {
    attr(res_df, "note") <- note
  }

  return(res_df)
}
