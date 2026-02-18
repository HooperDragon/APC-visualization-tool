# Format model results into an academic-style combined table
# Sections: Fixed Effects (OR, 95% CI, p-value with stars),
#           Random Effects (Variance, SD),
#           Model Fit (BIC)
get_model_results_table <- function(model, digits = 2, z = 1.96) {
  if (is.null(model)) {
    return(NULL)
  }

  fmt <- function(x, d = digits) formatC(x, format = "f", digits = d)

  ## helper: significance stars
  p_stars <- function(p) {
    ifelse(
      is.na(p),
      "",
      ifelse(
        p < 0.001,
        "***",
        ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))
      )
    )
  }

  ## ---- Fixed Effects ----
  coef_tbl <- summary(model)$coefficients$cond
  est <- coef_tbl[, "Estimate"]
  se <- coef_tbl[, "Std. Error"]
  pval <- coef_tbl[, "Pr(>|z|)"]

  or <- exp(est)
  or_l <- exp(est - z * se)
  or_u <- exp(est + z * se)

  stars <- p_stars(pval)

  fixed_df <- data.frame(
    Term = names(est),
    OR = paste0(
      fmt(or),
      stars,
      " (",
      fmt(or_l),
      ", ",
      fmt(or_u),
      ")"
    ),
    stringsAsFactors = FALSE
  )

  # prettify term names: ':' -> ' × ', '_' -> ' '
  fixed_df$Term <- gsub(":", " × ", fixed_df$Term)
  fixed_df$Term <- gsub("_", " ", fixed_df$Term)

  ## ---- Random Effects ----
  vc_cond <- tryCatch(VarCorr(model)$cond, error = function(e) NULL)

  rand_rows <- list()
  if (!is.null(vc_cond)) {
    for (nm in names(vc_cond)) {
      mat <- vc_cond[[nm]]
      sds <- attr(mat, "stddev")
      vars <- diag(mat)
      param_names <- names(sds)

      # display name: period_factor -> Period, cohort_group_factor -> Cohort
      display_nm <- gsub("_factor$", "", nm)
      display_nm <- gsub("_group", "", display_nm)
      display_nm <- paste0(
        toupper(substring(display_nm, 1, 1)),
        substring(display_nm, 2)
      )

      for (k in seq_along(param_names)) {
        param_label <- if (param_names[k] == "(Intercept)") {
          paste0(display_nm, " (Intercept)")
        } else {
          param_clean <- gsub("_", " ", param_names[k])
          paste0(display_nm, " (", param_clean, ")")
        }
        rand_rows[[length(rand_rows) + 1]] <- data.frame(
          Term = param_label,
          OR = paste0(fmt(vars[k], 4), " (", fmt(sds[k], 4), ")"),
          stringsAsFactors = FALSE
        )
      }

      # If there are correlations (multi-param), add them
      corr_mat <- attr(mat, "correlation")
      if (!is.null(corr_mat) && nrow(corr_mat) > 1) {
        for (r in 2:nrow(corr_mat)) {
          for (cc in 1:(r - 1)) {
            rand_rows[[length(rand_rows) + 1]] <- data.frame(
              Term = paste0(
                display_nm,
                " Corr(",
                param_names[r],
                ", ",
                param_names[cc],
                ")"
              ),
              OR = fmt(corr_mat[r, cc], 4),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  random_df <- if (length(rand_rows) > 0) bind_rows(rand_rows) else NULL

  ## ---- Model Fit ----
  bic_val <- tryCatch(BIC(model), error = function(e) NA_real_)
  aic_val <- tryCatch(AIC(model), error = function(e) NA_real_)
  loglik <- tryCatch(as.numeric(logLik(model)), error = function(e) NA_real_)
  n_obs <- tryCatch(nobs(model), error = function(e) NA_integer_)

  fit_df <- data.frame(
    Term = c("N", "Log-Likelihood", "AIC", "BIC"),
    OR = c(
      ifelse(is.na(n_obs), "—", format(n_obs, big.mark = ",")),
      ifelse(is.na(loglik), "—", fmt(loglik, 1)),
      ifelse(is.na(aic_val), "—", fmt(aic_val, 1)),
      ifelse(is.na(bic_val), "—", fmt(bic_val, 1))
    ),
    stringsAsFactors = FALSE
  )

  ## ---- Combine into one table ----
  sep_row <- function(label) {
    data.frame(Term = label, OR = "", stringsAsFactors = FALSE)
  }

  combined <- bind_rows(
    sep_row("— Fixed Effects —"),
    fixed_df,
    sep_row(""),
    sep_row("— Random Effects —"),
    if (!is.null(random_df)) {
      random_df
    } else {
      data.frame(
        Term = "(none)",
        OR = "",
        stringsAsFactors = FALSE
      )
    },
    sep_row(""),
    sep_row("— Model Fit —"),
    fit_df,
    sep_row(""),
    data.frame(
      Term = "Note",
      OR = paste0(
        "Fixed effects: OR (95% CI); ",
        "Random effects: Variance (SD). ",
        "* P < 0.05; ** P < 0.01; *** P < 0.001"
      ),
      stringsAsFactors = FALSE
    )
  )

  return(list(
    combined = combined,
    fixed = fixed_df,
    random = random_df,
    bic = bic_val
  ))
}
