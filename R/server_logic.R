#### HAPC analysis ####
#' Run the full HAPC analysis pipeline:
#'   parse covariates -> prepare data -> fit model -> check convergence -> extract summary
#'
#' @param raw_df      Cleaned grouped data frame
#' @param params      List with at least `intervals`
#' @param fixed_formula  Character string of fixed-effect formula
#' @param period_slopes  Character vector of period random slopes (or NULL)
#' @param cohort_slopes  Character vector of cohort random slopes (or NULL)
#' @return list(model, data_for_model, summary_table, success, converged, message)

run_analysis <- function(
  raw_df,
  params,
  fixed_formula,
  period_slopes,
  cohort_slopes
) {
  ## parse covariates
  incProgress(0.05, detail = "Parsing covariates...")
  all_covariates <- parse_covariates(
    fixed_formula,
    period_slopes,
    cohort_slopes
  )

  ## pre-process data
  incProgress(0.1, detail = "Preparing data...")
  data_for_model <- prepare_hapc_data(
    raw_df,
    list(intervals = params$intervals),
    all_covariates
  )

  ## model config
  model_config <- list(
    fixed_formula = fixed_formula,
    period_slopes = period_slopes,
    cohort_slopes = cohort_slopes
  )

  ## run the model (this is the slowest step)
  incProgress(0.15, detail = "Fitting HAPC model (this may take a while)...")
  model <- tryCatch(
    run_dynamic_hapc(data_for_model, model_config),
    error = function(e) NULL
  )

  incProgress(0.4, detail = "Checking convergence...")

  ## check the result and convergence
  if (is.null(model)) {
    return(list(
      model = NULL,
      data_for_model = data_for_model,
      summary_table = NULL,
      success = FALSE,
      message = "Model failed to run. Please check your formula or data."
    ))
  }

  converged <- !isTRUE(attr(model, "convergence_warning"))
  msg <- if (converged) {
    "Model converged successfully!"
  } else {
    fit_warns <- attr(model, "fit_warnings")
    if (any(grepl("singular", fit_warns, ignore.case = TRUE))) {
      paste0(
        "Warning: Singular fit detected. ",
        "The random slope variance is near zero or the correlation is at the boundary. ",
        "Consider removing random slopes (e.g. period/cohort slopes) ",
        "or using a simpler random-effects structure."
      )
    } else {
      "Warning: Model convergence problem. Results may be unreliable; try simplifying interactions or random slopes."
    }
  }

  ## summary table
  incProgress(0.1, detail = "Extracting results...")
  summary_table <- tryCatch(
    get_model_results_table(model),
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
