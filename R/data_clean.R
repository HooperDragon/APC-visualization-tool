#### packages ####
library(haven)
library(readxl)
library(bruceR)
library(dplyr)

#### define core cols ####
REQUIRED_COLS <- c("Disease", "Age", "Period")

#### read and validate data ####
read_and_validate_data <- function(file_path, ext) {
  df <- tryCatch(
    {
      ## load the data
      raw_data <- import(file_path, as = "data.frame")

      ## remove labels
      clean_data <- raw_data

      ## if dta/sav, convert labelled to factor
      if (ext %in% c("dta", "sav")) {
        clean_data <- haven::as_factor(clean_data)
      }

      clean_data <- as.data.frame(clean_data)

      ## ensure to remove any remaining labelled
      clean_data[] <- lapply(clean_data, function(x) {
        if (inherits(x, "haven_labelled")) as.vector(x) else x
      })

      clean_data
    },
    error = function(e) {
      print(paste("Error reading file:", e$message))
      return(NULL)
    }
  )

  if (is.null(df)) {
    return(NULL)
  }

  ## rename core cols to standard names
  current_cols <- colnames(df)
  lower_current <- tolower(current_cols)
  lower_required <- tolower(REQUIRED_COLS)

  ## check out core cols
  missing_idx <- which(!lower_required %in% lower_current)
  missing_cols_names <- REQUIRED_COLS[missing_idx]

  if (length(missing_cols_names) > 0) {
    attr(df, "error_msg") <- paste0(
      "Missing columns: ",
      paste(missing_cols_names, collapse = ", ")
    )
    return(df)
  }

  ## standardize core col names (Age, Period, Disease)
  for (req_col in REQUIRED_COLS) {
    match_idx <- which(lower_current == tolower(req_col))
    colnames(df)[match_idx] <- req_col
  }

  ## convert core cols to numeric
  f_to_numeric <- function(x) {
    if (is.factor(x)) {
      as.numeric(as.character(x))
    } else {
      as.numeric(x)
    }
  }

  df$Age <- f_to_numeric(df$Age)
  df$Period <- f_to_numeric(df$Period)
  df$Disease <- f_to_numeric(df$Disease)

  ## warn if Disease is not binary
  disease_vals <- df$Disease[!is.na(df$Disease)]
  if (length(disease_vals) > 0) {
    unique_vals <- unique(disease_vals)
    if (length(unique_vals) != 2) {
      warning(
        "Disease is expected to be a binary variable."
      )
    }
  }

  ## remove NAs in core cols
  df <- df %>% filter(!is.na(Age) & !is.na(Period) & !is.na(Disease))

  return(df)
}

#### filter data by params ####
filter_data_by_params <- function(df, params) {
  if (is.null(df) || is.null(params)) {
    return(df)
  }

  if (!is.null(params$age_start)) {
    df <- df %>% filter(Age >= params$age_start)
  }
  if (!is.null(params$age_end)) {
    df <- df %>% filter(Age <= params$age_end)
  }
  if (!is.null(params$period_start)) {
    df <- df %>% filter(Period >= params$period_start)
  }
  if (!is.null(params$period_end)) {
    df <- df %>% filter(Period <= params$period_end)
  }

  df
}

#### add age / cohort grouping columns ####
add_age_cohort_groups <- function(df, params) {
  if (is.null(df) || is.null(params) || nrow(df) == 0) {
    return(df)
  }

  intv <- params$intervals
  age_s <- params$age_start
  age_e <- params$age_end

  df <- df %>%
    mutate(
      Age_Start = floor((Age - age_s) / intv) * intv + age_s,
      Age_Label = if (intv == 1) {
        as.character(Age_Start)
      } else {
        paste0(Age_Start, "-", pmin(Age_Start + intv - 1, age_e))
      },

      Cohort_Raw = Period - Age_Start,
      Cohort_Start = floor(Cohort_Raw / intv) * intv,
      Cohort_Label = if (intv == 1) {
        as.character(Cohort_Start)
      } else {
        paste0(Cohort_Start, "-", Cohort_Start + intv - 1)
      }
    )

  df
}
