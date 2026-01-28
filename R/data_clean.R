## packages
library(haven)
library(readxl)
library(bruceR)
library(dplyr)

## define core cols
REQUIRED_COLS <- c("Disease", "Age", "Period")

## read and validate data
read_and_validate_data <- function(file_path, ext) {
  df <- tryCatch(
    {
      # load the data
      raw_data <- bruceR::import(file_path, as = "data.frame")

      # remove labels
      clean_data <- raw_data

      # if dta/sav, convert labelled to factor 
      if (ext %in% c("dta", "sav")) {
        clean_data <- haven::as_factor(clean_data)
      }

      clean_data <- as.data.frame(clean_data)

      # ensure to remove any remaining labelled
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

  # check out core cols
  missing_idx <- which(!lower_required %in% lower_current)
  missing_cols_names <- REQUIRED_COLS[missing_idx]

  if (length(missing_cols_names) > 0) {
    attr(df, "error_msg") <- paste0(
      "Missing columns: ",
      paste(missing_cols_names, collapse = ", ")
    )
    return(df)
  }

  # standardize core col names (Age, Period, Disease)
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

  ## remove NAs in core cols
  df <- df %>% filter(!is.na(Age) & !is.na(Period) & !is.na(Disease))

  return(df)
}
