#### data input ui ####
mod_data_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      ## left: parameter card
      column(
        width = 4,
        div(
          class = "card-style",
          h3(tags$span(
            style = "color: #337ab7; font-weight: 700;",
            "Analysis Parameters"
          )),
          textInput(
            ns("proj_title"),
            "Project title",
            placeholder = "My Analysis",
            width = "100%"
          ),
          textAreaInput(
            ns("desc"),
            "Description",
            placeholder = "Analysis notes...",
            rows = 3,
            width = "100%",
            resize = "none"
          ),

          tags$label("Period range"),
          fluidRow(
            column(
              5,
              numericInput(
                ns("period_start"),
                NULL,
                value = DEFAULTS$period_start
              )
            ),
            column(
              2,
              div(
                ":",
                style = "text-align:center; font-weight:bold; font-size:20px;"
              )
            ),
            column(
              5,
              numericInput(ns("period_end"), NULL, value = DEFAULTS$period_end)
            )
          ),

          tags$label("Age range"),
          fluidRow(
            column(
              5,
              numericInput(ns("age_start"), NULL, value = DEFAULTS$age_start)
            ),
            column(
              2,
              div(
                ":",
                style = "text-align:center; font-weight:bold; font-size:20px;"
              )
            ),
            column(
              5,
              numericInput(ns("age_end"), NULL, value = DEFAULTS$age_end)
            )
          ),

          numericInput(
            ns("intervals"),
            "Intervals (year)",
            value = DEFAULTS$intervals
          ),
          hr(),

          # variables pool
          div(
            style = "margin-top: 5px;",
            selectInput(
              ns("model_vars"),
              "Select Variables:",
              choices = NULL,
              multiple = TRUE,
              selectize = TRUE
            )
          ),

          # buttons
          div(
            style = "display: flex; gap: 5px; margin-top: -6px; margin-bottom: 12px;",
            actionButton(
              ns("add_main_effect"),
              "Add Main",
              class = "btn-xs btn-primary"
            ),
            actionButton(
              ns("add_interaction"),
              "Add Interaction",
              class = "btn-xs btn-warning"
            ),
            actionButton(
              ns("clear_formula"),
              "Clear",
              class = "btn-xs btn-default"
            )
          ),

          # fixed formula (tag-based)
          div(
            style = "margin-top: 6px;",
            tags$label("Fixed Effects Structure:"),
            uiOutput(ns("formula_tags_display"))
          ),

          tags$small(
            style = "color:grey;",
            icon("info-circle"),
            " Select 2 vars and click 'Add Interaction' to add terms like 'sex:residence'."
          ),

          hr(),

          # random formula
          div(
            style = "margin-top: 10px;",
            tags$label("Random Effects:")
          ),
          pickerInput(
            ns("period_slopes"),
            "Vars varying by Period (cov | period):",
            choices = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            ns("cohort_slopes"),
            "Vars varying by Cohort (cov | cohort):",
            choices = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),

          ## errors
          uiOutput(ns("validation_alert"))
        )
      ),

      ## right:data upload and preview
      column(
        width = 8,
        div(
          class = "card-style input-data-card",
          h3(tags$span(
            style = "color: #337ab7; font-weight: 700;",
            "Input data"
          )),
          tags$style(HTML(
            ".input-data-card .form-group { margin-bottom: 0px !important; }
             .input-data-card .shiny-input-container { margin-bottom: 0px !important; }
             .input-data-card .progress { margin-bottom: 0px !important; }"
          )),
          tags$small(
            style = "color: #666; display: block; margin-bottom: 4px;",
            icon("info-circle"),
            " Data requirements: individual-level data with disease status (*), age (*), period (*), and covariates; disease is coded 0 = no disease, 1 = disease."
          ),
          div(
            style = "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;",
            tags$label(
              `for` = ns("file_input"),
              "Upload data file (.csv, .dta, .sav, .xlsx, .RData, .rds)",
              style = "font-size: 14px; margin: 0;"
            ),
            actionLink(
              ns("load_sample1"),
              tagList(
                icon("download"),
                tags$span(
                  " Load sample 1",
                  style = "color: #337ab7; font-size:14px;"
                )
              ),
              style = "text-decoration: none;"
            )
          ),
          div(
            style = "margin-bottom: 0;",
            fileInput(
              ns("file_input"),
              label = NULL,
              multiple = FALSE,
              width = "100%",
              accept = c(
                ".csv",
                ".dta",
                ".xlsx",
                ".xls",
                ".sav",
                ".Rdata",
                ".rds"
              )
            )
          ),
          h5(
            "Data Preview (first 10 rows):",
            style = "margin-top: 10px; margin-bottom: 4px; font-weight: bold"
          ),
          DTOutput(ns("data_preview"))
        ),
        div(
          style = "margin-top: 6px;",
          actionButton(
            ns("run_btn"),
            "Run Analysis",
            icon = icon("play"),
            class = "btn-success",
            style = "padding: 10px 20px; font-size: 15px; box-shadow: 0 2px 6px rgba(0,0,0,0.2); border-radius: 5px;"
          )
        )
      )
    )
  )
}

#### data input server ####
mod_data_input_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # helper: canonicalize interaction term so order doesn't matter
    canonicalize_interaction <- function(term) {
      if (grepl(":", term)) {
        parts <- unlist(strsplit(term, ":"))
        parts_sorted <- sort(parts)
        paste(parts_sorted, collapse = ":")
      } else {
        term
      }
    }

    ## parameters
    validation_error_msg <- reactive({
      req(
        input$period_start,
        input$period_end,
        input$age_start,
        input$age_end,
        input$intervals
      )
      if (
        input$period_start < 0 ||
          input$period_end < 0 ||
          input$period_start >= input$period_end
      ) {
        return("Invalid Period")
      }
      if (
        input$age_start < 0 ||
          input$age_end < 0 ||
          input$age_start >= input$age_end
      ) {
        return("Invalid Age")
      }
      if (input$intervals <= 0) {
        return("Invalid Interval")
      }
      return(NULL)
    })

    output$validation_alert <- renderUI({
      msg <- validation_error_msg()
      if (is.null(msg)) {
        return(NULL)
      }
      div(
        style = "color: red; font-weight: bold; margin-top: 10px;",
        icon("triangle-exclamation"),
        msg
      )
    })

    # error when age>105
    observeEvent(input$age_end, {
      req(input$age_end)
      if (!is.null(validation_error_msg())) {
        return()
      }

      if (input$age_end > 105) {
        showModal(modalDialog(
          title = "Warning: Unusual Age Range",
          tags$div(
            tags$p("Data for ages > 105 is often sparse or unreliable."),
            tags$p(paste0("Current input: ", input$age_end)),
            tags$p("Are you sure you want to proceed?")
          ),
          footer = tagList(
            actionButton(ns("cancel_age"), "Cancel (Reset to 105)"),
            actionButton(ns("confirm_age"), "Confirm (Keep value)")
          ),
          easyClose = FALSE
        ))
      }
    })

    observeEvent(input$cancel_age, {
      removeModal()
      updateNumericInput(session, "age_end", value = 105)
    })
    observeEvent(input$confirm_age, {
      removeModal()
    })

    ## data upload (inline, merged from mod_upload)
    raw_data <- reactive({
      req(input$file_input)
      ext <- tools::file_ext(input$file_input$name)
      read_and_validate_data(input$file_input$datapath, tolower(ext))
    })

    # sample-loaded data (if user clicks Load sample 1)
    sample_loaded_data <- reactiveVal(NULL)

    output$data_preview <- renderDT({
      df <- uploaded_data()
      if (is.null(df)) {
        return(NULL)
      }
      err <- attr(df, "error_msg")
      validate(need(is.null(err), err))
      datatable(
        df,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = 300,
          scroller = TRUE,
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          ),
          initComplete = JS(
            "function(settings, json){",
            " $('thead th', this.api().table().container()).css({'border-top':'1px solid #111'});",
            "}"
          )
        )
      )
    })

    uploaded_data <- reactive({
      # prefer sample-loaded data when present (so Load sample works without writing files)
      samp <- sample_loaded_data()
      if (!is.null(samp)) {
        return(samp)
      }
      df <- raw_data()
      if (!is.null(df) && is.null(attr(df, "error_msg"))) df else NULL
    })

    ## formula terms management (tag-based)
    formula_terms <- reactiveVal(c("age_c", "age_c2"))

    # render formula tags UI
    output$formula_tags_display <- renderUI({
      terms <- formula_terms()
      if (length(terms) == 0) {
        return(div(
          class = "formula-tags-container",
          tags$span(style = "color: #999;", "No terms added")
        ))
      }

      tag_elements <- list()
      for (i in seq_along(terms)) {
        if (i > 1) {
          tag_elements <- c(
            tag_elements,
            list(
              tags$span(class = "formula-tag-plus", "+")
            )
          )
        }
        btn_id <- ns(paste0("remove_term_", i))
        is_protected <- terms[i] %in% c("age_c", "age_c2")
        remove_btn <- if (is_protected) {
          # no remove button for required terms
          NULL
        } else {
          tags$span(
            class = "tag-remove",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("remove_term"),
              i
            ),
            HTML("&times;")
          )
        }
        tag_elements <- c(
          tag_elements,
          list(
            tags$span(
              class = "formula-tag",
              terms[i],
              remove_btn
            )
          )
        )
      }

      do.call(div, c(list(class = "formula-tags-container"), tag_elements))
    })

    # handle tag removal
    observeEvent(input$remove_term, {
      idx <- input$remove_term
      current <- formula_terms()
      if (idx >= 1 && idx <= length(current)) {
        # age_c and age_c2 are required and cannot be removed
        if (current[idx] %in% c("age_c", "age_c2")) {
          showNotification(
            "'age_c' and 'age_c2' are required and cannot be removed.",
            type = "warning"
          )
          return()
        }
        formula_terms(current[-idx])
      }
    })

    # listen to uploaded data to update variable choices
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      req(df)

      all_vars <- names(df)
      exclude_vars <- c("Period", "period", "Disease", "disease", "Age", "age")
      choices_vars <- setdiff(all_vars, exclude_vars)
      choices_vars <- c("age_c", choices_vars)

      updateSelectInput(session, "model_vars", choices = choices_vars)

      slope_choices <- setdiff(choices_vars, "age_c")
      updatePickerInput(
        session,
        "period_slopes",
        choices = slope_choices,
        selected = character(0)
      )
      updatePickerInput(
        session,
        "cohort_slopes",
        choices = slope_choices,
        selected = character(0)
      )

      # initialize formula terms only if user has not already customised them
      covs_for_default <- setdiff(choices_vars, "age_c")
      default_terms <- c("age_c", "age_c2")
      if (length(covs_for_default) > 0) {
        default_terms <- c(default_terms, covs_for_default)
      }
      formula_terms(default_terms)
    })

    # Load sample 1: load tests/small_test_data.RData and prefill parameters
    observeEvent(input$load_sample1, {
      sample_path <- file.path(getwd(), "tests", "small_test_data.RData")
      if (!file.exists(sample_path)) {
        showNotification(
          paste("Sample file not found:", sample_path),
          type = "error"
        )
        return()
      }
      e <- new.env()
      nm <- load(sample_path, envir = e)
      # find first data.frame-like object
      df <- NULL
      for (n in nm) {
        obj <- get(n, envir = e)
        if (is.data.frame(obj) || inherits(obj, "tbl_df")) {
          df <- obj
          break
        }
      }
      if (is.null(df)) {
        showNotification("No data.frame found in sample file.", type = "error")
        return()
      }

      # standardize core column names and types (match read_and_validate_data behavior)
      current_cols <- colnames(df)
      lower_current <- tolower(current_cols)

      # REQUIRED_COLS is defined in R/data_clean.R (Age, Period, Disease)
      req_cols <- c("Disease", "Age", "Period")
      for (req_col in req_cols) {
        match_idx <- which(lower_current == tolower(req_col))
        if (length(match_idx) == 1) {
          colnames(df)[match_idx] <- req_col
        }
      }

      # convert core cols to numeric where present
      f_to_numeric <- function(x) {
        if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
      }
      if ("Age" %in% names(df)) {
        df$Age <- f_to_numeric(df$Age)
      }
      if ("Period" %in% names(df)) {
        df$Period <- f_to_numeric(df$Period)
      }
      if ("Disease" %in% names(df)) {
        df$Disease <- f_to_numeric(df$Disease)
      }

      # remove rows with NA in core cols if they exist
      if (all(c("Age", "Period", "Disease") %in% names(df))) {
        df <- df %>% filter(!is.na(Age) & !is.na(Period) & !is.na(Disease))
      }

      # set sample data so uploaded_data() and preview/choices use it
      sample_loaded_data(df)

      # Defer parameter & formula updates until after reactive flush so preview/choices update first
      session$onFlushed(
        function() {
          # preset parameters per user's spec
          updateTextInput(
            session,
            "proj_title",
            value = "care needs among Chinese older adults"
          )
          updateTextAreaInput(
            session,
            "desc",
            value = "incidence of care needs calculated from CLHLS database, 2002-2021"
          )
          updateNumericInput(session, "period_start", value = 2002)
          updateNumericInput(session, "period_end", value = 2021)
          updateNumericInput(session, "age_start", value = 65)
          updateNumericInput(session, "age_end", value = 105)
          updateNumericInput(session, "intervals", value = 5)

          # clear any selected random slopes
          updatePickerInput(session, "period_slopes", selected = character(0))
          updatePickerInput(session, "cohort_slopes", selected = character(0))

          # set fixed formula terms exactly as requested (assumes test_data column names match)
          final_terms <- c(
            "age_c",
            "age_c2",
            "sex",
            "urban_rural",
            "education_level",
            "economic_status",
            "living_companion",
            "age_c:sex",
            "age_c:urban_rural"
          )
          formula_terms(final_terms)
        },
        once = TRUE
      )
    })

    ## formula construction
    observeEvent(input$add_main_effect, {
      req(input$model_vars)
      current <- formula_terms()
      new_terms <- input$model_vars
      # avoid duplicates
      new_terms <- setdiff(new_terms, current)
      if (length(new_terms) > 0) {
        formula_terms(c(current, new_terms))
      } else {
        showNotification(
          "Selected variable(s) is/are already in the formula.",
          type = "message"
        )
      }
      updateSelectInput(session, "model_vars", selected = character(0))
    })

    observeEvent(input$add_interaction, {
      req(input$model_vars)
      if (length(input$model_vars) < 2) {
        showNotification(
          "Please select at least 2 variables for interaction.",
          type = "warning"
        )
        return()
      }
      if (length(input$model_vars) > 2) {
        showNotification(
          "Please select at most 2 variables for interaction.",
          type = "warning"
        )
        return()
      }

      current <- formula_terms()
      new_term <- paste(input$model_vars, collapse = ":")

      current_canon <- vapply(current, canonicalize_interaction, FUN.VALUE = "")
      new_canon <- canonicalize_interaction(new_term)

      # avoid duplicates irrespective of variable order (a:b == b:a)
      if (!new_canon %in% current_canon) {
        formula_terms(c(current, new_term))
      } else {
        showNotification(
          "The interaction term is already in the formula.",
          type = "message"
        )
      }
      updateSelectInput(session, "model_vars", selected = character(0))
    })

    observeEvent(input$clear_formula, {
      formula_terms(c("age_c", "age_c2"))
    })

    ## return values
    return(list(
      uploaded_data = uploaded_data,
      validation_error_msg = validation_error_msg,
      run_btn = reactive(input$run_btn),
      params = reactive({
        list(
          intervals = input$intervals,
          age_start = input$age_start,
          age_end = input$age_end,
          period_start = input$period_start,
          period_end = input$period_end
        )
      }),
      fixed_formula = reactive(paste(formula_terms(), collapse = " + ")),
      period_slopes = reactive(input$period_slopes),
      cohort_slopes = reactive(input$cohort_slopes)
    ))
  })
}
