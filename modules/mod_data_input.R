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
          h4("Model Specification"),

          # variables pool
          selectInput(
            ns("model_vars"),
            "Select Variables:",
            choices = NULL,
            multiple = TRUE,
            selectize = TRUE
          ),

          # buttons
          div(
            style = "display: flex; gap: 5px; margin-bottom: 10px;",
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
          tags$label("Fixed Effects Structure:"),
          uiOutput(ns("formula_tags_display")),

          tags$small(
            style = "color:grey;",
            "Tip: Select 2 or 3 vars and click 'Add Interaction' to add terms like 'sex:residence'."
          ),

          br(),
          hr(),

          # random formula
          h5("Random Effects"),
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
          class = "card-style",
          h3(tags$span(
            style = "color: #337ab7; font-weight: 700;",
            "Input data"
          )),
          fileInput(
            ns("file_input"),
            "Upload data file (.csv, .dta, .sav, .xlsx, .RData, .rds)",
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
          ),
          h5("Data Preview (first 10 rows):"),
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

    output$data_preview <- renderDT({
      df <- raw_data()
      if (is.null(df)) {
        return(NULL)
      }
      err <- attr(df, "error_msg")
      validate(need(is.null(err), err))
      datatable(
        df,
        options = list(
          scrollX = TRUE,
          scrollY = 300,
          scroller = TRUE,
          dom = "t"
        )
      )
    })

    uploaded_data <- reactive({
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
        tag_elements <- c(
          tag_elements,
          list(
            tags$span(
              class = "formula-tag",
              terms[i],
              tags$span(
                class = "tag-remove",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', %d, {priority: 'event'})",
                  ns("remove_term"),
                  i
                ),
                HTML("&times;")
              )
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

      # initialize formula terms: age_c + age_c2 + all other covariates
      covs_for_default <- setdiff(choices_vars, "age_c")
      default_terms <- c("age_c", "age_c2")
      if (length(covs_for_default) > 0) {
        default_terms <- c(default_terms, covs_for_default)
      }
      formula_terms(default_terms)
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

      current <- formula_terms()
      new_term <- paste(input$model_vars, collapse = ":")
      # avoid duplicates
      if (!new_term %in% current) {
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
