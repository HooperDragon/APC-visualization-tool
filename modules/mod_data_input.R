# modules/mod_data_input.R
# Data Input 页面模块（包含参数设置、模型构建器、数据上传）

mod_data_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      # --- 左边：参数设置卡片 ---
      column(
        width = 4,
        div(
          class = "card-style",
          h3("Parameters"),
          textInput(
            ns("proj_title"),
            "Project title",
            placeholder = "My Analysis"
          ),
          textAreaInput(
            ns("desc"),
            "Description",
            placeholder = "Analysis notes...",
            rows = 3
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
          h4("Model Specification (SPSS Style)"),

          # 变量选择池
          selectInput(
            ns("model_vars"),
            "Select Variables:",
            choices = NULL,
            multiple = TRUE,
            selectize = TRUE
          ),

          # 构建按钮
          div(
            style = "display: flex; gap: 5px; margin-bottom: 10px;",
            actionButton(
              ns("add_main_effect"),
              "Add Main",
              class = "btn-xs btn-primary"
            ),
            actionButton(
              ns("add_interaction"),
              "Add Interaction (*)",
              class = "btn-xs btn-warning"
            ),
            actionButton(
              ns("clear_formula"),
              "Clear",
              class = "btn-xs btn-default"
            )
          ),

          # 固定效应公式显示框
          textAreaInput(
            ns("fixed_formula"),
            "Fixed Effects Structure:",
            value = "age_c + age_c2",
            rows = 3,
            resize = "vertical"
          ),

          tags$small(
            style = "color:grey;",
            "Tip: Select 2 or 3 vars and click 'Interaction' to add terms like 'sex:residence'."
          ),

          br(),
          hr(),

          # 随机斜率设置
          h5("Random Effects (Slopes)"),
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

          # 错误提示框
          uiOutput(ns("validation_alert"))
        )
      ),

      # --- 右边：数据上传与预览卡片 ---
      column(
        width = 8,
        div(
          class = "card-style",
          h3("Input data"),
          mod_upload_ui(ns("upload_module")),
          actionButton(
            ns("run_btn"),
            "Run Analysis",
            icon = icon("play"),
            class = "btn-success"
          )
        )
      )
    )
  )
}

mod_data_input_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # =========================================================
    # 1. 基础验证与警告
    # =========================================================
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

    # 年龄 > 105 时的软性警告弹窗
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

    # =========================================================
    # 2. 数据上传模块
    # =========================================================
    uploaded_data <- mod_upload_server("upload_module")

    # 监听数据上传，更新模型构建器变量选择列表
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

      covs_for_default <- setdiff(choices_vars, "age_c")
      default_fixed <- "age_c + age_c2"
      if (length(covs_for_default) > 0) {
        default_fixed <- paste(
          default_fixed,
          "+",
          paste(covs_for_default, collapse = " + ")
        )
      }
      updateTextAreaInput(session, "fixed_formula", value = default_fixed)
    })

    # =========================================================
    # 3. 模型构建器逻辑
    # =========================================================
    observeEvent(input$add_main_effect, {
      req(input$model_vars)
      current <- input$fixed_formula
      new_terms <- paste(input$model_vars, collapse = " + ")

      if (current == "") {
        updateTextAreaInput(session, "fixed_formula", value = new_terms)
      } else {
        updateTextAreaInput(
          session,
          "fixed_formula",
          value = paste(current, "+", new_terms)
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

      current <- input$fixed_formula
      new_term <- paste(input$model_vars, collapse = ":")

      if (current == "") {
        updateTextAreaInput(session, "fixed_formula", value = new_term)
      } else {
        updateTextAreaInput(
          session,
          "fixed_formula",
          value = paste(current, "+", new_term)
        )
      }
      updateSelectInput(session, "model_vars", selected = character(0))
    })

    observeEvent(input$clear_formula, {
      updateTextAreaInput(session, "fixed_formula", value = "age_c + age_c2")
    })

    # =========================================================
    # 4. 返回值（供 app.R 使用）
    # =========================================================
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
      fixed_formula = reactive(input$fixed_formula),
      period_slopes = reactive(input$period_slopes),
      cohort_slopes = reactive(input$cohort_slopes)
    ))
  })
}
