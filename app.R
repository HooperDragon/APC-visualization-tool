# environment
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(ggsci)
library(tidyr)
library(dplyr)
library(haven)
library(readxl)
library(tools)
library(shinyWidgets)
library(digest)

# load R file
source("R/data_clean.R")
source("R/plots.R")
source("R/descriptive.R")
source("R/hapc_model.R")

# load modules file
source("modules/mod_upload.R")
source("modules/mod_descriptive.R")
source("modules/mod_apc_result.R")
source("modules/mod_download.R")

# default parameters
DEFAULTS <- list(
  period_start = 2000,
  period_end = 2025,
  age_start = 0,
  age_end = 105,
  intervals = 5
)

# --- UI 部分 ---
ui <- navbarPage(
  title = "APC visualization platform",
  id = "main_nav",
  theme = NULL,

  # =========================================================
  # Page 1: Data Input
  # =========================================================
  tabPanel(
    "Data Input",
    value = "tab_input",
    tags$head(
      tags$style(HTML(
        "
        /* 卡片样式 */
        .card-style {
          background-color: #ffffff;
          border: 2px solid #333;
          border-radius: 5px;
          box-shadow: 5px 5px 0px rgba(0,0,0,0.2);
          padding: 20px;
          margin-bottom: 20px;
          height: 100%;
        }
        /* 运行按钮样式 */
        #run_btn {
          width: 100%;
          font-size: 18px;
          font-weight: bold;
          margin-top: 20px;
          height: 50px;
        }
        h3 { margin-top: 0px; border-bottom: 1px solid #eee; padding-bottom: 10px; margin-bottom: 20px;}
        
        /* 弹窗按钮样式：红色确认，蓝色取消 */
        #confirm_age { background-color: #d9534f; color: white; }
        #cancel_age { background-color: #5bc0de; color: white; }
      "
      ))
    ),

    br(),
    fluidRow(
      # --- 左边：参数设置卡片 ---
      column(
        width = 4,
        div(
          class = "card-style",
          h3("Parameters"),
          textInput("proj_title", "Project title", placeholder = "My Analysis"),
          textAreaInput(
            "desc",
            "Description",
            placeholder = "Analysis notes...",
            rows = 3
          ),

          tags$label("Period range"),
          fluidRow(
            column(
              5,
              numericInput("period_start", NULL, value = DEFAULTS$period_start)
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
              numericInput("period_end", NULL, value = DEFAULTS$period_end)
            )
          ),

          tags$label("Age range"),
          fluidRow(
            column(
              5,
              numericInput("age_start", NULL, value = DEFAULTS$age_start)
            ),
            column(
              2,
              div(
                ":",
                style = "text-align:center; font-weight:bold; font-size:20px;"
              )
            ),
            column(5, numericInput("age_end", NULL, value = DEFAULTS$age_end))
          ),

          numericInput(
            "intervals",
            "Intervals (year)",
            value = DEFAULTS$intervals
          ),
          hr(),
          h4("Model Specification (SPSS Style)"),

          # 1. 变量选择池
          selectInput(
            "model_vars",
            "Select Variables:",
            choices = NULL,
            multiple = TRUE,
            selectize = TRUE
          ),

          # 2. 构建按钮
          div(
            style = "display: flex; gap: 5px; margin-bottom: 10px;",
            actionButton(
              "add_main_effect",
              "Add Main",
              class = "btn-xs btn-primary"
            ),
            actionButton(
              "add_interaction",
              "Add Interaction (*)",
              class = "btn-xs btn-warning"
            ),
            actionButton("clear_formula", "Clear", class = "btn-xs btn-default")
          ),

          # 3. 固定效应公式显示框
          textAreaInput(
            "fixed_formula",
            "Fixed Effects Structure:",
            value = "age_c + age_c2", # age effect has to be in the model
            rows = 3,
            resize = "vertical"
          ),

          tags$small(
            style = "color:grey;",
            "Tip: Select 2 or 3 vars and click 'Interaction' to add terms like 'sex:residence'."
          ),

          br(),
          hr(),

          # 4. 随机斜率设置 (Random Slopes)
          h5("Random Effects (Slopes)"),
          pickerInput(
            "period_slopes",
            "Vars varying by Period (cov | period):",
            choices = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            "cohort_slopes",
            "Vars varying by Cohort (cov | cohort):",
            choices = NULL,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),

          # 错误提示框 (用于显示红色警告)
          uiOutput("validation_alert")
        )
      ),

      # --- 右边：数据上传与预览卡片 ---
      column(
        width = 8,
        div(
          class = "card-style",
          h3("Input data"),
          mod_upload_ui("upload_module_1"),
          actionButton(
            "run_btn",
            "Run Analysis",
            icon = icon("play"),
            class = "btn-success"
          )
        )
      )
    )
  ),

  # =========================================================
  # Page 2: Analysis Result
  # =========================================================
  tabPanel(
    "Analysis Report",
    value = "tab_analysis",
    fluidRow(
      column(
        12,
        div(style = "margin-top: 20px;", mod_descriptive_ui("desc_module_1"))
      )
    )
  ),

  # =========================================================
  # Page 3: Model Results
  # =========================================================
  tabPanel(
    "Model Results",
    value = "tab_model",
    mod_apc_result_ui("apc_result_1")
  ),

    # =========================================================
  # 页面 4: 数据导出 (Export)
  # =========================================================
  tabPanel("Export Results", value = "tab_export",
           icon = icon("download"), # 加个图标更直观
           mod_download_ui("download_module_1")
  )
)

# --- Server ---
server <- function(input, output, session) {
  # =========================================================
  # 1. 基础验证与警告 (Basic Validation)
  # =========================================================

  # 存储硬性参数错误信息
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
          actionButton(
            "cancel_age",
            "Cancel (Reset to 105)",
            id = "cancel_age"
          ),
          actionButton(
            "confirm_age",
            "Confirm (Keep value)",
            id = "confirm_age"
          )
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
  # 2. 数据上传与清洗 (Data Upload)
  # =========================================================

  # 调用上传模块
  uploaded_data <- mod_upload_server("upload_module_1")

  # --- 监听数据上传，更新模型构建器的变量选择列表 ---
  observeEvent(uploaded_data(), {
    df <- uploaded_data()
    req(df)

    # 获取所有列名
    all_vars <- names(df)
    # 排除不需要放入模型的列 (Age, Period, Disease 已经默认处理，Disease 是因变量)
    exclude_vars <- c("Period", "period", "Disease", "disease", "Age", "age")

    # 剩余的作为潜在协变量
    choices_vars <- setdiff(all_vars, exclude_vars)

    # 这是一个小技巧：我们在 hapc_model.R 里生成了 'age_c' (中心化年龄)
    # 所以我们在选项里手动加上 'age_c'，方便用户构建交互项
    choices_vars <- c("age_c", choices_vars)

    # 更新 UI
    updateSelectInput(session, "model_vars", choices = choices_vars)

    # Random slopes 选择：排除 age_c（年龄随 period/cohort 的随机斜率没有意义且容易误导）
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

    # 默认：把所有协变量自动纳入主效应（保持 age_c + age_c2 必含）
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
  # 3. 描述性分析逻辑 (Descriptive Analysis)
  # =========================================================

  # 根据参数筛选数据
  analysis_ready_data <- reactive({
    req(uploaded_data())
    raw_df <- uploaded_data()

    # 构造参数列表
    params <- list(
      intervals = input$intervals,
      age_start = input$age_start,
      age_end = input$age_end,
      period_start = input$period_start,
      period_end = input$period_end
    )

    # 调用计算函数
    res_df <- get_descriptive_data(raw_df, params)
    return(res_df)
  })

  # 调用描述性分析模块
  mod_descriptive_server("desc_module_1", data_r = analysis_ready_data)

  # =========================================================
  # 4. 模型构建器逻辑 (Model Builder Logic)
  # =========================================================

  # 添加主效应
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

    # 每次添加后清空选择，方便下次选择
    updateSelectInput(session, "model_vars", selected = character(0))
  })

  # 添加交互项
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

    # 每次添加后清空选择，方便下次选择
    updateSelectInput(session, "model_vars", selected = character(0))
  })

  # 清空公式
  observeEvent(input$clear_formula, {
    updateTextAreaInput(session, "fixed_formula", value = "age_c + age_c2")
  })

  # =========================================================
  # 5. 运行分析核心逻辑 (Run Analysis)
  # =========================================================

  # 存储模型结果的 Reactive Values
  model_results <- reactiveValues(
    model = NULL,
    summary_table = NULL,
    data_for_model = NULL # 缓存预处理后的数据，避免重复计算
  )

  observeEvent(input$run_btn, {
    # A. 基础检查
    if (!is.null(validation_error_msg())) {
      showNotification("Please fix invalid parameters first!", type = "error")
      return()
    }
    if (is.null(uploaded_data())) {
      showNotification("Please upload data first!", type = "error")
      return()
    }

    showNotification(
      "Analysis started! Building HAPC model...",
      type = "message"
    )

    # 跳转到结果页
    updateNavbarPage(session, "main_nav", selected = "tab_analysis")

    # --- B. 准备模型数据 ---
    raw_df <- uploaded_data()

    # 构造参数
    params <- list(
      intervals = input$intervals
      # age_start 等在 prepare_hapc_data 内部动态计算，或者也可以传进去
    )

    # 1. 解析公式中用到的所有变量，以便在预处理时转为因子
    # 使用正则提取变量名，排除 age_c, age_c2 和数字
    formula_vars <- unique(unlist(strsplit(
      input$fixed_formula,
      "[\\+\\*\\:]|\\s+"
    )))
    formula_vars <- setdiff(formula_vars, c("age_c", "age_c2", "", "1", "0"))

    # 还要加上随机斜率里选到的变量（用于因子化/建模）
    all_covariates <- unique(c(
      formula_vars,
      input$period_slopes,
      input$cohort_slopes
    ))

    # 2. 数据预处理 (hapc_model.R)
    data_for_model <- prepare_hapc_data(raw_df, params, all_covariates)

    # 缓存预处理后的数据，避免切换趋势图时重复计算
    model_results$data_for_model <- data_for_model

    # --- C. 运行模型 ---
    # 构造配置列表
    model_config <- list(
      fixed_formula = input$fixed_formula,
      period_slopes = input$period_slopes,
      cohort_slopes = input$cohort_slopes
    )

    # 运行 HAPC (hapc_model.R)
    # 这一步可能会花点时间
    mod <- run_dynamic_hapc(data_for_model, model_config)

    # --- D. 结果处理与收敛性检查 ---

    if (is.null(mod)) {
      showNotification(
        "Model failed to run (Error). Please check your formula or data.",
        type = "error",
        duration = NULL
      )
      return()
    }

    # 【核心需求：收敛性警告逻辑】
    if (isTRUE(attr(mod, "convergence_warning"))) {
      warning_msg <- "Warning: Model failed to converge. The model may be too complex, please simplify interactions."

      # 弹出持久警告 (duration = NULL)
      showNotification(warning_msg, type = "warning", duration = NULL)
    } else {
      showNotification("Model converged successfully!", type = "message")
    }

    # 保存模型结果
    model_results$model <- mod

    # 生成 bruceR 表格 (HTML)
    model_results$summary_table <- get_fixed_effects_bruceR(mod)
  })

  # --- E. 渲染模型结果 (如有 UI 对应) ---
  # 这里假设你在 UI 里放了一个 htmlOutput("model_output_table")
  output$model_output_table <- renderUI({
    req(model_results$summary_table)
    HTML(model_results$summary_table)
  })

  # ... (在 server 函数的最后) ...

  # 定义一个 reactive 来获取所有协变量名 (用于更新下拉框)
  covariates_list <- reactive({
    req(input$fixed_formula)
    vars <- unique(unlist(strsplit(input$fixed_formula, "[\\+\\*\\:]|\\s+")))
    setdiff(vars, c("age_c", "age_c2", "", "1", "0"))
  })

  # 调用结果展示模块
  # 注意：使用缓存的 data_for_model，避免切换趋势图时重复计算
  mod_apc_result_server(
    "apc_result_1",
    model_r = reactive(model_results$model),
    data_r = reactive(model_results$data_for_model),
    covariates_r = covariates_list
  )

  # 调用下载模块
  # 注意：我们需要把之前算出来的 model_results 和一些趋势数据传进去
  # 暂时先留空，等下一阶段我们实现下载逻辑时再连接数据
  mod_download_server("download_module_1", 
                      model_results = model_results,
                      trend_data = NULL)
}

shinyApp(ui, server)
