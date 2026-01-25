# --- 1. 加载依赖包 ---
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

# --- 2. 加载业务逻辑和模块 ---
source("R/config.R")
source("R/data_clean.R")
source("R/plots.R")
source("R/descriptive.R")
source("R/hapc_model.R")
source("R/server_logic.R")
source("R/ui_pages.R")

source("modules/mod_upload.R")
source("modules/mod_descriptive.R")
source("modules/mod_apc_result.R")
source("modules/mod_download.R")
source("modules/mod_data_input.R")

# --- 3. 构建 UI ---
ui <- build_ui()

# --- 4. Server 调度 ---
server <- function(input, output, session) {
  # A. 初始化各模块
  data_input <- mod_data_input_server("data_input_1", parent_session = session)
  model_results <- reactiveValues(
    model = NULL,
    summary_table = NULL,
    data_for_model = NULL
  )

  # B. 描述性分析数据
  analysis_ready_data <- reactive({
    req(data_input$uploaded_data())
    get_descriptive_data(data_input$uploaded_data(), data_input$params())
  })

  mod_descriptive_server("desc_module_1", data_r = analysis_ready_data)

  # C. 运行分析（点击按钮触发）
  observeEvent(data_input$run_btn(), {
    # 前置检查
    if (!is.null(data_input$validation_error_msg())) {
      showNotification("Please fix invalid parameters first!", type = "error")
      return()
    }
    if (is.null(data_input$uploaded_data())) {
      showNotification("Please upload data first!", type = "error")
      return()
    }

    showNotification(
      "Analysis started! Building HAPC model...",
      type = "message"
    )
    updateNavbarPage(session, "main_nav", selected = "tab_analysis")

    # 调用核心分析逻辑
    result <- run_analysis(
      raw_df = data_input$uploaded_data(),
      params = data_input$params(),
      fixed_formula = data_input$fixed_formula(),
      period_slopes = data_input$period_slopes(),
      cohort_slopes = data_input$cohort_slopes()
    )

    # 处理结果
    if (!result$success) {
      showNotification(result$message, type = "error", duration = NULL)
      return()
    }

    showNotification(
      result$message,
      type = if (result$converged) "message" else "warning",
      duration = NULL
    )

    model_results$model <- result$model
    model_results$data_for_model <- result$data_for_model
    model_results$summary_table <- result$summary_table
  })

  # D. 协变量列表（供子模块使用）
  covariates_list <- reactive({
    req(data_input$fixed_formula())
    parse_covariates(data_input$fixed_formula())
  })

  # E. 调用结果展示和下载模块
  mod_apc_result_server(
    "apc_result_1",
    model_r = reactive(model_results$model),
    data_r = reactive(model_results$data_for_model),
    covariates_r = covariates_list
  )

  mod_download_server(
    "download_module_1",
    model_results = model_results,
    trend_data = NULL
  )
}

# --- 5. 启动应用 ---
shinyApp(ui, server)
