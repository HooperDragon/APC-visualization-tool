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

# 手动加载 R 文件夹下的函数
source("R/data_clean.R")
source("R/plots.R")

# 加载模块
source("modules/mod_upload.R")
source("modules/mod_descriptive.R")

# --- 默认参数 ---
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
  # 页面 1: 数据输入 (Data Input)
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
        
        /* 【新增】弹窗按钮样式：红色确认，蓝色取消 */
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
  # 页面 2: 分析结果 (Analysis Result)
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
  )
)

# --- Server 部分 ---
server <- function(input, output, session) {
  # 1. 存储硬性参数错误信息 (负数、Start>End 等)
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

  # =========================================================
  # 年龄 > 105 时的软性警告弹窗
  # =========================================================
  observeEvent(input$age_end, {
    req(input$age_end)

    # 1. 如果有红色硬性错误（比如负数），先不弹窗，优先显示红色报错
    if (!is.null(validation_error_msg())) {
      return()
    }

    # 2. 如果数值大于 105，触发弹窗
    if (input$age_end > 105) {
      showModal(modalDialog(
        title = "Warning: Unusual Age Range",
        tags$div(
          tags$p("Data for ages > 105 is often sparse or unreliable."),
          tags$p(paste0("Current input: ", input$age_end)),
          tags$p("Are you sure you want to proceed?")
        ),
        footer = tagList(
          # 两个按钮：样式在 UI 的 CSS 里定义了
          actionButton("cancel_age", "Cancel (Reset to 105)"),
          actionButton("confirm_age", "Confirm (Keep value)")
        ),
        easyClose = FALSE # 强制用户点击按钮才能关闭
      ))
    }
  })

  # 监听“取消”按钮
  observeEvent(input$cancel_age, {
    removeModal()
    updateNumericInput(session, "age_end", value = 105) # 重置回 105
  })

  # 监听“确认”按钮
  observeEvent(input$confirm_age, {
    removeModal() # 仅关闭弹窗，保留当前的大数值
  })
  # =========================================================

  # 2. 调用上传模块
  uploaded_data <- mod_upload_server("upload_module_1")

  # 3. 处理数据
analysis_ready_data <- reactive({
    req(uploaded_data())
    raw_df <- uploaded_data()
    
    # 构造参数列表
    params <- list(
      intervals = input$intervals,
      age_start = input$age_start,
      age_end   = input$age_end,
      period_start = input$period_start,
      period_end   = input$period_end
    )
    
    # 调用 R/descriptive.R 里的计算函数
    # 这一步会返回包含 age_group, rate, period 等列的汇总表
    res_df <- get_descriptive_data(raw_df, params)
    
    return(res_df)
  })

  # 4. 调用描述性分析模块
  mod_descriptive_server("desc_module_1", data_r = analysis_ready_data)

  # 5. 运行跳转逻辑
  observeEvent(input$run_btn, {
    if (!is.null(validation_error_msg())) {
      showNotification("请先修正左侧红色的参数错误！", type = "error")
      return()
    }

    if (is.null(uploaded_data())) {
      showNotification("请先上传数据文件！", type = "error")
      return()
    }

    showNotification("分析开始！正在生成图表...", type = "message")
    updateNavbarPage(session, "main_nav", selected = "tab_analysis")
  })
}

shinyApp(ui, server)

