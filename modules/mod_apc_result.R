# modules/mod_apc_result.R
mod_apc_result_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # --- 左侧控制面板 ---
      column(
        3,
        wellPanel(
          h4("Visual Settings"),

          # 1. 选择 X 轴 (维度)
          radioButtons(
            ns("x_axis"),
            "X Axis Dimension:",
            choices = c("Age" = "age", "Period" = "period", "Cohort" = "cohort")
          ),
          hr(),

          # 2. 选择分层变量 (Stratified by)
          pickerInput(
            ns("stratify_by"),
            "Stratified by:",
            choices = c("Null (Overall)" = "null"),
            multiple = FALSE
          )
        )
      ),

      # --- 右侧绘图区域 ---
      column(
        9,
        div(
          style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: white;",
          plotOutput(ns("trend_plot"), height = "500px")
        ),
        div(id = ns("warning_msg"), style = "color: orange; margin-top: 10px;")
      )
    )
  )
}

mod_apc_result_server <- function(id, model_r, data_r, covariates_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # =========================================================
    # 缓存机制：避免每次切换都重新计算
    # =========================================================
    cache <- reactiveValues(
      age = NULL,
      period = NULL,
      cohort = NULL,
      model_hash = NULL # 用于检测模型是否更新
    )

    # 当模型更新时，预先计算所有趋势数据
    observeEvent(model_r(), {
      req(model_r(), data_r())

      model <- model_r()
      data_model <- data_r()

      # 生成模型 hash（简单用时间戳，确保每次运行新模型都会更新）
      new_hash <- digest::digest(model$fit)

      if (is.null(cache$model_hash) || cache$model_hash != new_hash) {
        cache$model_hash <- new_hash

        # 异步预计算三种趋势（Overall 版本）
        # Age 最慢，优先计算
        withProgress(message = "Computing trends...", value = 0, {
          incProgress(0.1, detail = "Age effect...")
          cache$age <- tryCatch(
            get_model_trend_data(model, "age", "null", data_model),
            error = function(e) NULL
          )

          incProgress(0.4, detail = "Period effect...")
          cache$period <- tryCatch(
            get_model_trend_data(model, "period", "null", data_model),
            error = function(e) NULL
          )

          incProgress(0.4, detail = "Cohort effect...")
          cache$cohort <- tryCatch(
            get_model_trend_data(model, "cohort", "null", data_model),
            error = function(e) NULL
          )
        })
      }
    })

    # 1. 动态更新分层变量的选项
    observeEvent(covariates_r(), {
      req(covariates_r())
      # 选项包括 "null" 和用户实际投入模型的协变量
      opts <- c("Null (Overall)" = "null", covariates_r())
      updatePickerInput(session, "stratify_by", choices = opts)
    })

    # 2. 获取趋势数据（优先使用缓存）
    trend_data <- reactive({
      req(model_r(), data_r())

      x_axis <- input$x_axis
      stratify <- input$stratify_by

      # 如果是 Overall 且有缓存，直接返回
      if (stratify == "null") {
        cached <- switch(
          x_axis,
          "age" = cache$age,
          "period" = cache$period,
          "cohort" = cache$cohort
        )
        if (!is.null(cached)) return(cached)
      }

      # 否则重新计算（分层情况）
      get_model_trend_data(
        model = model_r(),
        x_axis = x_axis,
        group_by = stratify,
        data_model = data_r()
      )
    })

    # 3. 绘图（直接使用 trend_data）
    output$trend_plot <- renderPlot({
      plot_data <- trend_data()

      if (is.null(plot_data)) {
        if (input$x_axis != "age" && input$stratify_by != "null") {
          validate(need(
            FALSE,
            "This variable was NOT selected as a Random Slope in the model. \nPlease go back to 'Data Input' and add it to 'Period/Cohort Slopes'."
          ))
        }
        return(NULL)
      }

      plot_hapc_trend(
        plot_data,
        x_label = tools::toTitleCase(input$x_axis),
        group_label = ifelse(
          input$stratify_by == "null",
          "Overall",
          input$stratify_by
        )
      )
    })
  })
}
