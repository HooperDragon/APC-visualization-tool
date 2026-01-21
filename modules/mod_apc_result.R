# modules/mod_apc_result.R
mod_apc_result_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # --- 左侧控制面板 ---
      column(3,
        wellPanel(
          h4("Visual Settings"),
          
          # 1. 选择 X 轴 (维度)
          radioButtons(ns("x_axis"), "X Axis Dimension:",
                       choices = c("Age" = "age", 
                                   "Period" = "period", 
                                   "Cohort" = "cohort")),
          hr(),
          
          # 2. 选择分层变量 (Stratified by)
          pickerInput(ns("stratify_by"), "Stratified by:",
                      choices = c("Null (Overall)" = "null"),
                      multiple = FALSE)
        )
      ),
      
      # --- 右侧绘图区域 ---
      column(9,
        div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: white;",
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
    
    # 1. 动态更新分层变量的选项
    observeEvent(covariates_r(), {
      req(covariates_r())
      # 选项包括 "null" 和用户实际投入模型的协变量
      opts <- c("Null (Overall)" = "null", covariates_r())
      updatePickerInput(session, "stratify_by", choices = opts)
    })
    
    # 2. 计算并绘图
    output$trend_plot <- renderPlot({
      req(model_r(), data_r())
      
      model <- model_r()
      data_model <- data_r()
      
      # 调用 hapc_model.R 里的提取函数
      # 注意：如果用户选了分层，但模型里没有对应的随机斜率，这个函数会返回 NULL
      plot_data <- get_model_trend_data(
        model = model,
        x_axis = input$x_axis,
        group_by = input$stratify_by,
        data_model = data_model
      )
      
      if (is.null(plot_data)) {
        # 如果返回空，说明该变量没有被设为随机斜率
        if (input$x_axis != "age" && input$stratify_by != "null") {
           validate(need(FALSE, "This variable was NOT selected as a Random Slope in the model. \nPlease go back to 'Data Input' and add it to 'Period/Cohort Slopes'."))
        }
        return(NULL)
      }
      
      # 调用 plots.R 里的绘图函数
      plot_hapc_trend(plot_data, 
                      x_label = tools::toTitleCase(input$x_axis), 
                      group_label = ifelse(input$stratify_by=="null", "Overall", input$stratify_by))
    })
  })
}