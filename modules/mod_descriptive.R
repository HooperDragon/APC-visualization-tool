# modules/mod_descriptive.R

# --- UI 部分 ---
mod_descriptive_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # 左侧：3D 图 (占 7/12)
      column(7, 
        h4("3D Surface View (可旋转/缩放)"),
        plotlyOutput(ns("plot_3d"), height = "500px")
      ),
      
      # 右侧：切面控制 + 2D 图 (占 5/12)
      column(5, 
        wellPanel(
          h4("Section View Control"),
          # 这是一个动态 UI，因为滑块的范围取决于上传的数据
          uiOutput(ns("slider_ui"))
        ),
        plotOutput(ns("plot_2d"), height = "350px")
      )
    )
  )
}

# --- Server 部分 ---
mod_descriptive_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. 动态生成滑块 (根据数据的 Period 范围)
    output$slider_ui <- renderUI({
      df <- data_r()
      req(df)
      
      periods <- sort(unique(df$Period))
      sliderInput(ns("period_slice"), "Select Period Slice:",
                  min = min(periods), 
                  max = max(periods), 
                  value = median(periods), 
                  step = periods[2] - periods[1], # 自动识别步长
                  animate = animationOptions(interval = 1000, loop = FALSE)) # 增加播放按钮
    })
    
    # 2. 渲染左侧 3D 图
    output$plot_3d <- renderPlotly({
      df <- data_r()
      req(df)
      # 调用 R/plots.R 里的函数
      plot_3d_surface(df)
    })
    
    # 3. 渲染右侧 2D 切面图 (与滑块联动)
    output$plot_2d <- renderPlot({
      df <- data_r()
      req(input$period_slice)
      
      # 调用 R/plots.R 里的函数
      plot_period_slice(df, input$period_slice)
    })
  })
}