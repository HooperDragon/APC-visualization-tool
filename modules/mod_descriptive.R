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

# --- Server ---
mod_descriptive_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ... (滑块逻辑保持不变) ...
    
    # 渲染左侧 3D 图
    output$plot_3d <- renderPlotly({
      df <- data_r()
      req(df)
      
      # 这里先对 df 进行处理，生成 descriptive 数据
      # 我们需要获取 Parameters 里的间隔设置
      # 但目前 data_r 传进来的是原始 df。
      # 这是一个架构小问题：我们需要在这里调用 get_descriptive_data
      
      # 暂时为了跑通，假设 data_r() 已经是处理好 descriptive 数据
      # (下一条回复我们会修正 app.R 里的数据传递逻辑)
      
      plot_3d_apc(df) 
    })
    
    # ... (2D图逻辑) ...
  })
}