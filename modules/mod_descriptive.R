#ui
mod_descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # =========================================================
      # 左侧区域 (占 7/12)：控制 + 3D图 + 滑块
      # =========================================================
      column(
        7,
        # 1. 维度选择 (Radio Buttons)
        div(
          style = "margin-bottom: 15px;",
          tags$strong(
            "Slice Dimension: ",
            style = "font-size: 16px; margin-right: 10px;"
          ),
          radioButtons(
            ns("slice_dim"),
            label = NULL,
            choices = c(
              "Period" = "period",
              "Age" = "age",
              "Cohort" = "cohort"
            ),
            inline = TRUE
          )
        ),

        # 2. 3D 图容器 (带灰色边框)
        div(
          style = "border: 2px solid #e0e0e0; border-radius: 8px; padding: 5px; background: white; box-shadow: 2px 2px 5px rgba(0,0,0,0.05);",

          # 稍微调低高度，确保滑块能同屏显示
          plotlyOutput(ns("plot_3d"), height = "550px")
        ),

        # 3. 滑块 (紧贴 3D 图下方)
        wellPanel(
          style = "margin-top: 15px; padding: 15px 20px; background-color: #f8f9fa; border: 1px solid #ddd;",
          uiOutput(ns("slider_ui"))
        )
      ),

      # =========================================================
      # 右侧区域 (占 5/12)：2D 切面图
      # =========================================================
      column(
        5,
        # 使用 card 样式包裹，与左侧呼应
        div(
          style = "margin-top: 45px; height: 100%; padding-left: 10px;",

          h4(
            "2D Section View",
            style = "text-align: center; margin-bottom: 25px; color: #333;"
          ),

          # 2D 图高度设为 500px，足够大且清晰
          plotOutput(ns("plot_2d"), height = "500px"),

          # 底部说明
          div(
            style = "margin-top: 20px; color: #777; font-size: 0.9em; text-align: center; font-style: italic;",
            icon("info-circle"),
            " Drag the slider on the left or click the 3D map to update."
          )
        )
      )
    )
  )
}

#server
mod_descriptive_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 用于追踪 3D 图是否已渲染
    plot_rendered <- reactiveVal(FALSE)

    # 1. 动态生成滑块 (UI)
    output$slider_ui <- renderUI({
      df <- data_r()
      req(df, input$slice_dim)
      if (!"period" %in% names(df)) {
        return(NULL)
      }

      min_val <- 0
      max_val <- 0
      val <- 0
      step_val <- 1
      label_txt <- ""

      if (input$slice_dim == "period") {
        vals <- sort(unique(df$period))
        label_txt <- span(icon("clock"), " Select Period:")

        # 使用 sliderTextInput 确保只能选择存在的 Period (离散选择)
        shinyWidgets::sliderTextInput(
          inputId = ns("slice_val"),
          label = label_txt,
          choices = vals,
          selected = vals[max(1, floor(length(vals) / 2))], # 选中中间值
          grid = TRUE,
          width = "100%",
          animate = animationOptions(interval = 1000, loop = FALSE) # 稍微慢一点
        )
      } else {
        # Age 和 Cohort 保持数值滑块 (通常是等间距的)
        if (input$slice_dim == "age") {
          vals <- sort(unique(df$age_start))
          label_txt <- span(icon("user"), " Select Age Group:")
          step_val <- vals[2] - vals[1]
        } else if (input$slice_dim == "cohort") {
          vals <- sort(unique(df$cohort_start))
          label_txt <- span(icon("users"), " Select Cohort:")
          step_val <- vals[2] - vals[1]
        }

        sliderInput(
          ns("slice_val"),
          label_txt,
          min = min(vals),
          max = max(vals),
          value = median(vals),
          step = step_val,
          width = "100%",
          animate = animationOptions(interval = 500, loop = FALSE)
        )
      }
    })

    # 2. 渲染 3D 底图
    output$plot_3d <- renderPlotly({
      df <- data_r()
      req(df)

      # 生成底图
      p <- plot_3d_base(df)

      # 注册点击事件
      if (!is.null(p)) {
        p <- event_register(p, "plotly_click")
      }

      # 标记图已渲染
      plot_rendered(TRUE)

      p
    })

    # 3. Proxy 更新 3D 切面
    observeEvent(input$slice_val, {
      req(input$slice_dim)
      df <- data_r()
      mesh_data <- get_plane_mesh(df, input$slice_dim, input$slice_val)

      plotlyProxy("plot_3d", session) %>%
        plotlyProxyInvoke(
          "restyle",
          list(
            x = list(mesh_data$x),
            y = list(mesh_data$y),
            z = list(mesh_data$z)
          ),
          list(1)
        )
    })

    # 4. 点击 3D 图反向更新滑块 (只有图渲染后才监听)
    observe({
      # 确保图已渲染后才尝试读取事件
      req(plot_rendered())

      click <- event_data("plotly_click", source = "A")
      req(click) # 只有当点击事件真正发生时才执行

      if (input$slice_dim == "period") {
        # Period 使用 sliderTextInput，需要找到最近的有效值并更新
        df <- data_r()
        if (!is.null(df)) {
          valid_periods <- sort(unique(df$period))
          # 找到最接近点击值的 Period
          idx <- which.min(abs(valid_periods - click$y))
          best_val <- valid_periods[idx]
          shinyWidgets::updateSliderTextInput(
            session,
            "slice_val",
            selected = best_val
          )
        }
      } else {
        # Age 和 Cohort 使用普通 Update
        new_val <- NULL
        if (input$slice_dim == "age") {
          new_val <- round(click$x / 5) * 5
        } else if (input$slice_dim == "cohort") {
          new_val <- round((click$y - click$x) / 5) * 5
        }

        if (!is.null(new_val)) {
          updateSliderInput(session, "slice_val", value = new_val)
        }
      }
    })

    # 5. 更新 2D 图
    output$plot_2d <- renderPlot({
      df <- data_r()
      req(input$slice_dim, input$slice_val)
      plot_2d_slice_generic(df, input$slice_dim, input$slice_val)
    })
  })
}
