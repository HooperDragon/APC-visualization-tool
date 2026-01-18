# modules/mod_upload.R

# --- Module UI ---
mod_upload_ui <- function(id) {
  ns <- NS(id) # 这一步是模块化的核心

  tagList(
    fileInput(
      ns("file_input"),
      "Upload data file (.csv, .dta, .sav, .xlsx)",
      multiple = FALSE,
      accept = c(".csv", ".dta", ".xlsx", ".xls", ".sav", ".Rdata", ".rds")
    ),

    h5("Data Preview:"),
    DTOutput(ns("data_preview")) # 注意这里也要用 ns
  )
}

# --- Module Server ---
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 1. 调用逻辑层函数读取数据
    raw_data <- reactive({
      req(input$file_input)
      ext <- tools::file_ext(input$file_input$name)
      # 直接调用我们在 R/data_clean.R 里写的函数
      read_and_validate_data(input$file_input$datapath, tolower(ext))
    })

    # 2. 渲染表格
    output$data_preview <- renderDT({
      df <- raw_data()
      if (is.null(df)) {
        return(NULL)
      }

      # 检查是否有错误信息属性
      err <- attr(df, "error_msg")
      validate(need(is.null(err), err))

      datatable(
        df,
        options = list(
          scrollX = TRUE,
          scrollY = 300,
          scroller = TRUE,
          dom = 't'
        )
      )
    })

    # 3. 【关键】将处理好的干净数据返回给主程序
    # 这样主程序才能拿去给“描述性分析”和“APC模型”用
    return(reactive({
      df <- raw_data()
      if (!is.null(df) && is.null(attr(df, "error_msg"))) {
        return(df)
      } else {
        return(NULL)
      }
    }))
  })
}
