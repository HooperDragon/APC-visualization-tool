# R/ui_pages.R
# UI 页面组装函数

#' 创建 Analysis Report 页面
page_analysis_report <- function() {
  fluidRow(
    column(
      12,
      div(style = "margin-top: 20px;", mod_descriptive_ui("desc_module_1"))
    )
  )
}

#' 创建 Model Results 页面
page_model_results <- function() {
  mod_apc_result_ui("apc_result_1")
}

#' 创建 Export 页面
page_export <- function() {
  mod_download_ui("download_module_1")
}

#' 组装完整的 UI
build_ui <- function() {
  navbarPage(
    title = "APC visualization platform",
    id = "main_nav",
    theme = NULL,

    # 全局样式
    tags$head(tags$style(HTML(APP_CSS))),

    # Page 1: Data Input
    tabPanel(
      "Data Input",
      value = "tab_input",
      mod_data_input_ui("data_input_1")
    ),

    # Page 2: 3D Figure
    tabPanel("3D Figure", value = "tab_analysis", page_analysis_report()),

    # Page 3: Model Results
    tabPanel("Model Results", value = "tab_model", page_model_results()),

    # Page 4: Export
    tabPanel(
      "Export Results",
      value = "tab_export",
      icon = icon("download"),
      page_export()
    )
  )
}
