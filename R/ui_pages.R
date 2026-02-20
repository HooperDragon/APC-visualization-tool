#### 3D figure ####
page_analysis_report <- function() {
  fluidRow(
    column(
      12,
      div(style = "margin-top: 20px;", mod_descriptive_ui("desc_module_1"))
    )
  )
}
#### model results ####
page_model_results <- function() {
  mod_apc_result_ui("apc_result_1")
}

#### export ####
page_export <- function() {
  mod_download_ui("download_module_1")
}

#### main ui ####
build_ui <- function() {
  tagList(
    tags$head(tags$style(HTML(APP_CSS))),
    navbarPage(
      title = "APC visualization platform",
      id = "main_nav",
      theme = NULL,

      ## Page 1: Data Input
      tabPanel(
        "Data Input",
        value = "tab_input",
        mod_data_input_ui("data_input_1")
      ),

      ## Page 2: 3D figure and 2D slice
      tabPanel("3D Figure", value = "tab_analysis", page_analysis_report()),

      ## Page 3: model results
      tabPanel("Model Results", value = "tab_model", page_model_results()),

      ## Page 4: export results
      tabPanel(
        "Export Results",
        value = "tab_export",
        icon = icon("download"),
        page_export()
      )
    )
  )
}
