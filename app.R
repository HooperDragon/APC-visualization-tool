#### packages ####
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

#### sources ####
source("R/config.R")
source("R/data_clean.R")
source("R/plots.R")
source("R/descriptive_fig.R")
source("R/descriptive_table.R")
source("R/hapc_model.R")
source("R/download_tables.R")
source("R/server_logic.R")
source("R/ui_pages.R")

source("modules/mod_descriptive.R")
source("modules/mod_apc_result.R")
source("modules/mod_download.R")
source("modules/mod_data_input.R")

#### ui ####
ui <- build_ui()

#### server ####
server <- function(input, output, session) {
  ## module of data input and parameter setting
  data_input <- mod_data_input_server("data_input_1", parent_session = session)
  model_results <- reactiveValues(
    model = NULL,
    summary_table = NULL,
    data_for_model = NULL
  )

  ## module of descriptive figures
  cleaned_grouped_data <- reactive({
    req(data_input$uploaded_data())
    df_filtered <- filter_data_by_params(
      data_input$uploaded_data(),
      data_input$params()
    )
    add_age_cohort_groups(df_filtered, data_input$params())
  })

  analysis_ready_data <- reactive({
    get_descriptive_fig(cleaned_grouped_data(), data_input$params())
  })

  mod_descriptive_server("desc_module_1", data_r = analysis_ready_data)

  ## module of model fitting and results
  observeEvent(data_input$run_btn(), {
    # check pre-conditions
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
    ) # 在这里加一个进度条
    updateNavbarPage(session, "main_nav", selected = "tab_analysis")

    # model fitting
    result <- run_analysis(
      raw_df = cleaned_grouped_data(),
      params = data_input$params(),
      fixed_formula = data_input$fixed_formula(),
      period_slopes = data_input$period_slopes(),
      cohort_slopes = data_input$cohort_slopes()
    )

    # show notifications based on result
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

  # covariate parsing
  covariates_list <- reactive({
    req(data_input$fixed_formula())
    parse_covariates(data_input$fixed_formula())
  })

  # HAPC results
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

#### run the app ####
shinyApp(ui, server)
