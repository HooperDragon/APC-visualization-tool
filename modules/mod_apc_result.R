#### ui ####
mod_apc_result_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      ## left colomun: settings
      column(
        3,
        wellPanel(
          h4("Visual Settings"),

          # x axis
          radioButtons(
            ns("x_axis"),
            "X Axis Dimension:",
            choices = c("Age" = "age", "Period" = "period", "Cohort" = "cohort")
          ),
          hr(),

          # stratified by
          pickerInput(
            ns("stratify_by"),
            "Stratified by:",
            choices = c("Null (Overall)" = "null"),
            multiple = FALSE
          )
        )
      ),

      ## right column: plots
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

#### server ####
mod_apc_result_server <- function(
  id,
  model_r,
  data_r,
  covariates_r,
  period_slopes_r,
  cohort_slopes_r
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## cache！！需要重新修改，没有在好好运行吧
    cache <- reactiveValues(
      age = NULL,
      period = NULL,
      cohort = NULL,
      model_hash = NULL # whether model is refreshed
    )

    ## when refreshed, renew data
    observeEvent(model_r(), {
      req(model_r(), data_r())

      model <- model_r()
      data_model <- data_r()

      # generate hash for current model fit
      new_hash <- digest::digest(model$fit)

      if (is.null(cache$model_hash) || cache$model_hash != new_hash) {
        cache$model_hash <- new_hash

        # calculate three trends data
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

    # 1. 动态更新分层变量的选项（根据 x 轴维度）
    observeEvent(
      list(input$x_axis, covariates_r(), period_slopes_r(), cohort_slopes_r()),
      {
        x_axis <- input$x_axis
        base_opts <- c("Null (Overall)" = "null")

        if (x_axis == "age") {
          covs <- covariates_r()
          opts <- c(base_opts, covs)
        } else if (x_axis == "period") {
          slopes <- period_slopes_r()
          opts <- c(base_opts, slopes)
        } else {
          slopes <- cohort_slopes_r()
          opts <- c(base_opts, slopes)
        }

        updatePickerInput(session, "stratify_by", choices = opts)
      },
      ignoreInit = FALSE
    )

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

    ## draw plots
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
