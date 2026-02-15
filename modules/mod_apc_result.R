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
          plotOutput(ns("trend_plot"), height = "700px")
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

    model_has_convergence_problem <- reactive({
      model <- model_r()
      if (is.null(model)) {
        return(TRUE)
      }
      isTRUE(attr(model, "convergence_warning"))
    })

    trend_msg <- reactiveVal(NULL)

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

        trend_msg(NULL)

        # calculate three trends data
        withProgress(message = "Computing trends...", value = 0, {
          incProgress(0.1, detail = "Age effect...")
          cache$age <- tryCatch(
            get_model_trend_data(model, "age", "null", data_model),
            error = function(e) {
              trend_msg(paste0("Age trend failed: ", conditionMessage(e)))
              NULL
            }
          )

          incProgress(0.4, detail = "Period effect...")
          cache$period <- tryCatch(
            get_model_trend_data(model, "period", "null", data_model),
            error = function(e) {
              trend_msg(paste0("Period trend failed: ", conditionMessage(e)))
              NULL
            }
          )

          incProgress(0.4, detail = "Cohort effect...")
          cache$cohort <- tryCatch(
            get_model_trend_data(model, "cohort", "null", data_model),
            error = function(e) {
              trend_msg(paste0("Cohort trend failed: ", conditionMessage(e)))
              NULL
            }
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

      if (model_has_convergence_problem()) {
        trend_msg(
          "Model convergence problem. Results/plots may be unreliable; try simplifying interactions or random slopes."
        )
      }

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
      out <- tryCatch(
        get_model_trend_data(
          model = model_r(),
          x_axis = x_axis,
          group_by = stratify,
          data_model = data_r()
        ),
        error = function(e) {
          trend_msg(
            paste0(
              "Trend computation failed (x_axis=",
              x_axis,
              ", stratify_by=",
              stratify,
              "): ",
              conditionMessage(e)
            )
          )
          NULL
        }
      )

      # detect silent NULL (get_model_trend_data returned NULL internally)
      if (is.null(out)) {
        cur <- trend_msg()
        if (is.null(cur) || !nzchar(cur)) {
          trend_msg(
            paste0(
              "No trend data returned (x_axis=",
              x_axis,
              ", stratify_by=",
              stratify,
              "). The random-effects structure may not support this stratification."
            )
          )
        }
        return(NULL)
      }

      # detect NaN in predictions (common with singular convergence)
      if (is.data.frame(out) && "prob" %in% names(out)) {
        n_bad <- sum(is.na(out$prob) | is.nan(out$prob))
        if (n_bad > 0 && n_bad == nrow(out)) {
          trend_msg(
            paste0(
              "All predicted values are NaN (x_axis=",
              x_axis,
              ", stratify_by=",
              stratify,
              "). Model estimates are unreliable — try simplifying random slopes."
            )
          )
          return(NULL)
        }
      }

      # pass through note attribute
      note <- attr(out, "note", exact = TRUE)
      if (!is.null(note) && nzchar(note)) {
        if (model_has_convergence_problem()) {
          trend_msg(paste0(note, " (model also has convergence problems)"))
        } else {
          trend_msg(note)
        }
      }

      out
    })

    output$warning_msg <- renderUI({
      msg <- trend_msg()
      if (is.null(msg) || !nzchar(msg)) {
        return(NULL)
      }

      if (model_has_convergence_problem()) {
        div(style = "color: #b00020;", msg)
      } else {
        div(style = "color: #b26a00;", msg)
      }
    })

    ## draw plots
    output$trend_plot <- renderPlot({
      plot_data <- trend_data()

      if (is.null(plot_data)) {
        msg <- trend_msg()
        if (!is.null(msg) && nzchar(msg)) {
          validate(need(FALSE, msg))
        } else {
          validate(need(
            FALSE,
            "No data available for this plot. Check the warning message below."
          ))
        }
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
