# modules/mod_download.R

mod_download_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # 引入一些美化样式 (如果 app.R 里已经有了 card-style，这里会自动继承，无需重复定义)
    # 这里加一点点针对下载按钮的微调
    tags$head(tags$style(HTML(
      "
      .download-btn-large { 
        width: 100%; 
        margin-bottom: 10px; 
        font-size: 16px; 
        height: 45px; 
        text-align: left; 
        padding-left: 20px;
      }
      .preview-container {
        border-top: 1px solid #eee;
        margin-top: 20px;
        padding-top: 20px;
      }
    "
    ))),

    fluidRow(
      # =========================================================
      # 左侧卡片：批量一键下载 (Batch Download)
      # =========================================================
      column(
        4,
        div(
          class = "card-style",
          h4(icon("box-archive"), " Batch Export"),
          tags$p(
            "Download all results and models in a single package.",
            style = "color: grey; font-size: 0.9em;"
          ),
          hr(),

          # 1. 下载 RData (包含所有对象，适合 R 用户)
          downloadButton(
            ns("dl_rdata"),
            " Download All as .RData",
            class = "btn-primary download-btn-large"
          ),

          # 2. 下载 Zip (包含所有 Excel/CSV，适合非 R 用户)
          downloadButton(
            ns("dl_zip"),
            " Download All Tables (.zip)",
            class = "btn-warning download-btn-large"
          ),

          br(),
          tags$small(
            icon("info-circle"),
            " The .zip file contains separate Excel files for Fixed Effects, Random Effects, and Trend Data."
          )
        )
      ),

      # =========================================================
      # 右侧卡片：单表下载 (Individual Download)
      # =========================================================
      column(
        8,
        div(
          class = "card-style",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            h4(icon("table"), " Individual Table Export"),
            # 在右上角放下载按钮
            downloadButton(
              ns("dl_single"),
              "Download Selected",
              class = "btn-success"
            )
          ),
          hr(),

          fluidRow(
            # 选择要下载哪个表
            column(
              6,
              selectInput(
                ns("select_table"),
                "Select Table:",
                choices = c(
                  "Fixed Effects (OR & CI)" = "fixed",
                  "Random Effects (Period)" = "re_period",
                  "Random Effects (Cohort)" = "re_cohort",
                  "Age Trend Data" = "age_trend",
                  "Period Trend Data" = "period_trend",
                  "Cohort Trend Data" = "cohort_trend"
                ),
                width = "100%"
              )
            ),
            # 选择格式
            column(
              6,
              radioButtons(
                ns("file_format"),
                "File Format:",
                choices = c("Excel (.xlsx)" = "xlsx", "CSV (.csv)" = "csv"),
                inline = TRUE
              )
            )
          ),

          # 表格预览区域 (让用户确认这就是他们想要的表)
          div(
            class = "preview-container",
            h5("Data Preview (First 10 rows):"),
            DTOutput(ns("preview_table"))
          )
        )
      )
    )
  )
}

mod_download_server <- function(id, model_results, trend_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: safe access to model and cached data
    get_model_obj <- reactive({
      if (
        !is.null(model_results) &&
          is.list(model_results) &&
          !is.null(model_results$model)
      ) {
        return(model_results$model)
      }
      return(NULL)
    })

    get_data_model <- reactive({
      if (
        !is.null(model_results) &&
          is.list(model_results) &&
          !is.null(model_results$data_for_model)
      ) {
        return(model_results$data_for_model)
      }
      return(NULL)
    })

    # Reactive: selected table (data.frame)
    selected_table <- reactive({
      tbl_name <- input$select_table
      model <- get_model_obj()
      data_model <- get_data_model()

      if (tbl_name == "fixed") {
        if (is.null(model)) {
          return(data.frame(Message = "Model not available"))
        }
        return(tryCatch(
          get_model_results_table(model)$fixed,
          error = function(e) data.frame(Message = paste0("Error: ", e$message))
        ))
      }

      if (tbl_name %in% c("re_period", "re_cohort")) {
        if (is.null(model)) {
          return(data.frame(Message = "Model not available"))
        }
        rand <- tryCatch(
          get_model_results_table(model)$random,
          error = function(e) NULL
        )
        if (is.null(rand)) {
          return(data.frame(Message = "No random-effect table available"))
        }
        if (tbl_name == "re_period") {
          sel <- rand[
            grepl("period", rand$effect, ignore.case = TRUE),
            ,
            drop = FALSE
          ]
        } else {
          sel <- rand[
            grepl("cohort", rand$effect, ignore.case = TRUE),
            ,
            drop = FALSE
          ]
        }
        if (nrow(sel) == 0) {
          return(data.frame(
            Message = "No matching random-effect found in model."
          ))
        }
        return(sel)
      }

      # Trend tables
      if (tbl_name == "age_trend") {
        if (is.null(model) || is.null(data_model)) {
          return(data.frame(Message = "Model or data not available"))
        }
        return(tryCatch(
          get_model_trend_data(
            model,
            x_axis = "age",
            group_by = "null",
            data_model = data_model
          ),
          error = function(e) data.frame(Message = paste0("Error: ", e$message))
        ))
      }
      if (tbl_name == "period_trend") {
        if (is.null(model) || is.null(data_model)) {
          return(data.frame(Message = "Model or data not available"))
        }
        return(tryCatch(
          get_model_trend_data(
            model,
            x_axis = "period",
            group_by = "null",
            data_model = data_model
          ),
          error = function(e) data.frame(Message = paste0("Error: ", e$message))
        ))
      }
      if (tbl_name == "cohort_trend") {
        if (is.null(model) || is.null(data_model)) {
          return(data.frame(Message = "Model or data not available"))
        }
        return(tryCatch(
          get_model_trend_data(
            model,
            x_axis = "cohort",
            group_by = "null",
            data_model = data_model
          ),
          error = function(e) data.frame(Message = paste0("Error: ", e$message))
        ))
      }

      data.frame(Message = "Unknown selection")
    })

    # Preview
    output$preview_table <- renderDT({
      df <- selected_table()
      if (is.null(df)) {
        df <- data.frame(Message = "No data")
      }
      df <- as.data.frame(df)
      DT::datatable(head(df, 10), options = list(pageLength = 10, dom = 't'))
    })

    # Single download
    output$dl_single <- downloadHandler(
      filename = function() {
        fmt <- input$file_format
        tbl <- input$select_table
        paste0(tbl, ifelse(fmt == "xlsx", ".xlsx", ".csv"))
      },
      content = function(file) {
        df <- selected_table()
        df <- as.data.frame(df)
        fmt <- input$file_format
        if (fmt == "csv") {
          write.csv(df, file, row.names = FALSE, na = "")
        } else {
          if (requireNamespace("writexl", quietly = TRUE)) {
            writexl::write_xlsx(df, path = file)
          } else {
            write.csv(df, file, row.names = FALSE, na = "")
          }
        }
      }
    )

    # RData download (all objects)
    output$dl_rdata <- downloadHandler(
      filename = function() paste0("hapc_results_", Sys.Date(), ".RData"),
      content = function(file) {
        model <- get_model_obj()
        data_model <- get_data_model()
        fixed_tbl <- if (!is.null(model)) {
          tryCatch(get_model_results_table(model)$fixed, error = function(e) {
            NULL
          })
        } else {
          NULL
        }
        random_tbl <- if (!is.null(model)) {
          tryCatch(get_model_results_table(model)$random, error = function(e) {
            NULL
          })
        } else {
          NULL
        }
        age_tbl <- if (!is.null(model) && !is.null(data_model)) {
          tryCatch(
            get_model_trend_data(model, "age", "null", data_model),
            error = function(e) NULL
          )
        } else {
          NULL
        }
        period_tbl <- if (!is.null(model) && !is.null(data_model)) {
          tryCatch(
            get_model_trend_data(model, "period", "null", data_model),
            error = function(e) NULL
          )
        } else {
          NULL
        }
        cohort_tbl <- if (!is.null(model) && !is.null(data_model)) {
          tryCatch(
            get_model_trend_data(model, "cohort", "null", data_model),
            error = function(e) NULL
          )
        } else {
          NULL
        }
        save(
          model,
          data_model,
          fixed_tbl,
          random_tbl,
          age_tbl,
          period_tbl,
          cohort_tbl,
          file = file
        )
      }
    )

    # Zip download (CSV files)
    output$dl_zip <- downloadHandler(
      filename = function() paste0("hapc_tables_", Sys.Date(), ".zip"),
      content = function(zipfile) {
        tmpdir <- tempdir()
        files <- c()
        model <- get_model_obj()
        data_model <- get_data_model()

        if (!is.null(model)) {
          fixed_tbl <- tryCatch(
            get_model_results_table(model)$fixed,
            error = function(e) NULL
          )
          if (!is.null(fixed_tbl)) {
            f <- file.path(tmpdir, "fixed_effects.csv")
            write.csv(as.data.frame(fixed_tbl), f, row.names = FALSE)
            files <- c(files, f)
          }
          random_tbl <- tryCatch(
            get_model_results_table(model)$random,
            error = function(e) NULL
          )
          if (!is.null(random_tbl)) {
            f2 <- file.path(tmpdir, "random_effects.csv")
            write.csv(as.data.frame(random_tbl), f2, row.names = FALSE)
            files <- c(files, f2)
          }
        }

        if (!is.null(model) && !is.null(data_model)) {
          age_tbl <- tryCatch(
            get_model_trend_data(model, "age", "null", data_model),
            error = function(e) NULL
          )
          if (!is.null(age_tbl)) {
            f3 <- file.path(tmpdir, "age_trend.csv")
            write.csv(as.data.frame(age_tbl), f3, row.names = FALSE)
            files <- c(files, f3)
          }
          period_tbl <- tryCatch(
            get_model_trend_data(model, "period", "null", data_model),
            error = function(e) NULL
          )
          if (!is.null(period_tbl)) {
            f4 <- file.path(tmpdir, "period_trend.csv")
            write.csv(as.data.frame(period_tbl), f4, row.names = FALSE)
            files <- c(files, f4)
          }
          cohort_tbl <- tryCatch(
            get_model_trend_data(model, "cohort", "null", data_model),
            error = function(e) NULL
          )
          if (!is.null(cohort_tbl)) {
            f5 <- file.path(tmpdir, "cohort_trend.csv")
            write.csv(as.data.frame(cohort_tbl), f5, row.names = FALSE)
            files <- c(files, f5)
          }
        }

        # Basic descriptive (guess common vars)
        if (
          !is.null(data_model) &&
            exists("get_basic_characteristics", where = globalenv())
        ) {
          guess_vars <- intersect(
            c(
              "sex",
              "residence",
              "age_group",
              "living_arrangement",
              "education_level",
              "economic_status"
            ),
            names(data_model)
          )
          if (length(guess_vars) > 0) {
            basic <- tryCatch(
              get_basic_characteristics(data_model, guess_vars),
              error = function(e) NULL
            )
            if (!is.null(basic)) {
              fb <- file.path(tmpdir, "basic_overall.csv")
              write.csv(as.data.frame(basic), fb, row.names = FALSE)
              files <- c(files, fb)
            }
            basic_by_period <- tryCatch(
              get_basic_characteristics_by_period(data_model, guess_vars),
              error = function(e) NULL
            )
            if (!is.null(basic_by_period)) {
              fb2 <- file.path(tmpdir, "basic_by_period.csv")
              write.csv(as.data.frame(basic_by_period), fb2, row.names = FALSE)
              files <- c(files, fb2)
            }
          }
        }

        if (length(files) == 0) {
          placeholder <- file.path(tmpdir, "empty.txt")
          writeLines("No tables available", placeholder)
          files <- c(placeholder)
        }
        # Some installations of the 'zip' package do not support the 'extras' argument.
        # Call without 'extras' for compatibility. If you need to remove paths inside
        # the archive, consider using utils::zip(..., flags = "-j") on the host.
        zip::zip(zipfile = zipfile, files = files)
      }
    )
  })
}
