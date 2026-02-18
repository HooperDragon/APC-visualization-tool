#### ui ####
mod_download_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## card-style
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

    ## layout
    fluidRow(
      # left column: batch download
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

          downloadButton(
            ns("dl_rdata"),
            " Download All as .RData",
            class = "btn-primary download-btn-large"
          ),

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

      # right colunm: single table download
      column(
        8,
        div(
          class = "card-style",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            h4(icon("table"), " Individual Table Export"),

            downloadButton(
              ns("dl_single"),
              "Download Selected",
              class = "btn-success"
            )
          ),
          hr(),

          fluidRow(
            # choose the table
            column(
              6,
              selectInput(
                ns("select_table"),
                "Select Table:",
                choices = c(
                  "HAPC Effect Table" = "combined",
                  "Basic Characteristics" = "basic_characteristics",
                  "Basic Characteristics by Period" = "basic_characteristics_by_period",
                  "Age trend by HAPC model" = "age_trend",
                  "Period trend by HAPC model" = "period_trend",
                  "Cohort trend by HAPC model" = "cohort_trend"
                ),
                width = "100%"
              )
            ),
            # choose the format
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

          # preview
          div(
            class = "preview-container",
            h5("Data Preview:"),
            DTOutput(ns("preview_table"))
          )
        )
      )
    )
  )
}

#### helpers (module-local) ####

## Columns to exclude when guessing basic-characteristic variables.
## These are either required input columns or model-generated internal columns.
EXCLUDE_COLS <- c(
  "Disease",
  "Age",
  "Period",
  "age_c",
  "age_c2",
  "period_factor",
  "cohort_group",
  "cohort_group_factor"
)

## Table-name -> export-filename mapping
TABLE_FILENAMES <- c(
  combined = "hapc_effect_table",
  basic_characteristics = "basic_characteristics",
  basic_characteristics_by_period = "basic_characteristics_by_period",
  age_trend = "age_trend_by_hapc_model",
  period_trend = "period_trend_by_hapc_model",
  cohort_trend = "cohort_trend_by_hapc_model"
)

## Slim trend data to key columns, rounded to 4 decimals
slim_trend <- function(df, x_axis) {
  if (is.null(df) || "Message" %in% names(df)) {
    return(df)
  }
  label_col <- switch(
    x_axis,
    age = {
      df$Age <- df$x_val
      "Age"
    },
    period = {
      df$Period <- df$x_val
      "Period"
    },
    {
      df$Cohort <- if ("x_label" %in% names(df)) df$x_label else df$x_val
      "Cohort"
    }
  )
  out <- df[, c(label_col, "group", "prob", "lower", "upper"), drop = FALSE]
  out[c("prob", "lower", "upper")] <- lapply(
    out[c("prob", "lower", "upper")],
    round,
    4
  )
  out
}

## Prefix group values with variable name (e.g., sex1, urban_rural2)
label_group_values <- function(df, group_var) {
  if (is.null(df) || "Message" %in% names(df)) {
    return(df)
  }
  if (is.null(group_var) || !nzchar(group_var)) {
    return(df)
  }
  if (!"group" %in% names(df)) {
    return(df)
  }
  df$group <- ifelse(
    df$group == "Overall",
    "Overall",
    paste0(group_var, df$group)
  )
  df
}

## Guess which basic-characteristic variables exist in data.
## Uses exclusion: removes known required/model-internal columns;
## everything else is treated as a user-supplied covariate.
guess_basic_vars <- function(df) {
  if (is.null(df)) {
    return(character(0))
  }
  setdiff(names(df), EXCLUDE_COLS)
}

## Write a data.frame to file (xlsx or csv)
write_table <- function(df, file, fmt = "csv") {
  df <- as.data.frame(df)
  if (fmt == "xlsx" && requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, path = file)
  } else {
    write.csv(df, file, row.names = FALSE, na = "")
  }
}

#### server ####
mod_download_server <- function(
  id,
  model_results,
  trend_data = NULL,
  covariates_r = reactive(character(0)),
  period_slopes_r = reactive(character(0)),
  cohort_slopes_r = reactive(character(0))
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── safe accessors ──
    get_model_obj <- reactive({
      if (is.list(model_results) && !is.null(model_results$model)) {
        model_results$model
      }
    })

    get_data_model <- reactive({
      if (is.list(model_results) && !is.null(model_results$data_for_model)) {
        model_results$data_for_model
      }
    })

    # ── build full trend (Overall + stratified groups) ──
    build_full_trend <- function(model, x_axis, data_model) {
      overall <- tryCatch(
        slim_trend(
          get_model_trend_data(model, x_axis, "null", data_model),
          x_axis
        ),
        error = function(e) NULL
      )

      group_vars <- switch(
        x_axis,
        age = covariates_r(),
        period = period_slopes_r(),
        cohort = cohort_slopes_r(),
        character(0)
      ) %||%
        character(0)

      stratified <- Filter(
        Negate(is.null),
        lapply(group_vars, function(gv) {
          tryCatch(
            label_group_values(
              slim_trend(
                get_model_trend_data(model, x_axis, gv, data_model),
                x_axis
              ),
              gv
            ),
            error = function(e) NULL
          )
        })
      )

      parts <- Filter(Negate(is.null), c(list(overall), stratified))
      if (length(parts) == 0) NULL else do.call(rbind, parts)
    }

    # ── generate any table by name ──
    build_table <- function(tbl_name, model, data_model) {
      switch(
        tbl_name,
        combined = {
          if (is.null(model)) {
            return(NULL)
          }
          get_model_results_table(model)$combined
        },
        basic_characteristics = {
          vars <- guess_basic_vars(data_model)
          if (is.null(data_model) || length(vars) == 0) {
            return(NULL)
          }
          get_basic_characteristics(data_model, vars)
        },
        basic_characteristics_by_period = {
          vars <- guess_basic_vars(data_model)
          if (is.null(data_model) || length(vars) == 0) {
            return(NULL)
          }
          get_basic_characteristics_by_period(data_model, vars)
        },
        age_trend = build_full_trend(model, "age", data_model),
        period_trend = build_full_trend(model, "period", data_model),
        cohort_trend = build_full_trend(model, "cohort", data_model),
        NULL
      )
    }

    safe_build_table <- function(tbl_name, model, data_model) {
      tryCatch(
        build_table(tbl_name, model, data_model),
        error = function(e) data.frame(Message = paste0("Error: ", e$message))
      )
    }

    # ── collect all exportable tables ──
    build_all_tables <- function(model, data_model) {
      tbls <- lapply(names(TABLE_FILENAMES), function(nm) {
        tryCatch(build_table(nm, model, data_model), error = function(e) NULL)
      })
      names(tbls) <- names(TABLE_FILENAMES)
      Filter(Negate(is.null), tbls)
    }

    # ── reactive: currently selected table ──
    selected_table <- reactive({
      tbl_name <- input$select_table
      result <- safe_build_table(tbl_name, get_model_obj(), get_data_model())
      if (is.null(result)) data.frame(Message = "No data available") else result
    })

    # ── preview ──
    output$preview_table <- renderDT({
      DT::datatable(
        as.data.frame(selected_table()),
        options = list(
          pageLength = 20,
          scrollY = "400px",
          scrollCollapse = TRUE,
          dom = "tip"
        ),
        rownames = FALSE
      )
    })

    # ── single table download ──
    output$dl_single <- downloadHandler(
      filename = function() {
        fmt <- input$file_format
        base <- TABLE_FILENAMES[input$select_table] %||% input$select_table
        paste0(base, if (fmt == "xlsx") ".xlsx" else ".csv")
      },
      content = function(file) {
        write_table(selected_table(), file, input$file_format)
      }
    )

    # ── RData download (all objects) ──
    output$dl_rdata <- downloadHandler(
      filename = function() paste0("hapc_results_", Sys.Date(), ".RData"),
      content = function(file) {
        model <- get_model_obj()
        data_model <- get_data_model()
        all_tbls <- build_all_tables(model, data_model)

        hapc_effect_tbl <- all_tbls[["combined"]]
        age_tbl <- all_tbls[["age_trend"]]
        period_tbl <- all_tbls[["period_trend"]]
        cohort_tbl <- all_tbls[["cohort_trend"]]
        basic_characteristics <- all_tbls[["basic_characteristics"]]
        basic_characteristics_by_period <- all_tbls[[
          "basic_characteristics_by_period"
        ]]

        save(
          model,
          data_model,
          hapc_effect_tbl,
          age_tbl,
          period_tbl,
          cohort_tbl,
          basic_characteristics,
          basic_characteristics_by_period,
          file = file
        )
      }
    )

    # ── zip download (CSV files) ──
    output$dl_zip <- downloadHandler(
      filename = function() paste0("hapc_tables_", Sys.Date(), ".zip"),
      content = function(zipfile) {
        # Use a dedicated sub-directory so we can zip with relative paths,
        # avoiding Windows absolute-path issues (colon in drive letter).
        tmpdir <- file.path(tempdir(), "hapc_zip_export")
        dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

        model <- get_model_obj()
        data_model <- get_data_model()
        all_tbls <- build_all_tables(model, data_model)

        basenames <- vapply(
          names(all_tbls),
          function(nm) {
            fname <- paste0(TABLE_FILENAMES[nm], ".csv")
            write.csv(
              as.data.frame(all_tbls[[nm]]),
              file.path(tmpdir, fname),
              row.names = FALSE
            )
            fname
          },
          character(1)
        )

        if (length(basenames) == 0) {
          writeLines("No tables available", file.path(tmpdir, "empty.txt"))
          basenames <- "empty.txt"
        }

        # zip with relative paths from tmpdir to avoid ':' in archive paths
        oldwd <- setwd(tmpdir)
        on.exit(setwd(oldwd), add = TRUE)
        zip::zip(
          zipfile = zipfile,
          files = unname(basenames)
        )
      }
    )
  })
}
