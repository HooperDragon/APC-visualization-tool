#### ui ####
mod_descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$head(tags$style(HTML("
      .slice-radio-container .form-group { margin-bottom: 0 !important; }
      .slice-radio-container .radio-inline { margin-top: 0 !important; padding-top: 0 !important; margin-bottom: 0 !important; }
    "))),

    ## Main Layout
    div(class = "row", 
      
      ## Left Column (7/12)
      div(class = "col-sm-7",
          
          # 3D Figure
          div(
            class = "card-style",
            style = "margin-bottom: 20px;", 
            
            div(
              style = "border-bottom: 1px solid #eeeeee; padding-bottom: 12px; margin-bottom: 15px; 
                       position: relative; z-index: 10; background-color: white;",
              h3("3D Figure View", 
                 style = "color: #337ab7; font-weight: 700; margin: 0; font-size: 22px; border: none !important;")
            ),
            div(
              style = "margin-top: -30px; margin-bottom: -20px; position: relative; z-index: 1;", 
              plotlyOutput(ns("plot_3d"), height = "550px") 
            )
          ),
          
          # Controls
          div(
            class = "card-style",
            style = "background-color: #fcfcfc; border: 1px solid #e9ecef; border-radius: 8px; padding: 20px;", 
            
            div(
              class = "slice-radio-container", 
              style = "display: flex; align-items: center; margin-bottom: 15px;",
              tags$strong("Slice Dimension:", style = "font-size: 16px; margin-right: 20px; color: #333;"),
              radioButtons(
                ns("slice_dim"),
                label = NULL,
                choices = c("Period" = "period", "Age" = "age", "Cohort" = "cohort", "Null" = "null"),
                inline = TRUE
              )
            ),
            div(
              style = "border-top: 1px dashed #dee2e6; padding-top: 15px;",
              uiOutput(ns("slider_ui"))
            ),
            div(
              style = "color: #888; font-size: 0.9em; margin-top: 5px; text-align: left;",
              icon("info-circle"),
              " Drag the slider above or click anywhere on the 3D figure to update the 2D slice."
            )
          )
      ),

      ## --- Right Column (5/12) ---
      div(class = "col-sm-5",
          
          # 2D View
          div(
            class = "card-style",
            style = "display: flex; flex-direction: column;",
            
            div(
              style = "border-bottom: 1px solid #eeeeee; padding-bottom: 12px; margin-bottom: 15px;",
              h3("2D Slice View", 
                 style = "color: #337ab7; font-weight: 700; margin: 0; font-size: 22px; border: none !important;")
            ),
            
            div(
              style = "flex-grow: 1;",
              plotOutput(ns("plot_2d"), height = "500px") 
            )
          )
      )
    )
  )
}

#### server ####
mod_descriptive_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## trace whether the 3D figure is rendered
    plot_rendered <- reactiveVal(FALSE)

    ## step size for age/cohort slider, derived from data intervals
    slice_step <- reactive({
      df <- data_r()
      req(df, input$slice_dim)

      vals <- NULL
      if (input$slice_dim == "age") {
        vals <- sort(unique(df$age_start))
      } else if (input$slice_dim == "cohort") {
        vals <- sort(unique(df$cohort_start))
      } else {
        return(1)
      }

      if (length(vals) >= 2) {
        vals[2] - vals[1]
      } else {
        1
      }
    })

    ## dynamic sliding block
    output$slider_ui <- renderUI({
      df <- data_r()
      req(df, input$slice_dim)
      if (input$slice_dim == "null" || !"period" %in% names(df)) {
        return(NULL)
      }

      min_val <- 0
      max_val <- 0
      val <- 0
      step_val <- 1
      label_txt <- ""

      if (input$slice_dim == "period") {
        vals <- sort(unique(df$period))
        label_txt <- span("Select Period:")

        # ensure only existing period can be selected
        shinyWidgets::sliderTextInput(
          inputId = ns("slice_val"),
          label = label_txt,
          choices = vals,
          selected = vals[max(1, floor(length(vals) / 2))],
          grid = TRUE,
          width = "100%",
          animate = animationOptions(interval = 1000, loop = FALSE)
        )
      } else {
        # age/cohort sliding block
        if (input$slice_dim == "age") {
          vals <- sort(unique(df$age_start))
          label_txt <- span("Select Age Group:")
          step_val <- slice_step()
        } else if (input$slice_dim == "cohort") {
          vals <- sort(unique(df$cohort_start))
          label_txt <- span("Select Cohort:")
          step_val <- slice_step()
        }

        sliderInput(
          ns("slice_val"),
          label_txt,
          min = min(vals),
          max = max(vals),
          value = median(vals),
          step = step_val,
          width = "100%",
          animate = animationOptions(interval = 1000, loop = FALSE)
        )
      }
    })

    ## render 3D figure
    output$plot_3d <- renderPlotly({
      df <- data_r()
      req(df)

      p <- plot_3d_base(df)

      if (!is.null(p)) {
        p <- event_register(p, "plotly_click")
      }

      plot_rendered(TRUE)

      p
    })

    ## hide mesh when slice_dim == "null"
    observeEvent(input$slice_dim, {
      if (input$slice_dim == "null") {
        plotlyProxy("plot_3d", session) %>%
          plotlyProxyInvoke("restyle", list(opacity = 0), list(1))
      } else {
        plotlyProxy("plot_3d", session) %>%
          plotlyProxyInvoke("restyle", list(opacity = 0.4), list(1))
      }
    })

    ## renew 3D slice
    observeEvent(input$slice_val, {
      req(input$slice_dim, input$slice_dim != "null")
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

    ## point at 3D figure and renew block
    observe({
      req(plot_rendered(), input$slice_dim != "null")

      click <- event_data("plotly_click", source = "A")
      req(click)

      if (input$slice_dim == "period") {
        # Period: find nearest value and renew
        df <- data_r()
        if (!is.null(df)) {
          valid_periods <- sort(unique(df$period))
          idx <- which.min(abs(valid_periods - click$y))
          best_val <- valid_periods[idx]
          shinyWidgets::updateSliderTextInput(
            session,
            "slice_val",
            selected = best_val
          )
        }
      } else {
        # Age and cohort: just normally renew
        new_val <- NULL
        step_val <- slice_step()
        if (input$slice_dim == "age") {
          new_val <- round(click$x / step_val) * step_val
        } else if (input$slice_dim == "cohort") {
          new_val <- round((click$y - click$x) / step_val) * step_val
        }

        if (!is.null(new_val)) {
          updateSliderInput(session, "slice_val", value = new_val)
        }
      }
    })

    ## renew 2D slice
    output$plot_2d <- renderPlot({
      df <- data_r()
      req(input$slice_dim, input$slice_dim != "null", input$slice_val)
      plot_2d_slice_generic(df, input$slice_dim, input$slice_val)
    })
  })
}
