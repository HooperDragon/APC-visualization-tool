#### ui ####
mod_descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      ## left column: 3D figure and controls
      column(
        7,
        # ratio buttons
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

        # 3D figure
        div(
          style = "border: 2px solid #e0e0e0; border-radius: 8px; padding: 5px; background: white; box-shadow: 2px 2px 5px rgba(0,0,0,0.05);",

          plotlyOutput(ns("plot_3d"), height = "550px")
        ),

        # sliding block
        wellPanel(
          style = "margin-top: 15px; padding: 15px 20px; background-color: #f8f9fa; border: 1px solid #ddd;",
          uiOutput(ns("slider_ui"))
        )
      ),

      ## righ column: 2D slice view
      column(
        5,
        div(
          style = "margin-top: 45px; height: 100%; padding-left: 10px;",

          h4(
            "2D Section View",
            style = "text-align: center; margin-bottom: 25px; color: #333;"
          ),

          plotOutput(ns("plot_2d"), height = "500px"),

          div(
            style = "margin-top: 20px; color: #777; font-size: 0.9em; text-align: center; font-style: italic;",
            icon("info-circle"),
            " Drag the slider on the left or click the 3D figure to update."
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
          label_txt <- span(icon("user"), " Select Age Group:")
          step_val <- slice_step()
        } else if (input$slice_dim == "cohort") {
          vals <- sort(unique(df$cohort_start))
          label_txt <- span(icon("users"), " Select Cohort:")
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

    ## renew 3D slice
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

    ## point at 3D figure and renew block
    observe({
      req(plot_rendered())

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
      req(input$slice_dim, input$slice_val)
      plot_2d_slice_generic(df, input$slice_dim, input$slice_val)
    })
  })
}
