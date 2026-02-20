#### descriptive figures data ####
get_descriptive_fig <- function(df, params) {
  ## get parameter inputs
  intv <- params$intervals
  age_s <- params$age_start
  age_e <- params$age_end
  pd_s <- params$period_start
  pd_e <- params$period_end

  if (nrow(df) == 0) {
    return(NULL)
  }

  ## cluster a/p/c groups and calculate rates
  # ensure age/cohort grouping columns exist (either computed earlier or compute now)
  if (!"Age_Start" %in% colnames(df)) {
    df <- add_age_cohort_groups(df, params)
  }

  df_grouped <- df %>%
    # rates grouped by age and period (cohort cols are deterministic given Age_Start/Period)
    group_by(Age_Start, Age_Label, Period, Cohort_Start, Cohort_Label) %>%
    summarise(
      Rate = 100 * mean(Disease, na.rm = TRUE),
      Count = n(), #sample size
      .groups = "drop"
    ) %>%

    # list and arrange cols
    select(
      age_group = Age_Label, # for display
      cohort_group = Cohort_Label, # for display
      period = Period,
      rate = Rate,

      # assittance cols
      age_start = Age_Start,
      cohort_start = Cohort_Start,
      N = Count
    ) %>%
    arrange(age_start, period)

  return(df_grouped)
}

#### calculate the coordinates for the slicing plane based on the selected dimension and value ####
get_plane_mesh <- function(df, slice_dim, slice_value) {
  x_range <- range(df$age_start)
  y_range <- range(df$period)
  z_max <- max(df$rate, na.rm = TRUE) * 1.1

  plane_x <- c()
  plane_y <- c()
  plane_z <- c()

  if (slice_dim == "period") {
    plane_y <- c(slice_value, slice_value, slice_value, slice_value)
    plane_x <- c(x_range[1], x_range[2], x_range[2], x_range[1])
    plane_z <- c(0, 0, z_max, z_max)
  } else if (slice_dim == "age") {
    plane_x <- c(slice_value, slice_value, slice_value, slice_value)
    plane_y <- c(y_range[1], y_range[2], y_range[2], y_range[1])
    plane_z <- c(0, 0, z_max, z_max)
  } else if (slice_dim == "cohort") {
    p1 <- slice_value + x_range[1]
    p2 <- slice_value + x_range[2]
    plane_x <- c(x_range[1], x_range[2], x_range[2], x_range[1])
    plane_y <- c(p1, p2, p2, p1)
    plane_z <- c(0, 0, z_max, z_max)
  }
  return(list(x = plane_x, y = plane_y, z = plane_z))
}

#### 3D figure ####
plot_3d_base <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  df_sorted <- df %>% arrange(age_start, period)
  matrix_data <- df_sorted %>%
    select(period, age_start, rate) %>%
    pivot_wider(names_from = age_start, values_from = rate) %>%
    arrange(period)

  y_vals <- matrix_data$period
  x_vals <- as.numeric(colnames(matrix_data)[-1])
  z_mat <- as.matrix(matrix_data[, -1])

  age_map <- df_sorted %>%
    select(age_start, age_group) %>%
    distinct() %>%
    arrange(age_start)
  npg_colors <- list(c(0, "#4DBBD5"), c(0.5, "#00A087"), c(1, "#E64B35"))

  ## initialize
  dummy_x <- rep(mean(x_vals), 4)
  dummy_y <- rep(mean(y_vals), 4)
  dummy_z <- rep(0, 4)

  fig <- plot_ly(source = "A") %>%
    add_surface(
      x = x_vals,
      y = y_vals,
      z = z_mat,
      colorscale = npg_colors,
      showscale = FALSE,
      opacity = 0.95,
      contours = list(
        x = list(show = TRUE, color = "grey", width = 1),
        y = list(show = TRUE, color = "grey", width = 1),
        z = list(show = FALSE)
      ),
      hoverinfo = "none"
    ) %>%
    add_mesh(
      x = dummy_x,
      y = dummy_y,
      z = dummy_z,
      opacity = 0.4,
      color = I("#d3d3d3"),
      hoverinfo = "none",
      i = c(0, 0),
      j = c(1, 2),
      k = c(2, 3)
    ) %>%
    layout(
      margin = list(l = 0, r = 0, b = 0, t = 0),
      dragmode = "turntable",
      showlegend = FALSE,
      scene = list(
        camera = list(
          eye = list(x = -1.5, y = -1.5, z = 0.5),
          up = list(x = 0, y = 0, z = 1),
          center = list(x = 0, y = 0, z = 0)
        ),
        xaxis = list(
          title = "Age",
          gridcolor = "lightgrey",
          showbackground = FALSE,
          range = range(x_vals)
        ),
        yaxis = list(
          title = "Period",
          gridcolor = "lightgrey",
          showbackground = FALSE,
          range = range(y_vals)
        ),
        zaxis = list(
          title = "Rate(%)",
          gridcolor = "lightgrey",
          showbackground = FALSE
        ),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = 0.3)
      )
    ) %>%
    config(displayModeBar = FALSE)

  fig <- event_register(fig, "plotly_click")

  ## lock camera
  js_fix_camera <- "function(el, x) {
    var gd = document.getElementById(el.id);
    if(!gd) return;
    var fixedZ = 0.5;
    var radius = Math.sqrt(1.5*1.5 + 1.5*1.5); 
    var minRadius = 1.0; var maxRadius = 5.0; 
    var fixedCenter = {x:0, y:0, z:0};
    var fixedUp = {x:0, y:0, z:1};
    var theta = Math.PI/4; 
    function setCamera(t, r) {
      var newEye = {x: r * Math.cos(t), y: r * Math.sin(t), z: fixedZ};
      Plotly.relayout(gd, {'scene.camera': {eye: newEye, up: fixedUp, center: fixedCenter}});
    }
    // Plotly.relayout(gd, {'dragmode': false}); 
    var isDragging = false; var lastX = 0;
    var sceneEl = gd.querySelector('.gl-container') || gd.querySelector('.plot-container') || gd;
    sceneEl.addEventListener('mousedown', function(e) { isDragging = true; lastX = e.clientX; e.preventDefault(); });
    document.addEventListener('mousemove', function(e) { if(!isDragging) return; var dx = e.clientX - lastX; lastX = e.clientX; theta += dx * 0.01; setCamera(theta, radius); });
    document.addEventListener('mouseup', function(e) { isDragging = false; });
    sceneEl.addEventListener('wheel', function(e) { e.preventDefault(); var delta = e.deltaY > 0 ? 0.1 : -0.1; radius = Math.max(minRadius, Math.min(maxRadius, radius + delta)); setCamera(theta, radius); }, {passive: false});
    sceneEl.addEventListener('touchstart', function(e) { if(e.touches.length === 1) { isDragging = true; lastX = e.touches[0].clientX; } });
    document.addEventListener('touchmove', function(e) { if(!isDragging || e.touches.length !== 1) return; var dx = e.touches[0].clientX - lastX; lastX = e.touches[0].clientX; theta += dx * 0.01; setCamera(theta, radius); });
    document.addEventListener('touchend', function(e) { isDragging = false; });
  }"

  fig <- fig %>% onRender(js_fix_camera)
  return(fig)
}

#### 2D slice ####
plot_2d_slice_generic <- function(df, slice_dim, slice_value) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  plot_data <- NULL
  x_col <- ""
  title_text <- ""

  if (slice_dim == "period") {
    plot_data <- df %>% filter(period == slice_value)
    x_col <- "age_group"
    title_text <- paste0("Period = ", slice_value)
  } else if (slice_dim == "age") {
    plot_data <- df %>% filter(age_start == slice_value)
    x_col <- "period"
    title_text <- paste0("Age Group = ", slice_value)
  } else if (slice_dim == "cohort") {
    plot_data <- df %>%
      filter(cohort_start == slice_value) %>%
      group_by(age_start, age_group) %>%
      summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop")

    x_col <- "age_group"
    title_text <- paste0("Cohort = ", slice_value)
  }

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(NULL)
  }

  # Fix alphabetical ordering on the age-group axis by explicitly setting factor levels
  if (x_col == "age_group") {
    level_df <- plot_data %>%
      distinct(age_start, age_group) %>%
      arrange(age_start)
    plot_data <- plot_data %>%
      mutate(age_group = factor(age_group, levels = level_df$age_group))
  }
  if (x_col == "period") {
    plot_data <- plot_data %>%
      mutate(period = factor(period, levels = sort(unique(period))))
  }

  ggplot(plot_data, aes(x = .data[[x_col]], y = rate, group = 1)) +
    geom_line(color = "#E64B35", linewidth = 1.2) +
    geom_point(color = "#4DBBD5", size = 3) +
    theme_minimal(base_size = 22) +
    labs(
      title = title_text,
      y = "Rate (%)",
      x = ifelse(x_col == "period", "Period", "Age Group")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(size = 15.5),
      axis.text = element_text(size = 15.5),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
