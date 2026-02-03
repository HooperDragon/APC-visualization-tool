library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggsci)
library(htmlwidgets)

# --- 辅助函数：计算切面坐标 ---
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

# --- 1. 画 3D 底图 ---
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

  # 初始化假切面
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
          eye = list(x = -1.5, y = -1.5, z = 0.1),
          up = list(x = 0, y = 0, z = 1),
          center = list(x = 0, y = 0, z = -0.2)
        ),
        xaxis = list(
          title = "Age",
          gridcolor = "lightgrey",
          showbackground = FALSE
        ),
        yaxis = list(
          title = "Period",
          gridcolor = "lightgrey",
          showbackground = FALSE
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

  # ====================================================
  # 【关键修复】显式注册点击事件！
  # 这一行告诉 Plotly：如果用户点了图，请把信息发给 Shiny
  # ====================================================
  fig <- event_register(fig, "plotly_click")

  # JS 锁定视角代码
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

# --- 2. 2D 切面图 (保持不变) ---
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
    # Cohort Slicing 可能会有多个 Period 的数据点对应同一个 Age Group
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
    theme_minimal() +
    labs(
      title = title_text,
      y = "Rate (%)",
      x = ifelse(x_col == "period", "Period", "Age Group")
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# ==============================================================================
# 4. HAPC 结果趋势图 (Line Chart for Model Results)
# ==============================================================================
plot_hapc_trend <- function(df, x_label, group_label) {
  if (is.null(df)) {
    return(NULL)
  }

  # 配色方案：Overall 用灰色，分组变量用 NPG 配色
  # 确保分组颜色与 Overall 不重复
  groups <- unique(df$group)
  npg_colors <- ggsci::pal_npg()(10)

  if ("Overall" %in% groups) {
    # Overall 用深灰色，其他组用 NPG 配色
    other_groups <- setdiff(groups, "Overall")
    color_map <- c("Overall" = "#4D4D4D") # 深灰色
    if (length(other_groups) > 0) {
      color_map <- c(
        color_map,
        setNames(npg_colors[1:length(other_groups)], other_groups)
      )
    }
  } else {
    # 没有 Overall，直接用 NPG 配色
    color_map <- setNames(npg_colors[1:length(groups)], groups)
  }

  p <- ggplot(df, aes(x = x_val, y = prob, group = group, color = group)) +
    # 绘制置信区间 (细密虚线，半透明)
    geom_line(
      aes(y = lower),
      linetype = "dotted",
      alpha = 0.5,
      linewidth = 0.6
    ) +
    geom_line(
      aes(y = upper),
      linetype = "dotted",
      alpha = 0.5,
      linewidth = 0.6
    ) +

    # 绘制主线
    geom_line(linewidth = 1) +
    geom_point(size = 2, fill = "white", shape = 21)

  # 添加均值线 (如果数据中有 mean_prob)
  if ("mean_prob" %in% names(df)) {
    mean_val <- df$mean_prob[1]
    p <- p +
      geom_hline(
        yintercept = mean_val,
        linetype = "dashed",
        color = "#888888",
        linewidth = 0.6,
        alpha = 0.5
      ) +
      annotate(
        "text",
        x = max(df$x_val, na.rm = TRUE),
        y = mean_val,
        label = paste0("Mean = ", round(mean_val, 3)),
        hjust = 1.1,
        vjust = -0.5,
        size = 3.5,
        color = "#666666",
        alpha = 0.7
      )
  }

  # 配色
  p <- p +
    scale_color_manual(values = color_map) +

    # 标签
    labs(
      x = x_label,
      y = "Predicted Probability",
      color = group_label,
      title = paste("Trend by", x_label)
    ) +

    # 主题
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # --- Cohort 特殊处理：每个点都有轴标签 (xxxx-xxxx年) ---
  if ("x_label" %in% names(df)) {
    # 获取唯一的 x_val 和对应的 x_label
    label_df <- df %>%
      select(x_val, x_label) %>%
      distinct() %>%
      arrange(x_val)

    p <- p +
      scale_x_continuous(
        breaks = label_df$x_val,
        labels = label_df$x_label
      )
  }

  return(p)
}
