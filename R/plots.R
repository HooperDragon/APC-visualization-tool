library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggsci)
library(htmlwidgets)

# --- 1. 画 3D APC 描述性曲面图 ---
plot_3d_apc <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # 数据准备
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

  # 彩虹渐变配色：蓝 -> 青 -> 绿 -> 黄 -> 橙 -> 红 -> 紫红
  npg_colors <- list(
    c(0, "#4575B4"), # 亮蓝
    c(0.17, "#74ADD1"), # 天蓝
    c(0.33, "#ABD9E9"), # 浅青
    c(0.5, "#FEE090"), # 浅黄
    c(0.67, "#FDAE61"), # 橙色
    c(0.83, "#F46D43"), # 橙红
    c(1, "#D73027") # 鲜红
  )

  fig <- plot_ly(x = x_vals, y = y_vals, z = z_mat) %>%
    add_surface(
      colorscale = npg_colors,
      showscale = FALSE,
      opacity = 0.9,
      contours = list(
        x = list(show = TRUE, color = "grey", width = 1, highlight = FALSE),
        y = list(show = TRUE, color = "grey", width = 1, highlight = FALSE),
        z = list(show = FALSE)
      )
    ) %>%
    layout(
      title = list(text = "APC描述结果示意图", y = 0.98),

      # 【关键修改 1】设置交互模式为 "turntable" (转盘模式)
      # 这种模式下，Z轴会自动保持垂直，不会翻转
      dragmode = "turntable",

      # 【关键修改 2】去除边距，让图填满区域 (显得更大)
      margin = list(l = 0, r = 0, b = 0, t = 30),

      scene = list(
        # 【关键修改 3】相机视角设置 (低角度 + 拉近)
        camera = list(
          # eye: 低角度视角
          eye = list(x = 1.5, y = 1.5, z = 0.5),

          # up: 强制锁定 Z 轴向上
          up = list(x = 0, y = 0, z = 1),

          # center: 看向图中心
          center = list(x = 0, y = 0, z = 0)
        ),

        xaxis = list(
          title = "年龄",
          tickmode = "array",
          tickvals = age_map$age_start,
          ticktext = age_map$age_group,
          tickangle = 0,
          side = "bottom",
          gridcolor = "lightgrey",
          showbackground = TRUE,
          backgroundcolor = "white",
          showspikes = FALSE
        ),
        yaxis = list(
          title = "时期",
          side = "bottom",
          gridcolor = "lightgrey",
          showbackground = TRUE,
          backgroundcolor = "white",
          showspikes = FALSE
        ),
        zaxis = list(
          title = "发病率 (%)",
          gridcolor = "lightgrey",
          showbackground = TRUE,
          backgroundcolor = "white",
          showspikes = FALSE
        ),

        # 调整长宽比，让 Z 轴更高
        aspectmode = "manual",
        aspectratio = list(x = 1.2, y = 1, z = 0.8)
      )
    )

  # Disable default 3D drag and implement custom horizontal-only rotation
  js_fix_camera <- "function(el, x) {
  var gd = document.getElementById(el.id);
  if(!gd) return;

  // Fixed camera parameters
  var fixedZ = 0.5;
  var radius = Math.sqrt(1.5*1.5 + 1.5*1.5); // ~2.12, now variable for zoom
  var minRadius = 1.0;  // minimum zoom (closest)
  var maxRadius = 5.0;  // maximum zoom (farthest)
  var fixedCenter = {x:0, y:0, z:0};
  var fixedUp = {x:0, y:0, z:1};
  var theta = Math.PI/4; // initial angle: 45 degrees

  // Set camera (no theta limits, full 360 rotation)
  function setCamera(t, r) {
    var newEye = {x: r * Math.cos(t), y: r * Math.sin(t), z: fixedZ};
    Plotly.relayout(gd, {'scene.camera': {eye: newEye, up: fixedUp, center: fixedCenter}});
  }

  // Disable default dragmode for 3D scene
  Plotly.relayout(gd, {'dragmode': false});

  // Custom mouse drag for horizontal rotation only

  var isDragging = false;
  var lastX = 0;

  // Find the scene container (the gl-container or similar)
  var sceneEl = gd.querySelector('.gl-container') || gd.querySelector('.plot-container') || gd;

  sceneEl.addEventListener('mousedown', function(e) {
    isDragging = true;
    lastX = e.clientX;
    e.preventDefault();
  });

  document.addEventListener('mousemove', function(e) {
    if(!isDragging) return;
    var dx = e.clientX - lastX;
    lastX = e.clientX;
    // Adjust theta based on horizontal mouse movement
    theta += dx * 0.01; // sensitivity factor
    setCamera(theta, radius);
  });

  document.addEventListener('mouseup', function(e) {
    isDragging = false;
  });

  // Scroll wheel for zoom
  sceneEl.addEventListener('wheel', function(e) {
    e.preventDefault();
    var delta = e.deltaY > 0 ? 0.1 : -0.1; // scroll down = zoom out, scroll up = zoom in
    radius = Math.max(minRadius, Math.min(maxRadius, radius + delta));
    setCamera(theta, radius);
  }, {passive: false});

  // Also handle touch for mobile
  sceneEl.addEventListener('touchstart', function(e) {
    if(e.touches.length === 1) {
      isDragging = true;
      lastX = e.touches[0].clientX;
    }
  });

  document.addEventListener('touchmove', function(e) {
    if(!isDragging || e.touches.length !== 1) return;
    var dx = e.touches[0].clientX - lastX;
    lastX = e.touches[0].clientX;
    theta += dx * 0.01;
    setCamera(theta, radius);
  });

  document.addEventListener('touchend', function(e) {
    isDragging = false;
  });
}"

  fig <- fig %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "orbitRotation",
        "tableRotation",
        "resetCameraDefault3d",
        "resetCameraLastSave3d"
      ),
      scrollZoom = TRUE
    )

  fig <- htmlwidgets::onRender(fig, js_fix_camera)

  return(fig)
}

# --- 2. 2D 切面图 (保持不变) ---
plot_period_slice <- function(df, selected_period) {
  plot_data <- df %>% filter(period == selected_period)
  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  ggplot(plot_data, aes(x = age_group, y = rate, group = 1)) +
    geom_line(color = "#E64B35", size = 1.2) +
    geom_point(color = "#4DBBD5", size = 3) +
    theme_minimal() +
    labs(
      title = paste("Period:", selected_period),
      y = "Rate",
      x = "Age Group"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}
