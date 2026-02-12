#### line plot for HAPC trends (age, period, cohort) ####
plot_hapc_trend <- function(df, x_label, group_label) {
  if (is.null(df)) {
    return(NULL)
  }

  groups <- unique(df$group)
  npg_colors <- ggsci::pal_npg()(10)

  if ("Overall" %in% groups) {
    other_groups <- setdiff(groups, "Overall")
    color_map <- c("Overall" = "#4D4D4D")
    if (length(other_groups) > 0) {
      color_map <- c(
        color_map,
        setNames(npg_colors[1:length(other_groups)], other_groups)
      )
    }
  } else {
    color_map <- setNames(npg_colors[1:length(groups)], groups)
  }

  p <- ggplot(df, aes(x = x_val, y = prob, group = group, color = group)) +
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
    geom_line(linewidth = 1) +
    geom_point(size = 2, fill = "white", shape = 21)

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
        size = 5.5,
        color = "#666666",
        alpha = 0.7
      )
  }

  p <- p +
    scale_color_manual(values = color_map) +
    labs(
      x = x_label,
      y = "Predicted Probability",
      color = group_label,
      title = paste("Trend by", x_label)
    ) +
    theme_minimal(base_size = 22) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(size = 15.5),
      axis.text = element_text(size = 15.5),
      legend.title = element_text(size = 15.5),
      legend.text = element_text(size = 15.5),
      legend.position = "bottom",
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  if ("x_label" %in% names(df)) {
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
