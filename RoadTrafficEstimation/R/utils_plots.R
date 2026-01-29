# ==============================================================================
# GENERIC PLOTTING UTILITIES
# ==============================================================================
# 
# ------------------------------------------------------------------------------
# Common ggplot theme
# ------------------------------------------------------------------------------
#' @title Common ggplot theme for the NM_OSM pipeline
#' @description Provides a minimal, consistent theme for all figures generated 
#'              in the pipeline. Minor grid lines are removed for clarity.
#' @param base_size Base font size (default = 12)
#' @return A ggplot2 theme object
theme_pipeline <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}
# ------------------------------------------------------------------------------
# Figure saving utility
# ------------------------------------------------------------------------------
#'@title  Save a ggplot object only if output path and name are provided
#' @description This helper prevents unwanted side effects such as the creation 
#'              of default Rplots.pdf files. If fig_path or fig_name is NULL, 
#'              the plot is returned but not saved.
#' @param plot A ggplot or grob object
#' @param fig_path Directory where the figure should be saved
#' @param fig_name File name of the figure (e.g. "my_plot.pdf")
#' @param width Figure width in inches
#' @param height Figure height in inches
#' @return The input plot (invisibly)
save_plot_if_needed <- function(plot,
                                fig_path = NULL,
                                fig_name = NULL,
                                width = 10,
                                height = 6) {
  if (!is.null(fig_path) && !is.null(fig_name)) {
    ggsave(filename = fig_name, 
           plot = plot, 
           path = fig_path, 
           width = width, 
           height = height)
  }
  invisible(plot)
}
# ------------------------------------------------------------------------------
# Hourly traffic profile (24h pattern) plotting utility
# ------------------------------------------------------------------------------
#'@title Plot hourly traffic flow profile 
#' @description Creates a 24-hour traffic profile showing:
#'              \itemize{
#'                \item average flow (solid line), 
#'                \item median flow (dashed line), 
#'                \item interquartile range (Q25–Q75 ribbon).
#'              }
#' @param traffic_hourly_patterns Data frame with hourly aggregated statistics
#' @param fig_path Optional output directory
#' @param fig_name Optional output file name
#' @return A ggplot object
plot_hourly_traffic_profile <- function(traffic_hourly_patterns,
                                        fig_path = NULL,
                                        fig_name = NULL) {
  p <- ggplot(data = traffic_hourly_patterns, mapping = aes(x = hour)) +
    geom_ribbon(mapping = aes(ymin = q25_flow, ymax = q75_flow), 
                fill = "steelblue", alpha = 0.3) + 
    geom_line(mapping = aes(y = avg_flow), 
              color = "steelblue", linewidth = 1.2) + 
    geom_point(mapping = aes(y = avg_flow), color = "steelblue", size = 2) + 
    geom_line(mapping = aes(y = median_flow), color = "orange", 
              linetype = "dashed", linewidth = 0.8) + 
    scale_x_continuous(breaks = seq(0, 23, 2), 
                       labels = paste0(seq(0, 23, 2), "h")) + 
    labs(title = "24-Hour Traffic Profile", 
         subtitle = "Average flow with Q1–Q3 range 
                    (blue = mean, orange = median)", 
         x = "Hour of Day", 
         y = "Traffic Flow (vehicles/hour)") +
    theme_pipeline()
  
  save_plot_if_needed(p, fig_path, fig_name)
}
# ------------------------------------------------------------------------------
# Speed and truck percentage by hour plotting utility
# ------------------------------------------------------------------------------
#'@title Plot hourly speed and truck percentage
#' @description Displays the evolution of average speed and truck percentage 
#'              throughout the day on a shared time axis.
#' @param traffic_hourly_patterns Data frame with hourly aggregated statistics
#' @param fig_path Optional output directory
#' @param fig_name Optional output file name
#' @return A ggplot object
plot_speed_and_truck_percentage <- function(traffic_hourly_patterns,
                                            fig_path = NULL,
                                            fig_name = NULL) {
  hourly_long <- compute_hourly_long_format(traffic_hourly_patterns)
  
  p <- ggplot(data = hourly_long, 
              mapping = aes(x = hour, y = value, color = metric_label)) + 
    geom_line(linewidth = 1.2) + 
    geom_point(size = 2) + 
    scale_x_continuous(breaks = seq(0, 23, 2), 
                       labels = paste0(seq(0, 23, 2), "h")) +
    scale_color_manual(values = c("Average Speed (km/h)" = "#2ecc71", 
                                  "Truck Percentage (%)" = "#e74c3c")) + 
    labs(title = "Speed and Truck Percentage Throughout the Day", 
         subtitle = "Different physical units shown on the same plot", 
         x = "Hour of Day", 
         y = "Value", 
         color = "Metric") +
    theme_pipeline() +
    theme(legend.position = "bottom")
  
  save_plot_if_needed(p, fig_path, fig_name)
}
# ------------------------------------------------------------------------------
# Period comparison (Day / Evening / Night) plotting utility
# ------------------------------------------------------------------------------
#' @title Plot traffic metrics comparison by period (D/E/N)
#' @description Creates a three-panel figure comparing:
#'              \itemize{
#'                \item average traffic flow, 
#'                \item average speed, 
#'                \item truck percentage, 
#'              }
#'              across day, evening and night periods.
#' @param aggregated_traffic_data Aggregated traffic dataset
#' @param fig_path Optional output directory
#' @param fig_name Optional output file name
#' @return A grob object (arrangeGrob)
plot_period_comparison <- function(aggregated_traffic_data,
                                   fig_path = NULL,
                                   fig_name = NULL) {
  period_stats <- compute_period_statistics(aggregated_traffic_data)
  
  make_bar_plot <- function(y, title, label_fmt) {
    ggplot(data = period_stats, 
           mapping = aes(x = period_label, y = .data[[y]], fill = period_label)) + 
      geom_col(width = 0.7) + 
      geom_text(mapping = aes(label = sprintf(label_fmt, .data[[y]])), 
                vjust = -0.5, size = 4) + 
      scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) + 
      labs(title = title, x = NULL) + 
      theme_pipeline(base_size = 10) + 
      theme(legend.position = "none")
  }
  
  p_flow <- make_bar_plot("avg_flow",  "Average Flow",  "%.0f")
  p_speed <- make_bar_plot("avg_speed", "Average Speed", "%.1f")
  p_trucks <- make_bar_plot("pct_trucks", "Truck Percentage", "%.1f%%")
  p <- gridExtra::arrangeGrob(
    p_flow, p_speed, p_trucks, 
    ncol = 3, 
    top = grid::textGrob(label = "Period Comparison (Day / Evening / Night)", 
                         gp = grid::gpar(fontface = "bold", fontsize = 14))
  )
  
  save_plot_if_needed(p, fig_path, fig_name, width = 14, height = 5)
  
  return(list(flow = p_flow, 
              speed = p_speed, 
              trucks = p_trucks))
}
# ------------------------------------------------------------------------------
# Flow distribution and data quality
# ------------------------------------------------------------------------------
#' @title Plot traffic flow distribution and data quality indicators
#' @description Combines:
#'              \itemize{
#'                \item a histogram of traffic flow distribution, 
#'                \item boxplots of predicted vs measured data ratios.
#'              }
#' @param aggregated_traffic_data Aggregated traffic dataset
#' @param fig_path Optional output directory
#' @param fig_name Optional output file name
#' @return A grob object (arrangeGrob)
plot_flow_distribution_and_quality <- function(aggregated_traffic_data,
                                               fig_path = NULL,
                                               fig_name = NULL) {
  
  flow_data <- compute_flow_distribution(aggregated_traffic_data)
  quality_data <- compute_quality_metrics_long(aggregated_traffic_data)
  
  p_flow <- ggplot(data = flow_data, mapping = aes(x = aggregate_flow)) +
    geom_histogram(bins = 50, fill = "steelblue", 
                   color = "white", alpha = 0.8) +
    geom_vline(xintercept = median(x = flow_data$aggregate_flow, na.rm = TRUE), 
               color = "red", linetype = "dashed", linewidth = 1) + 
    scale_x_continuous(labels = scales::comma) + 
    labs(title = "Traffic Flow Distribution", 
         subtitle = sprintf("Median: %.0f veh/h", 
                            median(x = flow_data$aggregate_flow, na.rm = TRUE)), 
      x = "Flow (vehicles/hour)", 
      y = "Count") +
    theme_pipeline(base_size = 10)
  
  p_quality <- ggplot(
    data = quality_data, 
    mapping = aes(x = metric_label, y = pct_predicted, fill = metric_label)) + 
    geom_boxplot(alpha = 0.7) + 
    scale_fill_brewer(palette = "Set2") + 
    labs(title = "Data Quality: % Predicted vs Measured", 
         subtitle = "Lower values indicate higher data availability", 
         x = NULL, 
         y = "% Predicted (0–100%)") + 
    theme_pipeline(base_size = 10) + 
    theme(legend.position = "none")
  
  p <- gridExtra::arrangeGrob(p_flow, p_quality, ncol = 2)
  
  save_plot_if_needed(p, fig_path, fig_name, width = 12, height = 5)
  
  return(list(flow = p_flow, quality = p_quality))
}
