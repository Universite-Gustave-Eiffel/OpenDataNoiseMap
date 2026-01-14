#' @title Standardize columns across a list of data frames
#' @description Ensures that all data frames in a list share the same set of 
#'              columns. Missing columns are added and filled with `NA`, and 
#'              columns are reordered consistently across all data frames. This 
#'              is particularly useful when combining heterogeneous data sources 
#'              (e.g. chunked downloads or partial results) before row-binding 
#'              them with `dplyr::bind_rows()` or `rbind()`.
#' @param data_list A list of data frames with potentially different column 
#'                  sets.
#' @return A list of data frames where: 
#'         \itemize{
#'              \item all data frames contain the same columns, 
#'              \item missing columns have been added and filled with `NA`, 
#'              \item columns are ordered identically in every data frame.
#'         }
#' @examples
#' df1 <- data.frame(id = 1:2, value = c("a", "b"))
#' df2 <- data.frame(id = 3, score = c(10, 20))
#' standardized <- standardize_columns(list(df1, df2))
#' # Safe to combine afterwards
#' combined <- do.call(rbind, standardized)
#' @export
standardize_columns <- function(data_list) {
  # Compute the union of all column names across the data frames
  all_columns <- unique(unlist(lapply(X = data_list, 
                                      FUN = names)))
  # For each data frame:
  # - add missing columns filled with NA
  # - reorder columns consistently
  lapply(X = data_list, FUN = function(df) {
    # Identify columns missing in the current data frame
    missing_cols <- setdiff(x = all_columns, y = names(df))
    # Add missing columns as NA
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    # Reorder columns to match the global column set
    df[, all_columns, drop = FALSE]
  })
}
#' 
#' @title Compute hourly traffic patterns (24h profile)
#' @description Computes descriptive traffic statistics for each hour of the day 
#'              (h0–h23) based on aggregated traffic measures. Only hourly 
#'              periods encoded as "hX" are considered.
#' @param aggregated_measures_df A data frame containing aggregated traffic 
#'                               measures with a `period` column and traffic 
#'                               metrics.
#' @return A data frame with one row per hour (0–23) and summary statistics
#'   including mean, standard deviation, median, and quartiles of traffic flow,
#'   as well as average speed and truck percentage.
#' @export)
compute_hourly_patterns <- function(aggregated_measures_df) {
  aggregated_measures_df %>%
    dplyr::filter(filter = grepl(pattern = "^h\\d+$", x = period)) %>% 
    dplyr::mutate(hour = as.integer(gsub(pattern = "h", 
                                         replacement = "", 
                                         x = period))) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      avg_flow = mean(x = aggregate_flow, 
                      na.rm = TRUE),
      sd_flow = sd(x = aggregate_flow, 
                   na.rm = TRUE),
      median_flow = median(x = aggregate_flow, 
                           na.rm = TRUE),
      q25_flow = quantile(x = aggregate_flow, 
                          probs = 0.25, 
                          na.rm = TRUE),
      q75_flow = quantile(x = aggregate_flow, 
                          probs = 0.75, 
                          na.rm = TRUE),
      avg_speed = mean(x = aggregate_speed, 
                       na.rm = TRUE),
      pct_trucks = mean(x = 100 * aggregate_flow_trucks / aggregate_flow, 
                        na.rm = TRUE),
      n_observations = dplyr::n(),
      .groups = "drop") %>%
    dplyr::arrange(hour)
}
#' 
#' @title Convert hourly patterns to long format
#' @description Transforms hourly traffic patterns into a long format suitable 
#'              for multi-metric plotting (e.g. average speed and truck 
#'              percentage by hour).
#' @param hourly_patterns A data frame produced by 
#'                        \code{compute_hourly_patterns()}.
#' @return A long-format data frame with one row per hour and metric.
#' @export
compute_hourly_long_format <- function(hourly_patterns) {
  hourly_patterns %>%
    dplyr::select(hour, avg_speed, pct_trucks) %>%
    tidyr::pivot_longer(
      cols = c(avg_speed, pct_trucks),
      names_to = "metric",
      values_to = "value") %>%
    dplyr::mutate(
      metric_label = factor(
        x = metric,
        levels = c("avg_speed", "pct_trucks"),
        labels = c("Average Speed (km/h)", "Truck Percentage (%)")))
}
#' 
#' @title Compute traffic statistics by period (Day / Evening / Night)
#' @description Computes aggregated traffic statistics for the main daily 
#'              periods (D = Day, E = Evening, N = Night).
#' @param aggregated_measures_df A data frame containing aggregated traffic
#'                               measures with a `period` column.
#' @return A data frame with one row per period and average traffic indicators.
#' @export
compute_period_statistics <- function(aggregated_measures_df) {
  aggregated_measures_df %>%
    dplyr::filter(filter = period %in% c("D", "E", "N")) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      avg_flow = mean(x = aggregate_flow, 
                      na.rm = TRUE),
      avg_speed = mean(x = aggregate_speed, 
                       na.rm = TRUE),
      pct_trucks = mean(x = 100 * aggregate_flow_trucks / aggregate_flow, 
                        na.rm = TRUE),
      n_observations = dplyr::n(),
      .groups = "drop") %>%
    dplyr::mutate(
      period_label = factor(
        x = period,
        levels = c("D", "E", "N"),
        labels = c("Day\n(6-18h)", "Evening\n(18-22h)", "Night\n(22-6h)")))
}
#' 
#' @title Categorize traffic flow values
#' @description Categorizes aggregated traffic flow values into predefined flow 
#'              classes to support distribution analysis and data quality 
#'              diagnostics.
#' @param aggregated_measures_df A data frame containing an `aggregate_flow` 
#'                               column.
#' @return A data frame with an additional `flow_category` factor.
#' @export
compute_flow_distribution <- function(aggregated_measures_df) {
  aggregated_measures_df %>%
    dplyr::filter(filter = !is.na(aggregate_flow)) %>%
    dplyr::mutate(
      flow_category = cut(x = aggregate_flow, 
                          breaks = c(0, 500, 1000, 2000, 5000, Inf),
                          labels = c("0-500", "501-1000", "1001-2000", 
                                     "2001-5000", ">5000"),
                          include.lowest = TRUE))
  

  
  quality_data <- aggregated_measures_df %>%
    select(perc_flow_predicted, 
           perc_flow_trucks_predicted, 
           perc_speed_predicted, 
           perc_occupancy_predicted) %>%
    tidyr::pivot_longer(cols = everything(), 
                        names_to = "metric", 
                        values_to = "pct_predicted") %>%
    filter(filter = !is.na(pct_predicted)) %>%
    mutate(metric_label = factor(x = metric,
                                 levels = c("perc_flow_predicted", 
                                            "perc_flow_trucks_predicted", 
                                            "perc_speed_predicted", 
                                            "perc_occupancy_predicted"),
                                 labels = c("Flow", 
                                            "Trucks", 
                                            "Speed", 
                                            "Occupancy")))
  
  
}
#' 
#' @title Compute data quality metrics
#' @description This function extracts prediction quality indicators from 
#'              aggregated traffic measures and reshapes them into a tidy long 
#'              format suitable for visualization and statistical summaries.
#'              The quality indicators represent the percentage of predicted 
#'              data (as opposed to measured data) for different traffic 
#'              variables.
#'              Lower values indicate higher data quality.
#' @param aggregated_measures_df A data.frame or tibble containing aggregated 
#'                               traffic measures, including quality indicator 
#'                               columns such as `perc_flow_predicted`, 
#'                               `perc_speed_predicted`, etc.
#' @return A tibble in long format with the following columns:
#'         \itemize{
#'              \item \code{metric} – internal metric name (e.g. 
#'                          "perc_flow_predicted")
#'              \item \code{pct_predicted} – percentage of predicted data 
#'                          (0–100)
#'              \item \code{metric_label} – human-readable label for plotting
#' }
#' @details The function:
#'          \enumerate{
#'              \item selects all available quality indicator columns,
#'              \item reshapes them using \code{pivot_longer()},
#'              \item removes missing values,
#'              \item attaches descriptive factor labels for visualization.
#'          }
#' @examples
#' \dontrun{
#' quality_data <- compute_quality_metrics_long(aggregated_measures_df)
#' }
#' @export
compute_quality_metrics_long <- function(aggregated_measures_df) {
  aggregated_measures_df %>%
    dplyr::select(
      perc_flow_predicted,
      perc_flow_trucks_predicted,
      perc_speed_predicted,
      perc_occupancy_predicted
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "metric",
      values_to = "pct_predicted"
    ) %>%
    dplyr::filter(
      filter = !is.na(pct_predicted)) %>%
    dplyr::mutate(
      metric_label = factor(
        x = metric,
        levels = c("perc_flow_predicted", "perc_flow_trucks_predicted", 
                   "perc_speed_predicted", "perc_occupancy_predicted"),
        labels = c("Flow", "Trucks", "Speed", "Occupancy")))
}