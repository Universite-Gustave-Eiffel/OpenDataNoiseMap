# ==============================================================================
# STAGE 4: DATA INTEGRATION - MINIMAL VERSION
# ==============================================================================

pipeline_message(text = "Avatar data post-processing", 
                 level = 0, progress = "start", process = "calc")

# ------------------------------------------------------------------------------
# Clean Avatar data
# ------------------------------------------------------------------------------
if (!exists(x= 'avatar_data', inherits = FALSE)){
  
  pipeline_message(
    text = paste0("Loading downloaded Avatar data from ", 
                  rel_path(CONFIG$AVATAR_DATA_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  avatar_data <- readRDS(file = CONFIG$AVATAR_DATA_FILEPATH)
  
  pipeline_message(
    text = paste0("Avatar data successfully loaded", 
                  rel_path(CONFIG$AVATAR_DATA_FILEPATH)), 
    level = 1, progress = "end", process = "valid")
}

# Coerce Avatar data frame to data.table
setDT(avatar_data)

# Create periods
if (!inherits(avatar_data$measure_datetime, "POSIXct")) {
  avatar_data[, measure_datetime := as.POSIXct(measure_datetime, 
                                               format="%Y-%m-%dT%H:%M:%S")]
}
avatar_data[, hour := hour(measure_datetime)]
avatar_data[, period := ifelse(test = hour >= 6 & hour < 18, 
                               yes = "D", 
                               no = ifelse(test = hour >= 18 & hour < 22, 
                                           yes = "E", 
                                           no = "N"))]

# Hourly aggregation
pipeline_message(text = "Hourly aggregating Avatar data", 
                 level = 1, progress = "start", process = "calc")

# Intermediate hourly data
hourly_aggregated <- aggregate_avatar_hourly(dt = avatar_data)

pipeline_message(text = "Hourly aggregation successfully done", 
                 level = 1, progress = "end", process = "valid")

# Aggregate by period (D/E/N)
pipeline_message(text = "Aggregating Avatar data by period", 
                 level = 1, progress = "start", process = "calc")

# Data Aggregation over Day/Evening/Night periods
aggregated_measures_period <- aggregate_avatar_metrics(
  dt = hourly_aggregated, 
  by_vars = c("count_point_id", "period"))

pipeline_message(text = "Aggregation by period successfully done", 
                 level = 1, progress = "end", process = "valid")

# Aggregate by hour (h0-h23) - create separate rows with period = "h0", "h1", 
# etc.
pipeline_message(
  text = "Calculating traffic flow metrics from hourly aggregated Avatar data", 
  level = 1, progress = "start", process = "calc")

# Hourly data aggregation
aggregated_measures_hourly <- aggregate_avatar_metrics(
  dt = hourly_aggregated, 
  by_vars = c("count_point_id", "hour"))

# Add period column with "h0", "h1", etc. format
aggregated_measures_hourly[, period := paste0("h", hour)]
aggregated_measures_hourly[, hour := NULL]


pipeline_message(text = "Traffic flow metrics successfully calculated", 
                 level = 1, progress = "end", process = "valid")

# Union of aggregated data by period and by hour
aggregated_measures <- rbind(aggregated_measures_period, 
                             aggregated_measures_hourly, 
                             fill = TRUE)

# ------------------------------------------------------------------------------
# Create relative values (% of period D for each count_point)
# ------------------------------------------------------------------------------

pipeline_message(text = "Calculating traffic flow relative metrics", 
                 level = 1, progress = "start", process = "calc")

setDT(aggregated_measures)
avatar_aggregated <- compute_avatar_relative_metrics(aggregated_measures)

# Save aggregated measures with ratios data frame to disk
saveRDS(object = avatar_aggregated, 
        file = CONFIG$AVATAR_AGGREGATED_FILEPATH)

pipeline_message(
  text = sprintf("Traffic flow relative metrics sucessfully calculated and saved 
                 into file ", rel_path(CONFIG$AVATAR_AGGREGATED_FILEPATH)), 
  level = 1, progress = "end", process = "valid")

# Memory cleanup
rm(aggregated_measures)

# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------

pipeline_message(text = "Plotting aggregated data", 
                 level = 1, progress = "start", process = "plot")

# hourly aggregated statistics
hourly_patterns <- compute_hourly_patterns(avatar_aggregated)

# ************************************* #
# Hourly traffic patterns (24h profile) #
# ************************************* #

pipeline_message(text = "Plotting hourly traffic patterns", 
                 level = 1, progress = "start", process = "plot")

# Plot 1: hourly traffic patterns (24h profile)
p1 <- plot_hourly_traffic_profile(traffic_hourly_patterns = hourly_patterns, 
                                  fig_path = CONFIG$FIGS_DIR, 
                                  fig_name = CONFIG$FIG_HOURLY_TRAFFIC_FILENAME) 

pipeline_message(
  text = sprintf("Hourly aggregated data successfully plotted and saved into 
                 file ", paste(rel_path(CONFIG$FIGS_DIR), 
                               CONFIG$FIG_HOURLY_TRAFFIC_FILENAME, 
                                      sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# ********************************** #
# Speed and truck percentage by hour #
# ********************************** #

pipeline_message(text = "Plotting speed and truck percentage by hour", 
                 level = 1, progress = "start", process = "plot")

# Plot 2: speed and truck percentage by hour
p2 <- plot_speed_and_truck_percentage(
  traffic_hourly_patterns = hourly_patterns, 
  fig_path = CONFIG$FIGS_DIR, 
  fig_name = CONFIG$SPEED_AND_TRUCK_PERCENTAGE)

pipeline_message(
  text = sprintf("Hourly speed and truck percentage successfully plotted and saved 
                 into file ", paste(rel_path(CONFIG$FIGS_DIR), 
                                    CONFIG$SPEED_AND_TRUCK_PERCENTAGE, 
                                    sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# ******************************************** #
# Period comparison (D/E/N) - multiple metrics #
# ******************************************** #

pipeline_message(text = "Plotting period comparison (D/E/N)", 
                 level = 1, progress = "start", process = "plot")

p3 <- plot_period_comparison(aggregated_traffic_data = avatar_aggregated, 
                           fig_path = CONFIG$FIGS_DIR, 
                           fig_name = CONFIG$TRAFFIC_PERIOD_COMPARISONS)

pipeline_message(
  text = sprintf("Period comparison (D/E/N) successfully plotted and saved into 
                 file ", paste(rel_path(CONFIG$FIGS_DIR), 
                               CONFIG$TRAFFIC_PERIOD_COMPARISONS, 
                               sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# ****************************************** #
# Traffic flow distribution and data quality #
# ****************************************** #

pipeline_message(text = "Plotting traffic flow distribution and data quality", 
                 level = 1, progress = "start", process = "plot")

p4 <- plot_flow_distribution_and_quality(
  aggregated_traffic_data = avatar_aggregated, 
  fig_path = CONFIG$FIGS_DIR, 
  fig_name = CONFIG$TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY)

pipeline_message(
  text = sprintf("Traffic flow distribution and data quality successfully 
                  plotted and saved into file ", 
                 paste(rel_path(CONFIG$FIGS_DIR), 
                       CONFIG$TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY, 
                       sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# Display plots if local run
if (isFALSE(CONFIG$IS_TTY)) {
  p1
  p2
  p3$flow
  p3$speed
  p3$trucks
  p4$flow
  p4$quality
}

pipeline_message(text = "Avatar data successfully post-processed", 
                 level = 0, progress = "end", process = "valid")