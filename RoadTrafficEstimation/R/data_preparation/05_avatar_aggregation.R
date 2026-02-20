# ==============================================================================
# STAGE 4: DATA INTEGRATION - MINIMAL VERSION
# ==============================================================================

pipeline_message("Avatar data post-processing", level = 0, 
                 progress = "start", process = "calc")

# ------------------------------------------------------------------------------
# Clean Avatar data
# ------------------------------------------------------------------------------
if (!exists(x= 'avatar_data', inherits = FALSE)){
  
  pipeline_message(sprintf("Loading downloaded Avatar data from %s", 
                           rel_path(cfg_data$AVATAR_DATA_FILEPATH)), 
                   level = 1, progress = "start", process = "load")
  
  # Memory check before loading large RDS (~3.8 GB in RAM)
  avail_gb <- get_available_memory_gb()
  if (!is.na(avail_gb) && avail_gb < 6) {
    pipeline_message(
      text = sprintf("Low RAM detected (%.1f GB available) - switching to chunk-streaming mode", 
                     avail_gb),
      process = "warning")
    use_chunk_streaming <- TRUE
  } else {
    check_memory_available(
      operation_name = "Load Avatar raw traffic RDS (~4 GB in RAM)",
      min_gb = 6, warn_gb = 10)
    
    avatar_data <- readRDS(file = cfg_data$AVATAR_DATA_FILEPATH)
    
    pipeline_message(text = describe_df(avatar_data), process = "info")
    
    pipeline_message(
      text = "Avatar data successfully loaded", 
      level = 1, progress = "end", process = "valid")
  }
}

if (isTRUE(use_chunk_streaming)) {
  pipeline_message(
    text = "Chunk-streaming mode: hourly aggregation directly from CSV chunks", 
    level = 1, progress = "start", process = "calc")
  
  files <- list.files(path = cfg_data$AVATAR_CSV_DATA_DIRPATH, 
                      pattern = "avatar_data_chunk_.*\\.csv", 
                      full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No Avatar CSV chunk files found for low-memory streaming mode")
  }
  
  temp_hourly_csv <- file.path(cfg_data$AVATAR_CSV_DATA_DIRPATH, 
                               "avatar_hourly_aggregated_tmp.csv")
  if (file.exists(temp_hourly_csv)) {
    file.remove(temp_hourly_csv)
  }
  
  wrote_header <- FALSE
  n_files <- length(files)
  
  for (i in seq_along(files)) {
    dt_chunk <- data.table::fread(
      file = files[i],
      sep = ";",
      stringsAsFactors = FALSE
    )
    
    # Sanitize raw column names (flow[veh/h] -> flow.veh.h.)
    setnames(dt_chunk, make.names(names(dt_chunk), unique = TRUE))
    
    hourly_chunk <- aggregate_avatar_hourly(dt = dt_chunk)
    
    data.table::fwrite(
      x = hourly_chunk,
      file = temp_hourly_csv,
      sep = ";",
      append = wrote_header,
      col.names = !wrote_header,
      quote = TRUE,
      na = ""
    )
    wrote_header <- TRUE
    
    rm(dt_chunk, hourly_chunk)
    gc(verbose = FALSE)
    
    if (i %% 25 == 0 || i == n_files) {
      pipeline_message(
        text = sprintf("Chunk-streaming progress: %d/%d files", i, n_files),
        process = "info")
    }
  }
  
  hourly_aggregated <- data.table::fread(
    file = temp_hourly_csv,
    sep = ";",
    stringsAsFactors = FALSE
  )
  unlink(temp_hourly_csv)
  
  pipeline_message(
    text = "Hourly aggregation successfully completed in chunk-streaming mode", 
    level = 1, progress = "end", process = "valid")
} else {
  pipeline_message(
    text = "Conversion of Avatar data into a data table and creation of period and 
          hour columns", 
    level = 1, progress = "start", process = "configure")

pipeline_message("Conversion of Avatar data into a data table and creation of ", 
                 "period and hour columns", 
                 level = 1, progress = "start", process = "configure")

  # Coerce Avatar data frame to data.table
  setDT(avatar_data)

  # Sanitize column names (CSV via fread keeps special chars like flow[veh/h])
  setnames(avatar_data, make.names(names(avatar_data), unique = TRUE))

# Create periods and hours
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

pipeline_message("Avatar data successfully converted and columns created", 
                 level = 1, progress = "end", process = "valid")

# Hourly aggregation
pipeline_message("Hourly aggregating Avatar data", level = 1, 
                 progress = "start", process = "calc")

# Intermediate hourly data
hourly_aggregated <- aggregate_avatar_hourly(dt = avatar_data)

pipeline_message("Hourly aggregation successfully done", level = 1, 
                 progress = "end", process = "valid")

# Aggregate by period (D/E/N)
pipeline_message("Aggregating Avatar data by period", level = 1, 
                 progress = "start", process = "calc")

# Data Aggregation over Day/Evening/Night periods
aggregated_measures_period <- aggregate_avatar_metrics(
  dt = hourly_aggregated, 
  by_vars = c("count_point_id", "period"))

pipeline_message("Aggregation by period successfully done", level = 1, 
                 progress = "end", process = "valid")

# Aggregate by hour (h0-h23) - create separate rows with period = "h0", "h1", 
# etc.
pipeline_message(
  "Calculating traffic flow metrics from hourly aggregated Avatar data", 
  level = 1, progress = "start", process = "calc")

# Hourly data aggregation (all days)
aggregated_measures_hourly <- aggregate_avatar_metrics(
  dt = hourly_aggregated, 
  by_vars = c("count_point_id", "hour"))

# Add period column with "h0", "h1", etc. format
aggregated_measures_hourly[, period := paste0("h", hour)]
aggregated_measures_hourly[, hour := NULL]

pipeline_message("Traffic flow metrics successfully calculated", level = 1, 
                 progress = "end", process = "valid")

pipeline_message("Traffic flow metrics successfully calculated", level = 1, 
                 progress = "end", process = "valid")

pipeline_message(
  text = "Calculating weekday/weekend hourly traffic flow metrics", 
  level = 1, progress = "start", process = "calc")

# Weekday hourly aggregation
hourly_wd <- hourly_aggregated[day_type == "wd"]
aggregated_measures_hourly_wd <- aggregate_avatar_metrics(
  dt = hourly_wd, 
  by_vars = c("count_point_id", "hour"))
aggregated_measures_hourly_wd[, period := paste0("h", hour, "_wd")]
aggregated_measures_hourly_wd[, hour := NULL]

# Weekend hourly aggregation
hourly_we <- hourly_aggregated[day_type == "we"]
aggregated_measures_hourly_we <- aggregate_avatar_metrics(
  dt = hourly_we, 
  by_vars = c("count_point_id", "hour"))
aggregated_measures_hourly_we[, period := paste0("h", hour, "_we")]
aggregated_measures_hourly_we[, hour := NULL]

pipeline_message(
  text = sprintf("Weekday/weekend hourly metrics: %s wd rows, %s we rows", 
                 fmt(nrow(aggregated_measures_hourly_wd)), 
                 fmt(nrow(aggregated_measures_hourly_we))), 
  level = 1, progress = "end", process = "valid")

# Union of all aggregated data
aggregated_measures <- rbind(aggregated_measures_period, 
                             aggregated_measures_hourly, 
                             aggregated_measures_hourly_wd,
                             aggregated_measures_hourly_we,
                             fill = TRUE)

# Memory cleanup
rm(hourly_wd, hourly_we, aggregated_measures_hourly_wd, 
   aggregated_measures_hourly_we)

# ------------------------------------------------------------------------------
# Create relative values (% of period D for each count_point)
# ------------------------------------------------------------------------------

pipeline_message("Calculating traffic flow relative metrics", level = 1, 
                 progress = "start", process = "calc")

setDT(aggregated_measures)
avatar_aggregated <- compute_avatar_relative_metrics(aggregated_measures)

# Save aggregated measures with ratios data frame to disk
saveRDS(object = avatar_aggregated, 
        file = cfg_data$AVATAR_AGGREGATED_FILEPATH)

pipeline_message(describe_df(avatar_aggregated), process = "info")

pipeline_message(
  sprintf("Traffic flow relative metrics sucessfully calculated and saved ", 
          "into file %s", rel_path(cfg_data$AVATAR_AGGREGATED_FILEPATH)), 
  level = 1, progress = "end", process = "valid")

# Memory cleanup
rm(aggregated_measures)

# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------

pipeline_message("Plotting aggregated data", level = 1, 
                 progress = "start", process = "plot")

# hourly aggregated statistics
hourly_patterns <- compute_hourly_patterns(avatar_aggregated)

# ************************************* #
# Hourly traffic patterns (24h profile) #
# ************************************* #

pipeline_message("Plotting hourly traffic patterns", level = 1, 
                 progress = "start", process = "plot")

# Plot 1: hourly traffic patterns (24h profile)
p1 <- plot_hourly_traffic_profile(traffic_hourly_patterns = hourly_patterns, 
                                  fig_path = cfg_g$FIGS_DIR, 
                                  fig_name = cfg_data$FIG_HOURLY_TRAFFIC_FILENAME) 

pipeline_message(
  sprintf("Hourly aggregated data successfully plotted and saved into file %s", 
          paste(rel_path(cfg_g$FIGS_DIR), cfg_data$FIG_HOURLY_TRAFFIC_FILENAME, 
                sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# ********************************** #
# Speed and truck percentage by hour #
# ********************************** #

pipeline_message("Plotting speed and truck percentage by hour", level = 1, 
                 progress = "start", process = "plot")

# Plot 2: speed and truck percentage by hour
p2 <- plot_speed_and_truck_percentage(
  traffic_hourly_patterns = hourly_patterns, 
  fig_path = cfg_g$FIGS_DIR, 
  fig_name = cfg_data$FIG_SPEED_AND_TRUCK_PERCENTAGE)

pipeline_message(
  sprintf("Hourly speed and truck percentage successfully plotted and saved ", 
          "into file %s", paste(rel_path(cfg_g$FIGS_DIR), 
                                cfg_data$FIG_SPEED_AND_TRUCK_PERCENTAGE, 
                                sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# ******************************************** #
# Period comparison (D/E/N) - multiple metrics #
# ******************************************** #

pipeline_message("Plotting period comparison (D/E/N)", level = 1, 
                 progress = "start", process = "plot")

p3 <- plot_period_comparison(aggregated_traffic_data = avatar_aggregated, 
                           fig_path = cfg_g$FIGS_DIR, 
                           fig_name = cfg_data$FIG_TRAFFIC_PERIOD_COMPARISONS)

pipeline_message(
  sprintf("Period comparison (D/E/N) successfully plotted and saved into ", 
          "file %s", paste(rel_path(cfg_g$FIGS_DIR), 
                           cfg_data$FIG_TRAFFIC_PERIOD_COMPARISONS, 
                           sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# ****************************************** #
# Traffic flow distribution and data quality #
# ****************************************** #

pipeline_message("Plotting traffic flow distribution and data quality", 
                 level = 1, progress = "start", process = "plot")

p4 <- plot_flow_distribution_and_quality(
  aggregated_traffic_data = avatar_aggregated, 
  fig_path = cfg_g$FIGS_DIR, 
  fig_name = cfg_data$FIG_TRAFFIC_FLOW_DISTRIB_AND_DATA_QUALITY)

pipeline_message(
  sprintf("Traffic flow distribution and data quality successfully plotted and ", 
          "saved into file %s", 
          paste(rel_path(cfg_g$FIGS_DIR), 
                cfg_data$FIG_TRAFFIC_FLOW_DISTRIB_AND_DATA_QUALITY, 
                sep = .Platform$file.sep)), 
  level = 1, progress = "end", process = "valid")

# Display plots if local run
if (interactive() && isatty(stdout())) {
  p1
  p2
  p3$flow
  p3$speed
  p3$trucks
  p4$flow
  p4$quality
}

pipeline_message("Avatar data successfully post-processed", level = 0, 
                 progress = "end", process = "valid")
