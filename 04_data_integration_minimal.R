# ==============================================================================
# STAGE 4: DATA INTEGRATION - MINIMAL VERSION
# ==============================================================================

message("\n === Avatar data post-processing === \n")

# ------------------------------------------------------------------------------
# Clean Avatar data
# ------------------------------------------------------------------------------
if (!exists(x= 'avatar_data', inherits = FALSE)){
  
  message("\t 🔗 Loading downloaded Avatar data \n")
  
  start_timer()
  avatar_data <- readRDS(file = CONFIG$AVATAR_DATA_FILEPATH)
  elapsed <- stop_timer()
  
  message("\t\t ✓ Avatar data loaded in ", elapsed, "\n")
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
message("\t 🔗 Hourly aggregating Avatar data \n")
start_timer()

hourly_aggregated <- aggregate_avatar_hourly(dt = avatar_data)
# hourly_aggregated <- avatar_data[, .(
#   # Use MEDIAN to eliminate outliers
#   # Force all results to be double type to avoid type conflicts
#   hourly_flow <- ifelse(test = sum(!is.na(flow.veh.h.)) > 0, 
#                         yes = as.double(x = median(flow.veh.h., 
#                                                na.rm = TRUE)), 
#                         no = NA_real_),
#   hourly_flow_trucks <- ifelse(test = sum(!is.na(flow_trucks.veh.h.)) > 0, 
#                                yes = as.double(x = median(flow_trucks.veh.h., 
#                                                       na.rm = TRUE)), 
#                                no = NA_real_),
#   hourly_occupancy <- ifelse(test = sum(!is.na(occupancy...)) > 0, 
#                              yes = as.double(x = median(occupancy..., 
#                                                     na.rm = TRUE)), 
#                              no = NA_real_),
#   hourly_speed <- ifelse(sum(!is.na(speed.km.h.)) > 0, 
#                          yes = as.double(x = median(speed.km.h., na.rm = TRUE)), 
#                          no = NA_real_),
#   # Quality indicators: % of data that is predicted vs measured (0-100%)
#   # LOWER is better (0% = all measured, 100% = all predicted)
#   perc_flow_predicted <- ifelse(
#     test = sum(!is.na(perc_flow_predicted)) > 0, 
#     yes = as.double(x = mean(perc_flow_predicted, 
#                          na.rm = TRUE)), 
#     no = NA_real_),
#   perc_flow_trucks_predicted <- ifelse(
#     test = sum(!is.na(perc_flow_trucks_predicted)) > 0, 
#     yes = as.double(x = mean(perc_flow_trucks_predicted, 
#                          na.rm = TRUE)), 
#     no = NA_real_),
#   perc_occupancy_predicted <- ifelse(
#     test = sum(!is.na(perc_occupancy_predicted)) > 0, 
#     yes = as.double(x = mean(perc_occupancy_predicted, 
#                          na.rm = TRUE)), 
#     no = NA_real_),
#   perc_speed_predicted <- ifelse(
#     test = sum(!is.na(perc_speed_predicted)) > 0, 
#     yes = as.double(x = mean(perc_speed_predicted, 
#                          na.rm = TRUE)), 
#     no = NA_real_),
#   # Quality metrics for this hour (force to double for consistency)
#   n_obs_this_hour = as.double(x = .N),
#   first_timestamp = as.double(x = min(measure_datetime, 
#                                       na.rm = TRUE)),
#   last_timestamp = as.double(x = max(measure_datetime, 
#                                      na.rm = TRUE))
#   ), 
#     by = .(count_point_id, period, hour)]

elapsed <- stop_timer()
message("\t\t ✓ Hourly aggregation done in ", elapsed, "\n")

# Aggregate by period (D/E/N)
message("\t 🔗 Aggregating Avatar data by period \n")
start_timer()

aggregated_measures_period <- aggregate_avatar_metrics(
  dt = hourly_aggregated, 
  by_vars = c("count_point_id", "period"))
# aggregated_measures_period <- hourly_aggregated[, .(
#   aggregate_flow <- ifelse(
#     test = sum(!is.na(hourly_flow)) > 0, 
#     yes = as.double(x = mean(hourly_flow, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   aggregate_flow_trucks <- ifelse(
#     test = sum(!is.na(hourly_flow_trucks)) > 0, 
#     yes = as.double(x = mean(hourly_flow_trucks, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   aggregate_occupancy <- ifelse(
#     test = sum(!is.na(hourly_occupancy)) > 0, 
#     yes = as.double(x = mean(hourly_occupancy, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   aggregate_speed <- ifelse(
#     test = sum(!is.na(hourly_speed)) > 0, 
#     yes = as.double(x = mean(hourly_speed, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   # Quality indicators (average across hours in this period)
#   perc_flow_predicted <- ifelse(
#     test = sum(!is.na(perc_flow_predicted)) > 0, 
#     yes = as.double(x = mean(perc_flow_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   perc_flow_trucks_predicted <- ifelse(
#     test = sum(!is.na(perc_flow_trucks_predicted)) > 0, 
#     yes = as.double(x = mean(perc_flow_trucks_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   perc_occupancy_predicted <- ifelse(
#     test = sum(!is.na(perc_occupancy_predicted)) > 0, 
#     yes = as.double(x = mean(perc_occupancy_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   perc_speed_predicted <- ifelse(
#     test = sum(!is.na(perc_speed_predicted)) > 0, 
#     yes = as.double(x = mean(perc_speed_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   n_hours_with_data = as.double(
#     x = sum(!is.na(hourly_flow))),
#   n_hours_with_trucks = as.double(
#     x = sum(!is.na(hourly_flow_trucks))),
#   n_hours_with_occupancy = as.double(
#     x = sum(!is.na(hourly_occupancy))),
#   n_hours_with_speed = as.double(
#     x = sum(!is.na(hourly_speed))),
#   n_total_observations = as.double(
#     x = sum(n_obs_this_hour, 
#             na.rm = TRUE)),
#   first_timestamp = as.double(
#     x = min(first_timestamp, 
#             na.rm = TRUE)),
#   last_timestamp = as.double(
#     x = max(last_timestamp, 
#             na.rm = TRUE))
#   ), 
#   by = .(count_point_id, period)]

elapsed <- stop_timer()
message("\t\t ✓ Aggregation by period done in ", elapsed, "\n")

# Aggregate by hour (h0-h23) - create separate rows with period = "h0", "h1", 
# etc.
message("\t 🔗 Aggregating Avatar data by hour \n")
start_timer()

aggregated_measures_hourly <- aggregate_avatar_metrics(
  dt = hourly_aggregated, 
  by_vars = c("count_point_id", "hour"))
# aggregated_measures_hourly <- hourly_aggregated[, .(
#   aggregate_flow <- ifelse(
#     test = sum(!is.na(hourly_flow)) > 0, 
#     yes = as.double(x = mean(hourly_flow, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   aggregate_flow_trucks <- ifelse(
#     test = sum(!is.na(hourly_flow_trucks)) > 0, 
#     yes = as.double(x = mean(hourly_flow_trucks, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   aggregate_occupancy <- ifelse(
#     test = sum(!is.na(hourly_occupancy)) > 0, 
#     yes = as.double(x = mean(hourly_occupancy, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   aggregate_speed <- ifelse(
#     test = sum(!is.na(hourly_speed)) > 0, 
#     yes = as.double(x = mean(hourly_speed, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   # Quality indicators (average for this hour)
#   perc_flow_predicted <- ifelse(
#     test = sum(!is.na(perc_flow_predicted)) > 0, 
#     yes = as.double(x = mean(perc_flow_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   perc_flow_trucks_predicted <- ifelse(
#     test = sum(!is.na(perc_flow_trucks_predicted)) > 0, 
#     yes = as.double(x = mean(perc_flow_trucks_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   perc_occupancy_predicted <- ifelse(
#     test = sum(!is.na(perc_occupancy_predicted)) > 0, 
#     yes = as.double(x = mean(perc_occupancy_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   perc_speed_predicted <- ifelse(
#     test = sum(!is.na(perc_speed_predicted)) > 0, 
#     yes = as.double(x = mean(perc_speed_predicted, 
#                              na.rm = TRUE)), 
#     no = NA_real_),
#   n_hours_with_data = as.double(
#     x = sum(!is.na(hourly_flow))),
#   n_hours_with_trucks = as.double(
#     x = sum(!is.na(hourly_flow_trucks))),
#   n_hours_with_occupancy = as.double(
#     x = sum(!is.na(hourly_occupancy))),
#   n_hours_with_speed = as.double(
#     x = sum(!is.na(hourly_speed))),
#   n_total_observations = as.double(
#     x = sum(n_obs_this_hour, 
#             na.rm = TRUE)),
#   first_timestamp = as.double(
#     x = min(first_timestamp, 
#             na.rm = TRUE)),
#   last_timestamp = as.double(
#     x = max(last_timestamp, 
#             na.rm = TRUE))
#   ), 
#   by = .(count_point_id, hour)]

# Add period column with "h0", "h1", etc. format
aggregated_measures_hourly[, period := paste0("h", hour)]
aggregated_measures_hourly[, hour := NULL]

elapsed <- stop_timer()
message("\t\t ✓ Aggregation by hour done in ", elapsed, "\n")

# Combine period and hourly data
aggregated_measures <- rbind(aggregated_measures_period, 
                             aggregated_measures_hourly, 
                             fill = TRUE)

# Join with roads
aggregated_measures_df <- as.data.frame(aggregated_measures)

# ------------------------------------------------------------------------------
# Create relative values (% of period D for each count_point)
# ------------------------------------------------------------------------------

# Extract period D values as baseline for each count_point
baseline_D <- aggregated_measures_df %>% 
  filter(period == "D") %>% 
  select(count_point_id, 
         flow_D = aggregate_flow,
         flow_trucks_D = aggregate_flow_trucks,
         speed_D = aggregate_speed,
         occupancy_D = aggregate_occupancy,
         truck_pct_D = truck_pct) %>% 
  filter(!is.na(flow_D))  # Only keep count_points with valid D data

n_points_with_D <- nrow(x = baseline_D)
n_total_points <- length(x = unique(aggregated_measures_df$count_point_id))

# Join baseline D values to all periods
aggregated_measures_with_baseline <- aggregated_measures_df %>%
  left_join(baseline_D, 
            by = "count_point_id")

# Calculate ratios (% relative to D)
aggregated_measures_with_ratios <- aggregated_measures_with_baseline %>%
  mutate(
    # Ratios for flow
    ratio_flow <- ifelse(
      test = !is.na(flow_D) & flow_D > 0, 
      yes = aggregate_flow / flow_D, 
      no = NA_real_),
    # Ratios for trucks
    ratio_flow_trucks <- ifelse(
      test = (!is.na(flow_trucks_D) & flow_trucks_D > 0), 
      yes = aggregate_flow_trucks / flow_trucks_D, 
      no = NA_real_),
    # Ratios for speed
    ratio_speed <- ifelse(
      test = (!is.na(speed_D) & speed_D > 0), 
      yes = aggregate_speed / speed_D, 
      no = NA_real_),
    # Ratios for occupancy
    ratio_occupancy <- ifelse(
      test = (!is.na(occupancy_D) & occupancy_D > 0), 
      yes = aggregate_occupancy / occupancy_D, 
      no = NA_real_),
    # Also compute truck percentage (HGV/TV %) instead of absolute truck count
    # (this is more stable than absolute values)
    truck_pct <- ifelse(
      test = (!is.na(aggregate_flow) & aggregate_flow > 0), 
      yes = 100 * aggregate_flow_trucks / aggregate_flow, 
      no = NA_real_),
    truck_pct_D <- ifelse(
      test = (!is.na(flow_D) & flow_D > 0), 
      yes = 100 * flow_trucks_D / flow_D, 
      no = NA_real_),
    # Ratio for truck percentage (not truck count!)
    ratio_truck_pct <- ifelse(
      test = (!is.na(truck_pct_D) & truck_pct_D > 0), 
      yes = truck_pct / truck_pct_D, 
      no = NA_real_)
  )

# Replace original dataframe with version including ratios
aggregated_measures_with_ratios_df <- aggregated_measures_with_ratios

# Save aggregated measures with ratios data frame to disk
saveRDS(object = aggregated_measures_with_ratios_df , 
        file = CONFIG$AVATAR_AGGREGATED_FILEPATH)

message("\t\t ✓ Aggregated data save into file ", 
        CONFIG$AVATAR_AGGREGATED_FILEPATH, "\n")

# Memory cleanup
rm(baseline_D, aggregated_measures_with_baseline, 
   aggregated_measures_with_ratios, ratio_stats, ratio_by_period)

# ------------------------------------------------------------------------------
# Descriptive analysis
# ------------------------------------------------------------------------------

# Prepare data
total_obs <- nrow(x = aggregated_measures_df)
count_points_with_data <- length(x = unique(aggregated_measures_df$count_point_id))

# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------

# ************************************* #
# Hourly traffic patterns (24h profile) #
# ************************************* #

hourly_patterns <- compute_hourly_patterns(aggregated_measures_df)

# Plot 1: hourly traffic patterns (24h profile)
p1 <- ggplot(data = hourly_patterns, 
             mapping = aes(x = hour)) +
  geom_ribbon(mapping = aes(ymin = q25_flow, ymax = q75_flow), 
              fill = "steelblue", 
              alpha = 0.3) +
  geom_line(mapping = aes(y = avg_flow), 
            color = "steelblue", 
            size = 1.2) +
  geom_point(mapping = aes(y = avg_flow), 
             color = "steelblue", 
             size = 2) +
  geom_line(mapping = aes(y = median_flow), 
            color = "orange", 
            linetype = "dashed", 
            size = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2), 
                     labels = paste0(seq(0, 23, 2), "h")) +
  labs(
    title = "24-Hour Traffic Profile",
    subtitle = "Average flow with Q1-Q3 range (blue = mean, orange = median)",
    x = "Hour of Day",
    y = "Traffic Flow (vehicles/hour)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", 
                              size = 14),
    panel.grid.minor = element_blank())

# Save plot 1 as pdf file
ggsave(filename = CONFIG$FIG_HOURLY_TRAFFIC_FILENAME,
       plot = p1,
       path = CONFIG$FIGS_DIR)

# ********************************** #
# Speed and truck percentage by hour #
# ********************************** #

hourly_long <- compute_hourly_long_format(hourly_patterns)

# Plot 2: speed and truck percentage by hour
p2 <- ggplot(data = hourly_long, 
             mapping = aes(x = hour, 
                           y = value, 
                           color = metric_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 23, 2), 
                     labels = paste0(seq(0, 23, 2), "h")) +
  scale_color_manual(values = c("Average Speed (km/h)" = "#2ecc71", 
                                "Truck Percentage (%)" = "#e74c3c")) +
  labs(
    title = "Speed and Truck Percentage Throughout the Day",
    subtitle = "Note: Different scales on same plot",
    x = "Hour of Day",
    y = "Value",
    color = "Metric") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", 
                              size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank())

# Save plot 2 as pdf file
ggsave(filename = CONFIG$SPEED_AND_TRUCK_PERCENTAGE,
       plot = p2,
       path = CONFIG$FIGS_DIR)

# ******************************************** #
# Period comparison (D/E/N) - multiple metrics #
# ******************************************** #

period_stats    <- compute_period_statistics(aggregated_measures_df)

# Plot 3: period comparison (D/E/N) - multiple metrics 
p3a <- ggplot(data = period_stats, 
              mapping = aes(x = period_label, 
                            y = avg_flow, 
                            fill = period_label)) +
  geom_col(width = 0.7) +
  geom_text(mapping = aes(label = sprintf("%.0f", avg_flow)), 
            vjust = -0.5, 
            size = 4) +
  scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) +
  labs(title = "Average Flow", y = "Vehicles/hour", x = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"))

p3b <- ggplot(data = period_stats, 
              mapping = aes(x = period_label, 
                            y = avg_speed, 
                            fill = period_label)) +
  geom_col(width = 0.7) +
  geom_text(mapping = aes(label = sprintf("%.1f", avg_speed)), 
            vjust = -0.5, 
            size = 4) +
  scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) +
  labs(title = "Average Speed", y = "km/h", x = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"))

p3c <- ggplot(data = period_stats, 
              mapping = aes(x = period_label, 
                            y = pct_trucks, 
                            fill = period_label)) +
  geom_col(width = 0.7) +
  geom_text(mapping = aes(label = sprintf("%.1f%%", pct_trucks)), 
            vjust = -0.5, 
            size = 4) +
  scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) +
  labs(title = "Truck Percentage", y = "% of flow", x = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"))

p3 <- gridExtra::grid.arrange(p3a, p3b, p3c, 
                              ncol = 3,
                              top = grid::textGrob(
                                label = "Period Comparison (Day/Evening/Night)", 
                                gp = grid::gpar(fontface = "bold", 
                                                fontsize = 14)))

# Save plot 3 as pdf file
ggsave(filename = CONFIG$TRAFFIC_PERIOD_COMPARISONS,
       plot = p3,
       path = CONFIG$FIGS_DIR)

# ****************************************** #
# Traffic flow distribution and data quality #
# ****************************************** #

flow_data <- compute_flow_distribution(aggregated_measures_df)
quality_data <- compute_quality_metrics_long(aggregated_measures_df)

# Plot 4: traffic flow distribution and data quality
p4a <- ggplot(data = flow_data, 
              mapping = aes(x = aggregate_flow)) +
  geom_histogram(bins = 50, 
                 fill = "steelblue", 
                 color = "white", 
                 alpha = 0.8) +
  geom_vline(mapping = aes(xintercept = median(x = aggregate_flow, 
                                               na.rm = TRUE)), 
            color = "red", 
            linetype = "dashed", 
            size = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Traffic Flow Distribution",
    subtitle = sprintf("Median: %.0f veh/h (red line)", 
                      median(x = flow_data$aggregate_flow, 
                             na.rm = TRUE)),
    x = "Flow (vehicles/hour)",
    y = "Count") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

p4b <- ggplot(data = quality_data, 
              mapping = aes(x = metric_label, 
                            y = pct_predicted, 
                            fill = metric_label)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Data Quality: % Predicted vs Measured",
    subtitle = "Lower = better (more measured data)",
    x = NULL,
    y = "% Predicted (0-100%)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"))

p4 <- gridExtra::grid.arrange(p4a, p4b, 
                              ncol = 2)

# Save plot 4 as pdf file
ggsave(filename = CONFIG$TRAFFIC_FLOW_DISTRIBUTION_AND_DATA_QUALITY,
       plot = p4,
       path = CONFIG$FIGS_DIR)

# Display plots if local run
if (isFALSE(CONFIG$IS_TTY)) {
  p1
  p2
  p3a
  p3b
  p3c
  p4a
  p4b
}