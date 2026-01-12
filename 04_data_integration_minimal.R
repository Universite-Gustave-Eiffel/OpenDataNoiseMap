# =============================================================================
# STAGE 4: DATA INTEGRATION - MINIMAL VERSION
# =============================================================================

#########################################
####################### CLEAN AVATAR DATA
#########################################

avatar_data <- readRDS( "rdsFiles/avatar_data.rds")

# Process avatar data
library(data.table)
setDT(avatar_data)

# Create periods
if (!inherits(avatar_data$measure_datetime, "POSIXct")) {
  avatar_data[, measure_datetime := as.POSIXct(measure_datetime, format="%Y-%m-%dT%H:%M:%S")]
}
avatar_data[, hour := hour(measure_datetime)]
avatar_data[, period := ifelse(hour >= 6 & hour < 18, "D", 
                      ifelse(hour >= 18 & hour < 22, "E", "N"))]

# Aggregate by hour first
hourly_aggregated <- avatar_data[, .(
  # Use MEDIAN to eliminate outliers
  # Force all results to be double type to avoid type conflicts
  hourly_flow = ifelse(sum(!is.na(flow.veh.h.)) > 0,
                       as.double(median(flow.veh.h., na.rm = TRUE)), NA_real_),
  hourly_flow_trucks = ifelse(sum(!is.na(flow_trucks.veh.h.)) > 0,
                              as.double(median(flow_trucks.veh.h., na.rm = TRUE)), NA_real_),
  hourly_occupancy = ifelse(sum(!is.na(occupancy...)) > 0,
                            as.double(median(occupancy..., na.rm = TRUE)), NA_real_),
  hourly_speed = ifelse(sum(!is.na(speed.km.h.)) > 0,
                        as.double(median(speed.km.h., na.rm = TRUE)), NA_real_),
  
  # Quality indicators: % of data that is predicted vs measured (0-100%)
  # LOWER is better (0% = all measured, 100% = all predicted)
  perc_flow_predicted = ifelse(sum(!is.na(perc_flow_predicted)) > 0,
                               as.double(mean(perc_flow_predicted, na.rm = TRUE)), NA_real_),
  perc_flow_trucks_predicted = ifelse(sum(!is.na(perc_flow_trucks_predicted)) > 0,
                                      as.double(mean(perc_flow_trucks_predicted, na.rm = TRUE)), NA_real_),
  perc_occupancy_predicted = ifelse(sum(!is.na(perc_occupancy_predicted)) > 0,
                                    as.double(mean(perc_occupancy_predicted, na.rm = TRUE)), NA_real_),
  perc_speed_predicted = ifelse(sum(!is.na(perc_speed_predicted)) > 0,
                                as.double(mean(perc_speed_predicted, na.rm = TRUE)), NA_real_),
  
  # Quality metrics for this hour (force to double for consistency)
  n_obs_this_hour = as.double(.N),
  first_timestamp = as.double(min(measure_datetime, na.rm = TRUE)),
  last_timestamp = as.double(max(measure_datetime, na.rm = TRUE))
), by = .(count_point_id, period, hour)]

# Aggregate by period (D/E/N)
aggregated_measures_period <- hourly_aggregated[, .(
  aggregate_flow = ifelse(sum(!is.na(hourly_flow)) > 0,
                          as.double(mean(hourly_flow, na.rm = TRUE)), NA_real_),
  aggregate_flow_trucks = ifelse(sum(!is.na(hourly_flow_trucks)) > 0,
                                 as.double(mean(hourly_flow_trucks, na.rm = TRUE)), NA_real_),
  aggregate_occupancy = ifelse(sum(!is.na(hourly_occupancy)) > 0,
                               as.double(mean(hourly_occupancy, na.rm = TRUE)), NA_real_),
  aggregate_speed = ifelse(sum(!is.na(hourly_speed)) > 0,
                           as.double(mean(hourly_speed, na.rm = TRUE)), NA_real_),
  
  # Quality indicators (average across hours in this period)
  perc_flow_predicted = ifelse(sum(!is.na(perc_flow_predicted)) > 0,
                               as.double(mean(perc_flow_predicted, na.rm = TRUE)), NA_real_),
  perc_flow_trucks_predicted = ifelse(sum(!is.na(perc_flow_trucks_predicted)) > 0,
                                      as.double(mean(perc_flow_trucks_predicted, na.rm = TRUE)), NA_real_),
  perc_occupancy_predicted = ifelse(sum(!is.na(perc_occupancy_predicted)) > 0,
                                    as.double(mean(perc_occupancy_predicted, na.rm = TRUE)), NA_real_),
  perc_speed_predicted = ifelse(sum(!is.na(perc_speed_predicted)) > 0,
                                as.double(mean(perc_speed_predicted, na.rm = TRUE)), NA_real_),
  
  n_hours_with_data = as.double(sum(!is.na(hourly_flow))),
  n_hours_with_trucks = as.double(sum(!is.na(hourly_flow_trucks))),
  n_hours_with_occupancy = as.double(sum(!is.na(hourly_occupancy))),
  n_hours_with_speed = as.double(sum(!is.na(hourly_speed))),
  n_total_observations = as.double(sum(n_obs_this_hour, na.rm = TRUE)),
  first_timestamp = as.double(min(first_timestamp, na.rm = TRUE)),
  last_timestamp = as.double(max(last_timestamp, na.rm = TRUE))
), by = .(count_point_id, period)]

# Aggregate by hour (h0-h23) - create separate rows with period = "h0", "h1", etc.
aggregated_measures_hourly <- hourly_aggregated[, .(
  aggregate_flow = ifelse(sum(!is.na(hourly_flow)) > 0,
                          as.double(mean(hourly_flow, na.rm = TRUE)), NA_real_),
  aggregate_flow_trucks = ifelse(sum(!is.na(hourly_flow_trucks)) > 0,
                                 as.double(mean(hourly_flow_trucks, na.rm = TRUE)), NA_real_),
  aggregate_occupancy = ifelse(sum(!is.na(hourly_occupancy)) > 0,
                               as.double(mean(hourly_occupancy, na.rm = TRUE)), NA_real_),
  aggregate_speed = ifelse(sum(!is.na(hourly_speed)) > 0,
                           as.double(mean(hourly_speed, na.rm = TRUE)), NA_real_),
  
  # Quality indicators (average for this hour)
  perc_flow_predicted = ifelse(sum(!is.na(perc_flow_predicted)) > 0,
                               as.double(mean(perc_flow_predicted, na.rm = TRUE)), NA_real_),
  perc_flow_trucks_predicted = ifelse(sum(!is.na(perc_flow_trucks_predicted)) > 0,
                                      as.double(mean(perc_flow_trucks_predicted, na.rm = TRUE)), NA_real_),
  perc_occupancy_predicted = ifelse(sum(!is.na(perc_occupancy_predicted)) > 0,
                                    as.double(mean(perc_occupancy_predicted, na.rm = TRUE)), NA_real_),
  perc_speed_predicted = ifelse(sum(!is.na(perc_speed_predicted)) > 0,
                                as.double(mean(perc_speed_predicted, na.rm = TRUE)), NA_real_),
  
  n_hours_with_data = as.double(sum(!is.na(hourly_flow))),
  n_hours_with_trucks = as.double(sum(!is.na(hourly_flow_trucks))),
  n_hours_with_occupancy = as.double(sum(!is.na(hourly_occupancy))),
  n_hours_with_speed = as.double(sum(!is.na(hourly_speed))),
  n_total_observations = as.double(sum(n_obs_this_hour, na.rm = TRUE)),
  first_timestamp = as.double(min(first_timestamp, na.rm = TRUE)),
  last_timestamp = as.double(max(last_timestamp, na.rm = TRUE))
), by = .(count_point_id, hour)]

# Add period column with "h0", "h1", etc. format
aggregated_measures_hourly[, period := paste0("h", hour)]
aggregated_measures_hourly[, hour := NULL]

# Combine period and hourly data
aggregated_measures <- rbind(aggregated_measures_period, aggregated_measures_hourly, fill = TRUE)

# Join with roads
aggregated_measures_df <- as.data.frame(aggregated_measures)

# =============================================================================
# CREATE RELATIVE VALUES (% of period D for each count_point)
# =============================================================================

# Extract period D values as baseline for each count_point
baseline_D <- aggregated_measures_df %>%
  filter(period == "D") %>%
  select(count_point_id, 
         flow_D = aggregate_flow,
         flow_trucks_D = aggregate_flow_trucks,
         speed_D = aggregate_speed,
         occupancy_D = aggregate_occupancy) %>%
  filter(!is.na(flow_D))  # Only keep count_points with valid D data

n_points_with_D <- nrow(baseline_D)
n_total_points <- length(unique(aggregated_measures_df$count_point_id))

# Join baseline D values to all periods
aggregated_measures_with_baseline <- aggregated_measures_df %>%
  left_join(baseline_D, by = "count_point_id")

# Calculate ratios (% relative to D)
aggregated_measures_with_ratios <- aggregated_measures_with_baseline %>%
  mutate(
    # Ratios for flow
    ratio_flow = ifelse(!is.na(flow_D) & flow_D > 0, 
                        aggregate_flow / flow_D, 
                        NA_real_),
    
    # Ratios for trucks
    ratio_flow_trucks = ifelse(!is.na(flow_trucks_D) & flow_trucks_D > 0, 
                               aggregate_flow_trucks / flow_trucks_D, 
                               NA_real_),
    
    # Ratios for speed
    ratio_speed = ifelse(!is.na(speed_D) & speed_D > 0, 
                        aggregate_speed / speed_D, 
                        NA_real_),
    
    # Ratios for occupancy
    ratio_occupancy = ifelse(!is.na(occupancy_D) & occupancy_D > 0, 
                            aggregate_occupancy / occupancy_D, 
                            NA_real_),
    
    # Also compute truck percentage (HGV/TV %) instead of absolute truck count
    # This is more stable than absolute values
    truck_pct = ifelse(!is.na(aggregate_flow) & aggregate_flow > 0,
                      100 * aggregate_flow_trucks / aggregate_flow,
                      NA_real_),
    
    truck_pct_D = ifelse(!is.na(flow_D) & flow_D > 0,
                        100 * flow_trucks_D / flow_D,
                        NA_real_),
    
    # Ratio for truck percentage (not truck count!)
    ratio_truck_pct = ifelse(!is.na(truck_pct_D) & truck_pct_D > 0,
                            truck_pct / truck_pct_D,
                            NA_real_)
  )

# Replace original dataframe with version including ratios
aggregated_measures_with_ratios_df <- aggregated_measures_with_ratios

saveRDS(aggregated_measures_with_ratios_df , "data/avatar_aggregated_hourly_period.rds")

rm(baseline_D, aggregated_measures_with_baseline, aggregated_measures_with_ratios,
   ratio_stats, ratio_by_period)

####################################
############ DESCRIPTIVE ANALYSIS
####################################


library(ggplot2)
library(gridExtra)

# Prepare data
total_obs <- nrow(aggregated_measures_df)
count_points_with_data <- length(unique(aggregated_measures_df$count_point_id))


# =============================================================================
# PLOT 1: HOURLY TRAFFIC PATTERNS (24h profile)
# =============================================================================

hourly_patterns <- aggregated_measures_df %>%
  filter(grepl("^h\\d+$", period)) %>%  # Only hourly periods (h0-h23)
  mutate(hour = as.integer(gsub("h", "", period))) %>%
  group_by(hour) %>%
  summarise(
    avg_flow = mean(aggregate_flow, na.rm = TRUE),
    sd_flow = sd(aggregate_flow, na.rm = TRUE),
    median_flow = median(aggregate_flow, na.rm = TRUE),
    q25_flow = quantile(aggregate_flow, 0.25, na.rm = TRUE),
    q75_flow = quantile(aggregate_flow, 0.75, na.rm = TRUE),
    avg_speed = mean(aggregate_speed, na.rm = TRUE),
    pct_trucks = mean(100 * aggregate_flow_trucks / aggregate_flow, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  arrange(hour)

p1 <- ggplot(hourly_patterns, aes(x = hour)) +
  geom_ribbon(aes(ymin = q25_flow, ymax = q75_flow), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = avg_flow), color = "steelblue", size = 1.2) +
  geom_point(aes(y = avg_flow), color = "steelblue", size = 2) +
  geom_line(aes(y = median_flow), color = "orange", linetype = "dashed", size = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste0(seq(0, 23, 2), "h")) +
  labs(
    title = "24-Hour Traffic Profile",
    subtitle = "Average flow with Q1-Q3 range (blue = mean, orange = median)",
    x = "Hour of Day",
    y = "Traffic Flow (vehicles/hour)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# PLOT 2: SPEED AND TRUCK PERCENTAGE BY HOUR
# =============================================================================

hourly_long <- hourly_patterns %>%
  select(hour, avg_speed, pct_trucks) %>%
  tidyr::pivot_longer(cols = c(avg_speed, pct_trucks), 
                      names_to = "metric", values_to = "value")

hourly_long$metric_label <- factor(hourly_long$metric,
                                   levels = c("avg_speed", "pct_trucks"),
                                   labels = c("Average Speed (km/h)", "Truck Percentage (%)"))

p2 <- ggplot(hourly_long, aes(x = hour, y = value, color = metric_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 23, 2), labels = paste0(seq(0, 23, 2), "h")) +
  scale_color_manual(values = c("Average Speed (km/h)" = "#2ecc71", 
                                "Truck Percentage (%)" = "#e74c3c")) +
  labs(
    title = "Speed and Truck Percentage Throughout the Day",
    subtitle = "Note: Different scales on same plot",
    x = "Hour of Day",
    y = "Value",
    color = "Metric"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# =============================================================================
# PLOT 3: PERIOD COMPARISON (D/E/N) - MULTIPLE METRICS
# =============================================================================

period_stats <- aggregated_measures_df %>%
  filter(period %in% c("D", "E", "N")) %>%
  group_by(period) %>%
  summarise(
    avg_flow = mean(aggregate_flow, na.rm = TRUE),
    avg_speed = mean(aggregate_speed, na.rm = TRUE),
    pct_trucks = mean(100 * aggregate_flow_trucks / aggregate_flow, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  mutate(
    period_label = factor(period, 
                         levels = c("D", "E", "N"),
                         labels = c("Day\n(6-18h)", "Evening\n(18-22h)", "Night\n(22-6h)"))
  )

# Create 3 subplots
p3a <- ggplot(period_stats, aes(x = period_label, y = avg_flow, fill = period_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f", avg_flow)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) +
  labs(title = "Average Flow", y = "Vehicles/hour", x = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

p3b <- ggplot(period_stats, aes(x = period_label, y = avg_speed, fill = period_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", avg_speed)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) +
  labs(title = "Average Speed", y = "km/h", x = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

p3c <- ggplot(period_stats, aes(x = period_label, y = pct_trucks, fill = period_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct_trucks)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#f39c12", "#e67e22", "#34495e")) +
  labs(title = "Truck Percentage", y = "% of flow", x = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

p3 <- gridExtra::grid.arrange(p3a, p3b, p3c, ncol = 3,
                              top = grid::textGrob("Period Comparison (Day/Evening/Night)", 
                                                  gp = grid::gpar(fontface = "bold", fontsize = 14)))

# =============================================================================
# PLOT 4: TRAFFIC FLOW DISTRIBUTION + DATA QUALITY
# =============================================================================

# Flow distribution
flow_data <- aggregated_measures_df %>%
  filter(!is.na(aggregate_flow)) %>%
  mutate(flow_category = cut(aggregate_flow, 
                             breaks = c(0, 500, 1000, 2000, 5000, Inf),
                             labels = c("0-500", "501-1000", "1001-2000", 
                                       "2001-5000", ">5000"),
                             include.lowest = TRUE))

p4a <- ggplot(flow_data, aes(x = aggregate_flow)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = median(aggregate_flow, na.rm = TRUE)), 
            color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Traffic Flow Distribution",
    subtitle = sprintf("Median: %.0f veh/h (red line)", 
                      median(flow_data$aggregate_flow, na.rm = TRUE)),
    x = "Flow (vehicles/hour)",
    y = "Count"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

# Data quality
quality_data <- aggregated_measures_df %>%
  select(perc_flow_predicted, perc_flow_trucks_predicted, 
         perc_speed_predicted, perc_occupancy_predicted) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "metric", values_to = "pct_predicted") %>%
  filter(!is.na(pct_predicted)) %>%
  mutate(metric_label = factor(metric,
                               levels = c("perc_flow_predicted", "perc_flow_trucks_predicted",
                                         "perc_speed_predicted", "perc_occupancy_predicted"),
                               labels = c("Flow", "Trucks", "Speed", "Occupancy")))

p4b <- ggplot(quality_data, aes(x = metric_label, y = pct_predicted, fill = metric_label)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Data Quality: % Predicted vs Measured",
    subtitle = "Lower = better (more measured data)",
    x = NULL,
    y = "% Predicted (0-100%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

p4 <- gridExtra::grid.arrange(p4a, p4b, ncol = 2)

p1
p2
p3a
p3b
p3c
p4a
p4b

####################################
############ END ANALYSIS
####################################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #########################################
# ####################### CLEAN ROAD DATA
# #########################################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Use pointsId_with_roads (count_points matched to roads) instead of roads_with_pointsId
# # This ensures we have osm_id for each count_point
# points_with_road_attrs <- st_drop_geometry(pointsId_with_roads)
# points_with_road_attrs <- as.data.frame(points_with_road_attrs)
# 
# # Select essential columns
# essential_cols <- c("id", "highway", "lanes", "maxspeed", "oneway", "surface", 
#                    "osm_id", "name", "ref", "DEGRE")
# available_cols <- intersect(essential_cols, names(points_with_road_attrs))
# points_with_road_attrs <- points_with_road_attrs[, available_cols, drop = FALSE]
# 


#            format(length(unique(aggregated_measures_df$count_point_id)), big.mark=",")))

#            format(length(unique(points_with_road_attrs$id)), big.mark=",")))
# 
# # Clean names
# names(aggregated_measures_df) <- make.names(names(aggregated_measures_df))
# names(points_with_road_attrs) <- make.names(names(points_with_road_attrs))
# 
# # Merge with roads (all.x = TRUE keeps all traffic data, even if no road match)
# traffic_with_roads <- merge(aggregated_measures_df, points_with_road_attrs,
#                            by.x = "count_point_id", by.y = "id", all.x = TRUE)
# 

# 
# # Check for unmatched count_points (no osm_id)
# n_unmatched <- sum(is.na(traffic_with_roads$osm_id))
# if (n_unmatched > 0) {

#              format(n_unmatched, big.mark=","),
#              100 * n_unmatched / nrow(traffic_with_roads)))

# } else {

# }
# 

# 
# # =============================================================================
# # AGGREGATE BY OSM_ID (Sum flows from both directions for total segment traffic)
# # =============================================================================
# 

# 
# # Diagnostic: How many unique osm_ids and count_points?
# n_unique_osm <- length(unique(traffic_with_roads$osm_id[!is.na(traffic_with_roads$osm_id)]))
# n_unique_points <- length(unique(traffic_with_roads$count_point_id))
# 





#            format(n_unique_osm * length(unique(traffic_with_roads$period)), big.mark=",")))
# 
# # Count observations per osm_id to see how many count_points per segment
# directions_per_segment <- traffic_with_roads %>%
#   group_by(osm_id, period) %>%
#   summarise(n_count_points = n(), .groups = "drop")
# 


#            format(sum(directions_per_segment$n_count_points == 1), big.mark=","),
#            100 * mean(directions_per_segment$n_count_points == 1)))

#            format(sum(directions_per_segment$n_count_points == 2), big.mark=","),
#            100 * mean(directions_per_segment$n_count_points == 2)))
# if (any(directions_per_segment$n_count_points > 2)) {

#              format(sum(directions_per_segment$n_count_points > 2), big.mark=","),
#              100 * mean(directions_per_segment$n_count_points > 2)))
#   max_points <- max(directions_per_segment$n_count_points)

# }
# 

# 
# # Aggregate by osm_id + period (sum flows, average speed/occupancy)
# # Only keep columns that actually exist in traffic_with_roads
# available_attr_cols <- intersect(c("highway", "lanes", "maxspeed", "oneway", 
#                                    "surface", "name", "ref", "DEGRE"),
#                                  names(traffic_with_roads))
# 

#            paste(available_attr_cols, collapse=", ")))
# 
# # Build summarise arguments dynamically
# summarise_args <- list(
#   # SUM flows (both directions = total segment traffic)
#   # IMPORTANT: Return NA if no valid data, not 0!
#   aggregate_flow = expr(ifelse(sum(!is.na(aggregate_flow)) > 0,
#                                sum(aggregate_flow, na.rm = TRUE), NA_real_)),
#   aggregate_flow_trucks = expr(ifelse(sum(!is.na(aggregate_flow_trucks)) > 0,
#                                       sum(aggregate_flow_trucks, na.rm = TRUE), NA_real_)),
#   
#   # AVERAGE speed and occupancy (they shouldn't be summed)
#   aggregate_speed = expr(ifelse(sum(!is.na(aggregate_speed)) > 0,
#                                 mean(aggregate_speed, na.rm = TRUE), NA_real_)),
#   aggregate_occupancy = expr(ifelse(sum(!is.na(aggregate_occupancy)) > 0,
#                                     mean(aggregate_occupancy, na.rm = TRUE), NA_real_)),
#   
#   # AVERAGE quality indicators across directions (lower = better quality)
#   # 0% = all measured data, 100% = all predicted/imputed data
#   perc_flow_predicted = expr(ifelse(sum(!is.na(perc_flow_predicted)) > 0,
#                                     mean(perc_flow_predicted, na.rm = TRUE), NA_real_)),
#   perc_flow_trucks_predicted = expr(ifelse(sum(!is.na(perc_flow_trucks_predicted)) > 0,
#                                            mean(perc_flow_trucks_predicted, na.rm = TRUE), NA_real_)),
#   perc_occupancy_predicted = expr(ifelse(sum(!is.na(perc_occupancy_predicted)) > 0,
#                                          mean(perc_occupancy_predicted, na.rm = TRUE), NA_real_)),
#   perc_speed_predicted = expr(ifelse(sum(!is.na(perc_speed_predicted)) > 0,
#                                      mean(perc_speed_predicted, na.rm = TRUE), NA_real_)),
#   
#   # Quality metrics
#   n_directions_measured = expr(n()),  # How many directions have data
#   n_hours_with_data = expr(sum(n_hours_with_data, na.rm = TRUE)),
#   n_total_observations = expr(sum(n_total_observations, na.rm = TRUE))
# )
# 
# # Add road attributes that exist
# for (col in available_attr_cols) {
#   summarise_args[[col]] <- expr(first(!!sym(col)))
# }
# 
# traffic_by_segment <- traffic_with_roads %>%
#   filter(!is.na(osm_id)) %>%
#   group_by(osm_id, period) %>%
#   summarise(!!!summarise_args, .groups = "drop")
# 
# n_before <- nrow(traffic_with_roads)
# n_after <- nrow(traffic_by_segment)
# reduction_pct <- 100 * (1 - n_after/n_before)
# 
# # Calculate unique segments and coverage per period
# n_unique_segments <- length(unique(traffic_by_segment$osm_id))
# segments_per_period <- traffic_by_segment %>%
#   group_by(period) %>%
#   summarise(n_segments = n(), .groups = "drop") %>%
#   arrange(period)
# 


#            format(n_before, big.mark=",")))

#            format(n_after, big.mark=",")))

# 




#            format(n_unique_segments * length(unique(traffic_by_segment$period)), big.mark=",")))

#            format(n_after, big.mark=","),
#            100 * n_after / (n_unique_segments * length(unique(traffic_by_segment$period)))))
# 

# for (i in 1:min(5, nrow(segments_per_period))) {
#   p <- segments_per_period$period[i]
#   n <- segments_per_period$n_segments[i]
#   pct <- 100 * n / n_unique_segments

#              p, format(n, big.mark=","), pct))
# }
# if (nrow(segments_per_period) > 5) {

# }

# 


#            format(n_before, big.mark=",")))

#            format(n_after, big.mark=",")))

# 



# 
# # Replace traffic_with_roads with aggregated version
# traffic_with_roads <- as.data.frame(traffic_by_segment)
# 
# # =============================================================================
# # FINAL DATA STRUCTURE DOCUMENTATION
# # =============================================================================
# 




# 

# 















# 




# 

# 



# 





# 






# 







# 









# 








# 



# 





# 























# 

# 
# # Sample data structure
# sample_data <- traffic_with_roads %>% 
#   filter(!is.na(osm_id)) %>%
#   head(3) %>%
#   select(osm_id, period, aggregate_flow, aggregate_speed, truck_pct, 
#          flow_D, ratio_flow, highway, lanes, DEGRE)
# 

# print(sample_data)

# 
# 
# # Save
# saveRDS(traffic_with_roads, "rdsFiles/traffic_with_roads.rds")
# assign("traffic_with_roads", traffic_with_roads, envir = .GlobalEnv)
# 
# rm(hourly_aggregated, aggregated_measures_period, aggregated_measures_hourly,
#    aggregated_measures_df, aggregated_measures, roads_avatar_data_clean, 
#    directions_per_segment, traffic_by_segment)
# 
