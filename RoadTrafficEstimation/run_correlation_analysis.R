# ==============================================================================
# Correlation analysis: dB error vs speed, dB error vs flow
# ==============================================================================

setwd("/home/aumond/Documents/github/OpenDataNoiseMap/RoadTrafficEstimation")
source("bootstrap.R")
source("config_pipeline.R")

pipeline_message(text = "Correlation analysis: error vs speed/flow",
                 level = 0, progress = "start", process = "plot")

# --- Load artifacts ---
models_list <- readRDS(CONFIG$XGB_MODELS_WITH_RATIOS_FILEPATH)
feature_info <- readRDS(CONFIG$XGB_RATIO_FEATURE_INFO_FILEPATH)
road_feature_formula <- feature_info$road_feature_formula
all_periods <- feature_info$all_periods
training_data <- readRDS(CONFIG$TRAINING_RDS_DATA_FILEPATH)

if (!"ratio_speed_to_osm" %in% names(training_data)) {
  training_data$ratio_speed_to_osm <- ifelse(
    !is.na(training_data$aggregate_speed) & training_data$aggregate_speed >= 0 &
      !is.na(training_data$speed) & training_data$speed > 0,
    training_data$aggregate_speed / training_data$speed, NA_real_)
}

# --- Reconstruct sensor split ---
d_data_for_split <- training_data %>% filter(period == "D")
all_d_sensors <- unique(d_data_for_split$count_point_id)
set.seed(42)
n_train <- max(1, floor(0.8 * length(all_d_sensors)))
shared_base_train_sensors <- sample(all_d_sensors, size = n_train)
shared_base_test_sensors <- setdiff(all_d_sensors, shared_base_train_sensors)

pipeline_message(text = sprintf("Test sensors: %d", length(shared_base_test_sensors)),
                 process = "info")

# --- Build emission_test ---
align_matrix_to_model <- function(mat, model) {
  expected <- model$feature_names
  if (is.null(expected)) return(mat)
  cur <- colnames(mat)
  missing <- setdiff(expected, cur)
  if (length(missing) > 0) {
    zero_mat <- Matrix::Matrix(0, nrow = nrow(mat), ncol = length(missing), sparse = TRUE)
    colnames(zero_mat) <- missing
    mat <- cbind(mat, zero_mat)
  }
  mat[, expected, drop = FALSE]
}

d_data <- training_data %>%
  filter(period == "D",
         count_point_id %in% shared_base_test_sensors,
         !is.na(aggregate_flow), aggregate_flow >= 1)

d_matrix <- Matrix::sparse.model.matrix(object = road_feature_formula, data = d_data)
if (nrow(d_matrix) != nrow(d_data)) {
  kept_rows <- as.integer(rownames(d_matrix))
  d_data <- d_data[kept_rows, ]
}

pred_flow_D_log <- predict(models_list[["flow_D"]]$model,
                           align_matrix_to_model(d_matrix, models_list[["flow_D"]]$model))
pred_flow_D <- 10^pred_flow_D_log
pred_truck_D <- predict(models_list[["truck_pct_D"]]$model,
                        align_matrix_to_model(d_matrix, models_list[["truck_pct_D"]]$model))
pred_speed_ratio <- predict(models_list[["speed_D"]]$model,
                            align_matrix_to_model(d_matrix, models_list[["speed_D"]]$model))

osm_speed_raw <- suppressWarnings(as.numeric(d_data$speed))
osm_speed_missing <- is.na(osm_speed_raw) | osm_speed_raw < 5
speed_base <- osm_speed_raw
speed_base[osm_speed_missing] <- CONFIG$DEFAULT_VEHICLE_SPEED
pred_speed_D <- pmax(5, pred_speed_ratio * speed_base)

osm_speed_val <- osm_speed_raw
osm_speed_val[osm_speed_missing] <- pred_speed_D[osm_speed_missing]

actual_speed_D <- ifelse(!is.na(d_data$aggregate_speed) & d_data$aggregate_speed >= 5,
                         d_data$aggregate_speed, speed_base)
actual_truck_D <- ifelse(!is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
                         d_data$truck_pct, 0)

emission_D <- data.frame(
  osm_id = d_data$osm_id, count_point_id = d_data$count_point_id,
  period = "D", highway = as.character(d_data$highway),
  DEGRE = if ("DEGRE" %in% names(d_data)) as.character(d_data$DEGRE) else NA_character_,
  pred_flow = as.numeric(pred_flow_D), actual_flow = as.numeric(d_data$aggregate_flow),
  pred_truck_pct = as.numeric(pred_truck_D), actual_truck_pct = as.numeric(actual_truck_D),
  pred_speed = as.numeric(pred_speed_D), actual_speed = as.numeric(actual_speed_D),
  osm_speed = as.numeric(osm_speed_val),
  has_measured_speed = !is.na(d_data$aggregate_speed) & d_data$aggregate_speed >= 5,
  has_measured_truck = !is.na(d_data$truck_pct) & d_data$truck_pct >= 0,
  stringsAsFactors = FALSE)
emission_D <- emission_D[!is.na(emission_D$actual_speed) & emission_D$actual_speed >= 5, ]

emission_parts <- list(emission_D)
test_sensor_ids <- unique(emission_D$count_point_id)
non_d_periods <- setdiff(all_periods, "D")

for (p in non_d_periods) {
  fk <- paste0("ratio_flow_", p)
  tk <- paste0("ratio_truck_pct_", p)
  sk <- paste0("ratio_speed_", p)
  if (is.null(models_list[[fk]]$model)) next

  rf <- predict(models_list[[fk]]$model, align_matrix_to_model(d_matrix, models_list[[fk]]$model))
  rt <- if (!is.null(models_list[[tk]]$model))
    predict(models_list[[tk]]$model, align_matrix_to_model(d_matrix, models_list[[tk]]$model))
  else rep(1, nrow(d_matrix))
  rs <- if (!is.null(models_list[[sk]]$model))
    predict(models_list[[sk]]$model, align_matrix_to_model(d_matrix, models_list[[sk]]$model))
  else rep(1, nrow(d_matrix))

  pf <- pmax(0, pred_flow_D * rf)
  pt <- pmin(100, pmax(0, pred_truck_D * rt))
  ps <- pmax(5, pred_speed_D * rs)

  p_data <- training_data %>% filter(period == p, count_point_id %in% test_sensor_ids)
  if (nrow(p_data) == 0) next
  mi <- match(p_data$count_point_id, d_data$count_point_id)
  vm <- !is.na(mi); p_data <- p_data[vm, ]; rid <- mi[vm]
  if (nrow(p_data) == 0) next

  af <- ifelse(!is.na(p_data$aggregate_flow) & p_data$aggregate_flow >= 0, p_data$aggregate_flow, NA_real_)
  at <- ifelse(!is.na(p_data$truck_pct) & p_data$truck_pct >= 0, p_data$truck_pct, 0)
  as_ <- ifelse(!is.na(p_data$aggregate_speed) & p_data$aggregate_speed >= 5,
                p_data$aggregate_speed, speed_base[rid])

  ep <- data.frame(
    osm_id = d_data$osm_id[rid], count_point_id = p_data$count_point_id,
    period = p, highway = as.character(d_data$highway[rid]),
    DEGRE = if ("DEGRE" %in% names(d_data)) as.character(d_data$DEGRE[rid]) else NA_character_,
    pred_flow = as.numeric(pf[rid]), actual_flow = as.numeric(af),
    pred_truck_pct = as.numeric(pt[rid]), actual_truck_pct = as.numeric(at),
    pred_speed = as.numeric(ps[rid]), actual_speed = as.numeric(as_),
    osm_speed = as.numeric(osm_speed_val[rid]),
    has_measured_speed = !is.na(p_data$aggregate_speed) & p_data$aggregate_speed >= 5,
    has_measured_truck = !is.na(p_data$truck_pct) & p_data$truck_pct >= 0,
    stringsAsFactors = FALSE)
  ep <- ep[!is.na(ep$actual_speed) & ep$actual_speed >= 5 & !is.na(ep$actual_flow), ]
  if (nrow(ep) > 0) emission_parts <- c(emission_parts, list(ep))
}

emission_test <- do.call(rbind, emission_parts)
rm(emission_parts, emission_D)

pipeline_message(text = sprintf("Emission test: %s rows", fmt(nrow(emission_test))),
                 process = "info")

# --- Compute CNOSSOS emissions ---
emission_test$pred_db <- compute_emission_cnossos(
  flow = emission_test$pred_flow,
  truck_pct = emission_test$pred_truck_pct,
  speed = emission_test$pred_speed)
emission_test$actual_db <- compute_emission_cnossos(
  flow = emission_test$actual_flow,
  truck_pct = emission_test$actual_truck_pct,
  speed = emission_test$actual_speed)
emission_test$db_error <- emission_test$pred_db - emission_test$actual_db
emission_test$abs_db_error <- abs(emission_test$db_error)

valid <- is.finite(emission_test$db_error)
emission_test <- emission_test[valid, ]

pipeline_message(text = sprintf("Valid rows: %s", fmt(nrow(emission_test))),
                 process = "info")

# ==============================================================================
# CORRELATION ANALYSIS
# ==============================================================================

if (!dir.exists(CONFIG$FIGS_DIR)) {
  dir.create(CONFIG$FIGS_DIR, recursive = TRUE, showWarnings = FALSE)
}

pdf_path <- file.path(CONFIG$FIGS_DIR, "07_correlation_error_vs_speed_flow.pdf")
grDevices::pdf(file = pdf_path, width = 14, height = 10)
old_par <- par(no.readonly = TRUE)

# --- Correlations numériques ---
cor_err_actual_speed <- cor(emission_test$db_error, emission_test$actual_speed,
                            use = "complete.obs")
cor_abserr_actual_speed <- cor(emission_test$abs_db_error, emission_test$actual_speed,
                               use = "complete.obs")
cor_err_actual_flow <- cor(emission_test$db_error, emission_test$actual_flow,
                           use = "complete.obs")
cor_abserr_actual_flow <- cor(emission_test$abs_db_error, emission_test$actual_flow,
                              use = "complete.obs")
cor_err_pred_speed <- cor(emission_test$db_error, emission_test$pred_speed,
                          use = "complete.obs")
cor_err_pred_flow <- cor(emission_test$db_error, emission_test$pred_flow,
                         use = "complete.obs")
cor_abserr_log_flow <- cor(emission_test$abs_db_error,
                           log10(pmax(1, emission_test$actual_flow)),
                           use = "complete.obs")
cor_err_log_flow <- cor(emission_test$db_error,
                        log10(pmax(1, emission_test$actual_flow)),
                        use = "complete.obs")

pipeline_message(text = sprintf(
  "Correlations — Error vs actual_speed: r=%.3f | |Error| vs actual_speed: r=%.3f",
  cor_err_actual_speed, cor_abserr_actual_speed), process = "info")
pipeline_message(text = sprintf(
  "Correlations — Error vs actual_flow: r=%.3f | |Error| vs actual_flow: r=%.3f",
  cor_err_actual_flow, cor_abserr_actual_flow), process = "info")
pipeline_message(text = sprintf(
  "Correlations — Error vs log10(actual_flow): r=%.3f | |Error| vs log10(actual_flow): r=%.3f",
  cor_err_log_flow, cor_abserr_log_flow), process = "info")

# ============================================================================
# PAGE 1: Erreur dB vs Vitesse observée
# ============================================================================
par(mfrow = c(2, 3), mar = c(4.5, 4.5, 3.5, 1))

# 1a: Scatter error vs actual speed
plot(emission_test$actual_speed, emission_test$db_error,
     pch = 16, cex = 0.2, col = rgb(0.2, 0.4, 0.8, 0.08),
     xlab = "Vitesse observée (km/h)",
     ylab = "Erreur dB (Prédit - Observé)",
     main = "Erreur dB vs Vitesse observée", cex.main = 1.0)
abline(h = 0, col = "red", lwd = 2, lty = 2)
lo_speed <- lowess(emission_test$actual_speed, emission_test$db_error, f = 0.3)
lines(lo_speed, col = "orange", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_err_actual_speed),
       cex = 0.9, bty = "n", text.font = 2)

# 1b: |Error| vs actual speed
plot(emission_test$actual_speed, emission_test$abs_db_error,
     pch = 16, cex = 0.2, col = rgb(0.8, 0.2, 0.2, 0.08),
     xlab = "Vitesse observée (km/h)",
     ylab = "|Erreur dB|",
     main = "|Erreur dB| vs Vitesse observée", cex.main = 1.0)
lo_abs_speed <- lowess(emission_test$actual_speed, emission_test$abs_db_error, f = 0.3)
lines(lo_abs_speed, col = "darkred", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_abserr_actual_speed),
       cex = 0.9, bty = "n", text.font = 2)

# 1c: Boxplot error by speed bins
speed_bins <- cut(emission_test$actual_speed,
                  breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 110, 130, Inf),
                  labels = c("≤20", "20-30", "30-40", "40-50", "50-60",
                             "60-70", "70-80", "80-90", "90-110", "110-130", ">130"),
                  right = TRUE)
dt_speed <- data.table(db_error = emission_test$db_error,
                       abs_error = emission_test$abs_db_error,
                       bin = speed_bins)
boxplot(db_error ~ bin, data = dt_speed, outline = FALSE,
        col = colorRampPalette(c("#4393C3", "#D6604D"))(nlevels(speed_bins)),
        las = 2, cex.axis = 0.75,
        main = "Erreur dB par classe de vitesse",
        ylab = "Erreur dB", xlab = "", cex.main = 1.0)
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# 1d: MAE + Biais par classe de vitesse
speed_stats <- dt_speed[, .(n = .N,
                            bias = mean(db_error, na.rm = TRUE),
                            mae = mean(abs_error, na.rm = TRUE)),
                        by = bin][order(bin)]
speed_stats <- speed_stats[!is.na(bin)]

bp_x <- barplot(speed_stats$mae, names.arg = as.character(speed_stats$bin),
                col = "#4393C3", border = NA, las = 2, cex.names = 0.7,
                main = "MAE par classe de vitesse",
                ylab = "MAE (dB)", cex.main = 1.0)
text(bp_x, speed_stats$mae, labels = sprintf("%.1f", speed_stats$mae),
     pos = 3, cex = 0.65, font = 2)
mtext(paste0("n=", speed_stats$n, collapse = "  "),
      side = 1, line = 3.5, cex = 0.55)

# 1e: Biais par classe de vitesse
bias_cols <- ifelse(speed_stats$bias >= 0, "#D6604D", "#4393C3")
bp_b <- barplot(speed_stats$bias, names.arg = as.character(speed_stats$bin),
                col = bias_cols, border = NA, las = 2, cex.names = 0.7,
                main = "Biais par classe de vitesse",
                ylab = "Biais (dB)", cex.main = 1.0)
abline(h = 0, col = "red", lty = 2)
text(bp_b, speed_stats$bias,
     labels = sprintf("%+.1f", speed_stats$bias),
     pos = ifelse(speed_stats$bias >= 0, 3, 1), cex = 0.65, font = 2)

# 1f: Effectif par classe
barplot(speed_stats$n, names.arg = as.character(speed_stats$bin),
        col = "grey70", border = NA, las = 2, cex.names = 0.7,
        main = "Effectif par classe de vitesse",
        ylab = "N", cex.main = 1.0)

mtext("Corrélation entre erreur dB et vitesse observée",
      side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

# ============================================================================
# PAGE 2: Erreur dB vs Flux observé
# ============================================================================
par(mfrow = c(2, 3), mar = c(4.5, 4.5, 3.5, 1))

# 2a: Scatter error vs actual flow (log scale)
log_flow <- log10(pmax(1, emission_test$actual_flow))
plot(log_flow, emission_test$db_error,
     pch = 16, cex = 0.2, col = rgb(0.2, 0.6, 0.3, 0.08),
     xlab = "log10(Flux observé) (véh/h)",
     ylab = "Erreur dB (Prédit - Observé)",
     main = "Erreur dB vs log10(Flux observé)", cex.main = 1.0)
abline(h = 0, col = "red", lwd = 2, lty = 2)
lo_flow <- lowess(log_flow, emission_test$db_error, f = 0.3)
lines(lo_flow, col = "darkgreen", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_err_log_flow),
       cex = 0.9, bty = "n", text.font = 2)

# 2b: |Error| vs actual flow (log scale)
plot(log_flow, emission_test$abs_db_error,
     pch = 16, cex = 0.2, col = rgb(0.6, 0.3, 0.1, 0.08),
     xlab = "log10(Flux observé) (véh/h)",
     ylab = "|Erreur dB|",
     main = "|Erreur dB| vs log10(Flux observé)", cex.main = 1.0)
lo_abs_flow <- lowess(log_flow, emission_test$abs_db_error, f = 0.3)
lines(lo_abs_flow, col = "brown", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_abserr_log_flow),
       cex = 0.9, bty = "n", text.font = 2)

# 2c: Boxplot error by flow bins
flow_bins <- cut(emission_test$actual_flow,
                 breaks = c(0, 5, 20, 50, 100, 200, 500, 1000, 2000, 5000, Inf),
                 labels = c("≤5", "5-20", "20-50", "50-100", "100-200",
                            "200-500", "500-1k", "1k-2k", "2k-5k", ">5k"),
                 right = TRUE)
dt_flow <- data.table(db_error = emission_test$db_error,
                      abs_error = emission_test$abs_db_error,
                      bin = flow_bins)
boxplot(db_error ~ bin, data = dt_flow, outline = FALSE,
        col = colorRampPalette(c("#1a9850", "#d73027"))(nlevels(flow_bins)),
        las = 2, cex.axis = 0.7,
        main = "Erreur dB par classe de flux",
        ylab = "Erreur dB", xlab = "", cex.main = 1.0)
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# 2d: MAE par classe de flux
flow_stats <- dt_flow[, .(n = .N,
                          bias = mean(db_error, na.rm = TRUE),
                          mae = mean(abs_error, na.rm = TRUE)),
                      by = bin][order(bin)]
flow_stats <- flow_stats[!is.na(bin)]

bp_f <- barplot(flow_stats$mae, names.arg = as.character(flow_stats$bin),
                col = "#1a9850", border = NA, las = 2, cex.names = 0.7,
                main = "MAE par classe de flux",
                ylab = "MAE (dB)", cex.main = 1.0)
text(bp_f, flow_stats$mae, labels = sprintf("%.1f", flow_stats$mae),
     pos = 3, cex = 0.65, font = 2)

# 2e: Biais par classe de flux
bias_f_cols <- ifelse(flow_stats$bias >= 0, "#D6604D", "#4393C3")
bp_fb <- barplot(flow_stats$bias, names.arg = as.character(flow_stats$bin),
                 col = bias_f_cols, border = NA, las = 2, cex.names = 0.7,
                 main = "Biais par classe de flux",
                 ylab = "Biais (dB)", cex.main = 1.0)
abline(h = 0, col = "red", lty = 2)
text(bp_fb, flow_stats$bias,
     labels = sprintf("%+.1f", flow_stats$bias),
     pos = ifelse(flow_stats$bias >= 0, 3, 1), cex = 0.65, font = 2)

# 2f: Effectif
barplot(flow_stats$n, names.arg = as.character(flow_stats$bin),
        col = "grey70", border = NA, las = 2, cex.names = 0.7,
        main = "Effectif par classe de flux",
        ylab = "N", cex.main = 1.0)

mtext("Corrélation entre erreur dB et flux observé",
      side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

# ============================================================================
# PAGE 3: Erreur dB vs Vitesse prédite et Flux prédit
# ============================================================================
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1))

# 3a: Error vs pred speed
plot(emission_test$pred_speed, emission_test$db_error,
     pch = 16, cex = 0.2, col = rgb(0.5, 0.2, 0.7, 0.08),
     xlab = "Vitesse prédite XGBoost (km/h)",
     ylab = "Erreur dB",
     main = "Erreur dB vs Vitesse prédite", cex.main = 1.0)
abline(h = 0, col = "red", lwd = 2, lty = 2)
lo_ps <- lowess(emission_test$pred_speed, emission_test$db_error, f = 0.3)
lines(lo_ps, col = "purple", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_err_pred_speed),
       cex = 0.9, bty = "n", text.font = 2)

# 3b: Error vs pred flow (log)
log_pf <- log10(pmax(1, emission_test$pred_flow))
cor_err_log_pred_flow <- cor(emission_test$db_error, log_pf, use = "complete.obs")
plot(log_pf, emission_test$db_error,
     pch = 16, cex = 0.2, col = rgb(0.7, 0.5, 0.1, 0.08),
     xlab = "log10(Flux prédit) (véh/h)",
     ylab = "Erreur dB",
     main = "Erreur dB vs log10(Flux prédit)", cex.main = 1.0)
abline(h = 0, col = "red", lwd = 2, lty = 2)
lo_pf <- lowess(log_pf, emission_test$db_error, f = 0.3)
lines(lo_pf, col = "darkorange", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_err_log_pred_flow),
       cex = 0.9, bty = "n", text.font = 2)

# 3c: Error speed (pred - actual) vs dB error
speed_error <- emission_test$pred_speed - emission_test$actual_speed
cor_speed_err_vs_db <- cor(speed_error, emission_test$db_error, use = "complete.obs")
plot(speed_error, emission_test$db_error,
     pch = 16, cex = 0.2, col = rgb(0.2, 0.2, 0.8, 0.08),
     xlab = "Erreur vitesse (préd - obs, km/h)",
     ylab = "Erreur dB",
     main = "Erreur dB vs Erreur de vitesse", cex.main = 1.0)
abline(h = 0, col = "red", lty = 2); abline(v = 0, col = "red", lty = 2)
lo_se <- lowess(speed_error, emission_test$db_error, f = 0.3)
lines(lo_se, col = "blue", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_speed_err_vs_db),
       cex = 0.9, bty = "n", text.font = 2)

# 3d: Error flow (log ratio) vs dB error
flow_log_ratio <- log10(pmax(0.01, emission_test$pred_flow) /
                        pmax(0.01, emission_test$actual_flow))
cor_flow_ratio_vs_db <- cor(flow_log_ratio, emission_test$db_error, use = "complete.obs")
plot(flow_log_ratio, emission_test$db_error,
     pch = 16, cex = 0.2, col = rgb(0.1, 0.6, 0.3, 0.08),
     xlab = "log10(Flux prédit / Flux observé)",
     ylab = "Erreur dB",
     main = "Erreur dB vs Ratio de flux (log)", cex.main = 1.0,
     xlim = c(-2, 2))
abline(h = 0, col = "red", lty = 2); abline(v = 0, col = "red", lty = 2)
lo_fr <- lowess(flow_log_ratio[abs(flow_log_ratio) < 2],
                emission_test$db_error[abs(flow_log_ratio) < 2], f = 0.3)
lines(lo_fr, col = "darkgreen", lwd = 3)
legend("topright", legend = sprintf("r = %.3f", cor_flow_ratio_vs_db),
       cex = 0.9, bty = "n", text.font = 2)

mtext("Corrélation erreur dB vs erreurs de prédiction (vitesse et flux)",
      side = 3, outer = FALSE, line = -1.5, cex = 0.7, font = 3)

# ============================================================================
# PAGE 4: Synthèse textuelle
# ============================================================================
par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
plot.new()
title(main = "Synthèse des corrélations Erreur dB vs Vitesse / Flux", cex.main = 1.2)

synth <- c(
  "═══ Corrélations linéaires (Pearson) ═══",
  "",
  "  Variable                          vs Erreur dB    vs |Erreur dB|",
  sprintf("  Vitesse observée (km/h)           r = %+.3f        r = %+.3f",
          cor_err_actual_speed, cor_abserr_actual_speed),
  sprintf("  Vitesse prédite (km/h)            r = %+.3f        r = %+.3f",
          cor_err_pred_speed,
          cor(emission_test$abs_db_error, emission_test$pred_speed, use = "complete.obs")),
  sprintf("  Flux observé (véh/h)              r = %+.3f        r = %+.3f",
          cor_err_actual_flow, cor_abserr_actual_flow),
  sprintf("  log10(Flux observé)               r = %+.3f        r = %+.3f",
          cor_err_log_flow, cor_abserr_log_flow),
  sprintf("  Flux prédit (véh/h)               r = %+.3f",
          cor_err_pred_flow),
  "",
  "═══ Corrélations erreur-erreur ═══",
  "",
  sprintf("  Erreur vitesse (préd-obs) vs Err dB  : r = %+.3f", cor_speed_err_vs_db),
  sprintf("  log10(flux préd/obs) vs Err dB       : r = %+.3f", cor_flow_ratio_vs_db),
  "",
  "═══ Interprétation ═══",
  ""
)

# Interpret
if (abs(cor_err_actual_speed) > abs(cor_err_log_flow)) {
  synth <- c(synth,
    sprintf("  La vitesse est PLUS corrélée à l'erreur (|r|=%.3f) que le flux (|r|=%.3f).",
            abs(cor_err_actual_speed), abs(cor_err_log_flow)))
} else {
  synth <- c(synth,
    sprintf("  Le flux est PLUS corrélé à l'erreur (|r|=%.3f) que la vitesse (|r|=%.3f).",
            abs(cor_err_log_flow), abs(cor_err_actual_speed)))
}

if (cor_err_actual_speed < -0.05) {
  synth <- c(synth,
    "  -> Vitesse plus haute = erreur plus négative (sous-estimation croissante).")
} else if (cor_err_actual_speed > 0.05) {
  synth <- c(synth,
    "  -> Vitesse plus haute = erreur plus positive (surestimation croissante).")
}

if (cor_err_log_flow < -0.05) {
  synth <- c(synth,
    "  -> Flux plus élevé = erreur plus négative (sous-estimation croissante).")
} else if (cor_err_log_flow > 0.05) {
  synth <- c(synth,
    "  -> Flux plus élevé = erreur plus positive (surestimation croissante).")
}

synth <- c(synth, "",
  sprintf("  Erreur de prédiction de vitesse vs erreur dB : r = %+.3f", cor_speed_err_vs_db),
  sprintf("  Erreur de prédiction de flux vs erreur dB    : r = %+.3f", cor_flow_ratio_vs_db),
  "",
  if (abs(cor_speed_err_vs_db) > abs(cor_flow_ratio_vs_db)) {
    "  -> L'erreur de vitesse explique PLUS l'erreur dB que l'erreur de flux."
  } else {
    "  -> L'erreur de flux explique PLUS l'erreur dB que l'erreur de vitesse."
  },
  "",
  "  Note : CNOSSOS-EU est Lw ~ 10*log10(flow) + f(speed), donc",
  "  l'erreur dB est par construction ~10*log10(pred_flow/obs_flow).",
  "  La corrélation avec le ratio de flux est attendue et mécanique."
)

text(0.02, seq(0.95, 0.95 - 0.03 * (length(synth) - 1),
     length.out = length(synth)),
     labels = synth, adj = c(0, 0.5), cex = 0.72, family = "mono")

par(old_par)
grDevices::dev.off()

pipeline_message(
  text = sprintf("Correlation report saved to %s", rel_path(pdf_path)),
  process = "save")

pipeline_message(text = "Correlation analysis completed",
                 level = 0, progress = "end", process = "valid")
