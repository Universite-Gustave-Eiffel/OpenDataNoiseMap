#' @title Download a file from a remote URL
#' @description Downloads a file from a given URL and saves it to a local target 
#'              path. Optionally supports authenticated requests using a Bearer 
#'              token.
#'              This function is designed for use in automated pipelines and HPC 
#'              environments, with explicit timeout handling and strict HTTP 
#'              status checking.
#' @param url Character string. Remote URL of the file to download.
#' @param target Character string. Local file path where the downloaded content 
#'               will be written.
#' @param use_auth Logical. If `TRUE`, the request includes an 
#'                 `Authorization: Bearer` header using the `AVATAR_API_TOKEN` 
#'                 environment variable.
#' @return Invisibly returns `NULL`. An error is raised if the download fails or 
#'         the HTTP status code indicates a failure.
#' @details The file is downloaded using `httr::GET()` and written in binary 
#'          mode. If authentication is enabled, the function expects a valid API 
#'          token to be available in the `AVATAR_API_TOKEN` environment 
#'          variable.
#' @examples
#' \dontrun{
#' download_file(
#'   url = "https://example.com/data.csv",
#'   target = "data/data.csv")
#' }
#' @export
download_file <- function(
    url, 
    target, 
    use_auth = FALSE,
    as = "raw",        # "raw" (default) or "text"
    timeout = 30) {
  if (use_auth && nchar(AVATAR_API_TOKEN) > 0) {
    # Use Authorization header (Bearer token)
    response <- httr::GET(
      url = url, 
      httr::add_headers(
        Authorization = paste("Bearer", AVATAR_API_TOKEN)), 
      httr::timeout(timeout))
  } else {
    response <- httr::GET(
      url = url, 
      httr::timeout(timeout))
  }
  # Converts http errors to R errors or warnings
  httr::stop_for_status(x = response)
  if (as == "raw") {
    writeBin(
      object = httr::content(response, as = "raw"), 
      con = target)
    message("\t\t ✓ Data downloaded successfully in raw format! \n")
  } else if (as == "text") {
    txt <- httr::content(
      response, 
      as = "text", 
      encoding = "UTF-8")
    writeLines(text = txt, 
               con = target)
    message("\t\t ✓ Data downloaded successfully in text format! \n")
  } else {
    stop("\t\t ⛔ Unsupported content type: .", as)
  }
}
#' 
#' @title Download a file with automatic retry logic and API throttling
#' @description Attempts to download a file multiple times before failing.
#'              Adds a delay between requests to comply with API rate limits.
#' @param url Character string. Remote URL of the file to download.
#' @param target Character string. Local file path where the downloaded content 
#'               will be written.
#' @param max_retries Integer. Maximum number of download attempts.
#' @param use_auth Logical. Whether to use authentication.
#' @param throttle_delay Numeric. Delay (seconds) between requests. 
#'                       Default = 15 sec (safe for AVATAR API).
#' @return Logical. TRUE if download succeeds.
#' @export
download_with_retry <- function(
    url,
    target,
    max_retries = 3,
    use_auth = FALSE,
    throttle_delay = 15
) {
  for (i in 1:max_retries) {
    tryCatch({
      # ---------------------- #
      # Download attempt       #
      # ---------------------- #
      download_file(
        url = url,
        target = target,
        use_auth = use_auth
      )
      # ---------------------- #
      # Success check          #
      # ---------------------- #
      if (file.exists(target) &&
          file.size(target) > 0) {
        # Throttle after success
        Sys.sleep(time = throttle_delay)
        return(TRUE)
      }
    }, error = function(e) {
      msg <- e$message
      # ---------------------- #
      # HTTP 429 handling      #
      # ---------------------- #
      if (grepl(pattern = "429", x = msg) ||
          grepl(pattern = "Too Many Requests", x = msg, ignore.case = TRUE)) {
        wait_time <- 60
        pipeline_message(
          sprintf("Rate limit reached. Waiting %s sec", wait_time), 
          process = "warning")
        Sys.sleep(time = wait_time)
      } else {
        
        # Standard retry wait
        Sys.sleep(time = throttle_delay)
      }
      # ---------------------- #
      # Stop if last retry     #
      # ---------------------- #
      if (i == max_retries) {
        pipeline_message(
          sprintf("Download failed after retries: ", msg,
                  "\n\t\t ⚠️ If necessary, make sure required token is set in ",
                  "~/.Renviron or ~/.env \n"), 
          process = "stop")
      }
    })
  }
}
#' #' @title Download a file with automatic retry logic
#' #' @description Attempts to download a file multiple times before failing. This 
#' #'              is particularly useful for unstable network connections or 
#' #'              remote APIs with intermittent availability.
#' #'              The function retries the download after a fixed delay if an 
#' #'              error occurs or if the resulting file is empty.
#' #' @param url Character string. Remote URL of the file to download.
#' #' @param target Character string. Local file path where the downloaded content 
#' #'               will be written.
#' #' @param max_retries Integer. Maximum number of download attempts before 
#' #'                    stopping with an error. Default is `3`.
#' #' @param use_auth Logical. If `TRUE`, authentication is enabled and passed to 
#' #'                 `download_file()`.
#' #' @return Logical. Returns `TRUE` if the file was successfully downloaded and 
#' #'         written. An error is raised if all retry attempts fail.
#' #' @details The function wraps `download_file()` in a retry loop using 
#' #'          `tryCatch()`. Between retries, the process pauses for a short fixed 
#' #'          delay to reduce the likelihood of repeated transient failures.
#' #' @examples
#' #' \dontrun{
#' #' download_with_retry(
#' #'   url = "https://example.com/large_file.gpkg",
#' #'   target = "data/large_file.gpkg",
#' #'   max_retries = 5)
#' #' }
#' #' @export
#' download_with_retry <- function(
#'     url, 
#'     target, 
#'     max_retries = 3, 
#'     use_auth = FALSE) {
#'   for (i in 1:max_retries) {
#'     tryCatch({
#'       download_file(url = url, 
#'                     target = target, 
#'                     use_auth = use_auth)
#'       if (file.exists(target) && file.size(target) > 0) {
#'         return(TRUE)
#'       }
#'     }, error = function(e) {
#'       if (i == max_retries) {
#'         stop("\t\t ⛔ Download failed after retries: ", e$message, 
#'              "\n\t\t ⚠️ If necessary, make sure required token is set in 
#'              ~/.Renviron or ~/.env \n")
#'       }
#'       Sys.sleep(5)
#'     })
#'   }
#' }
#' 
#' @title Validate a downloaded data chunk
#' @description Checks whether an existing chunk file is valid, non-empty, 
#'              readable, and not corrupted. This function is mainly used in 
#'              data download or processing pipelines to detect incomplete, 
#'              empty, or corrupted chunk files before deciding whether they 
#'              should be re-downloaded or recomputed.
#' @param file_path Character string. Path to the chunk file to validate.
#' @param expected_points Integer. Expected number of data points in the chunk. 
#'                        Currently not enforced but kept for future consistency 
#'                        checks.
#' @return A list with two elements:
#' \describe{
#'   \item{valid}{Logical. `TRUE` if the chunk is considered valid.}
#'   \item{reason}{Character string describing the validation result (e.g. 
#'                 `"ok"`, `"missing"`, `"empty"`, `"corrupted"`).}
#' }
#' @details The validation performs the following checks: 
#'          \enumerate{
#'            \item File existence
#'            \item File size greater than zero
#'            \item File contains more than a header line
#'            \item File can be parsed as a CSV file
#'          }
#' @examples
#' \dontrun{
#' res <- validate_chunk(
#'   file_path = "data/chunk_001.csv",
#'   expected_points = 1000)
#' if (!res$valid) {
#'   message("Invalid chunk: ", res$reason)
#' }
#' }
#' @export
validate_chunk <- function(
    file_path, 
    expected_points) {
  if (!file.exists(file_path)){
    return(list(valid = FALSE, 
                reason = "missing"))
  }
  # Check file size
  file_size <- file.size(file_path)
  if (file_size == 0){
    return(list(valid = FALSE, 
                reason = "empty (0 bytes)"))
  }
  # Count lines (1 line = header only, empty chunk)
  lines <- tryCatch({
    length(x = readLines(con = file_path, warn = FALSE))
  }, error = function(e){0})
  if (lines <= 1){
    return(list(valid = FALSE, 
                reason = "empty (header only)"))
  }
  # Check if file is corrupted (can't be parsed)
  data_valid <- tryCatch({
    df <- read.csv(file = file_path, 
                   sep = ";", 
                   stringsAsFactors = FALSE, 
                   nrows = 1)
    TRUE
  }, error = function(e) FALSE)
  if (!data_valid){
    return(list(valid = FALSE, 
                reason = "corrupted"))
  }
  return(list(valid = TRUE, 
              reason = "ok"))
}
#' 
#' @title Build Avatar aggregated measures download URL
#' @description Constructs a valid Avatar API URL for downloading aggregated
#'              road traffic measures for a given set of count points.
#' @param count_point_ids Character vector or character string. Count point IDs
#'                        to include in the request.
#' @param start_time Character string. Start datetime of traffic measures (ISO 
#'                   format expected).
#' @param end_time Character string. End datetime of traffic measures (ISO 
#'                 format expected).
#' @param aggregation_period Character string. Aggregation period (e.g. "hour",
#'                           "day"). Default is "hour".
#' @param time_zone Character string. Time zone identifier URL-encoded
#'                  (default: "Europe%2FParis").
#' @return Character string. Fully constructed Avatar API download URL.
#' @examples
#' \dontrun{
#' url <- build_avatar_aggregated_url(
#'   count_point_ids = c("123", "456"),
#'   start_time = "2023-01-01T00:00:00",
#'   end_time   = "2023-01-31T23:00:00")
#' }
#' @export
build_avatar_aggregated_url <- function(
    count_point_ids,
    start_time,
    end_time,
    aggregation_period = "hour",
    time_zone = "Europe%2FParis"
) {
  # Count point IDs list
  if (length(x = count_point_ids) > 1) {
    count_point_ids <- paste(count_point_ids, collapse = ",")
  }
  # Avatar API request URL
  api_url <- paste0(
    "https://avatar.cerema.fr/api/aggregated_measures/download", 
    "?time_zone=", time_zone, 
    "&end_time=", end_time, 
    "&start_time=", start_time, 
    "&aggregation_period=", aggregation_period, 
    "&count_point_ids=", count_point_ids)
  return(api_url)
}
#' 
#' @title Download Avatar count points metadata
#' @description Downloads the full list of Avatar count points from the Avatar API
#'              and saves the raw JSON response to a local file.
#' @param target Character string. Output JSON file path.
#' @param api_token Character string. Avatar API token.
#' @param max_retries Integer. Maximum number of retries.
#' @param limit Integer. Maximum number of API requests (default limit is10000 
#'              to get all count points (if no limit is given, the query will 
#'              return the 10 first points)
#' @return Invisibly returns TRUE if successful.
#' @export
download_avatar_count_points <- function(
    target,
    api_token = NULL,
    max_retries = 3, 
    limit = 10000) {
  # Avatar API URL
  url <- paste0("https://avatar.cerema.fr/api/countpoints/?limit=", limit)
  # # Temporarily expose token for download_file
  # old_token <- AVATAR_API_TOKEN
  # on.exit(expr = AVATAR_API_TOKEN <<- old_token, add = TRUE)
  # AVATAR_API_TOKEN <<- api_token
  # Download Avatar data ain text format
  download_with_retry(
    url = url,
    target = target,
    max_retries = max_retries,
    use_auth = (nchar(api_token) > 0))
}
#' 
#' @title Aggregate raw Avatar data at hourly resolution
#' @description Converts raw Avatar measurements into hourly aggregated metrics 
#'              using median-based statistics to reduce the influence of 
#'              outliers.
#' @param dt A `data.table` of raw Avatar measurements.
#' @return A `data.table` aggregated at the hourly level.
#' @export
aggregate_avatar_hourly <- function(dt) {
  # Ensure POSIXct datetime
  if (!inherits(x = dt$measure_datetime, what = "POSIXct")) {
    dt[, measure_datetime := as.POSIXct(x = measure_datetime,
                                        format = "%Y-%m-%dT%H:%M:%S")]}
  # Create temporal features
  dt[, hour := lubridate::hour(measure_datetime)]
  dt[, period := data.table::fifelse(test = hour >= 6 & hour < 18, 
                                     yes = "D", 
                                     no = data.table::fifelse(
                                       test = hour >= 18 & hour < 22, 
                                       yes = "E", 
                                       no = "N"))]
  # Day type: weekday (Mon-Fri) vs weekend (Sat-Sun)
  # lubridate::wday(): 1=Sun, 2=Mon, ..., 6=Fri, 7=Sat
  dt[, day_type := data.table::fifelse(
    test = lubridate::wday(measure_datetime) %in% c(1, 7),
    yes = "we", no = "wd")]
  dt[, .(
    hourly_flow =
      ifelse(test = sum(!is.na(flow.veh.h.)) > 0,
             yes = as.double(x = median(x = flow.veh.h., 
                                        na.rm = TRUE)), 
             no = NA_real_),
    hourly_flow_trucks =
      ifelse(test = sum(!is.na(flow_trucks.veh.h.)) > 0,
             yes = as.double(x = median(x = flow_trucks.veh.h., 
                                        na.rm = TRUE)), 
             no = NA_real_),
    hourly_occupancy =
      ifelse(test = sum(!is.na(occupancy...)) > 0,
             yes = as.double(x = median(x = occupancy..., 
                                        na.rm = TRUE)), 
             no = NA_real_),
    hourly_speed =
      ifelse(test = sum(!is.na(speed.km.h.)) > 0,
             yes = as.double(x = median(x = speed.km.h., 
                                        na.rm = TRUE)), 
             no = NA_real_),
    perc_flow_predicted =
      ifelse(test = sum(!is.na(perc_flow_predicted)) > 0,
             yes = as.double(x = mean(x = perc_flow_predicted, 
                                      na.rm = TRUE)), 
             no = NA_real_),
    perc_flow_trucks_predicted =
      ifelse(test = sum(!is.na(perc_flow_trucks_predicted)) > 0,
             yes = as.double(x = mean(x = perc_flow_trucks_predicted, 
                                      na.rm = TRUE)), 
             no = NA_real_),
    perc_occupancy_predicted =
      ifelse(test = sum(!is.na(perc_occupancy_predicted)) > 0,
             yes = as.double(x = mean(x = perc_occupancy_predicted, 
                                      na.rm = TRUE)), 
             no = NA_real_),
    perc_speed_predicted =
      ifelse(test = sum(!is.na(perc_speed_predicted)) > 0,
             yes = as.double(x = mean(x = perc_speed_predicted, 
                                      na.rm = TRUE)), 
             no = NA_real_),
    n_obs_this_hour = as.double(x = .N),
    first_timestamp = as.double(x = min(x = measure_datetime, 
                                        na.rm = TRUE)),
    last_timestamp  = as.double(x = max(x = measure_datetime, 
                                        na.rm = TRUE))
  ), by = .(count_point_id, period, hour, day_type)]
}
#' 
#' @title Aggregate Avatar hourly metrics over arbitrary grouping variables
#' @description Aggregates hourly Avatar traffic metrics into higher-level 
#'              summaries (e.g. by period D/E/N or by hour h0–h23). The function 
#'              computes mean-based aggregate indicators from hourly-level 
#'              metrics, while:
#'              \itemize{
#'                \item Ignoring missing values (`NA`)
#'                \item Returning `NA` if no valid data is available for a group
#'                \item Preserving temporal coverage information (first/last 
#'                      timestamps)
#'              }
#'              This function is designed to be applied to the output of an 
#'              intermediate hourly aggregation step (e.g. `hourly_aggregated`) 
#'              and avoids code duplication across different aggregation levels.
#' @param dt A `data.table` containing hourly aggregated Avatar metrics.
#'           Expected columns include:
#'           \describe{
#'             \item{hourly_flow}{Hourly traffic flow}
#'             \item{hourly_flow_trucks}{Hourly truck traffic flow}
#'             \item{hourly_occupancy}{Hourly occupancy rate}
#'             \item{hourly_speed}{Hourly average speed}
#'             \item{perc_*_predicted}{Percentage of predicted (vs measured) data}
#'             \item{n_obs_this_hour}{Number of raw observations per hour}
#'             \item{first_timestamp}{Earliest timestamp in the hour}
#'             \item{last_timestamp}{Latest timestamp in the hour}
#'           }
#' @param by_vars A `list` of grouping variables passed to the `by` argument of
#'                `data.table`. Typical examples include:
#'                \itemize{
#'                  \item `.(count_point_id, period)` for D/E/N aggregation
#'                  \item `.(count_point_id, hour)` for hourly profiles
#'                }
#' @return A `data.table` containing aggregated metrics at the requested level.
#'         One row is returned per group defined by `by_vars`.
#' @details Aggregation rules:
#'          \itemize{
#'            \item Traffic and speed indicators are averaged across hours
#'            \item Quality indicators represent the mean percentage of 
#'                  predicted data
#'            \item Counts and timestamps summarize data availability and 
#'                  coverage
#'          }
#'          This function intentionally uses mean aggregation (rather than 
#'          median), assuming that outliers have already been handled at the 
#'          hourly aggregation stage.
#' @examples
#' \dontrun{
#' # Aggregate by period (Day / Evening / Night)
#' aggregated_period <- aggregate_avatar_metrics(
#'   dt = hourly_aggregated,
#'   by_vars = c("count_point_id", "period")
#' # Aggregate by hour (h0–h23 profiles)
#' aggregated_hourly <- aggregate_avatar_metrics(
#'   dt = hourly_aggregated,
#'   by_vars = c("count_point_id", "hour"))
#' }
#' @export
aggregate_avatar_metrics <- function(dt, by_vars) {
  res <- dt[, .(
    # Mean traffic flow (vehicles/hour)
    aggregate_flow =
      ifelse(test = sum(!is.na(hourly_flow)) > 0,
             yes = mean(x = hourly_flow, na.rm = TRUE), no = NA_real_),
    # Mean truck traffic flow (vehicles/hour)
    aggregate_flow_trucks =
      ifelse(test = sum(!is.na(hourly_flow_trucks)) > 0,
             yes = mean(x = hourly_flow_trucks, na.rm = TRUE),
             no = NA_real_),
    # Mean occupancy rate
    aggregate_occupancy =
      ifelse(test = sum(!is.na(hourly_occupancy)) > 0,
             yes = mean(x = hourly_occupancy, na.rm = TRUE),
             no = NA_real_),
    # Mean speed (km/h)
    aggregate_speed =
      ifelse(test = sum(!is.na(hourly_speed)) > 0,
             yes = mean(x = hourly_speed, na.rm = TRUE),
             no = NA_real_),
    # Quality indicators: average percentage of predicted data
    perc_flow_predicted =
      ifelse(test = sum(!is.na(perc_flow_predicted)) > 0,
             yes = mean(x = perc_flow_predicted, na.rm = TRUE),
             no = NA_real_),
    perc_flow_trucks_predicted =
      ifelse(test = sum(!is.na(perc_flow_trucks_predicted)) > 0,
             yes = mean(x = perc_flow_trucks_predicted, na.rm = TRUE),
             no = NA_real_),
    perc_occupancy_predicted =
      ifelse(test = sum(!is.na(perc_occupancy_predicted)) > 0,
             yes = mean(x = perc_occupancy_predicted, na.rm = TRUE),
             no = NA_real_),
    perc_speed_predicted =
      ifelse(test = sum(!is.na(perc_speed_predicted)) > 0, 
             yes = mean(x = perc_speed_predicted, na.rm = TRUE),
             no = NA_real_),
    # Number of hours with at least one valid flow observation
    n_hours_with_data =
      sum(!is.na(hourly_flow)),
    # Number of hours with trucks
    n_hours_with_trucks = as.double(
      x = sum(!is.na(hourly_flow_trucks))),
    # Number of hours with occupancy
    n_hours_with_occupancy = as.double(
      x = sum(!is.na(hourly_occupancy))),
    # Number of hours with speed
    n_hours_with_speed = as.double(
      x = sum(!is.na(hourly_speed))),
    # Total number of raw observations across all hours
    n_total_observations =
      sum(n_obs_this_hour, na.rm = TRUE),
    # Temporal coverage
    first_timestamp =
      min(x = first_timestamp, na.rm = TRUE),
    last_timestamp =
      max(x = last_timestamp, na.rm = TRUE)
    ), 
    by = by_vars]
  
  res[, truck_pct := ifelse(test = !is.na(aggregate_flow) & aggregate_flow > 0, 
                            yes = 100 * aggregate_flow_trucks / aggregate_flow, 
                            no = NA_real_)]
  return(res)
}
#' 
#' @title Compute Avatar relative metrics using Day (D) baseline
#' @description Computes relative traffic metrics (ratios and percentages) for 
#'              each count point using period D as baseline.
#' @param dt A data.table with aggregated Avatar metrics. Must contain columns:
#'           \itemize{
#'            \item count_point_id, 
#'            \item period, 
#'            \item aggregate_flow, 
#'            \item aggregate_flow_trucks, 
#'            \item aggregate_speed, 
#'            \item aggregate_occupancy.
#'           }
#' @return A data.table with additional relative metrics:
#'         \itemize{
#'          \item flow_D, 
#'          \item flow_trucks_D, 
#'          \item speed_D, 
#'          \item occupancy_D, 
#'          \item truck_pct, 
#'          \item truck_pct_D, 
#'          \item ratio_flow, 
#'          \item ratio_flow_trucks, 
#'          \item ratio_speed, 
#'          \item ratio_occupancy, 
#'          \item ratio_truck_pct
#' @export
compute_avatar_relative_metrics <- function(dt) {
  # Baseline D
  baseline_D <- dt[period == "D" & !is.na(aggregate_flow),
                   .(flow_D = aggregate_flow, 
                     flow_trucks_D = aggregate_flow_trucks, 
                     speed_D = aggregate_speed, 
                     occupancy_D = aggregate_occupancy), 
                   by = count_point_id]
  # Join baseline
  dt <- baseline_D[dt, on = "count_point_id"]
  # Compute metrics
  dt[, truck_pct := fifelse(test = aggregate_flow > 0, 
                            yes = 100 * aggregate_flow_trucks / aggregate_flow, 
                            no = NA_real_)]
  dt[, truck_pct_D := fifelse(test = flow_D > 0, 
                              yes = 100 * flow_trucks_D / flow_D, 
                              no = NA_real_)]
  dt[, ratio_flow := fifelse(test = !is.na(flow_D) & 
                                    flow_D > 0, 
                             yes = aggregate_flow / flow_D, 
                             no = NA_real_)]
  dt[, ratio_flow_trucks := fifelse(test = !is.na(flow_trucks_D) & 
                                           flow_trucks_D > 0, 
                                    yes = aggregate_flow_trucks / 
                                      flow_trucks_D, 
                                    no = NA_real_)]
  
  dt[, ratio_speed := fifelse(test = !is.na(speed_D) & 
                                     speed_D > 0, 
                              yes = aggregate_speed / speed_D, 
                              no = NA_real_)]
  
  dt[, ratio_occupancy := fifelse(test = !is.na(occupancy_D) & 
                                         occupancy_D > 0, 
                                  yes = aggregate_occupancy / occupancy_D, 
                                  no = NA_real_)]
  
  return(dt)
}
#' 
#' @title Apply Avatar data quality and safeguard rules
#' @description Applies post-processing rules to Avatar traffic measurements to 
#'              ensurephysical consistency and robustness of derived indicators.
#'              This function:
#'              \itemize{
#'                \item Recomputes truck percentages with upper bounds
#'                \item Recomputes baseline (D) truck percentages
#'                \item Caps ratio indicators to reasonable ranges
#'                \item Avoids divisions by near-zero baseline values
#'              }
#' @param dt A data.table containing Avatar aggregated traffic measurements.
#' @return The input data.table, modified by reference.
#' @details Expected columns include:
#'          \itemize{
#'            \item aggregate_flow, aggregate_flow_trucks
#'            \item flow_D, flow_trucks_D
#'            \item ratio_flow_trucks.
#'          }
#'          The function modifies the following columns:
#'          \itemize{
#'            \item truck_pct
#'            \item truck_pct_D
#'            \item ratio_truck_pct
#'            \item ratio_flow_trucks
#' @export
apply_avatar_quality_rules <- function(dt) {
  if (!inherits(x = dt, what = "data.table")) {
    stop("apply_avatar_quality_rules() expects a data.table")
  }
  # ----------------------------------------------------------------------------
  # Recompute truck percentage (current period)
  # ----------------------------------------------------------------------------
  dt[, truck_pct := fifelse(test = !is.na(aggregate_flow) & 
                                   !is.na(aggregate_flow_trucks) & 
                                   aggregate_flow > 0, 
                            yes = pmin(100, 
                                       100 * aggregate_flow_trucks / 
                                         aggregate_flow), 
                            no = NA_real_)]
  # ----------------------------------------------------------------------------
  # Recompute truck percentage for baseline period D
  # ----------------------------------------------------------------------------
  dt[, truck_pct_D := fifelse(test = !is.na(flow_D) & 
                                     !is.na(flow_trucks_D) & 
                                     flow_D > 0, 
                              yes = pmin(100, 100 * flow_trucks_D / flow_D), 
                              no = NA_real_)]
  # ----------------------------------------------------------------------------
  # Recompute ratio of truck percentage (with safeguards)
  # ----------------------------------------------------------------------------
  dt[, ratio_truck_pct := fifelse(test = !is.na(truck_pct) & 
                                         !is.na(truck_pct_D) & 
                                         truck_pct_D > 0.1,                     # Avoid near-zero division
                                  yes = pmin(5.0, truck_pct / truck_pct_D),     # Cap at 5x
                                  no = NA_real_)]
  # ----------------------------------------------------------------------------
  # Cap ratio of truck flows
  # ----------------------------------------------------------------------------
  dt[, ratio_flow_trucks := fifelse(test = !is.na(ratio_flow_trucks), 
                                    yes = pmin(5.0, 
                                               pmax(0.0, ratio_flow_trucks)),   # Cap between 0 and 5
                                    no = NA_real_)]
  invisible(dt)
}
