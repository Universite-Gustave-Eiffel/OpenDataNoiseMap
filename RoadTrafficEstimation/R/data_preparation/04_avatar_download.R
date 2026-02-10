# ==============================================================================
# STAGE 3: AVATAR FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS AND MATCHING - MINIMAL 
#          VERSION
# ==============================================================================

pipeline_message(
  text = "Matching Avatar traffic count points to OSM road segments", 
  level = 0, progress = "start", process = "join")

# ------------------------------------------------------------------------------
# Load full road network with connectivities and towns
# ------------------------------------------------------------------------------

if (!exists(x = 'osm_full_network', inherits = FALSE) || 
  !inherits(get('osm_full_network'), 'sf')){
  
  pipeline_message(
    text = sprintf("Loading road network data from %s", 
                   rel_path(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
    quiet = TRUE)
  
  if (sf::st_crs(osm_full_network) != CONFIG$TARGET_CRS){
    osm_full_network <- osm_full_network %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  pipeline_message(text = describe_df(osm_full_network), process = "info")
  
  pipeline_message(
    text = "OSM full road network loaded in successfully loaded", 
    level = 1, progress = "end", process = "valid")
  
}

# ------------------------------------------------------------------------------
# Download count_points from Avatar API if needed
# ------------------------------------------------------------------------------

if (file.exists(CONFIG$AVATAR_COUNT_POINTS_FILEPATH) && 
  isFALSE(CONFIG$FORCE_REDOWNLOAD_COUNT_POINTS)) {
  
  pipeline_message(
    text = sprintf("Loading previously downloaded count points data from %s", 
                   rel_path(CONFIG$AVATAR_COUNT_POINTS_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  # Read counting data to JSON file
  count_points_data <- jsonlite::read_json(
    path = CONFIG$AVATAR_COUNT_POINTS_FILEPATH, 
    simplifyVector = TRUE)
  
  pipeline_message(text = "Count points successfully loaded",  
                   level = 1, progress = "end", process = "valid")
  
} else if (CONFIG$FORCE_REDOWNLOAD_COUNT_POINTS || 
           !file.exists(CONFIG$AVATAR_COUNT_POINTS_FILEPATH)) {

  if (nchar(CONFIG$AVATAR_API_TOKEN) == 0) {
    stop(
      "Missing Avatar count points file and AVATAR_API_TOKEN is not set. ",
      "Please set AVATAR_API_TOKEN in your environment (e.g. ~/.Renviron) ",
      "or provide the file at ",
      CONFIG$AVATAR_COUNT_POINTS_FILEPATH,
      "."
    )
  }
  
  pipeline_message(text = "Downloading count points data from Avatar API",  
                   level = 1, progress = "start", process = "download")
  
  # Download Avatar data via API
  download_status <- download_avatar_count_points(
    target = CONFIG$AVATAR_COUNT_POINTS_FILEPATH, 
    api_token = CONFIG$AVATAR_API_TOKEN)
  
  # Ensure count points data is loaded in memory
  if (!exists("count_points_data")) {
    stopifnot(file.exists(CONFIG$AVATAR_COUNT_POINTS_FILEPATH))
    # Read JSON file
    count_points_data <- jsonlite::read_json(
      path = CONFIG$AVATAR_COUNT_POINTS_FILEPATH,
      simplifyVector = TRUE)
  }
  
  if (isTRUE(download_status)){
    pipeline_message(
      text = sprintf("Count point data successfully downloaded thanks to Avatar 
                     API and saved into file %s", 
                     rel_path(CONFIG$AVATAR_COUNT_POINTS_FILEPATH)), 
      level = 1, progress = "end", process = "valid")
  }
}

# ------------------------------------------------------------------------------
# Format Avatar count points
# ------------------------------------------------------------------------------

pipeline_message(text = "Formatting Avatar count points data",  
                 level = 1, progress = "start", process = "download")

# Convert count points data to sf points
count_points <- sf::st_as_sf(x = count_points_data, 
                             wkt = c("punctual_position"), 
                             crs = 4326) %>% 
  st_transform(crs = CONFIG$TARGET_CRS)

# Keep lane_number field (CRITICAL: Avatar measures lanes in ONE direction only)
count_points <- count_points %>%
  select(id, 
         count_point_name, 
         lane_number, 
         punctual_position, 
         everything())

pipeline_message(text = describe_df(count_points), process = "info")

# Reduce to test region if running in TEST mode
IS_TEST_MODE <- exists("TEST_REGION") && !is.null(TEST_REGION)
if (IS_TEST_MODE) {
  n_before <- nrow(count_points)
  count_points <- crop_to_test_region(count_points)
  pipeline_message(
    text = sprintf("TEST MODE: Count points cropped %d → %d", 
                   n_before, nrow(count_points)),
    level = 1, process = "info"
  )
}

pipeline_message(text = "Avatar data successfully formatted",  
                 level = 1, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Spatial matching: count points → osm roads
# ------------------------------------------------------------------------------

pipeline_message(text = "Spatial joining of Avatar count points and OSM roads", 
                 level = 1, progress = "start", process = "join")

pipeline_message(
  text = "Search for the OSM routes closest to the Avatar count points", 
  level = 2, progress = "start", process = "search")

# Buffer around points (O(n))
count_points_buffered <- sf::st_buffer(x = count_points, 
                                       dist = CONFIG$BUFFER_RADIUS)

# St_intersects with spatial index (O(n log n))
pointsId_with_roads <-sf::st_join(x = count_points_buffered, 
                                  y = osm_full_network, 
                                  join = st_intersects)

# Drop buffer geometry, restore original point geometry
sf::st_geometry(pointsId_with_roads) <- 
  sf::st_geometry(obj = count_points)[match(x = pointsId_with_roads$id, 
                                            table = count_points$id)]

# If multiple roads within buffer, keep closest one
if (nrow(x = pointsId_with_roads) > nrow(x = count_points)) {
  
  # Calculate exact distances only for candidates
  pointsId_with_roads$distance <- 
    sf::st_distance(
      x = count_points[match(x = pointsId_with_roads$id, 
                             table = count_points$id), ], 
      y = osm_full_network[match(x = pointsId_with_roads$osm_id, 
                                 table = osm_full_network$osm_id), ], 
      by_element = TRUE)
  
  # Keep closest road for each point
  pointsId_with_roads <- pointsId_with_roads %>%
    group_by(id) %>%
    slice_min(order_by = distance, 
              n = 1, 
              with_ties = FALSE) %>%
    ungroup() %>%
    select(-distance)
}

# Number of points after filtering
n_after_filter <- nrow(x = pointsId_with_roads)

# Clean up buffer
rm(count_points_buffered)

# Load count_points from global environment
count_points <- get(x = "count_points", 
                    envir = .GlobalEnv)

pipeline_message(text = describe_df(pointsId_with_roads), process = "info")

pipeline_message(text = "Closest OSM roads to buffered count points evaluated", 
                 level = 2, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
## Restrict OSM network to Avatar-measured roads only
# ------------------------------------------------------------------------------

pipeline_message(
  text = "Filtering OSM road network data with Avatar count points", 
  level = 2, progress = "start", process = "search")

# Filtering OSM road network data when Avatar data are available
full_network_avatar <- osm_full_network %>% 
  filter(osm_id %in% unique(pointsId_with_roads$osm_id))

# Restriction of the OSM road network to only roads accessible via AVATAR 
# through sensor ID
pointsId_with_roads_id <- pointsId_with_roads %>% 
  st_drop_geometry() %>% 
  select(id, osm_id)

# Merging of the OSM road network with count point data
full_network_avatar_id <- merge(
  x = full_network_avatar, 
  y = pointsId_with_roads_id, 
  by = "osm_id")

# Project data into target CRS if needed
if (sf::st_crs(full_network_avatar_id) != CONFIG$TARGET_CRS){
  full_network_avatar_id <- full_network_avatar_id %>% 
    st_transform(crs = CONFIG$TARGET_CRS)
}

# Write final OSM road network with count point data
sf::st_write(
  full_network_avatar_id,
  dsn = CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH,
  delete_dsn = TRUE,
  quiet = TRUE)

pipeline_message(
  text = sprintf("OSM road network successfully merged with count point data and saved into file %s", 
                 rel_path(CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH)), 
  level = 2, progress = "end", process = "valid")

# ------------------------------------------------------------------------------
# Download avatar traffic data for each point
# ------------------------------------------------------------------------------

pipeline_message(text = "Download Avatar traffic data for count point", 
                 level = 1, progress = "start", process = "download")

# Check API token
if (nchar(CONFIG$AVATAR_API_TOKEN) == 0) {
  pipeline_message(
    text = sprintf("AVATAR_API_TOKEN not set. Add it to ~/.Renviron:\n", 
                   "\t\t $echo 'AVATAR_API_TOKEN=your_token' >> ~/.Renviron\n", 
                   "\t\t Then restart R session.\n", 
                   "\t\t Continuing without authentication ", 
                   "(5 requests/min limit)..."), 
    process = "warning")
}

# Delete old chunks if forcing re-download
if (CONFIG$FORCE_REDOWNLOAD_CHUNKS) {
  old_chunks <- list.files(path = CONFIG$AVATAR_CSV_DATA_DIRPATH, 
                           pattern = "avatar_data_chunk_.*\\.csv",
                           full.names = TRUE)
  if (file.exists(CONFIG$AVATAR_RDS_DATA_FILEPATH)) {
    file.remove(CONFIG$AVATAR_RDS_DATA_FILEPATH)
  }
}

# Check existing files and validate them
existing_files <- list.files(path = CONFIG$AVATAR_CSV_DATA_DIRPATH, 
                             pattern = "avatar_data_chunk_.*\\.csv", 
                             full.names = TRUE)

# Download missing chunks 
chunk_size <- 20
total_points <- length(count_points$id)
total_chunks <- ceiling(x = total_points / chunk_size)

# First pass: validate all existing chunks
chunks_to_redownload <- c()
for (chunk_id in 1:total_chunks) {
  chunk_file <- file.path(CONFIG$AVATAR_CSV_DATA_DIRPATH, 
                          paste0("avatar_data_chunk_", 
                                 sprintf("%03d", chunk_id), 
                                 ".csv"))
  start_idx <- (chunk_id - 1) * chunk_size + 1
  end_idx <- min(chunk_id * chunk_size, total_points)
  expected_points <- end_idx - start_idx + 1
  
  validation <- validate_chunk(file_path = chunk_file, 
                               expected_points = expected_points)
  if (!validation$valid) {
    chunks_to_redownload <- c(chunks_to_redownload, chunk_id)
  }
}

if (length(chunks_to_redownload) > 0) {
  if (RUN_CONTEXT == "local"){
    pipeline_message(
      text = sprintf("Found %d chunks to re-download (empty/corrupted/missing)", 
                     length(chunks_to_redownload)), 
      process = "warning")
  } else {
    pipeline_message(
      text = sprintf("Found %d chunks to re-download (empty/corrupted/missing), 
                      but the computing environment does not allow it. Please 
                      run the shell script run_avatar_download.sh to re-download 
                     these chunks", length(chunks_to_redownload)), 
      process = "warning")
  }
}

# Download missing or invalid chunks if not downloaded yet
if (CONFIG$FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS && RUN_CONTEXT == "local"){
  
  pipeline_message(text = "Download missing or invalid chunks", 
                   level = 2, progress = "start", process = "download")
  
  # Download missing or invalid chunks
  chunks_to_process <- if (length(chunks_to_redownload) > 0) {
    chunks_to_redownload
  } else {
    # If no chunks to re-download, check which ones don't exist
    existing_chunk_ids <- 
      as.integer(x = gsub(pattern = ".*chunk_(\\d+)\\.csv", 
                          replacement = "\\1", 
                          x = basename(existing_files)))
    setdiff(x = seq_len(total_chunks), 
            y = existing_chunk_ids)
  }
  # Process chunks to re-download
  if (length(chunks_to_process) == 0) {
    pipeline_message(text = "No chunk to download", process = "info")
    return(invisible(NULL))
  }
  chunks_downloaded <- 0
  for (chunk_id in chunks_to_process) {
    start_idx <- (chunk_id - 1) * chunk_size + 1
    end_idx <- min(chunk_id * chunk_size, total_points)
    chunk_points <- count_points$id[start_idx:end_idx]
    count_point_ids_str <- paste(chunk_points, collapse = ",")
    # Chunk query URL
    target_url <- build_avatar_aggregated_url(
      count_point_ids = chunk_points,
      start_time = CONFIG$START_TIME,
      end_time = CONFIG$END_TIME
    )
    # Output filename
    target_file <- file.path(CONFIG$AVATAR_CSV_DATA_DIRPATH, 
                             paste0("avatar_data_chunk_", 
                                    sprintf("%03d", chunk_id), 
                                    ".csv"))
    pipeline_message(
      text = sprintf("[%3d/%3d] Downloading chunk %03d (points %d-%d)", 
                     chunk_id, total_chunks, chunk_id, start_idx, end_idx), 
      process = "download")
    
    tryCatch({
      if (IS_TEST_MODE) {
        Sys.sleep(2)
      }
      # Download the file with automatic retry logic
      download_with_retry(
        url = target_url, 
        target = target_file, 
        use_auth = (nchar(CONFIG$AVATAR_API_TOKEN) > 0))
      # Validate chunk immediately after download
      validation <- validate_chunk(
        file_path = target_file, 
        expected_points = length(chunk_points))
      if (validation$valid) {
        chunks_downloaded <- chunks_downloaded + 1
      } else {
        pipeline_message(text = sprintf("Chunk invalid: %s", validation$reason), 
                         process = "warning")
      }
    }, error = function(e) {
      pipeline_message(
        text = sprintf("Failed to download chunk %d: %s", 
                       chunk_id, e$message), 
        process = "stop")
    })
  }
  
  pipeline_message(text = "Missing or invalid chunks successfully downloaded", 
                   level = 2, progress = "end", process = "valid")
  pipeline_message(
    text = sprintf("%d chunks processed\n\t\t %d chunks successfully 
                   downloaded", length(chunks_to_process), chunks_downloaded), 
    process = "info")
}

# List of files for all chunks
files <- list.files(path = CONFIG$AVATAR_CSV_DATA_DIRPATH, 
                    pattern = "avatar_data_chunk_.*\\.csv", 
                    full.names = TRUE)

# Load and combine
if (length(files) > 0) {
  pipeline_message(text = "Combining all downloaded chunks", 
                   level = 2, progress = "start", process = "join")
  data_list <- lapply(files, function(file) {
    tryCatch(read.csv(file = file, 
                      sep = ";", 
                      stringsAsFactors = FALSE), 
             error = function(e){NULL})
  })
  valid_data <- data_list[!sapply(data_list, is.null)]
  
  if (length(valid_data) > 0) {
    # Standardize columns
    standardized_data <- standardize_columns(data_list = valid_data)
    avatar_data <- do.call(rbind, standardized_data)
    rownames(avatar_data) <- NULL
    
    # Save to RDS
    saveRDS(object = avatar_data, 
            file = CONFIG$AVATAR_RDS_DATA_FILEPATH)
    
    pipeline_message(
      text = sprintf("Avatar data successfully combined and saved into file ", 
                     rel_path(CONFIG$AVATAR_CSV_DATA_DIRPATH)), 
      level = 2, progress = "end", process = "save")
  } else {
    pipeline_message(text = "No valid Avatar data files", process = "stop")
  }
} else {
  pipeline_message(text = "No Avatar data files found", process = "stop")
}

rm(standardized_data, count_points_data, count_points, valid_data)

pipeline_message(text = "Avatar count points and OSM roads successfully merged", 
                 level = 0, progress = "end", process = "valid")