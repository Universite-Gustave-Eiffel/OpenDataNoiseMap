# ==============================================================================
# STAGE 3: AVATAR FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS AND MATCHING - MINIMAL 
#          VERSION
# ==============================================================================

message("\n === Matching Avatar count points to OSM road segments === \n")

# ------------------------------------------------------------------------------
# Load full road network with connectivities and towns
# ------------------------------------------------------------------------------

if (!exists(x = 'osm_full_network', inherits = FALSE)){
  
  message("\t 🔗 Loading OSM full road network \n")
  
  start_timer()
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH, 
    quiet = TRUE)
  elapsed <- stop_timer()
  
  if (sf::st_crs(osm_full_network) != CONFIG$TARGET_CRS){
    osm_full_network <- osm_full_network %>% 
      st_transform(crs = CONFIG$TARGET_CRS)
  }
  
  message("\t\t ✓ OSM full road network loaded in ", elapsed, "\n")
}

# ------------------------------------------------------------------------------
# Download count_points from Avatar API if needed
# ------------------------------------------------------------------------------

if (file.exists(CONFIG$AVATAR_COUNT_POINTS_FILEPATH) && 
    isFALSE(CONFIG$FORCE_REDOWNLOAD_COUNT_POINTS)){
  
  message("\t 📥 Loading previously downloaded count points data \n")
  
  stopifnot(
    is.logical(CONFIG$FORCE_REDOWNLOAD_COUNT_POINTS),
    length(CONFIG$FORCE_REDOWNLOAD_COUNT_POINTS) == 1)
  
  # Read counting data to JSON file
  count_points_data <- jsonlite::read_json(
    path = CONFIG$AVATAR_COUNT_POINTS_FILEPATH, 
    simplifyVector = TRUE)
  
  message("\t\t ✓ Count points from local file loaded! \n")
} else if (CONFIG$FORCE_REDOWNLOAD_COUNT_POINTS || 
    !file.exists(CONFIG$AVATAR_COUNT_POINTS_FILEPATH)) {
  
  message("\t 📥 Downloading count points data from Avatar API \n")
  
  # Download Avatar data via API
  start_timer()
  download_status <- download_avatar_count_points(
    target = CONFIG$AVATAR_COUNT_POINTS_FILEPATH, 
    api_token = CONFIG$AVATAR_API_TOKEN)
  elapsed <- stop_timer()
  
  # Ensure count points data is loaded in memory
  if (!exists("count_points_data")) {
    stopifnot(file.exists(CONFIG$AVATAR_COUNT_POINTS_FILEPATH))
    # Read JSON file
    count_points_data <- jsonlite::read_json(
      path = CONFIG$AVATAR_COUNT_POINTS_FILEPATH,
      simplifyVector = TRUE)
  }
  
  if (isTRUE(download_status)){
    message("\t\t ✓ Count point data successfully downloaded with Avatar API 
            in ", elapsed, " into file ", CONFIG$AVATAR_COUNT_POINTS_FILEPATH, 
            "\n")
  }
}

# ------------------------------------------------------------------------------
# Format Avatar count points
# ------------------------------------------------------------------------------

message("\t 🔗 Formatting Avatar data \n")

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

message("\t\t ✓ Avatar data formatted successfully! \n")

# ------------------------------------------------------------------------------
# Spatial matching: count points → osm roads
# ------------------------------------------------------------------------------

message("\t 🔗 Spatial matching between Avatar count points and OSM roads \n")

start_timer()

# Buffer around points (O(n))
count_points_buffered <- sf::st_buffer(x = count_points, 
                                       dist = CONFIG$BUFFER_RADIUS)

# St_intersects with spatial index (O(n log n))
pointsId_with_roads <-sf::st_join(x = count_points_buffered, 
                                  y = osm_full_network, 
                                  join = st_intersects)

# Drop buffer geometry, restore original point geometry
st_geometry(pointsId_with_roads) <- 
  sf::st_geometry(obj = count_points)[
    match(x = pointsId_with_roads$id, 
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

message("\t\t ✓ Closest OSM roads to buffered count points evaluated\n")

# ------------------------------------------------------------------------------
## Restrict OSM network to Avatar-measured roads only
# ------------------------------------------------------------------------------

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

elapsed <- stop_timer()

message("\t\t ✓ OSM road network data merged with count point data in ", 
        elapsed, "\n")

# Project data into target CRS if needed
if (sf::st_crs(full_network_avatar_id) != CONFIG$TARGET_CRS){
  full_network_avatar_id <- full_network_avatar_id %>% 
    st_transform(crs = CONFIG$TARGET_CRS)
}

# Write final OSM road network with count point data
start_timer()
sf::st_write(
  full_network_avatar_id,
  dsn = CONFIG$AVATAR_IDS_FULL_NETWORK_FILEPATH,
  delete_dsn = TRUE,
  quiet = TRUE)
elapsed <- stop_timer()

# ------------------------------------------------------------------------------
# Download avatar traffic data for each point
# ------------------------------------------------------------------------------

message("\t 📥 Download Avatar traffic data for each point \n")

start_timer()

# Check API token
if (nchar(CONFIG$AVATAR_API_TOKEN) == 0) {
  warning("\t\t ⚠️  AVATAR_API_TOKEN not set. Add it to ~/.Renviron:\n",
          "   echo 'AVATAR_API_TOKEN=your_token' >> ~/.Renviron\n",
          "   Then restart R session.\n",
          "   Continuing without authentication (5 requests/minute limit)...")
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
  cat(sprintf(
    "\t\t 🔁 Found %d chunks to re-download (empty/corrupted/missing) \n", 
    length(chunks_to_redownload)))
}

# Download missing or invalid chunks if not downloaded yet
if (CONFIG$FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS){
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
    message("\t\t ✓ No chunks to download")
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
    cat(sprintf("\t\t 📥 [%3d/%3d] Downloading chunk %03d (points %d-%d)... \n", 
                chunk_id, total_chunks, chunk_id, start_idx, end_idx))
    
    tryCatch({
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
        message("\t\t ⚠️ Chunk invalid: ", validation$reason)
      }
    }, error = function(e) {
      message("\t\t ⛔ Failed to download chunk ", chunk_id, ": ", e$message)
    })
}

elapsed <- stop_timer()

cat(sprintf("\t\t ✓ Download complete in %s : %d chunks processed, 
                                              %d successfully downloaded\n", 
            elapsed, length(chunks_to_process), chunks_downloaded))
}

# List of files for all chunks
files <- list.files(path = CONFIG$AVATAR_CSV_DATA_DIRPATH, 
                    pattern = "avatar_data_chunk_.*\\.csv", 
                    full.names = TRUE)

# Load and combine
if (length(files) > 0) {
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
    
    message("\t\t 📥 Avatar data merged with OSM road network saved into 
            directory ", CONFIG$AVATAR_CSV_DATA_DIRPATH, "\n")
  } else {
    stop("\t\t ⛔ No valid Avatar data files")
  }
} else {
  stop("\t\t ⛔ No Avatar data files found")
}

rm(standardized_data, count_points_data, count_points, valid_data)

message("\t ✓ Avatar count points and OSM roads spatially merged! \n")