# =============================================================================
# STAGE 3: AVATAR DOWNLOAD - MINIMAL VERSION
# =============================================================================

full_network <- st_read( "data/routes_france_osm_complete_including_connectivity_and_communes_for_R.gpkg")
full_network_2154 <- st_transform(full_network, 2154)



# Configuration
FORCE_REDOWNLOAD_COUNT_POINTS <- FALSE  # Set to TRUE to download fresh count_points from API


#### Download count_points from Avatar API if needed
if (FORCE_REDOWNLOAD_COUNT_POINTS || !file.exists("data/count_points.json")) {
  
  api_token <- Sys.getenv("AVATAR_API_TOKEN", unset = "")
  
  # Build URL - get ALL count_points (no region filter)
  # Use limit=10000 to get all count_points (not just first 10)
  # IMPORTANT: URL must end with / to avoid redirect to api_backend
  url <- "https://avatar.cerema.fr/api/countpoints/?limit=10000"
  
  tryCatch({
    library(httr)
    library(jsonlite)
    
    # Make request with authentication if token available
    if (nchar(api_token) > 0) {
      response <- httr::GET(url, 
                            httr::add_headers(Authorization = paste("Bearer", api_token)),
                            httr::timeout(120))
    } else {
      response <- httr::GET(url, httr::timeout(120))
    }
    
    httr::stop_for_status(response)
    
    # Parse JSON response
    count_points_data <- httr::content(response, "text", encoding = "UTF-8")
    
    # Save to file
    writeLines(count_points_data, "data/count_points.json")
    
  }, error = function(e) {
    stop("Failed to download count_points: ", e$message, 
         "\n  Make sure AVATAR_API_TOKEN is set in ~/.Renviron")
  })
}

#####-------------- AVATAR COUNT POINTS
points_json <- jsonlite::read_json("data/count_points.json", simplifyVector = TRUE)
count_points <- st_as_sf(points_json, wkt = c("punctual_position"), crs = 4326) %>% st_transform(crs = 2154)

# Keep lane_number field (CRITICAL: Avatar measures lanes in ONE direction only)
count_points <- count_points %>%
  select(id, count_point_name, lane_number, punctual_position, everything())

assign("count_points", count_points, envir = .GlobalEnv)


#-------------------- MATCH

# 1. Buffer 50m autour des points (O(n))
count_points_buffered <- st_buffer(count_points, dist = 50)

# 2. st_intersects avec spatial index (O(n log n) - 10-50x plus rapide!)
pointsId_with_roads <- st_join(count_points_buffered, full_network_2154, 
                               join = st_intersects)

# Drop buffer geometry, keep original point geometry
st_geometry(pointsId_with_roads) <- st_geometry(count_points)[match(pointsId_with_roads$id, count_points$id)]

# If multiple roads within 50m, keep closest one
if (nrow(pointsId_with_roads) > nrow(count_points)) {
  
  # Calculate exact distances only for candidates (much smaller set)
  pointsId_with_roads$distance <- st_distance(
    count_points[match(pointsId_with_roads$id, count_points$id), ],
    full_network_2154[match(pointsId_with_roads$osm_id, full_network_2154$osm_id), ],
    by_element = TRUE
  )
  
  # Keep closest road for each point
  pointsId_with_roads <- pointsId_with_roads %>%
    group_by(id) %>%
    slice_min(distance, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  pointsId_with_roads$distance <- NULL
}

n_after_filter <- nrow(pointsId_with_roads)

# Clean up buffer
rm(count_points_buffered)

count_points <- get("count_points", envir = .GlobalEnv)



###################################
## KEEP ONLY NETWORK WITH MATCH (id)

full_network_2154_avatar <- full_network_2154 %>% 
  filter(osm_id %in% unique(pointsId_with_roads$osm_id))

pointsId_with_roads_id <- pointsId_with_roads %>% st_drop_geometry() %>% select(id, osm_id)

full_network_2154_avatar_id <- merge(full_network_2154_avatar,pointsId_with_roads_id, by="osm_id" )


###################################
################################
###########" DOWNLOAD DATA AVATAR FOR EACH POINT


# Configuration
DOWNLOAD <- TRUE
FORCE_REDOWNLOAD <- TRUE  # Set to TRUE to delete old chunks and re-download everything
AVATAR_API_TOKEN <- Sys.getenv("AVATAR_API_TOKEN", unset = "")

# Check API token
if (nchar(AVATAR_API_TOKEN) == 0) {
  warning("⚠️  AVATAR_API_TOKEN not set. Add it to ~/.Renviron:\n",
          "   echo 'AVATAR_API_TOKEN=your_token' >> ~/.Renviron\n",
          "   Then restart R session.\n",
          "   Continuing without authentication (5 requests/minute limit)...")
} else {
}

# Delete old chunks if forcing redownload
if (FORCE_REDOWNLOAD) {
  data_dir <- "data/avatar"
  old_chunks <- list.files(path = data_dir, pattern = "avatar_data_chunk_.*\\.csv", full.names = TRUE)
  if (length(old_chunks) > 0) {
   # file.remove(old_chunks)
  }
  if (file.exists("rdsFiles/avatar_data.rds")) {
    file.remove("rdsFiles/avatar_data.rds")
  }
}
# Download function
download_file <- function(url, target, use_auth = FALSE) {
  if (use_auth && nchar(AVATAR_API_TOKEN) > 0) {
    # Use Authorization header (Bearer token)
    response <- httr::GET(url, 
                    httr::add_headers(Authorization = paste("Bearer", AVATAR_API_TOKEN)),
                    httr::timeout(30))
  } else {
    response <- httr::GET(url, httr::timeout(30))
  }
  httr::stop_for_status(response)
  writeBin(httr::content(response, "raw"), target)
}

# Download with retry
download_with_retry <- function(url, target, max_retries = 3, use_auth = FALSE) {
  for (i in 1:max_retries) {
    tryCatch({
      download_file(url, target, use_auth)
      if (file.exists(target) && file.size(target) > 0) return(TRUE)
    }, error = function(e) {
      if (i == max_retries) stop("Download failed after retries: ", e$message)
      Sys.sleep(5)
    })
  }
}

# Check existing files and validate them
data_dir <- "data/avatar"
existing_files <- list.files(path = data_dir, pattern = "avatar_data_chunk_.*\\.csv", full.names = TRUE)

# Validate existing chunks (check if empty or incomplete)
validate_chunk <- function(file_path, expected_points) {
  if (!file.exists(file_path)) return(list(valid = FALSE, reason = "missing"))
  
  # Check file size
  file_size <- file.size(file_path)
  if (file_size == 0) return(list(valid = FALSE, reason = "empty (0 bytes)"))
  
  # Count lines (1 line = header only, empty chunk)
  lines <- tryCatch({
    length(readLines(file_path, warn = FALSE))
  }, error = function(e) 0)
  
  if (lines <= 1) return(list(valid = FALSE, reason = "empty (header only)"))
  
  # Check if file is corrupted (can't be parsed)
  data_valid <- tryCatch({
    df <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, nrows = 1)
    TRUE
  }, error = function(e) FALSE)
  
  if (!data_valid) return(list(valid = FALSE, reason = "corrupted"))
  
  return(list(valid = TRUE, reason = "ok"))
}

# Download missing chunks
chunk_size <- 20
total_points <- length(count_points$id)
total_chunks <- ceiling(total_points / chunk_size)


# First pass: validate all existing chunks
chunks_to_redownload <- c()
for (chunk_id in 1:total_chunks) {
  chunk_file <- file.path(data_dir, paste0("avatar_data_chunk_", sprintf("%03d", chunk_id), ".csv"))
  start_idx <- (chunk_id - 1) * chunk_size + 1
  end_idx <- min(chunk_id * chunk_size, total_points)
  expected_points <- end_idx - start_idx + 1
  
  validation <- validate_chunk(chunk_file, expected_points)
  if (!validation$valid) {
    chunks_to_redownload <- c(chunks_to_redownload, chunk_id)
  }
}

if (length(chunks_to_redownload) > 0) {
  cat(sprintf("\n  Found %d chunks to redownload (empty/corrupted/missing)\n\n", length(chunks_to_redownload)))
} else {
}

## THIS LOOP IS ONLY IF NOT DOWNLOADED YET !
if (DOWNLOAD){
  # Download missing or invalid chunks
  chunks_to_process <- if (length(chunks_to_redownload) > 0) {
    chunks_to_redownload
  } else {
    # If no chunks to redownload, check which ones don't exist
    existing_chunk_ids <- as.integer(gsub(".*chunk_(\\d+)\\.csv", "\\1", basename(existing_files)))
    setdiff(1:total_chunks, existing_chunk_ids)
  }
  
  if (length(chunks_to_process) == 0) {
  } else {
    chunks_downloaded <- 0
    for (chunk_id in chunks_to_process) {
  if (length(chunks_to_process) == 0) {
  } else {
    chunks_downloaded <- 0
    for (chunk_id in chunks_to_process) {
      start_idx <- (chunk_id - 1) * chunk_size + 1
      end_idx <- min(chunk_id * chunk_size, total_points)
      chunk_points <- count_points$id[start_idx:end_idx]
      
      count_point_ids_str <- paste(chunk_points, collapse = ",")
      url <- paste0('https://avatar.cerema.fr/api/aggregated_measures/download',
                    '?time_zone=Europe%2FParis&end_time=', CONFIG$END_TIME, 
                    '&start_time=', CONFIG$START_TIME,
                    '&aggregation_period=hour&count_point_ids=', count_point_ids_str)
      
      target_file <- file.path(data_dir, paste0("avatar_data_chunk_", sprintf("%03d", chunk_id), ".csv"))
      
      cat(sprintf("  [%3d/%3d] Downloading chunk %03d (points %d-%d)... ", chunk_id, total_chunks, chunk_id, start_idx, end_idx))
      
      tryCatch({
        download_with_retry(url, target_file, use_auth = (nchar(AVATAR_API_TOKEN) > 0))
        
        # Validate immediately after download
        validation <- validate_chunk(target_file, length(chunk_points))
        if (validation$valid) {
          chunks_downloaded <- chunks_downloaded + 1
        } else {
          cat(sprintf("⚠️  Downloaded but %s\n", validation$reason))
        }
      }, error = function(e) {
        # Continue with next chunk on error
      })
    }
    
    cat(sprintf("\n✓ Download complete: %d chunks processed, %d successfully downloaded\n", 
                length(chunks_to_process), chunks_downloaded))
  }
    }
  }
}

# Load and combine all chunks
files <- list.files(path = data_dir, pattern = "avatar_data_chunk_.*\\.csv", full.names = TRUE)

if (length(files) > 0) {
  data_list <- lapply(files, function(file) {
    tryCatch(read.csv(file, sep = ";", stringsAsFactors = FALSE), error = function(e) NULL)
  })
  
  valid_data <- data_list[!sapply(data_list, is.null)]
  
  if (length(valid_data) > 0) {
    # Standardize columns
    all_columns <- unique(unlist(lapply(valid_data, names)))
    standardized_data <- lapply(valid_data, function(df) {
      missing_cols <- setdiff(all_columns, names(df))
      for (col in missing_cols) df[[col]] <- NA
      df[, all_columns, drop = FALSE]
    })
    
    avatar_data <- do.call(rbind, standardized_data)
    rownames(avatar_data) <- NULL
    
    # Save to RDS
    saveRDS(avatar_data, "rdsFiles/avatar_data.rds")
    assign("avatar_data", avatar_data, envir = .GlobalEnv)
  } else {
    stop("No valid Avatar data files")
  }
} else {
  stop("No Avatar data files found")
}

rm(standardized_data, points_json, count_points, valid_data)
saveRDS(avatar_data, "rdsFiles/avatar_data.rds")

