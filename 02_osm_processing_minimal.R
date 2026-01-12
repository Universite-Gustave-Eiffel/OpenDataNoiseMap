# =============================================================================
# STAGE 2: OSM PROCESSING - MINIMAL VERSION
# =============================================================================

library(sf)



all_roads_avatar_sample <- st_read("data/routes_france_osm_complete.gpkg", quiet = TRUE)

high_traffic_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", 
                        "unclassified", "residential", "motorway_link", "trunk_link", 
                        "primary_link", "secondary_link", "tertiary_link")
all_roads_avatar_sample <- all_roads_avatar_sample[all_roads_avatar_sample$highway %in% high_traffic_types, ]
assign("all_roads_avatar_sample", all_roads_avatar_sample, envir = .GlobalEnv)

unique_indices <- match(unique(all_roads_avatar_sample$osm_id), all_roads_avatar_sample$osm_id)
unique_roads <- all_roads_avatar_sample[unique_indices, ] %>%
  select(osm_id, geom) %>%
  st_transform(2154)

commune_data <- st_read("data/COMMUNE_TYPO_DENSITE.shp", quiet = TRUE) %>% select(DEGRE)



n_roads <- nrow(unique_roads)
chunk_size <- 100000
n_chunks <- ceiling(n_roads / chunk_size)

cat(sprintf("    • Processing %s roads in %d chunks of %s...\n", 
            format(n_roads, big.mark=","), n_chunks, format(chunk_size, big.mark=",")))

degre_result <- rep(NA_character_, n_roads)

for (chunk_idx in 1:n_chunks) {
  start_idx <- (chunk_idx - 1) * chunk_size + 1
  end_idx <- min(chunk_idx * chunk_size, n_roads)
  
  cat(sprintf("    • Chunk %d/%d: roads %s to %s...\n", 
              chunk_idx, n_chunks, 
              format(start_idx, big.mark=","), 
              format(end_idx, big.mark=",")))
  
  roads_chunk <- unique_roads[start_idx:end_idx, ]
  
  degre_chunk_dup <- st_join(roads_chunk, commune_data["DEGRE"], 
                             join = st_intersects, left = TRUE)
  
  degre_chunk <- degre_chunk_dup %>%
    group_by(osm_id) %>%
    slice(1) %>%
    ungroup()
  
  degre_result[start_idx:end_idx] <- degre_chunk$DEGRE
  
  rm(roads_chunk, degre_chunk_dup, degre_chunk)
  
  if (chunk_idx %% 5 == 0) {
  if (chunk_idx %% 5 == 0) {
    gc(verbose = FALSE)
  }
}

degre_lookup <- data.frame(
  osm_id = unique_roads$osm_id,
  DEGRE = degre_result,
  stringsAsFactors = FALSE
)

degre_lookup <- st_sf(degre_lookup, geometry = st_geometry(unique_roads))
roads_without_degre <- which(is.na(degre_lookup$DEGRE))
n_missing <- length(roads_without_degre)

if (n_missing > 0) {
  cat(sprintf("    • %s roads not inside communes, finding nearest (also in chunks)...\n", 
              format(n_missing, big.mark=",")))
  
  missing_chunk_size <- 10000
  n_missing_chunks <- ceiling(n_missing / missing_chunk_size)
  
  for (chunk_idx in 1:n_missing_chunks) {
    start_idx <- (chunk_idx - 1) * missing_chunk_size + 1
    end_idx <- min(chunk_idx * missing_chunk_size, n_missing)
    
    cat(sprintf("      • Missing chunk %d/%d (%s roads)...\n", 
                chunk_idx, n_missing_chunks, 
                format(end_idx - start_idx + 1, big.mark=",")))
    
    missing_indices <- roads_without_degre[start_idx:end_idx]
    roads_needing_nearest <- unique_roads[missing_indices, ]
    
    nearest_communes <- st_join(roads_needing_nearest, 
                                commune_data["DEGRE"], 
                                join = st_nearest_feature, 
                                left = TRUE)
    
    degre_lookup$DEGRE[missing_indices] <- nearest_communes$DEGRE
    rm(roads_needing_nearest, nearest_communes)
    if (chunk_idx %% 3 == 0) gc(verbose = FALSE)
  }
  
  cat(sprintf("    ✓ Nearest communes found for %s roads\n", 
              format(n_missing, big.mark=",")))
} else {
}
saveRDS(degre_lookup, "data/degre.gpkg")

# Final cleanup
rm(degre_result)
degre_lookup <- readRDS( "data/degre.gpkg")

gc(verbose = FALSE)



#### Calculate network topology features (connectivity, betweenness, closeness, pagerank)


library(igraph)
library(sf)

# STEP 1: Extract road network structure (lightweight)

# Get coordinates of road endpoints
roads_for_network <- st_cast(unique_roads, "LINESTRING")
n_roads <- nrow(roads_for_network)

# Create edge list by finding start/end points of each road
edge_list <- vector("list", n_roads)
node_coords <- list()

for (i in 1:n_roads) {
  coords <- st_coordinates(roads_for_network[i, ])
  
  # First point (start)
  start_point <- paste(round(coords[1, "X"], 2), round(coords[1, "Y"], 2), sep = "_")
  
  # Last point (end)
  n_coords <- nrow(coords)
  end_point <- paste(round(coords[n_coords, "X"], 2), round(coords[n_coords, "Y"], 2), sep = "_")
  
  edge_list[[i]] <- c(from = start_point, to = end_point, road_idx = i)
  
  # Progress every 10k roads
  if (i %% 10000 == 0) {
    cat(sprintf("      • Processed %s/%s roads (%.1f%%)...\r", 
                format(i, big.mark=","),
                format(n_roads, big.mark=","),
                100 * i / n_roads))
  }
}

edge_df <- do.call(rbind, edge_list)
edge_df <- as.data.frame(edge_df, stringsAsFactors = FALSE)
edge_df$road_idx <- as.integer(edge_df$road_idx)

g <- graph_from_data_frame(edge_df[, c("from", "to")], directed = FALSE)

n_nodes <- vcount(g)
n_edges <- ecount(g)

cat(sprintf("    • Network built: %s nodes, %s edges\n", 
            format(n_nodes, big.mark=","), 
            format(n_edges, big.mark=",")))

node_connectivity <- degree(g)

node_pagerank <- page_rank(g, directed = FALSE)$vector


cat(sprintf("      • Sampling %s/%s nodes (%.3f%%)...\n", 
            format(sample_size, big.mark=","),
            format(n_nodes, big.mark=","),
            100 * sample_size / n_nodes))


node_betweenness <- betweenness(g, cutoff = 100, directed = FALSE)

node_closeness <- closeness(g, cutoff = 20, mode = "all")

edges <- as_edgelist(g)

edge_connectivity <- numeric(nrow(edge_df))
edge_betweenness <- numeric(nrow(edge_df))
edge_closeness <- numeric(nrow(edge_df))
edge_pagerank <- numeric(nrow(edge_df))

node_names <- V(g)$name
node_lookup <- setNames(seq_along(node_names), node_names)

from_indices <- node_lookup[edge_df$from]
to_indices <- node_lookup[edge_df$to]
edge_connectivity <- (node_connectivity[from_indices] + node_connectivity[to_indices]) / 2
edge_betweenness <- (node_betweenness[from_indices] + node_betweenness[to_indices]) / 2
edge_closeness <- (node_closeness[from_indices] + node_closeness[to_indices]) / 2
edge_pagerank <- (node_pagerank[from_indices] + node_pagerank[to_indices]) / 2

network_features <- data.frame(
  osm_id = roads_for_network$osm_id,
  connectivity = edge_connectivity,
  betweenness = edge_betweenness,
  closeness = edge_closeness,
  pagerank = edge_pagerank
)

rm(g, edge_list, edge_df, edges, node_connectivity, node_betweenness, 
   node_closeness, node_pagerank, edge_connectivity, edge_betweenness,
   edge_closeness, edge_pagerank)
gc(verbose = FALSE)

#### Extract OSM attributes from other_tags field

# OPTIMIZED: Fully vectorized extraction (10-50x faster than sapply)
extract_tags_fast <- function(other_tags_vector, keys, show_progress = TRUE) {
  n_rows <- length(other_tags_vector)
  n_cols <- length(keys)
  
  # Pre-allocate result matrix (faster than data.frame operations)
  result <- matrix(NA_character_, nrow = n_rows, ncol = n_cols)
  colnames(result) <- keys
  
  # Replace NA with empty strings for pattern matching
  other_tags_vector[is.na(other_tags_vector)] <- ""
  
  # Progress bar setup
  if (show_progress) {
    pb_width <- 40
    progress_shown <- 0
  }
  
  # Extract all keys at once using vectorized regex
  for (i in seq_along(keys)) {
    key <- keys[i]
    # Escape special regex characters in key (e.g., "turn:lanes")
    key_escaped <- gsub("([:|.])", "\\\\\\1", key)
    pattern <- paste0('"', key_escaped, '"=>"([^"]*)"')
    
    # Vectorized regex extraction (MUCH faster than sapply)
    match_positions <- regexpr(pattern, other_tags_vector, perl = TRUE)
    
    # Extract matched strings
    matches <- regmatches(other_tags_vector, match_positions)
    
    # Initialize result column with NA
    result[, i] <- NA_character_
    
    # Extract values only from matched rows (remove the key part, keep only value)
    matched_indices <- which(match_positions > 0)
    if (length(matched_indices) > 0) {
      values <- sub(paste0('^"', key_escaped, '"=>"'), '', matches)
      values <- sub('"$', '', values)
      result[matched_indices, i] <- values
    }
    
    # Update progress bar
    if (show_progress) {
      progress_pct <- i / n_cols
      progress_chars <- floor(progress_pct * pb_width)
      
      # Only print new characters (avoid reprinting)
      if (progress_chars > progress_shown) {
        progress_shown <- progress_chars
      }
      
      # Complete the bar at the end
      if (i == n_cols) {
        remaining <- pb_width - progress_shown
      }
    }
  }
  
  # Convert to data.frame with proper column names
  return(as.data.frame(result, stringsAsFactors = FALSE))
}

# Define useful OSM tags to extract
# These will be used as features in the model
osm_tags <- c(
  "ref",             # Road reference (A6, D123, N7)
  "lanes",           # Number of lanes (both directions)
  "maxspeed",        # Speed limit (km/h)
  "oneway",          # One-way street (yes/no/-1)
  "surface",         # Road surface type (asphalt/concrete/etc)
  "lit",             # Street lighting (yes/no)
  "bridge",          # Is a bridge (yes/no)
  "tunnel",          # Is a tunnel (yes/no)
  "junction",        # Junction type (roundabout/etc)
  "turn:lanes",      # Turn lane configuration
  "cycleway",        # Bike lane presence
  "sidewalk",        # Sidewalk presence
  "access",          # Access restrictions
  "motor_vehicle"    # Motor vehicle access
)

# Extract tags from all_roads_avatar_sample (main dataset)
if ("other_tags" %in% names(all_roads_avatar_sample)) {
  
  # Use optimized extraction (10-50x faster than previous method)
  start_time <- Sys.time()
  extracted_tags <- extract_tags_fast(all_roads_avatar_sample$other_tags, osm_tags)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Add extracted tags to all_roads_avatar_sample (batch operation)
  for (tag in osm_tags) {
    col_name <- paste0(tag, "_osm")  # Add _osm suffix to avoid conflicts
    all_roads_avatar_sample[[col_name]] <- extracted_tags[[tag]]
  }
  
  cat(sprintf("    ✓ Extracted %d OSM attributes in %.1f seconds\n", length(osm_tags), elapsed))
}
# Clean up
rm(extract_tags_fast, extracted_tags, osm_tags, start_time, elapsed)


## HERE

all_roads_avatar_sample <- all_roads_avatar_sample %>% select(-other_tags)


degre_lookup <-  degre_lookup %>% st_drop_geometry()
network_features <- network_features %>% st_drop_geometry()
full_network <- merge(all_roads_avatar_sample, degre_lookup, by ='osm_id')
full_network <- merge(full_network, network_features, by ='osm_id')

st_write(full_network, "data/routes_france_osm_complete_including_connectivity_and_communes.gpkg", append= FALSE)


















