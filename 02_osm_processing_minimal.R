# ==============================================================================
# STAGE 2: OSM PROCESSING - MINIMAL VERSION
# ==============================================================================

message("\n === Merging OSM road and commune polygon data === \n")

if (file.exists(CONFIG$OSM_DEGRE_FILEPATH) && 
    isFALSE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)){
  
  message("\t 📥 Loading already merged OSM road and commune polygon data \n")
  
  degre_lookup <- readRDS(file = CONFIG$OSM_DEGRE_FILEPATH)
  
  message("\t\t ✓ Merged OSM road and commune polygon data loaded from ", 
          CONFIG$OSM_DEGRE_FILEPATH, "\n")
  
} else if (!file.exists(CONFIG$OSM_DEGRE_FILEPATH) || 
    isTRUE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)) {
  
  message("\t 📥 Loading OSM road and municipal polygon data \n")
  
  # ----------------------------------------------------------------------------
  # Load full OSM road dataset (GeoPackage)
  # ----------------------------------------------------------------------------
  
  # This dataset contains all OSM roads for France with geometry and attributes
  osm_roads <- st_read(dsn = CONFIG$OSM_ROADS_FILEPATH, 
                       quiet = TRUE)
  
  # ----------------------------------------------------------------------------
  # Filter roads by traffic-relevant highway types
  # ----------------------------------------------------------------------------
  
  # Keep only road categories relevant for traffic / noise modelling
  high_traffic_types <- c(
    "motorway", "trunk", "primary", "secondary", "tertiary", 
    "unclassified", "residential", "motorway_link", "trunk_link", 
    "primary_link", "secondary_link", "tertiary_link")
  
  # Subset roads to keep only selected highway types
  osm_roads <- osm_roads[osm_roads$highway %in% high_traffic_types, ]
  
  # Explicitly store in global environment (pipeline-style workflow)
  assign(x = "osm_roads", 
         value = osm_roads, 
         envir = .GlobalEnv)
  
  # ----------------------------------------------------------------------------
  # Remove duplicate OSM IDs (keep first geometry only)
  # ----------------------------------------------------------------------------
  
  # OSM data may contain duplicated osm_id entries
  unique_osm_ids <- match(
    x = unique(osm_roads$osm_id), 
    table = osm_roads$osm_id)
  
  # Keep only unique road geometries and transform CRS
  unique_roads <- osm_roads[unique_osm_ids, ] %>%
    select(osm_id, geom) %>%
    st_transform(CONFIG$TARGET_CRS)
  
  # ----------------------------------------------------------------------------
  # Load commune polygon data (urban density classification)
  # ----------------------------------------------------------------------------
  commune_data <- st_read(
    dsn = CONFIG$OSM_TYPOLOGIES_FILEPATH, 
    quiet = TRUE) %>% 
    select(DEGRE) %>%   # Urban density category
    st_transform(CONFIG$TARGET_CRS)
  
  # Safe simplification (topological)
  commune_data <- st_make_valid(x = commune_data)
  
  message("\t\t ✓ OSM road dataset and commune polygon data successfully 
                  loaded! \n")
  
  # ----------------------------------------------------------------------------
  # Chunked spatial join: assign commune DEGREE to each road
  # ----------------------------------------------------------------------------
  
  message("\t 🔗 Spatial joining of OSM road and commune polygon data \n")
  
  n_roads <- nrow(x = unique_roads)
  
  # Ensure planar geometry operations (GEOS) for performance and robustness
  suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
  
  # Spatial intersection: road segments × commune polygons (single vectorized 
  # spatial join - duplicates may occur when a road intersects multiple 
  # communes)
  degre_lookup <- st_join(
    x = unique_roads["osm_id"],   # keep only required attributes
    y = commune_data["DEGRE"],    # commune urban density class
    join = st_intersects,
    left = TRUE
  )
  
  # Resolve duplicates (a road may intersect several communes, only the first
  # match is kept, i.e. one DEGREE per osm_id)
  degre_lookup <- degre_lookup[!duplicated(degre_lookup$osm_id), ]
  
  # chunk_size <- 100000
  # n_chunks <- ceiling(n_roads / chunk_size)
  # 
  # cat(sprintf("\t\t\t Processing %s roads in %d chunks of %s...\n",
  #             fmt(n_roads),
  #             n_chunks,
  #             fmt(chunk_size)))
  # 
  # # Pre-allocate result vector
  # degre_result <- rep(NA_character_, n_roads)
  # # Progress bar configuration
  # pb <- progress_bar$new(
  #   format = "\t\t\t[:bar] :percent | chunk :current/:total | start: :start | end: :end  | ETA: :eta\n",
  #   total  = n_chunks,
  #   clear  = FALSE,
  #   width  = 90
  # )
  # 
  # # Loop over road segments
  # for (chunk_idx in 1:n_chunks) {
  #   start_idx <- (chunk_idx - 1) * chunk_size + 1
  #   end_idx <- min(chunk_idx * chunk_size, n_roads)
  # 
  #   # Progress update
  #   if (CONFIG$IS_TTY) {
  #     pb$tick(tokens = list(
  #       start = fmt(start_idx),
  #       end   = fmt(end_idx)
  #     ))
  #   } else if (chunk_idx %% 1 == 0 || chunk_idx == n_chunks) {
  #     cat(sprintf(
  #       "\t\t\t Progress: chunk %d/%d (%.1f%%) | roads %s → %s | %s\n",
  #       chunk_idx,
  #       n_chunks,
  #       100 * chunk_idx / n_chunks,
  #       fmt(start_idx),
  #       fmt(end_idx),
  #       format(x = Sys.time(), "%Y-%m-%d %H:%M:%S")
  #     ))
  #     flush.console()
  #   }
  #   
  #   # Extract current chunk
  #   roads_chunk <- unique_roads[start_idx:end_idx, ]
  # 
  #   # Spatial intersection with commune polygons
  #   degre_chunk_dup <- st_join(
  #     roads_chunk,
  #     commune_data["DEGRE"],
  #     join = st_intersects,
  #     left = TRUE)
  # 
  #   # Remove duplicates (a road may intersect several communes)
  #   degre_chunk <- degre_chunk_dup %>%
  #     group_by(osm_id) %>%
  #     slice(1) %>%
  #     ungroup()
  # 
  #   # Store results in pre-allocated vector
  #   degre_result[start_idx:end_idx] <- degre_chunk$DEGRE
  # 
  #   # Memory cleanup
  #   rm(roads_chunk, degre_chunk_dup, degre_chunk)
  # 
  #   if (chunk_idx %% 5 == 0) {
  #       gc(verbose = FALSE)
  #   }
  # }
  # 
  # # Build degre lookup table (road → commune category)
  # degre_lookup <- st_sf(
  #   data.frame(osm_id = unique_roads$osm_id,
  #              DEGRE = degre_result,
  #              stringsAsFactors = FALSE),
  #   geometry = st_geometry(unique_roads))
  
  # Handle roads not intersecting any commune polygon (→ assign the nearest 
  # commune DEGREE)
  idx_missing <- which(is.na(degre_lookup$DEGRE))
  n_missing <- length(x = idx_missing)
  
  if (n_missing > 0) {
    cat(sprintf(
      "\t\t\t ⚠️ %s roads not intersecting communes, assigning nearest 
                 commune \n",
      fmt(x = n_missing)))
    
    # Compute nearest commune polygon for all missing roads in one call
    nearest_idx <- st_nearest_feature(
      x = degre_lookup[idx_missing, ], 
      y = commune_data)
    
    # Assign DEGREE of the nearest commune
    degre_lookup$DEGRE[idx_missing] <- commune_data$DEGRE[nearest_idx]
    
    cat(sprintf(
      "\t\t\t ✓ Nearest communes assigned for %s roads \n",
      fmt(x = n_missing)))
  }
  
  # # Handle roads not intersecting any commune
  # # → assign nearest commune DEGREE
  # roads_without_degre <- which(is.na(degre_lookup$DEGRE))
  # n_missing <- length(roads_without_degre)
  # 
  # if (n_missing > 0) {
  #   cat(sprintf(
  #     "\t\t\t %s roads not inside communes, finding nearest (also in chunks)...\n", 
  #     format(x = n_missing, big.mark=",")))
  #   
  #   missing_chunk_size <- 10000
  #   n_missing_chunks <- ceiling(n_missing / missing_chunk_size)
  #   
  #   for (chunk_idx in 1:n_missing_chunks) {
  #     start_idx <- (chunk_idx - 1) * missing_chunk_size + 1
  #     end_idx <- min(chunk_idx * missing_chunk_size, n_missing)
  #     
  #     cat(sprintf("\t\t\t Missing chunk %d/%d (%s roads)...\n", 
  #                 chunk_idx, n_missing_chunks, 
  #                 format(x = end_idx - start_idx + 1, big.mark=",")))
  #     
  #     missing_indices <- roads_without_degre[start_idx:end_idx]
  #     roads_needing_nearest <- unique_roads[missing_indices, ]
  #     
  #     # Find nearest commune polygon
  #     nearest_communes <- st_join(
  #       roads_needing_nearest, 
  #       commune_data["DEGRE"], 
  #       join = st_nearest_feature, 
  #       left = TRUE)
  #     
  #     degre_lookup$DEGRE[missing_indices] <- nearest_communes$DEGRE
  #     # Memory cleanup
  #     rm(roads_needing_nearest, nearest_communes)
  #     
  #     if (chunk_idx %% 3 == 0) {
  #       gc(verbose = FALSE)
  #     }
  #   }
  #   
  #   cat(sprintf("\t\t✓ Nearest communes found for %s roads\n", 
  #               format(x = n_missing, big.mark=",")))
  # }
  
  message("\t\t ✓ OSM road and commune polygon data joined successfully! \n")
  
  # Save DEGREE lookup to disk
  saveRDS(object = degre_lookup, 
          file = CONFIG$OSM_DEGRE_FILEPATH)
  message("\t\t 📥 Merged data saved into file ", CONFIG$OSM_DEGRE_FILEPATH, "\n")
}

# ------------------------------------------------------------------------------
# Build road network graph and compute topological metrics (connectivity, 
# betweenness, closeness, pagerank)
# ------------------------------------------------------------------------------

if (file.exists(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH) && 
    isFALSE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)){
  
  message("\t 📥 Loading already built road network data \n")
  
  start_timer()
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH,
    quiet = TRUE)
  elapsed <- stop_timer()
  
  message("\t\t ✓ Road network data loaded in ", elapsed, "\n")
  
} else if (!file.exists(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH) || 
    isTRUE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)) {
  
  message("\t 📊 Building road network graph and compute topological 
                 metrics \n")
  
  message("\t 💻 Creating edge data from start/end points of each road \n")
  
  # Ensure planar geometry operations (GEOS) for performance and robustness
  suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
  
  # Convert road geometries to LINESTRING
  roads_for_network <- st_cast(x = unique_roads, 
                               to = "LINESTRING")
  n_roads <- nrow(x = roads_for_network)
  
  # Create edge dataframe by finding start/end points of each road
  edge_df <- data.frame(
    from = character(n_roads),
    to = character(n_roads),
    road_idx = seq_len(n_roads),
    stringsAsFactors = FALSE
  )
  
  # Road coordinates
  coords_dt <- as.data.table(
    st_coordinates(x = roads_for_network))
  
  # Start and end points (first and last vertices, i.e. first and last 
  # coordinate of each row)
  start_pts <- coords_dt[, .SD[1], by = L1]
  end_pts <- coords_dt[, .SD[.N], by = L1]
  
  # Edges of each row
  edge_df <- data.frame(from = paste(round(start_pts$X, 2), 
                                     round(start_pts$Y, 2), 
                                     sep = "_"), 
                        to = paste(round(end_pts$X, 2), 
                                   round(end_pts$Y, 2), 
                                   sep = "_"), 
                        road_idx = start_pts$L1, 
                        stringsAsFactors = FALSE)
  
  # # Road coordinates
  # coords <- st_coordinates(x = roads_for_network)
  # coords_dt <- as.data.table(coords)
  # # Extract start/end coordinates to build graph edges
  # for (i in seq_len(n_roads)) {
  #   # Progress every 10k roads
  #   if (i %% 1000 == 0) {
  #     cat(sprintf("\t\t• Processed %s/%s roads (%.1f%%)\n", 
  #                 format(x = i, big.mark=","),
  #                 format(x = n_roads, big.mark=","),
  #                 100 * i / n_roads))
  #   }
  #   # Road coordinates 
  #   coords <- st_coordinates(x = roads_for_network[i, ])
  #   # Start point (first vertex)
  #   edge_df$from[i] <- paste(
  #     round(coords[1, "X"], 2),
  #     round(coords[1, "Y"], 2),
  #     sep = "_"
  #   )
  #   # End point (last vertex)
  #   n_coords <- nrow(coords)
  #   edge_df$to[i] <- paste(
  #     round(coords[n_coords, "X"], 2),
  #     round(coords[n_coords, "Y"], 2),
  #     sep = "_"
  #   )
  # }
  
  message("\t\t ✓ Edge data created successfully \n")
  
  # Build undirected graph
  
  message("\t 💻 Creating igraph graph from road edges data \n")
  
  start_timer()
  g <- igraph::graph_from_data_frame(d = edge_df[, c("from", "to")], 
                                     directed = FALSE)
  elapsed <- stop_timer()
  message("\t\t ✓ Graph from road edges data created in ", elapsed, "\n")
  
  # Compute node-level metrics
  n_nodes <- igraph::vcount(graph = g)
  n_edges <- igraph::ecount(graph = g)
  
  cat(sprintf("\t\t ✓ Network built: %s nodes, %s edges \n", 
              fmt(x = n_nodes), 
              fmt(x = n_edges)))
  
  message("\t 📊 Computing centrality scores \n")
  
  # Node connectivity
  start_timer()
  node_connectivity <- igraph::degree(graph = g)
  elapsed <- stop_timer()
  message("\t\t ✓ Node connectivity computed in ", elapsed, "\n")
  
  # PageRank scores
  start_timer()
  node_pagerank <- igraph::page_rank(graph = g, 
                                     directed = FALSE)$vector
  elapsed <- stop_timer()
  message("\t\t ✓ Node pagerank computed in ", elapsed, "\n")
  
  # Betweenness centralities of positions on undirected geodesics
  start_timer()
  node_betweenness <- igraph::betweenness(graph = g,  
                                  cutoff = CONFIG$CUTOFF_BETWEENNESS, 
                                  directed = FALSE)
  elapsed <- stop_timer()
  message("\t\t ✓ Node betweenness computed in ", elapsed, "\n")
  
  # Closeness centrality measures (how many steps is required to access every 
  # other vertex from a given one)
  start_timer()
  node_closeness <- igraph::closeness(graph = g, 
                                      cutoff = CONFIG$CUTOFF_CLOSENESS, 
                                      mode = "all")
  elapsed <- stop_timer()
  message("\t\t ✓ Node closeness computed in ", elapsed, "\n")
  
  message("\t ✓ Centrality scores computed! \n")
  
  # # Pre-allocate numeric vectors for edge-level metrics (one value per road 
  # # segment)
  # edge_connectivity <- numeric(length = nrow(edge_df))
  # edge_betweenness  <- numeric(length = nrow(edge_df))
  # edge_closeness    <- numeric(length = nrow(edge_df))
  # edge_pagerank     <- numeric(length = nrow(edge_df))
  # 
  # # Retrieve node identifiers (vertex names) from the igraph object
  # node_names <- V(g)$name
  # 
  # # Build a fast lookup table (node name → node index in igraph) to avoid repeated 
  # # expensive matching operations
  # node_lookup <- setNames(
  #   seq_along(node_names), 
  #   node_names)
  # 
  # # Map edge endpoints ("from" and "to") to igraph node indices (each road segment 
  # # connects two graph nodes)
  # from_indices <- node_lookup[edge_df$from]
  # to_indices <- node_lookup[edge_df$to]
  
  message("\t 📊 Computing edge-level metrics \n")
  
  # Edge-level metrics (retrieve edge endpoints as vertex indices directly)
  edge_ends <- igraph::ends(graph = g, 
                            es = E(graph = g), 
                            names = FALSE)
  # Map edge endpoints ("from" and "to") to igraph node indices (each road segment
  # connects two graph nodes)
  from_indices <- edge_ends[, 1]
  to_indices <- edge_ends[, 2]
  
  # Compute edge-level metrics assigning each road segment the mean value of the 
  # metrics of its two endpoint nodes: average node degree (edge connectivity), 
  # average node betweenness centrality, average node closeness centrality and 
  # average node PageRank score
  edge_connectivity <- 
    (node_connectivity[from_indices] + node_connectivity[to_indices]) / 2
  edge_betweenness <- 
    (node_betweenness[from_indices] + node_betweenness[to_indices]) / 2
  edge_closeness <- 
    (node_closeness[from_indices] + node_closeness[to_indices]) / 2
  edge_pagerank <- 
    (node_pagerank[from_indices] + node_pagerank[to_indices]) / 2
  
  message("\t ✓ Edge-level metrics computed! \n")
  
  message("\t 📊 Assemble network feature table \n")
  network_features <- data.frame(
    osm_id = roads_for_network$osm_id,
    connectivity = edge_connectivity,
    betweenness = edge_betweenness,
    closeness = edge_closeness,
    pagerank = edge_pagerank)
  
  # Memory cleanup
  rm(g, edge_df, node_connectivity, node_betweenness, 
     node_closeness, node_pagerank, edge_connectivity, edge_betweenness,
     edge_closeness, edge_pagerank)
  gc(verbose = FALSE)
  
  message("\t ✓ Road network built and topological metrics computed! \n")
  
  # ----------------------------------------------------------------------------
  # Extract structured OSM attributes from "other_tags"
  # ----------------------------------------------------------------------------
  
  message("\t 🔗 Extracting structured OSM attributes from other_tags \n")
  
  # Select relevant OSM tags to extract for traffic noise modelling
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
  
  # Extract tags from osm_roads
  if ("other_tags" %in% names(osm_roads)) {
    start_timer()
    extracted_tags <- extract_osm_other_tags(
      other_tags_vector = osm_roads$other_tags, 
      keys = osm_tags)
    elapsed <- stop_timer()
    
    # Add extracted tags to osm_roads
    for (tag in osm_tags) {
      col_name <- paste0(tag, "_osm")  # Add _osm suffix to avoid conflicts
      osm_roads[[col_name]] <- extracted_tags[[tag]]
    }
    cat(sprintf("\t\t ✓ Extracted %d OSM attributes in %.1f seconds \n", 
                length(x = osm_tags), 
                elapsed))
  }
  
  # Memory cleanup
  rm(extracted_tags, osm_tags)
  
  # Remove raw other_tags field
  osm_roads <- osm_roads %>% 
    select(-other_tags)
  
  message("\t ✓ Structured OSM attributes from other_tags extracted! \n")
  
  # ----------------------------------------------------------------------------
  # Final merge and export
  # ----------------------------------------------------------------------------
  
  message("Final merging to build full road network...\n")
  
  # Drop geometries in degre_lookup and network_features
  degre_lookup <-  degre_lookup %>% st_drop_geometry()
  network_features <- network_features %>% st_drop_geometry()
  
  # Merge osm_roads and degre_lookup
  start_timer()
  osm_full_network <- merge(x = osm_roads, 
                            y = degre_lookup, 
                            by = 'osm_id')
  elapsed <- stop_timer()
  message("\t\t ✓ osm_roads and degre_lookup merged in ", 
          elapsed, "\n")
  
  # Merge osm_full_network and network_features
  start_timer()
  osm_full_network <- merge(x = osm_full_network, 
                            y = network_features, 
                            by = 'osm_id')
  elapsed <- stop_timer()
  message("\t\t ✓ osm_full_network and network_features merged in ", 
          elapsed, "\n")
  
  # Project data into target CRS
  osm_full_network <- osm_full_network %>% 
    st_transform(crs = CONFIG$TARGET_CRS)
  
  # Write final road dataset
  start_timer()
  sf::st_write(
    osm_full_network,
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH,
    delete_dsn = TRUE,
    quiet = TRUE)
  elapsed <- stop_timer()
  
  message("\t ✓ Final full road network built in ", elapsed, 
          " and saved into file ", CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH, "\n")
}