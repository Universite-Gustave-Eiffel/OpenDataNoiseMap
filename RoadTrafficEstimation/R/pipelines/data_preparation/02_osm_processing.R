# ==============================================================================
# STAGE 2: OSM PROCESSING - MINIMAL VERSION
# ==============================================================================

pipeline_message("Merging OSM road and commune data", level = 0, 
                 progress = "start", process = "join")

if (file.exists(cfg_data$OSM_DEGRE_FILEPATH) && 
    isFALSE(cfg_data$FORCE_REJOIN_OSM_AND_COMMUNES)){
  
  pipeline_message(
    sprintf("Loading already merged OSM road and commune data from %s", 
            rel_path(cfg_data$OSM_DEGRE_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  degre_lookup <- readRDS(file = cfg_data$OSM_DEGRE_FILEPATH)
  
  pipeline_message(describe_df(degre_lookup), process = "info")
  
  pipeline_message("Merged data successfully loaded", level = 1, 
                   progress = "end", process = "valid")
  
} else if (!file.exists(cfg_data$OSM_DEGRE_FILEPATH) || 
    isTRUE(cfg_data$FORCE_REJOIN_OSM_AND_COMMUNES)) {
  
  # ----------------------------------------------------------------------------
  # Load full OSM road dataset (GeoPackage)
  # ----------------------------------------------------------------------------
  
  pipeline_message(sprintf("Loading OSM road data from %s", 
                           rel_path(cfg_data$OSM_ROADS_FILEPATH)), 
                   level = 1, progress = "start", process = "load")
  
  # Load gpkg file
  osm_roads <- st_read(dsn = cfg_data$OSM_ROADS_FILEPATH, 
                       quiet = TRUE)
  
  # Project data into target CRS if needed
  if (sf::st_crs(osm_roads) != cfg_g$TARGET_CRS){
    pipeline_message(sprintf("Reproject into CRS %d", cfg_g$TARGET_CRS), 
                     process = "info")
    osm_roads <- osm_roads %>% st_transform(crs = cfg_g$TARGET_CRS)
  }
  
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
  
  pipeline_message(describe_df(osm_roads), process = "info")
  
  # Explicitly store in global environment (pipeline-style workflow)
  assign(x = "osm_roads", 
         value = osm_roads, 
         envir = .GlobalEnv)
  
  pipeline_message("OSM road data successfully loaded", level = 1, 
                   progress = "end", process = "valid")
  
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
    st_transform(cfg_g$TARGET_CRS)
  
  # ----------------------------------------------------------------------------
  # Load commune polygon data (urban density classification)
  # ----------------------------------------------------------------------------
  
  pipeline_message(
    sprintf("Loading commune data from %s", 
            rel_path(cfg_data$OSM_TYPOLOGIES_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  # Load shp file
  commune_data <- st_read(
    dsn = cfg_data$OSM_TYPOLOGIES_FILEPATH, 
    quiet = TRUE) %>% 
    select(DEGRE)   # Urban density category
  
  # Project data into target CRS if needed
  if (sf::st_crs(commune_data) != cfg_g$TARGET_CRS){
    pipeline_message(sprintf("Reproject into CRS %d", cfg_g$TARGET_CRS), 
                     process = "info")
    commune_data <- commune_data %>% st_transform(crs = cfg_g$TARGET_CRS)
  }
  
  # Safe simplification (topological)
  commune_data <- st_make_valid(x = commune_data)
  
  pipeline_message(describe_df(commune_data), process = "info")
  
  pipeline_message("Commune data successfully loaded", level = 1, 
                   progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Chunked spatial join: assign commune DEGREE to each road
  # ----------------------------------------------------------------------------
  
  pipeline_message("Spatial joining of OSM road and commune data", level = 1, 
                   progress = "start", process = "join")
  
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
  
  # Handle roads not intersecting any commune polygon (→ assign the nearest 
  # commune DEGREE)
  idx_missing <- which(is.na(degre_lookup$DEGRE))
  n_missing <- length(x = idx_missing)
  
  if (n_missing > 0) {
    
    pipeline_message(
      sprintf("%s roads not intersecting any commune, assigning nearest commune", 
              fmt(x = n_missing)), 
      process = "warning")
    
    # Compute nearest commune polygon for all missing roads in one call
    nearest_idx <- st_nearest_feature(
      x = degre_lookup[idx_missing, ], 
      y = commune_data)
    
    # Assign DEGREE of the nearest commune
    degre_lookup$DEGRE[idx_missing] <- commune_data$DEGRE[nearest_idx]
    
    pipeline_message(
      sprintf("Nearest communes assigned for %s roads", fmt(x = n_missing)), 
      process = "info")
  }
  
  # Save DEGREE lookup to disk
  saveRDS(object = degre_lookup, 
          file = cfg_data$OSM_DEGRE_FILEPATH)
  
  pipeline_message(describe_df(degre_lookup), process = "info")
  
  pipeline_message(
    sprintf("OSM road and commune data successfully joined and saved into file %s", 
            rel_path(cfg_data$OSM_DEGRE_FILEPATH)), 
    level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Build road network graph and compute topological metrics (connectivity, 
# betweenness, closeness, pagerank)
# ------------------------------------------------------------------------------

if (file.exists(cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH) && 
    isFALSE(cfg_data$FORCE_REJOIN_OSM_AND_COMMUNES)){
  
  pipeline_message(sprintf("Loading already built road network data from %s", 
                           rel_path(cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
                   level = 1, progress = "start", process = "load")
  
  osm_full_network <- sf::st_read(
    dsn = cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH,
    quiet = TRUE)
  
  pipeline_message("Road network data loaded", level = 1, 
                   progress = "end", process = "valid")
  
} else if (!file.exists(cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH) || 
    isTRUE(cfg_data$FORCE_REJOIN_OSM_AND_COMMUNES)) {
  
  pipeline_message("Building road network graph and compute topological metrics", 
                   level = 1, progress = "start", process = "calc")
  
  # Ensure osm_roads is available for network construction
  if (!exists("osm_roads")) {
    
    pipeline_message(
      sprintf("Loading OSM road data from %s (required for network graph)", 
              rel_path(cfg_data$OSM_ROADS_FILEPATH)), 
      level = 2, progress = "start", process = "load")
    
    # Load gpkg file
    osm_roads <- st_read(
      dsn = cfg_data$OSM_ROADS_FILEPATH,
      quiet = TRUE)
    
    # Project data into target CRS if needed
    if (sf::st_crs(osm_roads) != cfg_g$TARGET_CRS) {
      osm_roads <- osm_roads %>% st_transform(cfg_g$TARGET_CRS)
    }
    
    pipeline_message("OSM road data loaded for graph construction",
                     level = 2, progress = "end", process = "valid")
  }
  
  pipeline_message("Creating edge data from start/end points of each road", 
                   level = 2, progress = "start", process = "calc")
  
  # Ensure planar geometry operations (GEOS) for performance and robustness
  suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
  
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
    st_transform(cfg_g$TARGET_CRS)
  
  # ----------------------------------------------------------------------------
  # Road Endpoint Data
  # ----------------------------------------------------------------------------
  
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
  
  pipeline_message("Edge data successfully created for all roads", level = 2, 
                   progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Graph
  # ----------------------------------------------------------------------------
  
  # Build undirected graph
  pipeline_message("Creating igraph graph from edge data", level = 2, 
                   progress = "start", process = "calc")
  
  g <- igraph::graph_from_data_frame(d = edge_df[, c("from", "to")], 
                                     directed = FALSE)
  
  pipeline_message("Graph successfully created", level = 2, 
                   progress = "end", process = "valid")
  
  # Compute node-level metrics
  n_nodes <- igraph::vcount(graph = g)
  n_edges <- igraph::ecount(graph = g)
  
  pipeline_message(
    sprintf("Network built: %s nodes, %s edges", fmt(x = n_nodes), fmt(x = n_edges)), 
    process = "info")
  
  # ----------------------------------------------------------------------------
  # Centrality scores
  # ----------------------------------------------------------------------------
  
  pipeline_message("Computing centrality scores", level = 2, 
                   progress = "start", process = "calc")
  
  # Node connectivity
  pipeline_message("Computing node connectivity", process = "info")
  node_connectivity <- igraph::degree(graph = g)
  
  # PageRank scores
  pipeline_message("Computing node pagerank", process = "info")
  node_pagerank <- igraph::page_rank(graph = g, 
                                     directed = FALSE)$vector
  
  # Betweenness centralities of positions on undirected geodesics
  pipeline_message("Computing node betweenness", process = "info")
  node_betweenness <- igraph::betweenness(graph = g,  
                                  cutoff = cfg_data$CUTOFF_BETWEENNESS, 
                                  directed = FALSE)
  
  # Closeness centrality measures (how many steps is required to access every 
  # other vertex from a given one)
  pipeline_message("Computing node closeness", process = "info")
  node_closeness <- igraph::closeness(graph = g, 
                                      cutoff = cfg_data$CUTOFF_CLOSENESS, 
                                      mode = "all")

  # K-core index (node structural embeddedness)
  pipeline_message("Computing node coreness", process = "info")
  node_coreness <- igraph::coreness(graph = g, mode = "all")
  
  pipeline_message("Centrality scores computed", level = 2, 
                   progress = "start", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Edge-level metrics
  # ----------------------------------------------------------------------------
  
  pipeline_message("Computing edge-level metrics", level = 2, 
                   progress = "start", process = "calc")
  
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

  # Additional lightweight and predictive edge features
  edge_coreness <-
    (node_coreness[from_indices] + node_coreness[to_indices]) / 2
  edge_dead_end_score <-
    (as.numeric(node_connectivity[from_indices] == 1) +
       as.numeric(node_connectivity[to_indices] == 1)) / 2
  edge_length_m <- as.numeric(sf::st_length(roads_for_network))
  
  pipeline_message("Edge-level metrics computed", level = 2, 
                   progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Assembly of network feature tables
  # ----------------------------------------------------------------------------
  
  pipeline_message("Assembly of network feature tables", level = 2, 
                   progress = "start", process = "join")
  
  network_features <- data.frame(
    osm_id = roads_for_network$osm_id,
    connectivity = edge_connectivity,
    betweenness = edge_betweenness,
    closeness = edge_closeness,
    pagerank = edge_pagerank,
    coreness = edge_coreness,
    dead_end_score = edge_dead_end_score,
    edge_length_m = edge_length_m)
  
  # Memory cleanup
  rm(g, edge_df, node_connectivity, node_betweenness, 
      node_closeness, node_pagerank, node_coreness,
      edge_connectivity, edge_betweenness,
      edge_closeness, edge_pagerank, edge_coreness,
      edge_dead_end_score, edge_length_m,
     roads_for_network, unique_roads, edge_ends, from_indices, to_indices,
     coords_dt, start_pts, end_pts)
  gc(verbose = FALSE)
  
  pipeline_message("Feature tables assembled", level = 2, 
                   progress = "end", process = "valid")
  
  pipeline_message("Road network built and topological metrics computed", 
                   level = 1, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Extracting structured OSM attributes from "other_tags"
  # ----------------------------------------------------------------------------
  
  pipeline_message("Extracting structured OSM attributes from other_tags", 
                   level = 1, progress = "start", process = "calc")
  
  # Reload osm_roads if not in memory (happens when degre_lookup was cached)
  if (!exists("osm_roads")) {
    # Force garbage collection to reclaim memory before loading 1.6 Go GPKG
    gc(verbose = FALSE)
    
    pipeline_message(
      sprintf("Reloading OSM road data from %s", 
              rel_path(cfg_data$OSM_ROADS_FILEPATH)),
      level = 2, progress = "start", process = "load")
    
    osm_roads <- st_read(dsn = cfg_data$OSM_ROADS_FILEPATH, quiet = TRUE)
    
    high_traffic_types <- c(
      "motorway", "trunk", "primary", "secondary", "tertiary", 
      "unclassified", "residential", "motorway_link", "trunk_link", 
      "primary_link", "secondary_link", "tertiary_link")
    osm_roads <- osm_roads[osm_roads$highway %in% high_traffic_types, ]
    
    assign(x = "osm_roads", value = osm_roads, envir = .GlobalEnv)
    
    pipeline_message(
      text = "OSM road data reloaded for attribute extraction",
      level = 2, progress = "end", process = "valid")
  }
  
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
    extracted_tags <- extract_osm_other_tags(
      other_tags_vector = osm_roads$other_tags, 
      keys = osm_tags)
    
    # Add extracted tags to osm_roads
    for (tag in osm_tags) {
      col_name <- paste0(tag, "_osm")  # Add _osm suffix to avoid conflicts
      osm_roads[[col_name]] <- extracted_tags[[tag]]
    }
    pipeline_message(
      sprintf("Extracted %d OSM attributes \n", length(x = osm_tags)), 
      level = 1, progress = "end", process = "valid")
  }
  
  # Memory cleanup
  rm(extracted_tags, osm_tags)
  
  # Remove raw other_tags field
  osm_roads <- osm_roads %>% 
    select(-other_tags)
  
  pipeline_message("OSM attributes successfully extracted from other_tags", 
                   level = 1, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Final merge and export
  # ----------------------------------------------------------------------------
  
  pipeline_message("Final merging to build full road network with traffic data", 
                   level = 1, progress = "start", process = "join")
  
  pipeline_message("Merging OSM road network with commune urban density class data", 
                   level = 2, progress = "start", process = "calc")
  
  # Drop geometries in degre_lookup and network_features to save RAM
  # (geometry is already carried by osm_roads)
  if (inherits(degre_lookup, "sf")) {
    degre_lookup <- degre_lookup %>% st_drop_geometry()
  }
  if (inherits(network_features, "sf")) {
    network_features <- network_features %>% st_drop_geometry()
  }
  gc(verbose = FALSE)
  
  # Merge osm_roads and degre_lookup
  osm_full_network <- merge(x = osm_roads, 
                            y = degre_lookup, 
                            by = 'osm_id')
  
  # Free source objects immediately after merge to reclaim RAM
  rm(osm_roads, degre_lookup)
  gc(verbose = FALSE)
  
  pipeline_message("Road network successfully merged with commune density data", 
                   level = 2, progress = "end", process = "valid")
  
  # Merge osm_full_network and network_features
  pipeline_message("Merging OSM road network with network features", 
                   level = 2, progress = "start", process = "calc")
  osm_full_network <- merge(x = osm_full_network, 
                            y = network_features, 
                            by = 'osm_id')
  pipeline_message("Road network successfully merged with with network features", 
                   level = 2, progress = "end", process = "valid")
  
  # Free network_features after merge
  rm(network_features)
  gc(verbose = FALSE)
  pipeline_message(
    text = "Road network successfully merged with with network features", 
    level = 2, progress = "end", process = "valid")
  
  pipeline_message("Saving final road network", 
                   level = 2, progress = "start", process = "save")
  
  # Project data into target CRS if needed
  if (sf::st_crs(osm_full_network) != cfg_g$TARGET_CRS){
    pipeline_message(sprintf("Reproject into CRS %d", cfg_g$TARGET_CRS), 
                     process = "info")
    osm_full_network <- osm_full_network %>% st_transform(crs = cfg_g$TARGET_CRS)
  }
  
  # Write final road dataset
  start_timer()
  sf::st_write(
    osm_full_network,
    dsn = cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH,
    delete_dsn = TRUE,
    quiet = TRUE)
  
  pipeline_message(
    sprintf("Final road network saved in file %s", 
            rel_path(cfg_data$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
    level = 2, progress = "end", process = "valid")
}

# Cleanup large objects to free memory for next steps
rm(list = intersect(ls(), c("osm_roads", "commune_data", "degre_lookup",
                            "unique_roads", "edges", "g", "node_data")))
gc(verbose = FALSE)
