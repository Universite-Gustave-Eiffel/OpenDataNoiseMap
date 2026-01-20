# ==============================================================================
# STAGE 2: OSM PROCESSING - MINIMAL VERSION
# ==============================================================================

pipeline_message(text = "Merging OSM road and commune data", 
                 level = 0, progress = "start", process = "join")

if (file.exists(CONFIG$OSM_DEGRE_FILEPATH) && 
    isFALSE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)){
  
  pipeline_message(
    text = paste0("Loading already merged OSM road and commune data from ", 
                  rel_path(CONFIG$OSM_DEGRE_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  degre_lookup <- readRDS(file = CONFIG$OSM_DEGRE_FILEPATH)
  
  pipeline_message(text = "Merged data successfully loaded!", 
                   level = 1, progress = "end", process = "valid")
  
} else if (!file.exists(CONFIG$OSM_DEGRE_FILEPATH) || 
    isTRUE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)) {
  
  # ----------------------------------------------------------------------------
  # Load full OSM road dataset (GeoPackage)
  # ----------------------------------------------------------------------------
  
  pipeline_message(
    text = paste0("Loading OSM road data from ", 
                  rel_path(CONFIG$OSM_ROADS_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
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
  
  pipeline_message(text = paste0("OSM road data successfully loaded!", 
                                 rel_path(CONFIG$OSM_ROADS_FILEPATH)),  
                   level = 1, progress = "end", process = "valid")
  
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
  
  pipeline_message(
    text = paste0("Loading commune data from ", 
                  rel_path(CONFIG$OSM_TYPOLOGIES_FILEPATH)), 
    level = 1, progress = "start", process = "load")
  
  commune_data <- st_read(
    dsn = CONFIG$OSM_TYPOLOGIES_FILEPATH, 
    quiet = TRUE) %>% 
    select(DEGRE) %>%   # Urban density category
    st_transform(CONFIG$TARGET_CRS)
  
  # Safe simplification (topological)
  commune_data <- st_make_valid(x = commune_data)
  
  pipeline_message(
    text = paste0("Commune data successfully loaded!", 
                  rel_path(CONFIG$OSM_TYPOLOGIES_FILEPATH)), 
    level = 1, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Chunked spatial join: assign commune DEGREE to each road
  # ----------------------------------------------------------------------------
  
  pipeline_message(text = "Spatial joining of OSM road and commune data",  
                   level = 1, progress = "start", process = "join")
  
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
    text = sprintf("%s roads not intersecting any commune, assigning nearest 
                   commune", fmt(x = n_missing)), 
    level = 4, process = "warning")
    
    # Compute nearest commune polygon for all missing roads in one call
    nearest_idx <- st_nearest_feature(
      x = degre_lookup[idx_missing, ], 
      y = commune_data)
    
    # Assign DEGREE of the nearest commune
    degre_lookup$DEGRE[idx_missing] <- commune_data$DEGRE[nearest_idx]
    
    pipeline_message(
      text = sprintf("Nearest communes assigned for %s roads", 
                     fmt(x = n_missing)), 
      level = 4, process = "valid")
  }
  
  # Save DEGREE lookup to disk
  saveRDS(object = degre_lookup, 
          file = CONFIG$OSM_DEGRE_FILEPATH)
  
  pipeline_message(
    text = paste0("OSM road and commune data successfully joined and saved into 
                  file ", rel_path(CONFIG$OSM_DEGRE_FILEPATH)), 
    level = 1, progress = "end", process = "valid")
}

# ------------------------------------------------------------------------------
# Build road network graph and compute topological metrics (connectivity, 
# betweenness, closeness, pagerank)
# ------------------------------------------------------------------------------

if (file.exists(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH) && 
    isFALSE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)){
  
  pipeline_message(text = "Loading already built road network data",  
                   level = 1, progress = "start", process = "load")
  
  osm_full_network <- sf::st_read(
    dsn = CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH,
    quiet = TRUE)
  
  pipeline_message(text = "Road network data loaded",
                   level = 1, progress = "end", process = "valid")
  
} else if (!file.exists(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH) || 
    isTRUE(CONFIG$FORCE_REJOIN_OSM_AND_COMMUNES)) {
  
  pipeline_message(
    text = "Building road network graph and compute topological metrics", 
    level = 1, progress = "start", process = "calc")
  
  pipeline_message(
  text = "Creating edge data from start/end points of each road", 
  level = 2, progress = "start", process = "calc")
  
  # Ensure planar geometry operations (GEOS) for performance and robustness
  suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
  
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
  
  pipeline_message(text = "Edge data successfully created for all roads", 
                   level = 2, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Graph
  # ----------------------------------------------------------------------------
  
  # Build undirected graph
  pipeline_message(text = "Creating igraph graph from edge data", 
                   level = 2, progress = "start", process = "calc")
  
  g <- igraph::graph_from_data_frame(d = edge_df[, c("from", "to")], 
                                     directed = FALSE)
  
  pipeline_message(text = "Graph successfully created", 
                   level = 2, progress = "end", process = "valid")
  
  # Compute node-level metrics
  n_nodes <- igraph::vcount(graph = g)
  n_edges <- igraph::ecount(graph = g)
  
  pipeline_message(
    text = sprintf("Network built: %s nodes, %s edges", 
                   fmt(x = n_nodes), fmt(x = n_edges)), 
    level = 4, process = "info")
  
  # ----------------------------------------------------------------------------
  # Centrality scores
  # ----------------------------------------------------------------------------
  
  pipeline_message(text = "Computing centrality scores", 
                   level = 2, progress = "start", process = "calc")
  
  # Node connectivity
  pipeline_message(text = "Computing node connectivity", 
                   level = 3, progress = "start", process = "calc")
  node_connectivity <- igraph::degree(graph = g)
  pipeline_message(text = "Node connectivity computed", 
                   level = 3, progress = "end", process = "valid")
  
  # PageRank scores
  pipeline_message(text = "Computing node pagerank", 
                   level = 3, progress = "start", process = "calc")
  node_pagerank <- igraph::page_rank(graph = g, 
                                     directed = FALSE)$vector
  pipeline_message(text = "Node pagerank computed", 
                   level = 3, progress = "end", process = "valid")
  
  # Betweenness centralities of positions on undirected geodesics
  pipeline_message(text = "Computing node betweenness", 
                   level = 3, progress = "start", process = "calc")
  node_betweenness <- igraph::betweenness(graph = g,  
                                  cutoff = CONFIG$CUTOFF_BETWEENNESS, 
                                  directed = FALSE)
  pipeline_message(text = "Node betweenness computed", 
                   level = 3, progress = "end", process = "valid")
  
  # Closeness centrality measures (how many steps is required to access every 
  # other vertex from a given one)
  pipeline_message(text = "Computing node closeness", 
                   level = 3, progress = "start", process = "calc")
  node_closeness <- igraph::closeness(graph = g, 
                                      cutoff = CONFIG$CUTOFF_CLOSENESS, 
                                      mode = "all")
  pipeline_message(text = "Node closeness computed", 
                   level = 3, progress = "end", process = "valid")
  
  pipeline_message(text = "Centrality scores computed", 
                   level = 2, progress = "start", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Edge-level metrics
  # ----------------------------------------------------------------------------
  
  pipeline_message(text = "Computing edge-level metrics", 
                   level = 2, progress = "start", process = "calc")
  
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
  
  pipeline_message(text = "Edge-level metrics computed", 
                   level = 2, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Assembly of network feature tables
  # ----------------------------------------------------------------------------
  
  pipeline_message(text = "Assembly of network feature tables", 
                   level = 2, progress = "start", process = "join")
  
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
  
  pipeline_message(text = "Feature tables assembled", 
                   level = 2, progress = "end", process = "valid")
  
  pipeline_message(text = "Road network built and topological metrics computed", 
                   level = 1, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Extracting structured OSM attributes from "other_tags"
  # ----------------------------------------------------------------------------
  
  pipeline_message(text = "Extracting structured OSM attributes from other_tags", 
                   level = 1, progress = "start", process = "calc")
  
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
    pipeline_message(
      text = sprintf("Extracted %d OSM attributes \n", length(x = osm_tags)), 
      level = 1, progress = "end", process = "valid")
  }
  
  # Memory cleanup
  rm(extracted_tags, osm_tags)
  
  # Remove raw other_tags field
  osm_roads <- osm_roads %>% 
    select(-other_tags)
  
  pipeline_message(
    text = "OSM attributes successfully extracted from other_tags", 
    level = 1, progress = "end", process = "valid")
  
  # ----------------------------------------------------------------------------
  # Final merge and export
  # ----------------------------------------------------------------------------
  
  pipeline_message(
    text = "Final merging to build full road network with traffic data", 
    level = 1, progress = "start", process = "join")
  
  pipeline_message(
    text = "Merging OSM road network with commune urban density class data", 
    level = 2, progress = "start", process = "calc")
  
  # Drop geometries in degre_lookup and network_features
  degre_lookup <-  degre_lookup %>% st_drop_geometry()
  network_features <- network_features %>% st_drop_geometry()
  
  # Merge osm_roads and degre_lookup
  osm_full_network <- merge(x = osm_roads, 
                            y = degre_lookup, 
                            by = 'osm_id')
  pipeline_message(
    text = "Road network successfully merged with commune density data", 
    level = 2, progress = "end", process = "valid")
  
  # Merge osm_full_network and network_features
  pipeline_message(text = "Merging OSM road network with network features", 
                   level = 2, progress = "start", process = "calc")
  osm_full_network <- merge(x = osm_full_network, 
                            y = network_features, 
                            by = 'osm_id')
  pipeline_message(
    text = "Road network successfully merged with with network features", 
    level = 2, progress = "end", process = "valid")
  
  pipeline_message(text = "Saving final road network", 
                   level = 2, progress = "start", process = "save")
  
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
  
  pipeline_message(
    text = paste0("Final road network saved in file ", 
                  rel_path(CONFIG$OSM_ROADS_CONNECTIVITY_FILEPATH)), 
    level = 2, progress = "end", process = "valid")
}