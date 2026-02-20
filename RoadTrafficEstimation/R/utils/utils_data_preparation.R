# ==============================================================================
# UTILITIES: DATA PREPARATION PHASE
# ==============================================================================
# Module d'utilitaires spécifiques à la phase de préparation des données.
# Ce module étend les utilitaires génériques (utils_osm.R, utils_sf.R, etc.)
# avec des fonctions spécifiques aux étapes 01-06.
# ==============================================================================

# Note: utils_osm.R, utils_sf.R, utils_avatar.R are already loaded by bootstrap.R

# ------------------------------------------------------------------------------
# Fonctions supplémentaires spécifiques à la préparation des données
# ------------------------------------------------------------------------------

#' Validate OSM network structure
#'
#' @param osm_network sf data.frame with OSM roads
#' @return logical TRUE if valid, FALSE otherwise
validate_osm_network <- function(osm_network) {
  required_cols <- c("osm_id", "highway", "geom")
  missing_cols <- setdiff(required_cols, names(osm_network))
  
  if (length(missing_cols) > 0) {
    pipeline_message(
      text = sprintf("Missing required columns: %s", 
                     paste(missing_cols, collapse = ", ")),
      process = "error")
    return(FALSE)
  }
  
  if (nrow(osm_network) == 0) {
    pipeline_message(text = "OSM network is empty", process = "error")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Validate Avatar data structure
#'
#' @param avatar_data data.frame with Avatar traffic data
#' @return logical TRUE if valid, FALSE otherwise
validate_avatar_data <- function(avatar_data) {
  required_cols <- c("count_point_id", "period", "aggregate_flow")
  missing_cols <- setdiff(required_cols, names(avatar_data))
  
  if (length(missing_cols) > 0) {
    pipeline_message(
      text = sprintf("Missing required columns: %s", 
                     paste(missing_cols, collapse = ", ")),
      process = "error")
    return(FALSE)
  }
  
  if (nrow(avatar_data) == 0) {
    pipeline_message(text = "Avatar data is empty", process = "error")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Check feature completeness in engineered network
#'
#' @param network data.frame with engineered features
#' @return list with completeness statistics
check_feature_completeness <- function(network) {
  required_features <- c(
    "highway", "DEGRE", "ref_letter", "first_word",
    "oneway_osm", "lanes_osm", "lanes_directional",
    "speed", "junction_osm",
    "connectivity", "betweenness", "closeness", "pagerank",
    "coreness", "dead_end_score", "edge_length_m"
  )
  
  available <- intersect(required_features, names(network))
  missing <- setdiff(required_features, names(network))
  
  # Check for NA values in available features
  na_counts <- sapply(available, function(col) {
    sum(is.na(network[[col]]))
  })
  
  list(
    total_features = length(required_features),
    available_features = length(available),
    missing_features = missing,
    na_counts = na_counts,
    completeness_pct = (length(available) / length(required_features)) * 100
  )
}
