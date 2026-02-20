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
