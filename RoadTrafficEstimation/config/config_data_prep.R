# ==============================================================================
# Data preparation pipeline configuration
# ==============================================================================

pipeline_message("Definition of the data preparation pipeline configuration",
                 level = 0, progress = "start", process = "install")

# OSM paths
OSM_DIR      <- file.path(DATA_DIR, "osm")
OSM_PBF_DIR  <- file.path(OSM_DIR, "pbf")
OSM_GPKG_DIR <- file.path(OSM_DIR, "gpkg")
OSM_SHP_DIR  <- file.path(OSM_DIR, "shp")
OSM_RDS_DIR  <- file.path(OSM_DIR, "rds")

# Avatar paths
AVATAR_DIR          <- file.path(DATA_DIR, "avatar")
AVATAR_RDS_DATA_DIR <- file.path(AVATAR_DIR, "rds")
AVATAR_CSV_DATA_DIR <- file.path(AVATAR_DIR, "csv")
AVATAR_JSON_DIRPATH <- file.path(AVATAR_DIR, "json")
AVATAR_GPKG_DIRPATH <- file.path(AVATAR_DIR, "gpkg")

# Data preparation configuration list
CONFIG_DATA_PREP <- list(
  
  # ----------------------------------------------------------------------------
  # OSM
  # ----------------------------------------------------------------------------
  
  # *************** #
  # Process forcing #
  # *************** #
  FORCE_REJOIN_OSM_AND_COMMUNES = FALSE,
  
  # ********************* #
  # Directories and files #
  # ********************* #
  OSM_DIR                         = OSM_DIR,
  OSM_PBF_DIR                     = OSM_PBF_DIR,
  OSM_GPKG_DIR                    = OSM_GPKG_DIR,
  OSM_SHP_DIR                     = OSM_SHP_DIR,
  OSM_ROADS_FILEPATH              = file.path(OSM_GPKG_DIR,
                                              "france-latest.osm.gpkg"),
  OSM_TYPOLOGIES_FILEPATH         = file.path(OSM_SHP_DIR,
                                              "COMMUNE_TYPO_DENSITE.shp"),
  OSM_DEGRE_FILEPATH              = file.path(OSM_RDS_DIR, "degre.rds"),
  OSM_ROADS_CONNECTIVITY_FILEPATH = file.path(OSM_GPKG_DIR,
                   "osm_roads_france_including_connectivity_and_communes.gpkg"), 
  
  # ****************** #
  # Spatial parameters #
  # ****************** #
  # Maximum path length to consider when calculating the betweenness
  CUTOFF_BETWEENNESS = 100, 
  # Maximum path length to consider when calculating the closeness
  CUTOFF_CLOSENESS   = 20, 
  
  # ************************* #
  # Default allocation values #
  # ************************* #
  DEFAULT_NUMBER_OF_LANES = 2, 
  DEFAULT_VEHICLE_SPEED   = 2, 
  DEFAULT_NUMBER_OF_ROADS = 0, 
  DEFAULT_DEGRE           = 1, 
  
  # ----------------------------------------------------------------------------
  # AVATAR
  # ----------------------------------------------------------------------------
  
  # *************** #
  # Process forcing #
  # *************** #
  FORCE_REDOWNLOAD_COUNT_POINTS           = FALSE, 
  FORCE_REDOWNLOAD_CHUNKS                 = FALSE, 
  FORCE_REDOWNLOAD_MISSING_INVALID_CHUNKS = FALSE, 
  
  # ********************* #
  # Directories and files #
  # ********************* #
  AVATAR_DIR                       = AVATAR_DIR, 
  AVATAR_RDS_DATA_DIR              = AVATAR_RDS_DATA_DIR, 
  AVATAR_CSV_DATA_DIR              = AVATAR_CSV_DATA_DIR, 
  AVATAR_COUNT_POINTS_FILEPATH     = file.path(AVATAR_JSON_DIRPATH, 
                                               "count_points.json"), 
  AVATAR_RDS_DATA_FILEPATH         = file.path(AVATAR_RDS_DATA_DIR, 
                                               "avatar_data.rds"), 
  AVATAR_AGGREGATED_FILEPATH       = file.path(AVATAR_RDS_DATA_DIR, 
                                               "avatar_aggregated_hourly_period.rds"), 
  AVATAR_AGGREGATED_CLEAN_FILEPATH = file.path(AVATAR_RDS_DATA_DIR, 
                                               "avatar_aggregated_clean.rds"), 
  AVATAR_MERGED_WITH_OSM_FILEPATH  = file.path(AVATAR_GPKG_DIRPATH, 
                                               "avatar_merged_with_osm.gpkg"), 
  AVATAR_IMPUTATION_RULES_FILEPATH = file.path(AVATAR_RDS_DATA_DIR, 
                                               "avatar_imputation_rules.rds"), 
  AVATAR_NETWORK_DATA_FILEPATH     = file.path(AVATAR_DIR, 
                                               "avatar_clean_network.rds"), 
  
  # ***************** #
  # Download settings #
  # ***************** #
  START_TIME       = "2023-01-01T00:00:00", 
  END_TIME         = "2023-12-31T00:00:00", 
  AVATAR_API_TOKEN = Sys.getenv("AVATAR_API_TOKEN"), 
  
  # ****************** #
  # Spatial parameters #
  # ****************** #
  # Radius (in meters) used to match counting points to nearest OSM roads
  BUFFER_RADIUS = 50, 

  # ----------------------------------------------------------------------------
  # Figures
  # ----------------------------------------------------------------------------
  
  FIG_HOURLY_TRAFFIC_FILENAME               = "hourly_traffic_patterns.pdf", 
  FIG_SPEED_AND_TRUCK_PERCENTAGE            = "speed_and_truck_percentage.pdf", 
  FIG_TRAFFIC_PERIOD_COMPARISONS            = "traffic_period_comparisons.pdf", 
  FIG_TRAFFIC_FLOW_DISTRIB_AND_DATA_QUALITY = 
                              "traffic_flow_distribution_and_data_quality.pdf"
)

pipeline_message("Data preparation configuration loaded",
                 level = 0, progress = "end", process = "valid")
