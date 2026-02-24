cat("============================================================\n")
cat("   FULL DATA CONSISTENCY DIAGNOSTIC\n")
cat("============================================================\n\n")

library(sf)
library(tools)

# ------------------------------------------------------------------
# PATHS
# ------------------------------------------------------------------

gpkg_test  <- "RoadTrafficEstimation/data/osm/gpkg/02_osm_network_france_engineered.gpkg"
gpkg_ref   <- "RoadTrafficEstimation/data/data_pierre/02_osm_network_france_engineered.gpkg"

models_test <- "RoadTrafficEstimation/data/training/rds/06_xgboost_trained_models.rds"
models_ref  <- "RoadTrafficEstimation/data/data_pierre/06_xgboost_trained_models.rds"

feat_test <- "RoadTrafficEstimation/data/training/rds/06_xgboost_feature_info.rds"
feat_ref  <- "RoadTrafficEstimation/data/data_pierre/06_xgboost_feature_info.rds"

# ------------------------------------------------------------------
# 1️⃣ FILE SIZE CHECK
# ------------------------------------------------------------------

cat("---- FILE SIZE CHECK ----\n")

sizes <- data.frame(
  file = c("gpkg_test","gpkg_ref","models_test","models_ref","feat_test","feat_ref"),
  size_MB = round(file.info(c(gpkg_test,gpkg_ref,
                               models_test,models_ref,
                               feat_test,feat_ref))$size / 1024^2, 2)
)

print(sizes)
cat("\n")

# ------------------------------------------------------------------
# 2️⃣ GPKG STRUCTURE COMPARISON
# ------------------------------------------------------------------

cat("---- GPKG STRUCTURE CHECK ----\n")

layers_test <- st_layers(gpkg_test)
layers_ref  <- st_layers(gpkg_ref)

cat("Layers test:\n")
print(layers_test)

cat("\nLayers ref:\n")
print(layers_ref)

# Read small sample (avoid loading full France if huge)
n_max = 100
cat(sprintf("\nReading %d features from each file...\n", n_max))

x_test <- st_read(gpkg_test, n_max = n_max, quiet = TRUE)
x_ref  <- st_read(gpkg_ref, n_max = n_max, quiet = TRUE)

cat("Dimensions (test vs ref):\n")
print(dim(x_test))
print(dim(x_ref))

cat("\nColumn name differences:\n")
print(setdiff(names(x_test), names(x_ref)))
print(setdiff(names(x_ref), names(x_test)))

cat("\nCRS comparison:\n")
print(st_crs(x_test))
print(st_crs(x_ref))

cat("\nGeometry type comparison:\n")
print(unique(st_geometry_type(x_test)))
print(unique(st_geometry_type(x_ref)))

cat("\nMissing required columns in TEST:\n")
required_cols <- names(x_ref)
print(setdiff(required_cols, names(x_test)))

# ------------------------------------------------------------------
# 3️⃣ MODELS COMPARISON
# ------------------------------------------------------------------

cat("\n---- XGBOOST MODELS CHECK ----\n")

models_test_obj <- readRDS(models_test)
models_ref_obj  <- readRDS(models_ref)

cat("Class:\n")
print(class(models_test_obj))
print(class(models_ref_obj))

cat("\nNumber of models:\n")
print(length(models_test_obj))
print(length(models_ref_obj))

cat("\nPeriod name differences:\n")
print(setdiff(names(models_test_obj), names(models_ref_obj)))
print(setdiff(names(models_ref_obj), names(models_test_obj)))

cat("\nClass of first model:\n")
print(class(models_test_obj[[1]]))
print(class(models_ref_obj[[1]]))

# ------------------------------------------------------------------
# 4️⃣ FEATURE INFO COMPARISON
# ------------------------------------------------------------------

cat("\n---- FEATURE INFO CHECK ----\n")

feat_test_obj <- readRDS(feat_test)
feat_ref_obj  <- readRDS(feat_ref)

cat("Structure TEST:\n")
str(feat_test_obj, max.level = 1)

cat("\nStructure REF:\n")
str(feat_ref_obj, max.level = 1)

cat("\nall_periods differences:\n")
print(setdiff(feat_test_obj$all_periods, feat_ref_obj$all_periods))
print(setdiff(feat_ref_obj$all_periods, feat_test_obj$all_periods))

cat("\nFeature name differences:\n")
print(setdiff(feat_test_obj$features, feat_ref_obj$features))
print(setdiff(feat_ref_obj$features, feat_test_obj$features))

# ------------------------------------------------------------------
# 5️⃣ CRITICAL CONSISTENCY CHECK
#    GPKG vs feature_info vs models
# ------------------------------------------------------------------

cat("\n---- CRITICAL PIPELINE CONSISTENCY CHECK ----\n")

x_test_df <- st_drop_geometry(x_test)

missing_features_in_gpkg <- setdiff(feat_test_obj$features,
                                     names(x_test_df))

cat("Features required by models but missing in TEST GPKG:\n")
print(missing_features_in_gpkg)

missing_period_models <- setdiff(feat_test_obj$all_periods,
                                 names(models_test_obj))

cat("\nPeriods required but missing in TEST models:\n")
print(missing_period_models)

cat("\n============================================================\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("============================================================\n")