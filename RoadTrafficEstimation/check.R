library(sf)

gpkg_path <- "./RoadTrafficEstimation/data/osm/gpkg/02_osm_network_france_engineered.gpkg"

st_layers(gpkg_path)

x <- st_read(gpkg_path, quiet = FALSE)
dim(x)
names(x)