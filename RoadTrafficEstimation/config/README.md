# Configuration Files Overview

This project relies on several R configuration files to centralize parameters used across the data preparation, training, and forecasting pipelines. Each configuration file defines a structured list of constants and settings that control execution behavior, file locations, and model parameters.

------------------------------------------------------------------------

## `mode_registry.R`

### 🎯 Purpose

Defines the pipeline execution mode.

### ℹ️ Pipeline execution modes

-   `data_prep`: Data preparation (OSM + Avatar).
-   `avatar`: Avatar data download.
-   `training`: Learning model training.
-   `nantes`: Forecast for the southern district of the city of Nantes.
-   `paris`: Forecast for the city of Paris.
-   `sensors`: Forecast on a sensor network.

------------------------------------------------------------------------

## `config_global.R`

### 🎯 Purpose

Defines global settings shared by all pipelines, including execution context, coordinate reference system, and project directory structure.

### 🌍 Spatial Reference

-   `TARGET_CRS`: Target coordinate reference system (EPSG:2154 -- Lambert-93).

### 📂 Project Structure

-   `ROOT_DIR`: Root directory of the project.
-   `DATA_DIR`: Base data directory.
-   `FIGS_DIR`: Directory for generated figures.
-   `LOGS_DIR`: Directory for logs.

------------------------------------------------------------------------

## `config_data_prep.R`

### 🎯 Purpose

Controls all parameters related to data acquisition, preprocessing, spatial joins, and enrichment of OSM and AVATAR datasets.

### 🌍 OSM (OpenStreetMap)

**🚩 Process Forcing**

-   `FORCE_REJOIN_OSM_AND_COMMUNES`: Forces regeneration of OSM–commune joins.

**📂 Directories and Files**

-   Paths to OSM `*.pbf`, `.*gpkg`, `.shp` and derived datasets.

**📏 Spatial Parameters**

-   `CUTOFF_BETWEENNESS`: Maximum path length for betweenness computation.
-   `CUTOFF_CLOSENESS`: Maximum path length for closeness computation.

**🎫 Default Allocation Values**

-   Default values for missing road attributes (number of lanes, vehicle speeds, degree, etc.).

### 🚗 AVATAR Traffic Data

**🚩 Process Forcing**

-   Flags to force re-download or regeneration of AVATAR data and chunks.

**📂 Directories and Files**

-   Paths to raw, intermediate, and aggregated AVATAR datasets (`*.json`, `*.csv`, `*.rds`, `.*gpkg`).

**📥 Download Settings**

-   `START_TIME`, `END_TIME`: Time range for data extraction.
-   `AVATAR_API_TOKEN`: API token (read from environment variables).

**📏 Spatial Parameters**

-   `BUFFER_RADIUS`: Buffer radius (meters) for matching Avatat count points to OSM roads.

### 📊 Figures

-   Default filenames for generated analysis and diagnostic figures.

------------------------------------------------------------------------

## `config_training.R`

### 🎯 Purpose

Defines paths and hyperparameters for training machine learning models used in traffic and noise prediction.

### 📂 Directories and Files

-   Locations of training datasets, models, and feature metadata.

### 🎓 Training Configuration

**📋 General Training Parameters**

-   XGBoost hyperparameters for general traffic models.

**🚚 Truck-Specific Parameters**

-   Dedicated hyperparameter set optimized for heavy vehicle prediction.

**📍 Boosting Settings**

-   `NROUNDS`: Number of boosting iterations.

------------------------------------------------------------------------

## `config_forecast.R`

### 🎯 Purpose

Specifies output locations for forecasted datasets generated from trained models.

### 📂 Directories and Files

-   `FORECAST_DIR`: Base directory for forecast outputs.
-   Filepaths for sensor-level and city-scale (Paris, Nantes) forecast results.

------------------------------------------------------------------------

## 🔍 Usage Notes

-   All configuration objects are defined as named lists and are intended to be sourced at the beginning of each pipeline.
-   Environment variables are used for sensitive or context-dependent values (*e.g.* API tokens, execution context).

This modular configuration design improves reproducibility, readability, and maintainability of the full data and modeling pipeline.

------------------------------------------------------------------------
