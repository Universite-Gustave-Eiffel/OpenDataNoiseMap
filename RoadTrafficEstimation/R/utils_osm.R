#' @title Download an OpenStreetMap PBF file from Geofabrik
#' @description This function downloads a `.osm.pbf` file from the Geofabrik 
#'              download server for France or one of its regions. Before 
#'              downloading, it attempts to retrieve and display the file size 
#'              using an HTTP HEAD request.
#'              The function:
#'              \itemize{
#'                \item Validates the requested region, 
#'                \item Stops execution if data are unavailable, 
#'                \item Automatically converts region names into valid Geofabrik 
#'                    URLs.
#'              }
#' @param region Character string. Either `"France"` or the name of a French 
#'               region (e.g. `"Provence-Alpes-Côte d'Azur"`, `"Rhône-Alpes"`). 
#'               Default is `FALSE`.
#' @param dest_dir Character string. Directory where the file will be 
#'                 downloaded. Default is the current working directory.
#' @param overwrite Logical. Whether to overwrite an existing file. Default is 
#'                  `FALSE`.
#' @return The full path to the downloaded `.osm.pbf` file (invisibly).
#' @examples
#' \dontrun{
#' download_geofabrik_pbf("France")
#' download_geofabrik_pbf(dest_dir = "data/osm", region = "Bretagne")
#' }
#' @export
download_geofabrik_pbf <- function(region = "France", 
                                   dest_dir = ".", 
                                   overwrite = FALSE) {
  # ----------------------------------------------------------------------------
  # Supported regions
  # ----------------------------------------------------------------------------
  available_regions <- c(
    "France",
    "Alsace", "Aquitaine", "Auvergne", "Basse-Normandie", "Bourgogne",
    "Bretagne", "Centre", "Champagne-Ardenne", "Corse", "Franche-Comté",
    "Guadeloupe", "Guyane", "Haute-Normandie", "Île-de-France",
    "Languedoc-Roussillon", "Limousin", "Lorraine", "Midi-Pyrénées",
    "Nord-Pas-de-Calais", "Pays de la Loire", "Picardie",
    "Poitou-Charentes", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes"
  )
  unavailable_regions <- c(
    "Martinique", "Mayotte", "Réunion"
  )
  
  pipeline_message(
    text = "Verification of the zone name entered", 
    level = 2, progress = "start", process = "search")
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  if (region %in% unavailable_regions) {
    pipeline_message(
      text = sprintf("OSM data for region '%s' are not available on Geofabrik", 
                    region), 
      level = 4, process = "stop")
    stop(call. = FALSE)
  }
  
  if (!region %in% c(available_regions, unavailable_regions)) {
    pipeline_message(
      text = sprintf("Unknown region '%s'. Please provide a valid French region 
                     name.", region), 
      level = 4, process = "stop")
    stop(call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # Build URL
  # ---------------------------------------------------------------------------
  base_url <- "https://download.geofabrik.de/europe/"
  # Format region name for the URL
  region_slug <- function(x) {
    x |>
      tolower() |>
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
      gsub(pattern = "[[:space:]']", replacement = "-", x = _) |>
      gsub(pattern = "-+", replacement = "-", x = _) |>
      gsub(pattern = "^-|-$", replacement = "", x = _)
  }
  if (region == "France") {
    pbf_name <- "france-latest.osm.pbf"
    url <- paste0(base_url, pbf_name)
  } else {
    slug <- region_slug(region)
    pbf_name <- paste0(slug, "-latest.osm.pbf")
    url <- paste0(base_url, "france/", pbf_name)
  }
  
  pipeline_message(
    text = paste0("URL: ", url), level = 4, process = "valid")
  pipeline_message(
    text = paste0("PBF file name: ", pbf_name), level = 4, process = "valid")
  
  # Destination file name
  dest_file <- file.path(dest_dir, pbf_name)
  
  if (file.exists(dest_file) && !overwrite) {
    pipeline_message(
      text = paste0("File already exists: ", dest_file), 
      level = 4, process = "valid")
    return(invisible(dest_file))
  }
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---------------------------------------------------------------------------
  # Retrieve file size (HEAD request)
  # ---------------------------------------------------------------------------
  
  file_size_mb <- NA_real_
  head_cmd <- sprintf("curl -sI %s", shQuote(url))
  headers <- try(system(head_cmd, intern = TRUE), silent = TRUE)
  
  if (!inherits(headers, "try-error")) {
    size_line <- headers[grepl("Content-Length", headers)]
    if (length(size_line) == 1) {
      size_bytes <- as.numeric(gsub(".*: ", "", size_line))
      file_size_mb <- size_bytes / 1024^2
      pipeline_message(
        text = sprintf("File size: %.1f MB", file_size_mb), 
        level = 4, process = "pack")
    }
  }
  
  if (is.na(file_size_mb)) {
    pipeline_message(
      text = "File size: unavailable (server did not provide Content-Length)", 
      level = 4, process = "pack")
  }
  
  pipeline_message(
    text = sprintf("Zone name ok, destination URL ready and directory %s 
                   successfully created", rel_path(dest_file)), 
    level = 2, progress = "end", process = "valid")
  
  # ---------------------------------------------------------------------------
  # Download
  # ---------------------------------------------------------------------------
  pipeline_message(
    text = "Downloading file", 
    level = 2, progress = "start", process = "download")
  
  # ---------------------------------------------------------------------------
  # Download using curl (robust for large files)
  # ---------------------------------------------------------------------------
  
  cmd <- sprintf(
    "curl -L --fail --retry 5 --retry-delay 10 --continue-at - -o %s %s",
    shQuote(dest_file),
    shQuote(url)
  )
  
  status <- system(cmd)
  
  if (status != 0) {
    pipeline_message(
      text = "Download failed (curl returned a non-zero status)", 
      level = 4, process = "stop")
    stop(call. = FALSE)
  }
  
  pipeline_message(
    text = sprintf("Download successfully completed. File saved as %s", 
                   rel_path(dest_file)), 
    level = 2, progress = "end", process = "valid")
  
  invisible(dest_file)
}
#' 
#' @title Convert an OSM PBF file to GeoPackage format
#' @description This function converts an OpenStreetMap `.osm.pbf` file into a 
#'              `.gpkg` file using the GDAL command-line tool `ogr2ogr`. 
#'              GDAL must be available in the system environment.
#' @param pbf_file Character string. Path to the input `.osm.pbf` file.
#' @param gpkg_file Character string. Path to the output `.gpkg` file.
#' @param overwrite Logical. Whether to overwrite an existing GeoPackage. 
#'                  Default is `FALSE`.
#' @return The path to the generated `.gpkg` file (invisibly).
#' @examples
#' \dontrun{
#' convert_pbf_to_gpkg("data/osm/france-latest.osm.pbf", 
#'                     "data/osm/france.osm.gpkg")
#' }
#' @export
convert_pbf_to_gpkg <- function(pbf_file,
                                gpkg_file,
                                overwrite = FALSE) {
  pipeline_message(
    text = "Conversion of the OSM PBF file to GeoPackage format",  
    level = 2, progress = "start", process = "calc")
  
  if (!file.exists(pbf_file)) {
    pipeline_message(
      text = paste0("Input PBF file does not exist: ", pbf_file), 
      level = 4, process = "stop")
    stop(call. = FALSE)
  }
  
  if (file.exists(gpkg_file)) {
    if (!overwrite) {
      pipeline_message(
        text = paste0("Output GPKG file already exists: ", gpkg_file), 
        level = 4, process = "stop")
      stop(call. = FALSE)
    } else {
      file.remove(gpkg_file)
    }
  }
  
  # Check GDAL availability
  if (system("ogr2ogr --version", intern = TRUE, ignore.stderr = TRUE) |> length() == 0) {
    pipeline_message(
      text = "GDAL (ogr2ogr) is not available in the system environment", 
      level = 4, process = "stop")
    stop(call. = FALSE)
  }
  
  message("🔄 Converting PBF to GeoPackage...")
  message("   Input : ", pbf_file)
  message("   Output: ", gpkg_file)
  
  cmd <- sprintf(
    'ogr2ogr -f GPKG %s %s',
    shQuote(gpkg_file),
    shQuote(pbf_file)
  )
  
  status <- system(cmd)
  
  if (status != 0) {
    stop("ogr2ogr conversion failed.", call. = FALSE)
  }
  
  pipeline_message(
    text =sprintf("Conversion of the file %s to %s successfully completed", 
                  pbf_file, gpkg_file), 
    level = 2, progress = "end", process = "valid")
  
  invisible(gpkg_file)
}
#' 
#' @title Extract structured OSM attributes from the `other_tags` field
#' @description This function parses the OpenStreetMap `other_tags` field and 
#'              extracts a predefined set of key–value pairs into a structured 
#'              data frame. Extraction is performed using vectorized regular 
#'              expressions for performance, making it suitable for very large 
#'              OSM datasets. 
#'              The function supports progress reporting adapted to the 
#'              execution environment:
#'              \itemize{
#'                \item in an interactive TTY (local execution), a dynamic 
#'                      progress bar is shown, 
#'                \item in non-interactive environments (e.g. HPC batch jobs), 
#'                      periodic textual progress messages are printed instead.
#'              }
#' @param other_tags_vector A character vector containing the raw OSM 
#'                          `other_tags` field (as stored in OSM GeoPackages). 
#'                          Missing values are handled automatically.
#' @param keys A character vector of OSM tag keys to extract (e.g. `"lanes"`, 
#'             `"maxspeed"`, `"oneway"`, `"turn:lanes"`).
#' @param show_progress Logical. If `TRUE` (default), progress information is 
#'                      printed during extraction.
#' @param IS_TTY Logical. Indicates whether the output is connected to a TTY. 
#'               Defaults to automatic detection using `interactive()` and 
#'               `isatty(stdout())`. This controls how progress information is 
#'               displayed.
#' @return A data.frame with one column per requested OSM key and one row per 
#'        input element. Missing tags are returned as `NA`.
#' @details The extraction relies on the standard OSM `other_tags` serialization
#'          format: 
#'          \code{"key"=>"value"}.
#'          Regular expressions are applied in a vectorized manner for 
#'          efficiency. This implementation is typically 10–50× faster than 
#'          row-wise extraction using loops or `sapply`.
#' @examples
#' \dontrun{
#' osm_tags <- c("lanes", "maxspeed", "oneway")
#' tags_df <- extract_osm_other_tags(
#'   other_tags_vector = roads$other_tags,
#'   keys = osm_tags)
#' }
#' @export
extract_osm_other_tags <- function(other_tags_vector, 
                                   keys, 
                                   show_progress = TRUE, 
                                   IS_TTY = interactive() && isatty(stdout())) {
  n_rows <- length(x = other_tags_vector)
  n_cols <- length(x = keys)
  # Pre-allocate result matrix
  result <- matrix(data = NA_character_, nrow = n_rows, ncol = n_cols)
  colnames(result) <- keys
  # Replace NA with empty strings for pattern matching
  other_tags_vector[is.na(other_tags_vector)] <- ""
  # Progress bar setup
  if (show_progress) {
    pb_width <- 90
    last_printed <- 0
    if (IS_TTY) {
      pipeline_message(text = "Extracting OSM tags", 
                       level = 4, process = "info")
    } else {
      pipeline_message(
        text = sprintf("Extracting %d OSM tags (%d columns)", 
                       length(x = keys), n_cols), 
        level = 4, process = "info")
    }
  }
  # Extract all keys at once
  for (i in seq_along(keys)) {
    key <- keys[i]
    # Escape special regex characters in key (e.g., "turn:lanes")
    key_escaped <- gsub(pattern = "([:|.])", 
                        replacement = "\\\\\\1", 
                        x= key)
    pattern <- paste0('"', key_escaped, '"=>"([^"]*)"')
    # Vectorized regex extraction
    match_positions <- regexpr(pattern = pattern, 
                               text = other_tags_vector, 
                               perl = TRUE)
    # Extract matched strings
    matches <- regmatches(x = other_tags_vector, 
                          m = match_positions)
    # Initialize result column with NA
    result[, i] <- NA_character_
    # Extract values only from matched rows (remove the key part, keep only 
    # value)
    matched_indices <- which(match_positions > 0)
    if (length(x = matched_indices) > 0) {
      values <- sub(pattern = paste0('^"', key_escaped, '"=>"'), 
                    replacement = '', 
                    x = matches)
      values <- sub(pattern = '"$', 
                    replacement = '', 
                    x = values)
      result[matched_indices, i] <- values
    }
    # Progress update
    if (show_progress) {
      pct <- i / n_cols
      if (IS_TTY) {
        bar_len <- floor(x = pb_width * pct)
        bar <- paste0(strrep("=", bar_len), 
                      strrep(" ", pb_width - bar_len))
        pipeline_message(
          text = sprintf("[%s] %3.0f%% (%d/%d)", bar, 100 * pct, i, n_cols), 
          level = 4, process = "wait")
        if (i == n_cols) {
        }
      } else if (i %% 5 == 0 || i == n_cols) {
        if (i > last_printed) {
          last_printed <- i
          pipeline_message(
            text = sprintf("OSM tag %d/%d (%3.0f%%) processed", 
                           i, n_cols, 100 * pct), 
            level = 4, process = "valid")
        }
      }
    }
  }
  # Convert to data.frame with proper column names
  return(as.data.frame(x = result, 
                       stringsAsFactors = FALSE))
}
#' 
#'@title  Process and engineer road network features for traffic modeling
#' @description Cleans, standardizes, and engineers road network attributes in 
#'              preparation for traffic modeling. The function handles 
#'              categorical normalization, feature extraction from OSM 
#'              attributes, and imputation of missing values using 
#'              highway-specific rules.
#'              Key operations include:
#'              \itemize{
#'                  \item Standardization of highway categories with ordered 
#'                        factors,
#'                  \item Imputation of missing connectivity degree (DEGRE),
#'                  \item Extraction of reference letter from road identifiers,
#'                  \item Tokenization and frequency filtering of road names,
#'                  \item Normalization of oneway attributes,
#'                  \item Highway-based imputation of lane counts and speed 
#'                        limits.
#'              }
#'              The function is designed for machine learning pipelines where:
#'              \itemize{
#'                  \item Traffic flows are aggregated in both directions,
#'                  \item Network topology and semantic attributes are used as 
#'                        predictors,
#'                  \item Robust handling of missing and noisy OSM attributes is 
#'                        required.
#'              }
#' @param data A data.frame or data.table containing road network attributes.
#'             Expected columns include (when available):
#'             \code{highway, DEGRE, name, ref / ref_osm, lanes / lanes_osm,
#'             maxspeed / maxspeed_osm, oneway / oneway_osm}.
#' @param rules A data.frame or data.table containing imputation rules per
#'              highway type. Must include columns:
#'              \code{highway, median_lanes, median_speed}.
#' @return A data.frame with cleaned and engineered network features.
#' @examples
#' network_clean <- process_network_features(osm_subset, imputation_rules)
#' @export
process_network_features <- function(data, rules) {
  data.table::setDT(data)
  # ------------------------------------------- #
  # Highway type normalization (ordered factor) #
  # ------------------------------------------- #
  highway_levels <- c(
    "residential", "tertiary", "secondary", "primary", "trunk", "motorway",
    "tertiary_link", "secondary_link", "primary_link", "trunk_link",
    "motorway_link", "unclassified"
  )
  data[, highway := as.character(x = highway)]
  data[is.na(highway) | highway == "", highway := "unclassified"]
  data[, highway := factor(x = highway,
                           levels = c(highway_levels, "missing"),
                           ordered = TRUE)]
  # --------------------------- #
  # Connectivity degree (DEGRE) #
  # --------------------------- #
  data[, DEGRE := as.numeric(x = DEGRE)]
  n_missing_degre <- sum(is.na(data$DEGRE))
  if (n_missing_degre > 0) {
    pipeline_message(
      text = sprintf("Imputing %s missing DEGRE values → %d (urban default)", 
                     fmt(x = n_missing_degre), CONFIG$DEFAULT_DEGRE), 
      process = "warning")
    # Set default value when missing DEGRE
    data[is.na(DEGRE), DEGRE := CONFIG$DEFAULT_DEGRE]
  }
  # ------------------------------------- #
  # Road reference letter (A, D, N, etc.) #
  # ------------------------------------- #
  ref_col <- if ("ref_osm" %in% names(data)){
    "ref_osm"
    } else{
      "ref"
    }
  data[, ref_letter := {
    ref_val <- as.character(x = get(x = ref_col))
    ref_val[is.na(ref_val) | ref_val == ""] <- "missing"
    substr(x= gsub(pattern = "^([A-Za-z]).*", 
                   replacement = "\\1", 
                   x = ref_val), 
           start = 1, 
           stop = 1)}]
  data[ref_letter == "", ref_letter := "missing"]
  data[, ref_letter := factor(x = ref_letter)]
  # ------------------------------------------ #
  # First word of road name (semantic feature) #
  # ------------------------------------------ #
  data[, first_word := {
    name_val <- as.character(x = name)
    name_val[is.na(name_val) | name_val == "" | name_val == "NA"] <- "missing"
    fw <- tolower(
      x = trimws(
        x = sub(pattern = "\\s+.*$", 
                replacement = "", 
                x = name_val)))
    fw[fw == "" | fw == "na"] <- "missing"
    fw
  }]
  # Collapse rare first words (< 100 occurrences)
  first_word_counts <- data[, .N, by = first_word]
  rare_words <- first_word_counts[N < 100, first_word]
  if (length(x = rare_words) > 0) {
    n_rare <- sum(data$first_word %in% rare_words)
    pipeline_message(
      text = sprintf("Setting %s rare first_words (<100 occurrences) to 'missing'", 
                     fmt(n_rare)), 
      process = "warning")
    
    data[first_word %in% rare_words, first_word := "missing"]
  }
  data[, first_word := factor(x = first_word)]
  # -------------------- #
  # Oneway normalization #
  # -------------------- #
  oneway_col <- if ("oneway_osm" %in% names(data)){
    "oneway_osm"
    } else {
      "oneway"
    }
  data[, oneway_osm := {
    ow <- if (oneway_col %in% names(data)){
      as.character(x = get(x = oneway_col))
    } else {
        NA_character_
    }
    ow[is.na(ow) | ow == ""] <- "missing"
    ow[ow == "-1"] <- "yes"  # reverse oneway
    ow[!ow %in% c("yes", "no", "missing")] <- "yes"
    ow
  }]
  data[, oneway_osm := factor(x = oneway_osm, 
                              levels = c("yes", "no", "missing"))]
  # ---------------------------------------------- #
  # Lane count handling (both directions combined) #
  # ---------------------------------------------- #
  lanes_col <- if ("lanes_osm" %in% names(data)){
    "lanes_osm"
  } else {
      "lanes"
  }
  data[, lanes_osm := as.numeric(x = get(x = lanes_col))]
  missing_lanes <- is.na(data$lanes_osm) | data$lanes_osm <= 0
  n_missing_lanes <- sum(missing_lanes)
  if (n_missing_lanes > 0) {
    pipeline_message(
      text = sprintf("Imputing %s missing lane values using highway-specific medians", 
                     fmt(n_missing_lanes)), 
      process = "warning")
    lanes_lookup <- setNames(object = rules$median_lanes, 
                             rules$highway)
    data[missing_lanes, lanes_osm :=
           ifelse(test = is.na(lanes_lookup[as.character(x = highway)]),
                  yes = CONFIG$DEFAULT_NUMBER_OF_LANES,
                  no = lanes_lookup[as.character(x = highway)])]
  }
  # ---------------------- #
  # Speed limit imputation #
  # ---------------------- #
  maxspeed_col <- if ("maxspeed_osm" %in% names(data)){
    "maxspeed_osm"
  } else {
      "maxspeed"
  }
  data[, speed := as.numeric(x = get(x = maxspeed_col))]
  missing_speed <- is.na(data$speed) | data$speed <= 0
  n_missing_speed <- sum(missing_speed)
  if (n_missing_speed > 0) {
    pipeline_message(
      text = sprintf("Imputing %s missing speed values using highway-specific medians", 
                     fmt(n_missing_speed)), 
      process = "warning")
    speed_lookup <- setNames(object = rules$median_speed, 
                             rules$highway)
    data[missing_speed, speed :=
           ifelse(test = is.na(speed_lookup[as.character(x = highway)]),
                  yes = CONFIG$DEFAULT_VEHICLE_SPEED, 
                  no = speed_lookup[as.character(x = highway)])]
  }
  return(as.data.frame(data))
}