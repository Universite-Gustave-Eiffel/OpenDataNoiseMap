# ==============================================================================
# GLOBAL UTILITIES - I/O FUNCTIONS
# ==============================================================================
#' 
# -------------------------------------------------------------------------------
# Extract a command-line argument value
# -------------------------------------------------------------------------------
#' @title Extract a command-line argument value
#' @description Retrieves the value associated with a specific command-line flag 
#'              from a character vector of arguments (typically obtained via 
#'              \code{commandArgs(trailingOnly = TRUE)}).
#'              The function searches for a flag (e.g., \code{"--phase"}) and, if 
#'              found, returns the element immediately following it. If the flag 
#'              is not present or does not have an associated value, a default 
#'              value is returned instead.
#' @param flag A character string representing the command-line flag to search 
#'             for (e.g., \code{"--phase"}, \code{"--mode"}, \code{"--region"}, 
#'             \code{"--test"}).
#' @param default A default value returned if the flag is not found or if no value 
#'                follows the flag. Defaults to \code{NULL}.
#' @param args A character vector of command-line arguments. Typically obtained 
#'             via \code{commandArgs(trailingOnly = TRUE)}.
#' @return A character string corresponding to the value following the specified 
#'         flag, or the provided \code{default} value if the flag is absent or 
#'         invalid.
#' @details The function:
#'          \enumerate{
#'            \item Searches for the position of the specified flag in 
#'                  \code{args},
#'            \item Ensures exactly one occurrence is found,
#'            \item Checks that the flag is not the last element,
#'            \item Returns the subsequent value if valid,
#'            \item Otherwise returns the default value.
#'          }
#'          The function does not validate the semantic correctness of argument 
#'          values.
#' @examples
#' \dontrun{
#' args <- c("--phase", "training", "--mode", "paris")
#' get_arg_value("--phase", default = "all", args)
#' # Returns "training"
#' get_arg_value("--region", default = "full", args)
#' # Returns "full"
#' }
#' @export
get_arg_value <- function(flag, default = NULL, args) {
  
  # Identify the position of the requested flag
  idx <- which(args == flag)
  
  # Return the value following the flag if it exists
  if (length(idx) == 1 && idx < length(args)) {
    return(args[idx + 1])
  }
  
  # Otherwise return the default value
  return(default)
}
#'
# ------------------------------------------------------------------------------
# Setup project directory structure from configuration
# ------------------------------------------------------------------------------
#' @title Create required project directories from configuration
#' @description Creates all required directories defined in the unified 
#'              pipeline configuration object (typically \code{CFG}). 
#'              The function scans recursively through the configuration list, 
#'              extracts path-like entries based on naming conventions, 
#'              derives their parent directories, and ensures that all 
#'              necessary directories exist.
#'              This function centralizes directory creation in a single place, 
#'              avoiding scattered \code{dir.create()} calls across pipeline 
#'              scripts.
#' @param cfg A named list containing all configuration entries (typically 
#'            \code{CFG}), created by combining configuration objects such as 
#'            \code{CONFIG_GLOBAL}, \code{CONFIG_DATA_PREP}, 
#'            \code{CONFIG_TRAINING}, and \code{CONFIG_PREDICT}. 
#'            Entries representing paths must follow naming conventions:
#'            \itemize{
#'              \item Names ending with \code{_DIR} or \code{_DIRPATH},
#'              \item Names ending with \code{_FILEPATH}.
#'            }
#' @details The function operates as follows:
#'          \enumerate{
#'            \item Recursively flattens the configuration list to extract all 
#'                  values, 
#'            \item Assumes that character values represent filesystem paths, 
#'            \item Computes parent directories using \code{dirname()}, 
#'            \item Removes duplicates and ignores directories that already 
#'                  exist, 
#'            \item Creates missing directories recursively.
#'          }
#'          The function is intentionally conservative: it does not delete 
#'          directories, overwrite files, or validate path semantics beyond 
#'          existence checks.
#' @return Invisibly returns a character vector of directories that were 
#'         created.
#' @examples
#' \dontrun{
#' setup_directories(CFG)
#' }
#' @export
setup_directories <- function(cfg) {
  
  if (!is.list(cfg)) {
    pipeline_message("cfg must be a named list", 
                     process = "stop")
  }
  
  # Flatten configuration (preserve names)
  all_values <- unlist(x = cfg, recursive = TRUE, use.names = TRUE)
  
  # Keep only path-like entries based on naming convention
  path_idx <- grepl(pattern = "(_DIR$|_DIRPATH$|_FILEPATH$)", 
                    x       = names(all_values))
  paths <- all_values[path_idx]
  
  if (length(paths) == 0) {
    pipeline_message("No path entries detected in configuration.", 
                     process = "stop")
    return(invisible(character(0)))
  }
  
  # Parent directories for file paths
  parent_dirs <- dirname(path = paths)
  
  # Explicit directory entries
  explicit_dirs <- paths[grepl(pattern = "_DIR$|_DIRPATH$", names(paths))]
  dirs <- unique(x = c(parent_dirs, explicit_dirs))
  dirs <- dirs[nzchar(x = dirs)]
  
  created_dirs <- character(0)
  for (d in dirs) {
    if (!dir.exists(paths = d)) {
      dir.create(path = d, recursive = TRUE, showWarnings = FALSE)
      created_dirs <- c(created_dirs, d)
      pipeline_message(sprintf("Created directory: %s", d), process = "info")
    }
  }
  
  invisible(created_dirs)
}
#'
# ------------------------------------------------------------------------------
# Format numeric values for console output
# ------------------------------------------------------------------------------
#' @title Format numeric values for human-readable console output
#' @description This function formats numeric values by disabling scientific
#'              notation and inserting thousands separators. It is mainly 
#'              intended for logging and progress messages in the console.
#' @param x A numeric or integer vector to format.
#' @return A character vector with formatted numbers (no scientific notation, 
#'         comma as thousands separator).
#' @examples
#' fmt(3960671)
#' # "3,960,671"
#' fmt(c(1000, 2500000))
#' # "1,000" "2,500,000"
#' @export
fmt <- function(x){
  format(x, scientific = FALSE, big.mark = ",") 
}
#' 
# ------------------------------------------------------------------------------
# Format percentages
# ------------------------------------------------------------------------------
#' @title Format percentages for console output
#' @description This function formats numeric values as percentages with one 
#'              decimal point. It is mainly intended for logging and progress 
#'              messages in the console.
#' @param x numeric A value to format as percentage
#' @return formatted A character string representing the percentage value.
#' @examples
#' fmt_pct(0.5)
#' # "50.0%"
#' @export
fmt_pct <- function(x) {
  if (is.na(x = x) || !is.finite(x = x)) return("NA")
  sprintf("%.1f%%", 100 * x)
}
#'
# ------------------------------------------------------------------------------
# Internal environment for pipeline timing
# ------------------------------------------------------------------------------
#' @title Internal environment for pipeline timing
#' @description Internal environment used to store active timers for the 
#'              pipeline execution.
#'              Timers are indexed by pipeline hierarchy level (1, 2, 3, ...), 
#'              allowing nested timing of computation blocks without 
#'              interference.
#'              This environment should not be accessed directly by users.
#' @return An environment object.
#' @export
#' @keywords internal
.pipeline_env <- new.env(parent = emptyenv())
# Active timers indexed by level (character)
.pipeline_env$timers <- list()
#'
# ------------------------------------------------------------------------------
# Start a pipeline timer for a given hierarchy level
# ------------------------------------------------------------------------------
#' @title Start a pipeline timer
#' @description Starts (or resets) a timer for a given pipeline hierarchy level.
#'              This function supports nested timers by storing one active timer 
#'              per level in an internal environment.
#'              It is intended to be called internally by `pipeline_message()` 
#'              when `progress = "start"`.
#' @param level Integer. Pipeline hierarchy level (e.g. 1, 2, 3).
#' @return Invisibly returns NULL.
#' @keywords internal
pipeline_timer_start <- function(level) {
  .pipeline_env$timers[[as.character(level)]] <- Sys.time()
  invisible(NULL)
}
#'
# ------------------------------------------------------------------------------
# Stop a pipeline timer and return elapsed time
# ------------------------------------------------------------------------------
#' @title Stop a pipeline timer
#' @description Stops the timer associated with a given pipeline hierarchy level 
#'              and returns the elapsed execution time in seconds.
#'              If no timer is active for the requested level, the function 
#'              returns `NA_real_` instead of failing. This allows safe use in 
#'              complex pipelines.
#'              This function is intended to be called internally by 
#'              `pipeline_message()` when `progress = "end"`.
#' @param level Integer. Pipeline hierarchy level (e.g. 1, 2, 3).
#' @return Numeric scalar. Elapsed time in seconds, or `NA_real_` if no timer 
#'         was started for this level.
#' @keywords internal
pipeline_timer_stop <- function(level) {
  key <- as.character(x = level)
  if (!key %in% names(x = .pipeline_env$timers)) {
    return(NA_real_)
  }
  elapsed <- as.numeric(x = difftime(time1 = Sys.time(), 
                                     time2 = .pipeline_env$timers[[key]], 
                                     units = "secs"))
  .pipeline_env$timers[[key]] <- NULL
  elapsed
}
#'
# -------------------------------------------------------------------------------
# Calculation start timer utility for pipeline messages
# -------------------------------------------------------------------------------
#' @title Start a calculation timer
#' @description Starts a timer to measure elapsed execution time.
#' @return Invisibly returns the start time.
#' @export
start_timer <- function() {
  assign(x = ".timer_start", value = Sys.time(), envir = .GlobalEnv)
  invisible(NULL)
}
#' 
# -------------------------------------------------------------------------------
# Calculation stop timer utility for pipeline messages
# -------------------------------------------------------------------------------
#' @title Stop the calculation timer
#' @description Stops the timer and returns the elapsed time in seconds.
#' @return Numeric. Elapsed time in seconds.
#' @export
stop_timer <- function() {
  if (!exists(".timer_start", envir = .GlobalEnv)) {
    stop("Timer was not started")
  }
  elapsed <- as.numeric(
    difftime(time1 = Sys.time(), 
             time2 = get(".timer_start", envir = .GlobalEnv), 
             units = "secs")
  )
  rm(".timer_start", envir = .GlobalEnv)
  return(as.numeric(elapsed))
}
#'
# -------------------------------------------------------------------------------
# Describe a data.frame or data.table structure for logging
# -------------------------------------------------------------------------------
#' @title Describe a data.frame or data.table structure
#' @description Returns a compact textual description of a data.frame or 
#'              data.table, including:
#'              \itemize{
#'                \item object type (data.frame or data.table), 
#'                \item object name as passed to the function, 
#'                \item number of rows and columns, 
#'                \item column names (truncated if needed), 
#'                \item data.table key information when available.
#'              }
#'              Intended to be embedded into pipeline log messages.
#' @param df A data.frame or data.table.
#' @return A character string describing the object.
#' @examples
#' describe_df(osm_full_network)
#' @export
describe_df <- function(df) {
  if (!inherits(x = df, what = c("data.frame", "data.table"))) {
    pipeline_message(
      "Function describe_df() expects a data.frame or data.table", 
      process = "stop")
  }
  # Object name as passed by the caller
  obj_name <- deparse(expr = substitute(df))
  # Object type
  obj_type <- if (inherits(x = df, what = "data.table")) {
    "Data.table"
  } else {
    "Data.frame"
  }
  # Dimensions
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  # Column names (truncate if too many)
  max_cols_display <- 6
  col_names <- colnames(x = df)
  col_display <- if (length(col_names) > max_cols_display) {
    paste0(paste(col_names[1:max_cols_display], collapse = ", "), ", ...")
  } else {
    paste(col_names, collapse = ", ")
  }
  # data.table keys if present
  key_info <- ""
  if (inherits(df, "data.table")) {
    keys <- data.table::key(df)
    if (!is.null(keys) && length(keys) > 0) {
      key_info <- paste0(" | keys: ", paste(keys, collapse = ", "))
    }
  }
  # Final message
  paste0(obj_type, " '", obj_name, "' ", 
         "[nrows: ", n_rows, " | ncols: ", n_cols, "] ", 
         "contains '", col_display, "'", key_info)
}
#'
# --------------------------------------------------------------------------------
# Pipeline progress message utility for structured logging
# --------------------------------------------------------------------------------
#' @title Display structured pipeline progress messages
#' @description Displays standardized progress messages for pipeline execution 
#'              with:
#'              \itemize{
#'                \item hierarchical indentation based on processing level, 
#'                \item semantic icons depending on process type, 
#'                \item optional timing of computation blocks, 
#'                \item proper handling of informational messages, warnings, and 
#'                      fatal errors.
#'              }
#'              Two independent timers are supported:
#'              \itemize{
#'                \item level 1 timers for main processing steps, 
#'                \item level 2 timers for internal computations.
#'              }
#'              Informational messages, warnings, and fatal errors are handled 
#'              independently of the processing levels and do not use timers.
#' @details This function is designed for structured, readable logging in batch, 
#'          HPC, or long-running R pipelines. It avoids interactive prompts and 
#'          produces clean, linear output suitable for log files. 
#'          Processing levels control only structural messages and timers:
#'          \itemize{
#'            \item \code{level = 0}: Main pipeline section titles (no timer), 
#'            \item \code{level = 1}: Timed processing steps 
#'                                    (\code{"start"} / \code{"end"}), 
#'            \item \code{level = 2}: Timed internal computations 
#'                                    (\code{"start"} / \code{"end"}).
#'          }
#'          Severity and message routing are controlled exclusively by the 
#'          \code{process} argument:
#'          \itemize{
#'            \item \code{process = "info"} routes the message to 
#'                  \code{message()}, 
#'            \item \code{process = "warning"} routes the message to 
#'                  \code{warning()}, 
#'            \item \code{process = "stop"} routes the message to \code{stop()} 
#'                  and aborts execution.
#'          }
#' @param text Character string to display (should not include tabs, newlines, 
#'             or icons).
#' @param level Integer indicating the structural hierarchy of the message. 
#'              Ignored when \code{process} is \code{"info"}, \code{"warning"}, 
#'              or \code{"stop"}.
#' @param progress Character string indicating the step state. 
#'                 Must be either \code{"start"} or \code{"end"}. 
#'                 Used only for \code{level = 1} and \code{level = 2}.
#' @param process Optional character string indicating the semantic type of the 
#'                message and selecting an associated icon. 
#'                Supported values include: \code{"install"}, \code{"load"}, 
#'                \code{"save"}, \code{"pack"}, \code{"download"}, 
#'                \code{"wait"}, \code{"configure"}, \code{"search"}, 
#'                \code{"calc"}, \code{"join"}, \code{"learn"}, \code{"build"}, 
#'                \code{"convert"}, \code{"plot"}, \code{"info"}, 
#'                \code{"valid"}, \code{"warning"}, \code{"fail"}, 
#'                \code{"stop"}.
#' @return Invisibly returns \code{NULL}. The function is used for its side 
#'         effects (console output, warnings, or errors).
#' @export
pipeline_message <- function(text,
                             level = 1,
                             progress = c("start", "end"),
                             process = NULL) {
  # ----------------------------------------------------------------------------
  # List of icons associated with specific processes
  # ----------------------------------------------------------------------------
  icons <- list(
    install   = "💻",
    load      = "📥",
    save      = "💾",
    pack      = "📦",
    download  = "⬇️",
    wait      = "⏳",
    configure = "📌",
    search    = "🔍",
    calc      = "⚙️",
    join      = "🔗",
    learn     = "🎓",
    build     = "🚧",
    convert   = "🔄",
    plot      = "📊",
    info      = "ℹ️",
    valid     = "✓",
    warning   = "⚠️",
    fail      = "❌",
    stop      = "⛔"
  )
  icon <- if (!is.null(process) && process %in% names(icons)) {
    icons[[process]]
  } else {
    ""
  }
  
  # Secure process
  process <- if (is.null(process)) {NA_character_} else {process}
  
  # INFO / WARNING / STOP — no level, no timer
  if (process == "info") {
    message("\t\t ", icon, " ", text)
    return(invisible(NULL))
  }
  if (process == "warning") {
    warning(paste(icon, text), call. = FALSE)
    return(invisible(NULL))
  }
  if (process == "stop") {
    stop(paste(icon, text), call. = FALSE)
  }
  # From here: structural messages only
  progress <- match.arg(progress)
  
  # LEVEL 0 — Main pipeline sections
  if (level == 0) {
    if (progress == "start") {
      message("\n=== ", text, " ===\n")
    } else {
      message("\t 🏁 ", text, "\n")
    }
    return(invisible(NULL))
  }
  
  # LEVEL 1 — Timed processing steps
  if (level == 1) {
    if (progress == "start") {
      pipeline_timer_start(level = 1)
      message("\t ", icon, " ", text)
      return(invisible(NULL))
    }
    if (progress == "end") {
      elapsed <- pipeline_timer_stop(level = 1)
      message("\t\t ✓ ", text, sprintf(" in %.1f s", elapsed))
      return(invisible(NULL))
    }
  }

  # LEVEL 2 — Timed internal computations
  if (level == 2) {
    if (progress == "start") {
      pipeline_timer_start(level = 2)
      message("\t\t ", icon, " ", text)
      return(invisible(NULL))
    }
    if (progress == "end") {
      elapsed <- pipeline_timer_stop(level = 2)
      message("\t\t\t ✓ ", text, sprintf(" in %.1f s", elapsed))
      return(invisible(NULL))
    }
  }
  invisible(NULL)
}
#'
# -------------------------------------------------------------------------------
# Relative path to project root utility
# -------------------------------------------------------------------------------
#' @title Get project-relative path
#' @description Returns a path relative to the project root directory. Useful 
#'              for readable logs and portable messages.
#' @param path Character. Absolute or relative file path.
#' @return Character. Path relative to PROJECT_ROOT, prefixed with "./".
#' @export
rel_path <- function(path) {
  if (!exists("PROJECT_ROOT", envir = .GlobalEnv)) {
    return(path)
  }
  path     <- normalizePath(path = path, winslash = "/", mustWork = FALSE)
  rel_path <- fs::path_rel(path = path, start = PROJECT_ROOT)
  paste0("./", rel_path)
}
#'
# ------------------------------------------------------------------------------
# Get available memory
# ------------------------------------------------------------------------------
#' @title Get available system memory in GB
#' @description Reads /proc/meminfo on Linux to get available RAM.
#' @return Available memory in GB, or NA_real_ if unavailable.
#' @export
get_available_memory_gb <- function() {
  if (file.exists("/proc/meminfo")) {
    meminfo  <- readLines("/proc/meminfo", warn = FALSE)
    mem_line <- meminfo[grepl("^MemAvailable:", meminfo)]
    pipeline_message(sprintf("Available memory: %s", mem_line), process = "info")
    if (length(mem_line) == 1) {
      mem_kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", mem_line)))
      if (is.finite(mem_kb)) {
        return(mem_kb / 1024 / 1024)
      }
    }
  }
  NA_real_
}
#'
# -------------------------------------------------------------------------------
# Check available memory and optionally stop/warn if below thresholds
# -------------------------------------------------------------------------------
#' @title Check available system memory
#' @description Estimates currently available RAM and optionally stops/warns
#'              when below a threshold.
#' @param operation_name Character. Name of the operation for logs.
#' @param min_gb Numeric. Minimum required RAM in GB; below this threshold,
#'               execution stops.
#' @param warn_gb Numeric. Warning threshold in GB.
#' @return Invisibly returns the estimated available RAM in GB.
#' @export
check_memory_available <- function(operation_name = "Operation",
                                   min_gb = 1,
                                   warn_gb = 3) {
  available_gb <- get_available_memory_gb()

  if (!is.finite(available_gb)) {
    pipeline_message(sprintf("Memory check unavailable for: %s", operation_name), 
                     process = "warning")
    return(invisible(NA_real_))
  }

  if (available_gb < min_gb) {
    pipeline_message(
      sprintf("%s requires at least %.1f GB RAM, but only %.1f GB available", 
              operation_name, min_gb, available_gb), 
      process = "stop")
  }

  if (available_gb < warn_gb) {
    pipeline_message(
        sprintf("Low memory before %s: %.1f GB available (warning threshold: %.1f GB)", 
                operation_name, available_gb, warn_gb), 
        process = "warning")
  }

  invisible(available_gb)
}
#'
# ------------------------------------------------------------------------------
# Format duration in human-readable format
# ------------------------------------------------------------------------------
#' @title Format duration in seconds to human-readable string
#' @description Converts a duration in seconds to a formatted string showing
#'              hours, minutes, and seconds.
#' @param seconds Numeric. Duration in seconds.
#' @return Character. Formatted duration string (e.g., "2h 15m 30s").
#' @export
format_duration <- function(seconds) {
  if (!is.finite(seconds) || seconds < 0) {
    return("unknown")
  }

  hours   <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs    <- round(seconds %% 60, 1)

  parts <- c()
  if (hours > 0) {
    parts <- c(parts, sprintf("%dh", hours))
  }
  if (minutes > 0 || hours > 0) {
    parts <- c(parts, sprintf("%dm", minutes))
  }
  parts <- c(parts, sprintf("%.1fs", secs))

  paste(parts, collapse = " ")
}
