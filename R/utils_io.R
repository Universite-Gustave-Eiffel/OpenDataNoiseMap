# ==============================================================================
# GLOBAL UTILITIES FOR DISPLAYING LOG MESSAGES
# ==============================================================================
# 
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
#'
#' @export
fmt <- function(x){
  format(x, scientific = FALSE, big.mark = ",") 
}
# ------------------------------------------------------------------------------
# Internal environment for pipeline timing
# ------------------------------------------------------------------------------
#' @keywords internal
#' @description Internal environment used to store active timers for the 
#'              pipeline execution.
#'              Timers are indexed by pipeline hierarchy level (1, 2, 3, ...), 
#'              allowing nested timing of computation blocks without 
#'              interference.
#'              This environment should not be accessed directly by users.
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
#' @title Start a calculation timer
#' @description Starts a timer to measure elapsed execution time.
#' @return Invisibly returns the start time.
#' @export
start_timer <- function() {
  assign(x = ".timer_start", value = Sys.time(), envir = .GlobalEnv)
  invisible(NULL)
}
#' 
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
      text = "Function describe_df() expects a data.frame or data.table", 
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
#'                \code{"plot"}, \code{"info"}, \code{"valid"}, 
#'                \code{"warning"}, \code{"stop"}.
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
    plot      = "📊",
    info      = "ℹ️",
    valid     = "✓",
    warning   = "⚠️",
    stop      = "⛔"
  )
  icon <- if (!is.null(process) && process %in% names(icons)) {
    icons[[process]]
  } else {
    ""
  }
  
  # Secure process
  process <- if (is.null(process)) {NA_character_} else {process}
  
  # ----------------------------------------------------------------------------
  # INFO / WARNING / STOP — no level, no timer
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # LEVEL 0 — Main pipeline sections
  # ----------------------------------------------------------------------------
  if (level == 0) {
    if (progress == "start") {
      message("\n=== ", text, " ===\n")
    } else {
      message("\t 🏁 ", text, "\n")
    }
    return(invisible(NULL))
  }
  # ----------------------------------------------------------------------------
  # LEVEL 1 — Timed processing steps
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  # LEVEL 2 — Timed internal computations
  # ----------------------------------------------------------------------------
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
  path <- normalizePath(path = path, winslash = "/", mustWork = FALSE)
  rel_path <- fs::path_rel(path = path, start = PROJECT_ROOT)
  paste0("./", rel_path)
}
