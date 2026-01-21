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
#' @title Describe a data frame or data.table structure
#' @description Prints a compact textual description of a data frame or 
#'              data.table, including number of rows, columns, column names, and 
#'              index/key information when available. Intended to be embedded 
#'              into pipeline log messages.
#' @param df A data.frame or data.table.
#' @return A character string describing the object.
#' @examples
#' describe_df(df)
#' @export
describe_df <- function(df) {
  if (!inherits(df, c("data.frame", "data.table"))) {
    stop("describe_df() expects a data.frame or data.table")
  }
  n_rows <- nrow(x = df)
  n_cols <- ncol(x = df)
  col_names <- paste(colnames(df), collapse = ", ")
  # Detect data.table keys if present
  keys <- NULL
  if (inherits(df, "data.table")) {
    keys <- data.table::key(df)
  }
  key_info <- if (!is.null(keys) && length(keys) > 0) {
    paste0(" | keys: ", paste(keys, collapse = ", "))
  } else {
    ""
  }
  paste0("[rows: ", n_rows, ", cols: ", n_cols, "] ", 
         col_names, key_info)
}
#'
#' @title Display a structured pipeline progress message
#' @description Displays standardized progress messages for pipeline execution, 
#'              including hierarchical indentation, icons, and automatic timing 
#'              of computation blocks. 
#' @param text Character string to display (without tabs, newlines or icons).
#' @param level Integer. Hierarchy level (0 = main title, 1 = sub-block).
#' @param progress Character. Either "start" or "end".
#' @param process Character. Type of process (among "install", "load", "save", 
#'                "download", "configure", "search", calc", join", "learn", 
#'                "build", "plot", "info", "clip", "valid", "warning", "stop").
#' @return Invisibly returns NULL.
#'
#' @export
pipeline_message <- function(text, 
                             level = 1, 
                             progress = c("start", "end"), 
                             process = NULL) {
  progress <- match.arg(progress)
  # ----------------------------------------------------------------------------
  # Icon mapping by process type
  # ----------------------------------------------------------------------------
  icons <- list(
    install   = "💻",
    load      = "📥",
    save      = "💾",
    pack      = "📦",
    download  = "⬇️ ",
    wait      = "⏳",
    configure = "📌",
    search    = "🔍",
    calc      = "⚙️ ",
    join      = "🔗",
    learn     = "🎓",
    build     = "🚧",
    plot      = "📊",
    info      = "ℹ️",
    clip      = "📎",
    valid     = "✓",
    warning   = "⚠️",
    stop      = "⛔")
  icon <- if (!is.null(process) && length(process) == 1 
              && process %in% names(icons)) {
    icons[[process]]
  } else {
    ""
  }
  
  # ----------------------------------------------------------------------------
  # LEVEL 0 — Main titles
  # ----------------------------------------------------------------------------
  if (level == 0) {
    if (progress == "start") {
      message("\n=== ", text, " ===\n")
      return(invisible(NULL))
    }
    if (progress == "end") {
      message("\t ✓ ", text, "\n")
      return(invisible(NULL))
    }
  }
  # ----------------------------------------------------------------------------
  # LEVEL 1 — Sub-blocks with timer
  # ----------------------------------------------------------------------------
  if (level == 1) {
    if (progress == "start") {
      start_timer()
      message("\t ", icon, " ", text, "\n")
      return(invisible(NULL))
    }
    if (progress == "end") {
      # Special cases: warning / stop (no timer)
      if (!is.null(process) && process %in% c("warning", "stop")) {
        message("\t\t ", icon, " ", text, "\n")
        return(invisible(NULL))
      }
      elapsed <- tryCatch(
        stop_timer(),
        error = function(e) NA_real_
      )
      if (is.na(elapsed)) {
        message("\t\t ✓ ", text, "\n")
      } else {
        message("\t\t ✓ ", text, sprintf(" in %.1f s", elapsed), "\n")
      }
      return(invisible(NULL))
    }
  }
  # ----------------------------------------------------------------------------
  # LEVEL 2 — Informational / warning messages (no timer)
  # ----------------------------------------------------------------------------
  if (level == 2) {
    message("\t\t\t ", icon, " ", text, "\n")
    return(invisible(NULL))
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
