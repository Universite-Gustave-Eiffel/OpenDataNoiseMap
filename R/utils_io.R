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
  elapsed
}
