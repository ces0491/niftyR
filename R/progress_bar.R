#' Print a Progress Bar for For Loops - courtesy of helfRlein
#'
#' @param run the iterator of the \code{for} loop or an integer with the current loop number.
#' @param max_run either an integer with the maximum number of loops if \code{run} is also a number,
#' or a vector with all possible iterations in the correct order.
#' @param width width an integer that indicates how wide the progress bar is printed.
#' @param info  a string with additional information to be printed at the end of the line. The default is \code{run}.
#'
#' @return NULL
#'
#' @importFrom utils flush.console
#' @export
#'
#' @examples
#' for (i in 1:20) {
#'   Sys.sleep(0.1)
#'   progress_bar(run = i, max_run = 200, width = 60L)
#' }
#'
progress_bar <- function(run, max_run, width = 20L, info = run) {
  # check run
  if (length(run) > 1) {
    stop("run needs to be of length one!")
  }
  # check max_run
  if (length(max_run) == 0) {
    stop("max_run has length 0")
  }

  if (length(max_run) > 1 | is.character(max_run)) {
    percent <- which(run == max_run) / length(max_run)
  } else {
    percent <- run / max_run
  }

  percent_step <- round(percent * width, 0)
  progress <- paste0("[",
                     paste0(rep("=", percent_step), collapse = ""),
                     paste0(rep(" ", width - percent_step),
                            collapse = ""),
                     "] ",
                     sprintf("%7.2f", percent * 100, 2),
                     "% - ",
                     info)
  cat("\r", progress)
  flush.console()
}
