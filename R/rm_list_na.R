#' Remove NAs from lists - courtesy of helfRlein
#'
#' @param x object of type \code{list}
#' @param recursive logical indicating whether NAs contained within list elements should be removed too
#'
#' @return object of type \code{list} with NAs removed
#' @export
#'
#' @examples
#' x <- list(c(1:3), letters[1:4], NA, c(1, NA), list(c(5:6, NA), NA, "A"))
#' rm_na_list(x, recursive = TRUE)
#'
rm_na_list <- function(x, recursive = FALSE) {

  if (recursive && anyNA(x, recursive = TRUE)) {
    x <- x[!sapply(x, function(x) all(is.na(x)))]
    x <- sapply(x, rm_na_list, recursive = TRUE, simplify = TRUE)
  } else {
    x <- x[!sapply(x, function(z) all(is.na(z)), simplify = TRUE)]
  }

  x
}
