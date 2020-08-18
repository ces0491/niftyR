#' Replace \code{NaN} and/or \code{Inf} values
#'
#' @param x a vector containing numeric values and real and imaginary parts of complex values
#' @param convert_inf logical indicating whether Inf values should be replaced - default to TRUE
#' @param convert_nan logical indicating whether NaN values should be replaced - default to TRUE
#' @param replace replacement value - defaults to NA
#'
#' @return vector with replaced \code{NA} values.
#' @export
#'
#' @examples
#' x <- c(NaN, 1, 2, 3, Inf)
#' complexn_to_na(x)
#'
complexn_to_na <- function(x, convert_inf = TRUE, convert_nan = TRUE, replace = NA) {

  assertR::assert_true(is.vector(x), "logic error")

  if(convert_inf) x <- ifelse(is.infinite(x), replace, x)
  if(convert_nan) x <- ifelse(is.nan(x), replace, x)

  x
}
