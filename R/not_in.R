#' Not in operator - courtesy of helfRlein
#'
#' @param vec_match vector of values to be matched
#' @param vec_compare vector of values to compare against
#'
#' @return boolean vector
#' @export
#'
#' @examples
#' c(1,2,3,4) %nin% c(1,2,5)
#'
`%nin%` <- function(vec_match, vec_compare) {

  !match(vec_match, vec_compare, nomatch = 0) > 0

}
