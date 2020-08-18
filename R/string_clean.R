#' Clean Characters
#'
#' @param x character vector
#' @param keep_spaces logical indicating whether user wants to keep spaces by replacing them with "_"
#' @param keep_leading_num logical indicating whether you'd like to keep the leading numeric. It will be appended to the end of the clean string
#' @param to_lower logical indicating whether \code{x} should be lower case
#' @param non_alnum_replace optional string indicating the character to use to replace all non alpha numeric values except for spaces
#'
#' @return a character vector with all non-standard characters replaced by their standard counterparts
#' @export
#'
#'  @examples
#' x <- "91 Cesairé T & co"
#' string_clean(x)
#'
string_clean <- function(x, keep_spaces = FALSE, keep_leading_num = FALSE, to_lower = FALSE, non_alnum_replace = NULL) {

  char_map_file <- "char_map.rds"
  src_dir <- system.file("extdata", package = "niftyR")
  src <- paste(src_dir, char_map_file, sep = "/")

  char_map <- readRDS(src)

  clean_x <- x
  for (i_char in seq_along(char_map)) {
    clean_x <-
      gsub(pattern = names(char_map)[i_char],
           replacement = char_map[i_char],
           x = clean_x)
  }

  clean_x <- gsub(pattern = "^\\d+", replacement = "", x = clean_x) # remove leading numerics

  clean_x <- trimws(clean_x, "both")  # trim the whitespace on either end

  if (!is.null(non_alnum_replace)) {
    clean_x <- gsub(pattern = "[^a-zA-Z0-9 ]+", replacement = non_alnum_replace, x = clean_x) # replace all non alphanums except spaces "[^[:alnum:]]"
  }

  if (keep_spaces) {
    clean_x <- gsub(pattern = "[ \t\r\n]", replacement = "_", x = clean_x) # keep spaces by replacing them with _
  } else {
    clean_x <- gsub(pattern = "[ \t\r\n]", replacement = "", x = clean_x)
  }

  if (keep_leading_num) {
    num <-  gsub("[^0-9.]", "",  x)
    clean_x <- paste(clean_x, num, sep = "_")
  }

  if (to_lower == TRUE) {
    clean_x <- tolower(clean_x)
  }

  clean_x

}
