#' Split a string into string vector
#'
#' @description
#' `stringsplit` is an extension of `base::strsplit` but returns a vector
#' instead of a list. This function also removes blank characters and spaces.
#'
#' @param in_str Input string to be split
#' @param splt Character to split at. By default, split at spaces.
#' @return Returns string vector or character vector from strings.
#' @export
#'
#' @examples
#' str_split("Hello World")
#' ## c("Hello", "World")
#'
#' str_split("Hello World")
#' ## c("H", "e", "l", "l", "o", "W", "o", "r", "l", "d")

#------------------------------------------------------------------------------#
stringsplit <- function(in_str, splt = " ") {
  in_str <- strsplit(in_str, split = splt)[[1]]

  in_str <- in_str[in_str != ""]

  out_str_vec <- in_str[!(1:length(in_str) %in% grep(" ", in_str))]

  return(out_str_vec)
}
