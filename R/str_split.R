#' Split a string into comp
#'
#' This is where the documentation of my function begins.
#' ...
#' @export
str_split <- function(in_str, splt = " ") {
  in_str <- strsplit(in_str, split = splt)[[1]]

  in_str <- in_str[in_str != ""]

  out_str_vec <- in_str[!(1:length(in_str) %in% grep(" ", in_str))]

  return(out_str_vec)
}
