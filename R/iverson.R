#' Iverson Bracket
#'
#' @param in_boolean_vector Boolean vector
#'
#' @return
#'
#' @export
#'
#' @examples
iverson <- function(in_boolean_vector) {
  out_bit_vector <- rep(0, length(in_boolean_vector))
  out_bit_vector[in_boolean_vector] <- 1
  return(out_bit_vector)
}
