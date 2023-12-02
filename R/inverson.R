#' Inverse Iverson Bracket
#'
#' @param in_bit_vector
#'
#' @return
#' @export
#'
#' @examples
inverson <- function(in_bit_vector) {
  out_boolean_vector <- rep(FALSE, length(in_bit_vector))
  out_boolean_vector[in_bit_vector == 1] <- TRUE
}
