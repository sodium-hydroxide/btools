#' Iverson Bracket
#'
#' Return 0 for FALSE and 1 for TRUE
#'
#' @param ... Vector of boolean
#'
#' @return Vector
#'
#' @export
#'
#' @examples
#' # iverson(TRUE, FALSE, TRUE, FALSE)
#' ## c(1, 0, 1, 0)

#-----------------------------------------------------------------------------#

iverson <- function(...) {

    x <- c(...)

    if (!is.logical(x)) {
        stop(paste(
            "Error!",
            "Input vector must be logical.",
            sep = "\n"
        ))
    }

    out_bit_vector <- rep(0, length(x))
    out_bit_vector[x] <- 1

    return(out_bit_vector)
}
