#' Inverse Iverson Bracket
#'
#' Return FALSE for 0 and TRUE for 1
#'
#' @param ... Vector of 0 or 1
#'
#' @return Logical vector
#' @export
#'
#' @examples
#'
#' `inverson(0, 1, 0, 1)`
#' ## c(FALSE, TRUE, FALSE, TRUE)
inverson <- function(...) {

    x <- c(...)

    if (length(x[!(x %in% c(0,1))]) != 0) {
        stop(paste(
            "Error!",
            "Input must be bitstring",
            sep = "\n"
        ))
    }

    out_boolean_vector <- rep(FALSE, length(x))
    out_boolean_vector[x == 1] <- TRUE

    return(out_boolean_vector)
}
