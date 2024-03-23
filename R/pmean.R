#' Take generalized mean of x
#'
#' @param ... Vector to take generalized mean of
#' @param p Power for general mean
#' @param weight Vector containing weights
#'
#' @return generalized mean
#' @export
#'
pmean <- function(
        ...,
        p = 1,
        weight = NULL) {

    x <- c(...)

    if (is.null(weight)) {
        weight = rep(1, length(x))
    }

    if (length(weight) != length(x)) {
        stop("Length of weights must equal the length of x")
    }

    weight <- weight / sum(weight)

    if (p == Inf) {
        return(max(x))
    }

    else if (p == -Inf) {
        return(min(x))
    }

    else if (p == 0) {
        x <- x ^ weight
        return(prod(x))
    }

    else {
        x <- weight * (x ^ p)
        return(sum(x) ^ (1 / p))
    }

}
