#' Root sum squares
#'
#' @param ... Vector to find the root sum squares
#'
#' @export
#'
rss <- function(...) {
    x <- c(...)
    return(sqrt(sum(x ^ 2)))
}
