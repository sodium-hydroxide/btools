#' Check if all values are present in set
#'
#' @param x Values to check
#' @param y Vector containing values
#'
#' @return TRUE or FALSE if all values are present in set
#' @export
#'
"%allin%" <- function(
        x,
        y) {
    return(FALSE %in% (x %in% y))
}
