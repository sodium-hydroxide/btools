#' Check if all values are present in set
#'
#' @param ... Values to check
#' @param set Vector containing values
#'
#' @return TRUE or FALSE if all values are present in set
#' @export
#'
all_present <- function(
        ...,
        set = c()) {
    return(FALSE %in% (c(...) %in% set))
}
