#' Method of sequencing that throws an error if the sequence is not correct
#'
#' @param start Starting point
#' @param end Ending point
#'
#' @return Sequence of integers
#' @export
#'
safe_seq <- function(
        start,
        end) {
    if (start > end) {
        stop()
    }
    if (floor(start) != start || floor(end) != end) {
        stop()
    }
    return(start:end)
}
