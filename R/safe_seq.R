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
