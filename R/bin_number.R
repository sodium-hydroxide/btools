#' Calculate bin number for ggplot2
#'
#' @param data_vector Column of dataframe being plotted
#' @param method `c("fd")`
#'
#' @return
#' @export
#'
#' @examples
bin_number <- function(data_vector, method = "fd") {
    if (method == "fd") {
        bin_width <- 2 * IQR(data_vector) / (length(data_vector) ^ (1 / 3))
        number_bins <- ceiling(diff(range(data_vector)) / bin_width)

        return(number_bins)
    }
    else {
        return("Error! Incorrect method provided.")
    }
}
