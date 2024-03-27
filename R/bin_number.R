#' Calculate bin number for ggplot2
#'
#' @param data_vector Column of dataframe being plotted
#' @param method `c("fd")` String indicating method to use. Currently
#'  only the Freedman-Diaconis rule is implemented
#'
#' @return number_bins, integer containing the ideal number of bins
#'  for histogram
#' @export
#'

#-----------------------------------------------------------------------------#

bin_number <- function(data_vector, method = "fd") {
    if (method == "fd") {
        bin_width <-
            2 * stats::IQR(data_vector) / (length(data_vector) ^ (1 / 3))
        number_bins <- ceiling(diff(range(data_vector)) / bin_width)

        return(number_bins)
    }
    else {
        stop("Error! Incorrect method provided.")
    }
}
