#' Properly display scientific scales for ggplot
#'
#' @param x None
#'
#' @export
#'
scientific_label <- function(x) {
    parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
