#' Install pmder package
#'
#' Execution of this function will install the latest version of the `pmder` package into the system.
#'
#' @export
#'

#-----------------------------------------------------------------------------#

install_pmder <- function() {
    devtools::install_github("sodium-hydroxide/pmder", force = TRUE)
}
