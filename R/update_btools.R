#' Install pmder package
#'
#' Execution of this function will install the latest version of the `btools` package into the system.
#'
#' @export
#'

#-----------------------------------------------------------------------------#

update_btools <- function() {
    devtools::install_github("sodium-hydroxide/btools", force = TRUE)
}
