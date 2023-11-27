#' Install and load packages
#'
#' @description
#' `package_load` receives a list of packages, installs any packages which are
#' not currently installed, and loads the packages into the current environment
#'
#' @param ... Strings corresponding to names of packages
#' @param quiet Boolean, defaults to TRUE. If true, no warnings or messages
#'    (including package start-up messages) will be displayed. If false, these
#'    warnings will be displayed.
#' @export
#'
#' @examples
#' package_load("tidyverse", "pracma", "magrittr")

#------------------------------------------------------------------------------#
#
package_load <- function(..., quiet = TRUE) {
  x <- c(...)
  for (pkg in x) {
    # Install package if it is not already installed
    if (!pkg %in% installed.packages()) {
      install.packages(pkg)
    }
    if (quiet) {
      # Load package without printing output
      library(pkg, character.only = TRUE) |>
        suppressPackageStartupMessages() |>
        suppressMessages() |>
        suppressWarnings()
    } else {
      # Load package
      library(pkg, character.only = TRUE)
    }
  }
}
