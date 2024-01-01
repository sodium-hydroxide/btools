#' Source scripts into directory
#'
#' @param ... String vector containing scripts to be run. If
#' `extension = ".R"``, the names of the script are all that need to be
#' included.
#' @param directory Directory containins the scripts. Default directory is "R/".
#' @param extension File extension for scripts. Default extension is ".R".
#'
#' @export
#'
#' @examples
#' `script_source("function1", "function2")`
#' ## Loads the functions "R/function1.R" and "R/function2.R"
#'

#-----------------------------------------------------------------------------#

script_source <- function(
        ...,
        directory = "R/",
        extension = ".R"){

    x <-c(...)
    x <- paste(directory, x, extension, sep = "")

    for (i in 1:length(x)) {# loop over elements of list
      source(x[i])
    }
}
