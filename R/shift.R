#' Shift columns of dataframe
#'
#' @param data
#' @param column
#' @param shift_by
#' @param back
#' @param order_by
#' @param fill_with
#'
#' @return
#' @export
#'
#' @examples
shift <- function(
        data = NULL,
        column = "",
        shift_by = 1,
        back = TRUE,
        order_by = NULL,
        fill_with = 0) {

    # Checks for failures ----

    shift_number <- length(shift_by)

    if (shift_number != length(back)) {
        if (length(back) == 1) {back <- rep(back, times = shift_number)}
        else {stop(
            "Length of amount to shift by must equal length of back (directions to shift)."
        )}
    }

    if (is.null(data) || (column == "")) {
        stop("Must select dataframe to and column to shift by.")
    }

    # Subroutine to shift once ----
    shift_one <- function(
        .data_vector = NULL,
        .shift_by = 1,
        .back = TRUE,
        .fill_with = 0) {

        if (.back) {
            .data_vector <- c(.data_vector, rep(.fill_with, .shift_by))

            .data_vector <- .data_vector[(.shift_by+1):length(.data_vector)]

            .data_vector <- list(
                name = paste(column, .shift_by, "back", sep = "_"),
                data = .data_vector
            )
        }

        else {
            .data_vector <- c(rep(.fill_with, .shift_by), .data_vector)

            .data_vector <- .data_vector[1:(length(.data_vector) - .shift_by)]

            .data_vector <- list(
                name = paste(column, .shift_by, "forward", sep = "_"),
                data = .data_vector
            )
        }
        return(.data_vector)
    }

    # Perform all needed shifts ----

    if (!is.null(order_by)) {
        data$.original_order. <- 1:nrow(data)
        data <- data[order(unlist(unname(data[[order_by]]))),]
        data$.new_order. <- 1:nrow(data)
    }

    data_vector <- data[[column]]

    for (i in 1:shift_number) {

        current_data_vector <- shift_one(
            .data_vector = data_vector,
            .shift_by = shift_by[i],
            .back = back[i],
            .fill_with = fill_with
        )

        data[[current_data_vector$name]] <- current_data_vector$data
    }

    return(data)

}
