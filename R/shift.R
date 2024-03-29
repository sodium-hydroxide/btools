#' Shift columns of dataframe
#'
#' @param data Dataframe containing column to shift
#' @param column String, column to shift
#' @param shift_by Int, number of columns to shift by
#' @param back Boolean, if true shift back, if false shift forward
#' @param order_by String, column name to reorder prior to shifting
#' @param fill_with Float, fill shifted spaces with value
#'
#' @return Original dataframe with shifted values
#' @export
#'
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
        if (length(back) == 1) {
            back <- rep(back, times = shift_number)
        }
        else {
            stop(paste(
                "Length of amount to shift by must equal length of back",
                "(directions to shift)."
            ))
        }
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

            .data_vector <- .data_vector[(.shift_by + 1):length(.data_vector)]

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
