#' Display Uncertainty
#'
#' @param data dataframe containing values to round given an uncertainty
#' @param value string, variable name of values
#' @param uncertainty string, variable name of standard uncertainty
#' @param relative_uncertainty string, variable name of standard relative uncertainty
#' @param sig_fig int, number of significant digits should remain for the mantissa of the uncertainty, defaults to 2
#' @param display string, name of output column containing the data, defaults to "display"
#' @param latex boolean, output the display as latex instead of plain text, defaults to false
#' @param add_spaces boolean, add spaces to improve readability, defaults to false
#'
#' @return Original dataframe with new column containing the properly displayed value and uncertainty
#' @export
#'
display_uncertainty <- function(
        data,
        value = NULL,
        uncertainty = NULL,
        relative_uncertainty = NULL,
        sig_fig = 2,
        display = NULL,
        latex = FALSE,
        add_spaces = FALSE) {

    #----
    if (is.null(value)) {
        stop("Select column for quantity of interest")
    }
    data_temp <- data
    data_temp$.value. <- unlist(data[value])

    if (is.null(uncertainty)) {
        if (is.null(relative_uncertainty)) {
            stop(
                "Select column for uncertainty or relative uncertainty."
            )
        }
        else {
            data_temp$.u_value. <-
                unlist(data[relative_uncertainty]) * data_temp$.value.
        }
    }
    else {
        data_temp$.u_value. <- unlist(data[uncertainty])
    }

    #----
    # Save the exponents for the uncertainty and value
    data_temp$.u_exp. <- floor(log10(data_temp$.u_value.))
    data_temp$.value_exp. <- floor(log10(data_temp$.value.))
    # Round the uncertainty to the correct number of sig-figs
    # and display as a string
    data_temp$.u_rnd. <- data_temp$.u_value. / (10 ^ data_temp$.u_exp.)
    data_temp$.u_rnd. <- round(data_temp$.u_rnd., digits = sig_fig - 1)
    data_temp$.u_rnd. <- (10 ^ (sig_fig - 1)) * data_temp$.u_rnd.
    data_temp$.u_rnd. <- as.character(data_temp$.u_rnd.)
    # Round the value to the same digit as the most significant figure of the uncertainty
    data_temp$.value_rnd. <- data_temp$.value. / (10 ^ data_temp$.u_exp.)
    data_temp$.value_rnd. <- round(data_temp$.value_rnd., digits = sig_fig - 1)
    # Check if the process for rounding the value removes trailing zero. If so, add the zero back in
    data_temp$.value_string. <- as.character(data_temp$.value_rnd.)
    data_temp$.missing_zero. <- grepl(".", data_temp$.value_string., fixed = TRUE)
    data_temp[data_temp$.missing_zero. == FALSE,]$.value_string. <-
        paste(
            data_temp[data_temp$.missing_zero. == FALSE,]$.value_string.,
            ".0",
            sep = ""
        )
    # Convert the rounded mantissa of the value to a string
    data_temp$.value_rnd. <-
        (10 ^ (data_temp$.u_exp. - data_temp$.value_exp.)) *
        data_temp$.value_rnd.
    data_temp$.value_rnd. <- as.character(data_temp$.value_rnd.)
    # Add trailing zeros as needed
    data_temp$.missing_zero. <-
        nchar(data_temp$.value_string.) == nchar(data_temp$.value_rnd.)

    while (FALSE %in% data_temp$.missing_zero.) {
        data_temp[data_temp$.missing_zero. == FALSE,]$.value_rnd. <-
            paste(
                data_temp[data_temp$.missing_zero. == FALSE,]$.value_rnd.,
                "0",
                sep = ""
            )

        data_temp$.missing_zero. <-
            nchar(data_temp$.value_string.) == nchar(data_temp$.value_rnd.)
    }

    # Insert spaces to improve readability
    if (add_spaces) {
        space_inserts <- max(floor((nchar(data_temp$.value_rnd.) - 2) / 3), 1)

        for (i in 1:space_inserts) {

            regex_pattern <- paste(
                "^([[:print:]]{",
                (3 * i) + 2,
                "})([[:print:]]+)$", sep = "")

            data_temp$.value_rnd. <- gsub(
                regex_pattern,
                "\\1 \\2",
                data_temp$.value_rnd.
            )
        }
    }

    # Format for latex or plain text
    if (latex) {
        data_temp$.value_rnd. <- paste(
            "$",
            as.character(data_temp$.value_rnd.),
            "(",
            data_temp$.u_rnd.,
            ")\\,\\cdot 10^{",
            as.character(data_temp$.value_exp.),
            "}$",
            sep = ""
        )
    }
    else {
        data_temp$.value_rnd. <- paste(
            as.character(data_temp$.value_rnd.),
            "(",
            data_temp$.u_rnd.,
            ") E",
            as.character(data_temp$.value_exp.),
            sep = ""
        )
    }


    if (is.null(display)) {
        display <- "display"
    }
    data[display] <- data_temp$.value_rnd.

    return(data)

}
