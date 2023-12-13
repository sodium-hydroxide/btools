#' Identify tally locations for MCNP output
#'
#' Function acts as subroutine for the mcnparse function.
#'
#' @param mcnp_output String vector containing MCNP output file
#'
#' @return Dataframe containing each tally identifier, type, and location of the tally, the values, and number of energy bins
#' @export
#'
mcnparse_identify_tally <- function(mcnp_output) {
    # Initialize dataframe
    tally_info <- data.frame(
        index = which(grepl("1tally    ",mcnp_output))) |>
        dplyr::mutate(
            name = "",
            type = "",
            values = 0,
            numBin = 0
        ) |>
        dplyr::relocate(index, .before = values)

    for (
        i in 1:nrow(tally_info)
    ) {

        tally_i <- stringsplit(mcnp_output[tally_info$index[i]])[2]

        tally_info$name[i] <- tally_i

        tally_type_i <- stringsplit(tally_i, splt = "")
        tally_type_i <- tally_type_i[length(tally_type_i)]

        energy_weighted <- "energy" %in% stringsplit(
            mcnp_output[tally_info$index[i] + 1]
        )

        tally_info$type[i] <- tally_type_i

        tally_info$values[i] <- 6

        if (tally_info$type[i] == "4"){
            tally_info$values[i] <- tally_info$values[i] + 4
        }

        if (energy_weighted) {
            tally_info$type[i] <- paste("*F", tally_info$type[i], sep = "")
        } else {
            tally_info$type[i] <- paste("F", tally_info$type[i], sep = "")
        }

        # Find number of energy bins
        if (i == nrow(tally_info)) {
            next_tally <- length(mcnp_output)
        } else {
            next_tally <- tally_info$index[i + 1]
        }

        tally_section <- mcnp_output[tally_info$index[i]:next_tally]

        tally_info$numBin[i] <-
            which(grepl("    total", tally_section))[1] - tally_info$values[i] - 1

        if (tally_type_i == "2") {
            tally_info$numBin[i] <- tally_info$numBin[i] - 4
        }

    }

    return(tally_info)
}


#' Pull Tally Location Information
#'
#' This function will pull the data from each tally and return a dataframe containing vectors for the information.
#' Function acts as subroutine for the mcnparse function.
#'
#' @param in_tally_lines Subset of MCNP output containing tally information
#' @param tally_name Tally identifier
#'
#' @return Dataframe containing energy bin, tally value, relative
#'  error, and tally name
#' @export
#'
mcnparse_pull_tally_data <- function(in_tally_lines, tally_name) {

    num_lines <- length(in_tally_lines)
    energy_vect <- rep(0, num_lines)
    tally_vect <- rep(0, num_lines)
    rsd_vect <- rep(0, num_lines)

    for(
        i in 1:num_lines
    ){

        line_i <- stringsplit(in_tally_lines[i])

        energy_vect[i] <- as.numeric(line_i[1])
        tally_vect[i] <- as.numeric(line_i[2])
        rsd_vect[i] <- as.numeric(line_i[3])

    }

    return(
        data.frame(
            bin_mev = energy_vect,
            value = tally_vect,
            re = rsd_vect) |>
            dplyr::mutate(tally = tally_name)
    )

}


#' Parse single statistical checks for MCNP output
#'
#' Get results of statistical checks for a particular run.
#' Function acts as subroutine for the mcnparse function.
#'
#' @param mcnp_output
#'
#' @return Dataframe containing the results of MCNP statistical checks for each tally
#' @export
#'
mcnparse_stat_results <- function(mcnp_output) {

    # Find index where tally summaries occur

    line_index <- which(grepl(
        "observed",
        mcnp_output
    ))

    # Get lines of observed values for tally
    stat_summaries <- mcnp_output[line_index]

    num_tallies <- length(stat_summaries)

    tally <- mcnp_output[line_index - 6]

    stat_check <- data.frame(
        Tally = rep("", num_tallies),
        Mean_B = rep("", num_tallies),
        RE = rep("", num_tallies),
        RE_Decrease = rep("", num_tallies),
        RE_Rate = rep("", num_tallies),
        VOV = rep("", num_tallies),
        VOV_Decrease = rep("", num_tallies),
        VOV_Rate = rep("", num_tallies),
        FOM = rep("", num_tallies),
        FOM_Behavior = rep("", num_tallies),
        PDFSlope = rep("", num_tallies)
    )

    # Get the tally being calculated and the value of each stat. test
    for (
        i in 1:length(stat_summaries)
    ) {
        # Save tally statistical tests values
        stat_check[i, ] <- stringsplit(stat_summaries[i])

        # Save which tally is being used
        tally_i <- stringsplit(tally[i])

        stat_check$Tally[i] <- tally_i[length(tally_i)]
    }

    return(stat_check)
}


#' Parse single MCNP output file
#'
#' @param file_name String with name of MCNP output file
#' @param directory String containing the path to the directory of the output files. By default, this will be your current working directory. The command setwd("path/to/directory") will change the working directory in R.
#' @param extension Boolean. When set to true, the data for the runs will be saved to "mcnpData.csv" and the statistical checks will be saved to "statisticalChecks.csv".
#'
#' This function will pull the data from each tally and return a dataframe containing vectors for the information.
#' Function acts as subroutine for the mcnparse function.
#'
#' @return Returns dataframe containing results of individual run
#' @export
#'
mcnparse_one <- function(
        file_name,
        directory = "",
        extension = ".mcnpout") {
    # Read MCNP output file in and find location of information----
    mcnp_output <- paste(
        directory,
        file_name,
        extension,
        sep = ""
    ) |>
        readLines() |>
        as.vector()

    tally_info <- mcnparse_identify_tally(mcnp_output)

    # Get data frame for binned tallies ----

    # Pull the first tally
    bin_start_i <- tally_info$index[1] + tally_info$values[1]
    bin_end_i <- bin_start_i + tally_info$numBin[1] - 1
    tally_lines_i <- mcnp_output[bin_start_i:bin_end_i]

    tally_data <- mcnparse_pull_tally_data(tally_lines_i, tally_info$name[1])

    # Pull subsequent tallies and append to dataframe
    for (i in 2:nrow(tally_info)) {
        print(i)
        bin_start_i <- tally_info$index[i] + tally_info$values[i]
        bin_end_i <- bin_start_i + tally_info$numBin[i] - 1
        tally_lines_i <- mcnp_output[bin_start_i:bin_end_i]
        tally_data <- rbind(tally_data, mcnparse_pull_tally_data(
            tally_lines_i,
            tally_info$name[i]
        )
        )
    }

    # Get data frame for tally statistical checks ----
    stat_check <- mcnparse_stat_results(mcnp_output)

    # Compile the data
    tally_data$run_id <- file_name
    stat_check$run_id <- file_name

    return(list(
        data = tally_data,
        check = stat_check
    ))
}


#' Title
#'
#' The data for the mcnp runs contains four columns:
#  - bin_mev; the energy of your bin in MeV
#  - value; the value of the tally
#  - re; the relative uncertainty of the value
#  - Tally; the tally number used in the input
#  - run_id; the file_name for the given run
#'
#'
#' @param deck_list String vector containing the file_names (without extension) to be used
#' @param directory String containing the path to the directory of the output files. By default, this will be your current working directory. The command setwd("path/to/directory") will change the working directory in R.
#' @param extension String containing the file_name extension for all output files if no argument is provided, this will use ".mcnpout".
#' @param write_files Boolean. When set to true, the data for the runs will be saved to "mcnpData.csv" and the statistical checks will be saved to "statisticalChecks.csv".
#'
#' @return named list with element data: dataframe containing the data check: dataframe containing the statistical checks
#' @export
#'
#' @examples
#'
#' `mcnparse(c("run1", "run2", "run3"), directory = "", extension = ".o", write_files = FALSE)`
#' ## Will parse the files "run1.o", "run2.o", "run3.o" and will not save the output to excel.
mcnparse <- function(
        deck_list,
        directory = "",
        extension = ".mcnpout",
        write_files = TRUE) {

    results_i <- mcnparse_one(deck_list[1], directory, extension)
    all_data <- results_i$data
    all_stat <- results_i$check

    for (i in 2:length(deck_list)) {
        results_i <- mcnparse_one(deck_list[i], directory, extension)
        all_data <- rbind(all_data, results_i$data)
        all_stat <- rbind(all_stat, results_i$check)
    }

    if (write_files) {
        write.csv(all_data, file = "mcnpData.csv", row.names = FALSE)
        write.csv(all_stat, file = "statisticalChecks.csv", row.names = FALSE)
    } else {
        return(list(
            data = all_data,
            check = all_stat
        ))
    }
}
