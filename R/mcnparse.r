#     MCNP Parse
#     Noah J. Blair
# This script contains several functions for parsing a series of MCNP output
# files. When using this script, enter the r terminal (or console if using
# Rstudio), and execute:
#
#   source("pathToFile/mcnparse.r")
#
# This will load the functions into your environment. To parse the files, 
# execute the function batchReadIn with the following arguments:
# 
#   - deck_list; a string vector containing the file_names (without extension)
#               to be used.
#       ex; deck_list = c("run1", "run2", "run3") will parse the files
#      "run1.mcnpout", "run2.mcnpout", "run3.mcnpout".
#   - directory; a string containing the path to the directory of the output
#                files. By default, this will be your current working directory.
#                The command setwd("path/to/directory") will change the working
#                directory in R.
#   - extension; a string containing the file_name extension for all output files
#                if no argument is provided, this will use ".mcnpout".
#   - write_files; a boolean. When set to true, the data for the runs will be
#                 saved to "mcnpData.csv" and the statistical checks will be
#                 saved to "statisticalChecks.csv".
#                 When FALSE, the program will return a named list with elements
#                   data:  dataframe containing the data
#                   check: dataframe containing the statistical checks
# 
# The data for the mcnp runs contains four columns:
#   - Ebin_MeV; the energy of your bin in MeV
#   - Value; the value of the tally
#   - RelUnc; the relative uncertainty of the value
#   - Tally; the tally number used in the input
#   - runID; the file_name for the given run
#
# The data for the statistical checks contains the following columns;
#   - Tally; the tally number used in the input
#   - A column for the observed value of each statistical check
#     - Mean_B
#     - RE
#     - RE_Decrease
#     - RE_Rate
#     - VOV
#     - VOV_Decrease
#     - VOV_Rate
#     - FOM
#     - FOM_Behavior
#     - PDFSlope
#   - runID; the file_name for the given run
#   
# This function does require the dplyr package. The script will automatically
# check whether it is installed, and will install it if needed. This package
# contains several functions for parsing dataframes.
# 
# In addition to the batchReadIn function, the following functions are included
# in this script:
# 
#   - str_split; takes a string and splits into a string vector (removing NA and
#                elements just containing a space) at a given split.
#   - identify_tally; parses the MCNP output to determine:
#                           - the number of tallies
#                           - the number of energy bins for each tally
#                           - the lines containing the tally data
#                           - the types of tallies being run
#   - pull_tally_data; given the lines for a binned tally, returns the value of
#                    the tally, energy bin, and relative error as a dataframe
#   - get_stat_results; given the entire MCNP output, finds the statistical checks
#                     and saves the observed values in a dataframe
#   - mcnparse; given the name of an output, will use the above functions to
#                find the statistical checks and tally values to be returned as
#                a set of dataframes
# 
# The batchReadIn function is likely the only function which needs to be called
# as it will automatically call other functions as needed. This has only been
# tested using F1, F2, F4, and F8 tallies. I cannot guarentee there won't be any
# bugs for other tallies, or the previously listed tallies.
# 

if (!("dplyr" %in%  installed.packages())) {install.packages("dplyr")}
library(dplyr)

str_split <- function(in_str, splt = " ") {
  in_str <- strsplit(in_str, split = splt)[[1]]

  in_str <- in_str[in_str != ""]

  out_str_vec <- in_str[!(1:length(in_str) %in% grep(" ", in_str))]

  return(out_str_vec)
}

identify_tally <- function(mcnp_output) {
  # Initialize dataframe
  tally_info <- data.frame(index = which(grepl("1tally  ",mcnp_output))) |> 
    mutate(
      name = "",
      type = "",
      values = 0,
      numBin = 0
    ) |> 
    relocate(index, .before = values)

  # Find lines containing tally name, type, start point
  for (
    i in 1:nrow(tally_info)
  ) {

    # Determine which tally we are looking at
    tally_i <- str_split(mcnp_output[tally_info$index[i]])[2]

    tally_info$name[i] <- tally_i

    # Split tally for tally type as the last character
    tally_type_i <- str_split(tally_i, splt = "")
    tally_type_i <- tally_type_i[length(tally_type_i)]

    # Determine if the tally is starred or not
    energy_weighted <- "energy" %in% str_split(
      mcnp_output[tally_info$index[i] + 1]
    )

    # Determine tally type
    tally_info$type[i] <- tally_type_i

    # Determine where the energy bins start
    tally_info$values[i] <- 6

    if (tally_info$type[i] == "4"){
      tally_info$values[i] <- tally_info$values[i] + 4
    }

    # Make corrections for energy weighted
    if(energy_weighted){
      tally_info$type[i] <- paste("*F", tally_info$type[i], sep = "")
    }else{
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
      which(grepl("  total", tally_section))[1] - tally_info$values[i] - 1

    if (tally_type_i == "2") {
      tally_info$numBin[i] <- tally_info$numBin[i] - 4
    }

  }

  # Return the tally information
  return(tally_info)
}

pull_tally_data <- function(in_tally_lines, tally_name) {
  # This function will pull the data from each tally and return a dataframe
  # containing vectors for the information
  # Function acts as subroutine for the mcnparse function

  num_lines <- length(in_tally_lines)

  energy_vect <- rep(0, num_lines)
  tally_vect <- rep(0, num_lines)
  rsd_vect <- rep(0, num_lines)

  # Loop over all lines. Get the energy, tally, and relative uncertainty
  for(
    i in 1:num_lines
  ){
    # Get current line, remove elements with spaces and blank elements
    line_i <- str_split(in_tally_lines[i])

    energy_vect[i] <- as.numeric(line_i[1])
    tally_vect[i] <- as.numeric(line_i[2])
    rsd_vect[i] <- as.numeric(line_i[3])

  }

  return(data.frame(
    Ebin_MeV = energy_vect,
    Value = tally_vect,
    RelUnc = rsd_vect
  ) |>
  mutate(Tally = tally_name))

}

# Get results of statistical checks for a particular run
get_stat_results <- function(mcnp_output) {

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
    Tally        = rep("", num_tallies),
    Mean_B       = rep("", num_tallies),
    RE           = rep("", num_tallies),
    RE_Decrease  = rep("", num_tallies),
    RE_Rate      = rep("", num_tallies),
    VOV          = rep("", num_tallies),
    VOV_Decrease = rep("", num_tallies),
    VOV_Rate     = rep("", num_tallies),
    FOM          = rep("", num_tallies),
    FOM_Behavior = rep("", num_tallies),
    PDFSlope     = rep("", num_tallies)
  )

  # Get the tally being calculated and the value of each stat. test
  for (
    i in 1:length(stat_summaries)
  ) {
    # Save tally statistical tests values
    stat_check[i, ] <- str_split(stat_summaries[i])

    # Save which tally is being used
    tally_i <- str_split(tally[i])

    stat_check$Tally[i] <- tally_i[length(tally_i)]
  }

  return(stat_check)
}

one_file_parse <- function(file_name, directory = "", extension = ".mcnpout") {
  # Read MCNP output file in and find location of information----
  mcnp_output <- paste(
    directory,
    file_name,
    extension,
    sep = ""
  ) |>
    readLines() |>
    as.vector()

  tally_info <- identify_tally(mcnp_output)

  # Get data frame for binned tallies ----

  # Pull the first tally
  bin_start_i <- tally_info$index[1] + tally_info$values[1]
  bin_end_i <- bin_start_i + tally_info$numBin[1] - 1
  tally_lines_i <- mcnp_output[bin_start_i:bin_end_i]

  tally_data <- pull_tally_data(tally_lines_i, tally_info$name[1])

  # Pull subsequent tallies and append to dataframe
  for (i in 2:nrow(tally_info)) {
    bin_start_i <- tally_info$index[1] + tally_info$values[i]
    bin_end_i <- bin_start_i + tally_info$numBin[i]
    tally_lines_i <- mcnp_output[bin_start_i:bin_end_i]
    tally_data <- rbind(tally_data, pull_tally_data(
      tally_lines_i,
      tally_info$name[i]
      )
    )
  }

  # Get data frame for tally statistical checks ----
  stat_check <- get_stat_results(mcnp_output)

  # Compile the data
  tally_data$runID <- file_name
  stat_check$runID <- file_name

  return(list(
    data = tally_data,
    check = stat_check
  ))

}

mcnparse <- function(
    deck_list,
    directory = "",
    extension = ".mcnpout",
    write_files = TRUE) {

  results_i <- one_file_parse(deck_list[1], directory, extension)
  all_data <- results_i$data
  all_stat <- results_i$check

  for (i in 2:length(deck_list)) {
    results_i <- one_file_parse(deck_list[i], directory, extension)
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