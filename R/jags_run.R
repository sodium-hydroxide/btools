#' Interface with JAGS
#'
#' @description
#' This function allows for interfacing with the JAGS (just another gibbs
#' sampler) for Markov Chain Monte Carlo sampling of posterior distributions
#'
#'
#' @param model String vector containing the model parameters
#' @param data List, data for likelihood and prior distribution
#' @param variables String vector, variables to keep track of
#' @param burn_in Integer, number of burn in steps to perform
#' @param steps Number of sampling steps to perform
#' @param thin Thinning interval
#'
#' @return modeul_output Named list containing arrays of sampled posterior distributions
#' @export
#'
jags_run <- function(
        model,
        data = list(),
        variables = c(),
        burn_in = 1,
        steps = 1,
        thin = 1,
        directory = "models/jags/") {

    file_name <- paste(
        directory,
        "model-",
        strftime(Sys.time(),"%Y%m%d-%H%M%S"),
        ".temp",
        sep = "")

    file_conn <- file(file_name)
    writeLines(model, file_conn)
    close(file_conn)

    if (
        all(is.na(data))
    ) {
        jags_model <- rjags::jags.model(file = file_name)
    } else {
        jags_model <- rjags::jags.model(file = file_name, data = data)
    }

    update(jags_model, burn_in)

    jags_draw <- rjags::jags.samples(
        jags_model,
        n.iter = steps,
        thin = thin,
        variable.names = variables
    )

    model_output <- list()

    for (name in names(jags_draw)) {
        current_array <- as.array(jags_draw[[name]][,,1])
        # Transpose 2-D arrays

        if(
            length(dim(current_array)) == 2
        ) {
            current_array <- t(current_array)
        }

        model_output[[name]] <- current_array
    }

    return(model_output)
}
