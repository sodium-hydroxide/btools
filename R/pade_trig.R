
pade_sine <- function(x) {
    if (x < 0.0){
        return(-1.0 * pade_sine(-1.0 * x))
    }
    else if (x > 2.0 * pi) {
        return(pade_sine(x - (2.0 * pi)))
    }
    else if (x > pi) {
        return(-1.0 * pade_sine(x - pi))
    }
    else {
        return(
            (
                x
                - ((2363.0 / 18183.0) * (x ** 3))
                + ((12671.0 / 4363920.0) * (x ** 5))
            ) / (
                1.0
                + ((445.0 / 12122.0) * (x ** 2))
                + ((601.0 / 872784.0) * (x ** 4))
                + ((121.0 / 16662240.0) * (x ** 6))
            )
        )
    }
}

pade_cosine <- function(x) {
    return(pade_sine(x + (0.5 * pi)))
}

pade_tangent <- function(x) {
    return(pade_sine(x) / pade_cosine(x))
}