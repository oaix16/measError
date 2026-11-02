#' Generate a measurement-error regression data set
#' @param n Sample size.
#' @param beta0 Intercept.
#' @param beta1 Slope.
#' @param sigmaE Standard deviation of regression noise.
#' @param sigmaU Standard deviation(s) of measurement error.
#' @param sigmaQ Standard deviation(s) of the latent regressor.
#' @return A list with latent x, observed x, response y, and the supplied scales.
#' @export
generate_data <- function(n, beta0=1, beta1=2, sigmaE=0.2, sigmaU, sigmaQ) {
    stopifnot(length(n)==1, n >= 2, sigmaE > 0)
    if (length(sigmaU) == 1) sigmaU <- rep(sigmaU,n)
    if (length(sigmaQ) == 1) sigmaQ <- rep(sigmaQ,n)
    stopifnot(length(sigmaU)==n, length(sigmaQ)==n, all(sigmaU>0), all(sigmaQ>0))
    x <- rnorm(n, sd=sigmaQ)
    y <- beta0 + beta1*x + rnorm(n, sd=sigmaE)
    xobs <- x + rnorm(n, sd=sigmaU)
    list(x=x, xobs=xobs, y=y, sigmaU=sigmaU, sigmaQ=sigmaQ)
}

#' Generate data for the homework simulation
#' @inheritParams generate_data
#' @export
getData <- function(n, beta0=1, beta1=2, sigmaE=0.2, sigmaU, sigmaQ) {
    generate_data(n,beta0,beta1,sigmaE,sigmaU,sigmaQ)
}

#' Naive OLS regression using the observed regressor
#' @param data List returned by getData, or a list containing xobs and y.
#' @return c(intercept, slope)
#' @export
naiveRegression <- function(data) {
    x <- data$xobs; y <- data$y
    if(length(x)!=length(y) || length(x)<2) stop("invalid data")
    coef(lm(y ~ x))
}

#' Corrected regression for known heteroscedastic additive measurement error
#' @param data List returned by getData.
#' @return c(intercept, slope)
#' @export
correctedRegression <- function(data) {
    x <- data$xobs; y <- data$y
    su2 <- data$sigmaU^2
    if(length(x)!=length(y) || length(su2)!=length(x)) stop("invalid data")
    xc <- x - mean(x); yc <- y - mean(y)
    denom <- sum(xc^2 - su2)
    if(!is.finite(denom) || denom <= 0) stop("corrected variance is non-positive")
    b1 <- sum(xc*yc)/denom
    b0 <- mean(y)-b1*mean(x)
    c(`(Intercept)`=b0, x=b1)
}
