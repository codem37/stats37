#' Bootstrap a statistic (non-parametric or parametric)
#'
#' @description Resamples the data (non-parametric) or simulates from a fitted
#'   model (parametric) `reps` times and computes a statistic each time.
#'
#' @details **When to use:** Feb 2025 Q1. Non-parametric resamples the data with
#'   replacement (cannot exceed the observed maximum, spread near
#'   sd/sqrt(n)). Parametric fits the named distribution by maximum likelihood
#'   and simulates from it: with `"exponential"`, rate = 1/mean, so the spread
#'   of the mean is mean/sqrt(n), much larger than the non-parametric one when
#'   the data are less variable than an exponential, hinting that the model
#'   fits poorly. The random stream matches the hand-written `replicate` code.
#'
#' @param x Numeric data.
#' @param reps Number of bootstrap samples.
#' @param type `"nonparametric"`, `"exponential"`, `"normal"` or `"poisson"`.
#' @param stat Statistic function (default `mean`).
#' @param seed Optional integer seed.
#' @param plot Logical, histogram with density.
#' @param conf.level Level of the percentile interval.
#' @return A `cs1b_result` list: `boot` (all values), `mean`, `sd`,
#'   `percentile_ci`, `lambda_hat` (exponential only).
#' @examples
#' d <- cs1b_data("delivery")
#' cs1b_bootstrap(d, 1000, "nonparametric", seed = 2025, plot = FALSE)$sd
#' cs1b_bootstrap(d, 1000, "exponential", seed = 2025, plot = FALSE)$sd
#' @export
cs1b_bootstrap <- function(x, reps = 1000,
                           type = c("nonparametric", "exponential", "normal", "poisson"),
                           stat = mean, seed = NULL, plot = TRUE, conf.level = 0.95) {
  type <- match.arg(type); n <- length(x)
  if (!is.null(seed)) set.seed(seed)
  gen <- switch(type,
    nonparametric = function() sample(x, n, replace = TRUE),
    exponential = function() rexp(n, rate = 1 / mean(x)),
    normal = function() rnorm(n, mean(x), sd(x)),
    poisson = function() rpois(n, mean(x)))
  b <- replicate(reps, stat(gen()))
  if (plot) {
    hist(b, freq = FALSE, xlab = "Bootstrap statistic", main = paste("Bootstrap:", type))
    lines(density(b))
  }
  a <- (1 - conf.level) / 2
  out <- list(method = paste("Bootstrap,", type), boot = b, mean = mean(b), sd = sd(b),
              percentile_ci = quantile(b, c(a, 1 - a)))
  if (type == "exponential") out$lambda_hat <- 1 / mean(x)
  do.call(.res, out)
}

#' Poisson log-likelihood on a grid and its MLE
#'
#' @description Evaluates l(lambda) = S log(lambda) - n lambda on a grid,
#'   returns the grid maximiser, the exact MLE S/n and a numerical optimum.
#'
#' @details **When to use:** "plot the log-likelihood and estimate lambda from
#'   the graph" (May 2024 Q4). The grid estimate agrees with S/n to within the
#'   grid spacing.
#'
#' @param S Total of the Poisson observations.
#' @param n Number of observations.
#' @param lambda Grid of candidate values.
#' @param plot Logical, draw the log-likelihood.
#' @return A `cs1b_result` list: `grid_mle`, `exact_mle`, `optimise_mle`,
#'   `loglik`.
#' @examples
#' cs1b_loglik_grid(S = 280, n = 150, plot = FALSE)
#' @export
cs1b_loglik_grid <- function(S, n, lambda = seq(0.1, 3.3, by = 0.1), plot = TRUE) {
  ll <- S * log(lambda) - n * lambda
  if (plot) plot(lambda, ll, xlab = "lambda", ylab = "Log_likelihood",
                 main = "Log-likelihood of lambda")
  .res(method = "Poisson log-likelihood", grid_mle = lambda[which.max(ll)],
       exact_mle = S / n,
       optimise_mle = optimise(function(l) S * log(l) - n * l,
                               interval = range(lambda), maximum = TRUE)$maximum,
       loglik = data.frame(lambda = lambda, loglik = ll))
}

#' Maximise any log-likelihood numerically
#'
#' @description Wrapper for `optim` that maximises a log-likelihood function.
#'
#' @details **When to use:** a likelihood with no closed-form MLE, or several
#'   parameters. For one parameter supply `lower` and `upper` (Brent method);
#'   for several, `optim` uses BFGS from `start`. The function must take the
#'   parameter vector as its first argument and return the log-likelihood.
#'
#' @param loglik Function of the parameter vector (first argument) returning
#'   the log-likelihood.
#' @param start Starting value(s).
#' @param lower,upper Bounds, required for a single parameter.
#' @param ... Extra arguments passed to `loglik` (e.g. the data).
#' @return A `cs1b_result` list: `estimate`, `loglik`, `convergence`.
#' @examples
#' x <- c(2, 3, 1, 4, 2, 2, 5)
#' cs1b_mle(function(l, x) sum(dpois(x, l, log = TRUE)), 1, lower = 0.01, upper = 20, x = x)
#' @export
cs1b_mle <- function(loglik, start, lower = NULL, upper = NULL, ...) {
  if (length(start) == 1) {
    if (is.null(lower) || is.null(upper)) stop("one parameter: supply lower and upper")
    o <- optim(start, loglik, ..., method = "Brent", lower = lower, upper = upper,
               control = list(fnscale = -1))
  } else {
    o <- optim(start, loglik, ..., method = "BFGS", control = list(fnscale = -1))
  }
  .res(method = "Numerical maximum likelihood", estimate = o$par, loglik = o$value,
       convergence = o$convergence)
}
