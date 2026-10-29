#' Mean, variance and cdf of a discrete distribution given as a table
#'
#' @description Computes mu = sum(x p(x)), variance = sum(x^2 p(x)) - mu^2, the
#'   standard deviation and the cumulative probabilities.
#'
#' @details **When to use:** a question gives possible outcomes and their
#'   probabilities (casino game, lottery pay-outs, claim sizes). Never use
#'   `mean()` or `sd()` on the outcome vector: those weight every outcome
#'   equally.
#'
#' @param outcomes Numeric vector of possible values x.
#' @param prob Numeric vector of probabilities, same length, summing to 1.
#' @param plot Logical. If `TRUE`, draws the probability bar chart and the
#'   step-function cdf (two panels).
#' @return A `cs1b_result` list: `cumul`, `mean`, `variance`, `sd`.
#' @examples
#' cs1b_discrete(c(-400, 0, 100, 800), c(0.32, 0.48, 0.15, 0.05))
#' @export
cs1b_discrete <- function(outcomes, prob, plot = FALSE) {
  if (length(outcomes) != length(prob)) stop("outcomes and prob must have the same length")
  if (abs(sum(prob) - 1) > 1e-8) stop("prob must sum to 1 (sums to ", sum(prob), ")")
  cumul <- cumsum(prob)
  mu <- sum(outcomes * prob)
  v <- sum(outcomes^2 * prob) - mu^2
  if (plot) {
    op <- par(mfrow = c(1, 2)); on.exit(par(op))
    barplot(prob, names.arg = outcomes, xlab = "x", ylab = "Probability",
            main = "Distribution of X")
    plot(outcomes, cumul, type = "s", xlab = "x", ylab = "P(X <= x)",
         main = "Cumulative distribution of X")
  }
  .res(cumul = cumul, mean = mu, variance = v, sd = sqrt(v))
}

#' Linear combination of independent normal variables
#'
#' @description For W = sum(a_i X_i) with independent X_i ~ N(mu_i, sd_i^2):
#'   mean = sum(a_i mu_i), variance = sum(a_i^2 sd_i^2). Optionally returns a
#'   probability for W.
#'
#' @details **When to use:** "M is at least twice K", "the difference between
#'   two prices", "a portfolio of independent returns". Variances add with the
#'   *squared* coefficient, even for a difference.
#'
#' @param means Vector of means.
#' @param sds Vector of standard deviations (not variances).
#' @param coefs Coefficients a_i. Default all 1 (a sum). Use `c(1, -2)` for
#'   M - 2K.
#' @param threshold Optional value w. If given, returns P(W <= w) or P(W > w).
#' @param lower.tail `TRUE` for P(W <= w), `FALSE` for P(W > w).
#' @return A `cs1b_result` list: `mean`, `sd`, and `prob` if a threshold was
#'   supplied.
#' @examples
#' # Nov 2023 Q1: P(Mumbai price >= 2 x Kolkata price) = 0.2827
#' cs1b_norm_lincomb(c(5400, 3600), c(900, 1500), c(1, -2),
#'                   threshold = 0, lower.tail = FALSE)
#' @export
cs1b_norm_lincomb <- function(means, sds, coefs = rep(1, length(means)),
                              threshold = NULL, lower.tail = TRUE) {
  m <- sum(coefs * means)
  s <- sqrt(sum(coefs^2 * sds^2))
  out <- list(mean = m, sd = s)
  if (!is.null(threshold)) out$prob <- pnorm(threshold, m, s, lower.tail = lower.tail)
  do.call(.res, out)
}

#' Skewness of a Beta distribution
#'
#' @description skewness = 2 (b - a) sqrt(a + b + 1) / ((a + b + 2) sqrt(a b)).
#'
#' @details **When to use:** to explain the shape of Beta(a, b). Negative
#'   skewness (a > b) means a long left tail and mass near 1; positive means a
#'   long right tail and mass near 0; 0 (a = b) is symmetric.
#'
#' @param a,b Shape parameters (`shape1`, `shape2`), both positive.
#' @return Numeric skewness (vectorised in `a` and `b`).
#' @examples
#' cs1b_beta_skew(5, 1)   # -1.1832
#' cs1b_beta_skew(1, 5)   # +1.1832
#' cs1b_beta_skew(3, 3)   #  0
#' @export
cs1b_beta_skew <- function(a, b) {
  2 * ((b - a) / (a + b + 2)) * sqrt((a + b + 1) / (a * b))
}

#' Hypergeometric versus binomial moments
#'
#' @description Mean and variance of the number of "successes" in `k` draws
#'   from a finite population, with and without replacement.
#'
#' @details **When to use:** lottery/sampling questions where you must explain
#'   why the binomial is wrong. The means agree; the hypergeometric variance is
#'   smaller by the finite-population correction (N - k)/(N - 1).
#'
#' @param N Population size (total balls).
#' @param K Number of success items in the population.
#' @param k Number of draws.
#' @return A `cs1b_result` list: `mean`, `var_binomial`, `var_hypergeometric`,
#'   `fpc`.
#' @examples
#' # Dec 2022 Q4: 5 balls on the ticket out of 69, 5 drawn
#' cs1b_hyper_moments(N = 69, K = 5, k = 5)
#' @export
cs1b_hyper_moments <- function(N, K, k) {
  p <- K / N
  vb <- k * p * (1 - p)
  fpc <- (N - k) / (N - 1)
  .res(mean = k * p, var_binomial = vb, var_hypergeometric = vb * fpc, fpc = fpc)
}

#' Compare hypergeometric and binomial probabilities
#'
#' @description Tabulates P(X = x) under sampling without replacement
#'   (`dhyper`) and with replacement (`dbinom`).
#'
#' @details **When to use:** to show how much the binomial overstates the
#'   probability of extreme outcomes (for instance the jackpot) and therefore
#'   the expected pay-out.
#'
#' @param x Vector of counts of matches.
#' @param N Population size.
#' @param K Success items in the population.
#' @param k Number of draws.
#' @return Data frame with columns `x`, `hypergeometric`, `binomial`.
#' @examples
#' cs1b_hyper_vs_binom(0:5, N = 69, K = 5, k = 5)
#' @export
cs1b_hyper_vs_binom <- function(x, N, K, k) {
  data.frame(x = x,
             hypergeometric = dhyper(x, m = K, n = N - K, k = k),
             binomial = dbinom(x, size = k, prob = K / N))
}
