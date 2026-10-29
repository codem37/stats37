#' Gamma prior with Poisson data: posterior analysis
#'
#' @description Conjugate update Gamma(alpha, lambda) prior + Poisson data with
#'   total S over n observations gives a Gamma(alpha + S, lambda + n)
#'   posterior. Returns the Bayes estimates under each loss function, a credible
#'   interval and tail probabilities.
#'
#' @details **When to use:** claim counts or accident counts with a Gamma prior
#'   on the Poisson mean (Feb 2025 Q4, Nov 2025 Q2). A prior of Exponential(r)
#'   is Gamma(1, r). Estimates: squared-error loss gives the posterior mean
#'   a/l; absolute-error gives the median; all-or-nothing gives the mode
#'   (a - 1)/l (only when a >= 1). Rate parametrisation is used (R's
#'   `gamma` functions use rate unless `scale =` is named).
#'
#' @param x Observed counts. Alternatively give `n` and `S`.
#' @param n,S Number of observations and total count (used if `x` is `NULL`).
#' @param alpha Prior shape.
#' @param lambda Prior rate.
#' @param level Credible interval level (equal-tailed).
#' @param threshold Optional value(s) c: returns P(theta < c), P(theta > c) and
#'   the posterior density at c, to judge feasibility.
#' @param plot Logical: overlay prior (solid) and posterior (dashed).
#' @param from,to Plot range. Defaults to the 0.1 and 99.9 percent posterior
#'   quantiles.
#' @return A `cs1b_result` list: `prior`, `posterior` (shape, rate), `mean`,
#'   `median`, `mode`, `interval`, and if `threshold` given `p_below`,
#'   `p_above`, `density_at`.
#' @examples
#' # Nov 2025 Q2: posterior Gamma(20, 13), mean 1.5385, CI (0.9397, 2.2824)
#' cs1b_gamma_poisson(x = cs1b_data("claims8"), alpha = 2, lambda = 5, threshold = c(1, 5))
#' # Feb 2025 Q4: Exp(0.5) prior = Gamma(1, 0.5)
#' cs1b_gamma_poisson(x = cs1b_data("expressway"), alpha = 1, lambda = 0.5, level = 0.9)
#' @export
cs1b_gamma_poisson <- function(x = NULL, n = NULL, S = NULL, alpha, lambda,
                               level = 0.95, threshold = NULL, plot = FALSE,
                               from = NULL, to = NULL) {
  if (!is.null(x)) { n <- length(x); S <- sum(x) }
  if (is.null(n) || is.null(S)) stop("supply x, or both n and S")
  a <- alpha + S; l <- lambda + n
  tail <- (1 - level) / 2
  out <- list(method = "Gamma-Poisson Bayesian update",
              prior = c(shape = alpha, rate = lambda), n = n, total = S,
              posterior = c(shape = a, rate = l), mean = a / l,
              median = qgamma(0.5, a, l), mode = if (a >= 1) (a - 1) / l else NA_real_,
              interval = qgamma(c(tail, 1 - tail), a, l))
  if (!is.null(threshold)) {
    out$p_below <- pgamma(threshold, a, l)
    out$p_above <- pgamma(threshold, a, l, lower.tail = FALSE)
    out$density_at <- dgamma(threshold, a, l)
  }
  if (plot) {
    lo <- if (is.null(from)) qgamma(0.001, a, l) else from
    hi <- if (is.null(to)) qgamma(0.999, a, l) else to
    g <- seq(lo, hi, length.out = 400)
    yl <- c(0, max(dgamma(g, a, l), dgamma(g, alpha, lambda)) * 1.05)
    curve(dgamma(x, alpha, lambda), from = lo, to = hi, ylim = yl,
          xlab = "theta", ylab = "Density", main = "Prior and posterior")
    curve(dgamma(x, a, l), add = TRUE, lty = 2)
    legend("topright", legend = c("Prior", "Posterior"), lty = 1:2)
  }
  do.call(.res, out)
}

#' Beta prior with binomial data: posterior analysis
#'
#' @description Beta(a, b) prior + x successes in n trials gives a
#'   Beta(a + x, b + n - x) posterior.
#'
#' @details **When to use:** a proportion (claim probability, acceptance rate)
#'   with a Beta prior. Squared-error loss gives the posterior mean, absolute
#'   error the median, all-or-nothing the mode (a' - 1)/(a' + b' - 2).
#'
#' @param x Number of successes.
#' @param n Number of trials.
#' @param a,b Prior shape parameters.
#' @param level Credible interval level (equal-tailed).
#' @return A `cs1b_result` list: `posterior`, `mean`, `median`, `mode`,
#'   `interval`.
#' @examples
#' cs1b_beta_binomial(x = 12, n = 40, a = 2, b = 2)
#' @export
cs1b_beta_binomial <- function(x, n, a, b, level = 0.95) {
  a1 <- a + x; b1 <- b + n - x; tail <- (1 - level) / 2
  .res(method = "Beta-binomial Bayesian update", prior = c(a = a, b = b),
       posterior = c(a = a1, b = b1), mean = a1 / (a1 + b1),
       median = qbeta(0.5, a1, b1),
       mode = if (a1 > 1 && b1 > 1) (a1 - 1) / (a1 + b1 - 2) else NA_real_,
       interval = qbeta(c(tail, 1 - tail), a1, b1))
}
