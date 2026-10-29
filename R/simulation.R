#' Inverse-transform sampling
#'
#' @description If U is uniform(0,1) and F is a cdf, then F^-1(U) has cdf F.
#'   This applies the quantile function to supplied uniforms.
#'
#' @details **When to use:** "use these uniform random numbers to simulate a
#'   chi-square / exponential / gamma sample". The result matches
#'   `-log(1 - u) / rate` for an exponential, and `-2 * log(1 - u)` for
#'   chi-square with 2 df.
#'
#' @param u Numeric vector of uniforms in (0, 1).
#' @param qfun A quantile function such as `qchisq`, `qexp`, `qgamma`.
#' @param ... Further arguments for `qfun` (for example `df = 2`).
#' @return Numeric vector of simulated values.
#' @examples
#' u <- cs1b_data("chisq15_u")
#' x <- cs1b_inverse_transform(u, qchisq, df = 2)
#' sum(x)   # 30.98
#' @export
cs1b_inverse_transform <- function(u, qfun, ...) {
  if (any(u <= 0 | u >= 1)) stop("u must lie strictly between 0 and 1")
  qfun(u, ...)
}

#' Simulate a sampling distribution (central limit theorem)
#'
#' @description Repeats "draw n values, compute a statistic" `reps` times and
#'   optionally draws the histogram with a smoothed density.
#'
#' @details **When to use:** "generate 1,000 samples of size 15 and ...", or to
#'   show the CLT: compare small and large `n`. Equivalent to
#'   `replicate(reps, stat(rfun(n, ...)))`, so the random stream (and seed)
#'   matches that hand-written form.
#'
#' @param rfun A random generator with first argument the sample size, e.g.
#'   `rnorm`, `rchisq`, `rbeta`, `rexp`.
#' @param n Sample size of each replication.
#' @param reps Number of replications.
#' @param stat Function applied to each sample: `mean`, `sum`, `var`, ...
#' @param seed Optional integer for `set.seed()`.
#' @param plot Logical, draw histogram and density.
#' @param main,xlab Plot labels.
#' @param ... Extra arguments passed to `rfun` (e.g. `df = 2`, or shape
#'   parameters).
#' @return Numeric vector of `reps` simulated statistics.
#' @examples
#' y <- cs1b_clt_sim(rchisq, n = 15, reps = 1000, stat = sum, seed = 47, df = 2)
#' mean(y)  # near 30 = mean of chi-square(30)
#' @export
cs1b_clt_sim <- function(rfun, n, reps = 1000, stat = mean, seed = NULL,
                         plot = TRUE, main = "Histogram of simulated statistic",
                         xlab = "value", ...) {
  if (!is.null(seed)) set.seed(seed)
  out <- replicate(reps, stat(rfun(n, ...)))
  if (plot) {
    hist(out, freq = FALSE, main = main, xlab = xlab)
    lines(density(out))
  }
  out
}

#' Normal QQ plot
#'
#' @description Normal QQ plot with the reference line.
#'
#' @details **When to use:** to check normality of data, regression residuals
#'   (`residuals(model)`) or ANOVA residuals. Points hugging the line mean
#'   normal; a curve means skew; an S-shape means heavy or light tails. Larger
#'   samples hug the line more closely.
#'
#' @param x Numeric vector.
#' @param main Plot title.
#' @return `x`, invisibly.
#' @examples
#' set.seed(1)
#' cs1b_qq(rnorm(50))
#' @export
cs1b_qq <- function(x, main = "Normal Q-Q plot") {
  qqnorm(x, main = main)
  qqline(x)
  invisible(x)
}

#' Simulate binomial counts and compare with the normal approximation
#'
#' @description Simulates `reps` binomial(n, p) counts, reports their mean and
#'   variance against np and np(1-p), and overlays the N(np, np(1-p)) density.
#'
#' @details **When to use:** "simulate 5000 repeats of the number out of 100
#'   and comment on the normal approximation". The approximation is good when
#'   both np and n(1-p) are large (rule of thumb above 5 to 10).
#'
#' @param n Number of trials per count.
#' @param p Success probability.
#' @param reps Number of simulated counts.
#' @param seed Optional integer seed.
#' @param plot Logical, draw histogram, density and normal curve.
#' @return A `cs1b_result` list: `sim_mean`, `sim_var`, `theory_mean`,
#'   `theory_var`, `approx_ok`, `counts`.
#' @examples
#' r <- cs1b_binom_normal(100, 0.4, 5000, seed = 2025, plot = FALSE)
#' r$theory_mean; r$theory_var
#' @export
cs1b_binom_normal <- function(n = 100, p = 0.4, reps = 5000, seed = NULL, plot = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  counts <- rbinom(reps, size = n, prob = p)
  tm <- n * p; tv <- n * p * (1 - p)
  if (plot) {
    hist(counts, freq = FALSE, xlab = "Count", main = "Simulated binomial")
    lines(density(counts))
    curve(dnorm(x, tm, sqrt(tv)), add = TRUE, lty = 2)
    legend("topright", legend = c("Estimated density", "Normal approx"), lty = 1:2)
  }
  .res(sim_mean = mean(counts), sim_var = var(counts), theory_mean = tm,
       theory_var = tv, approx_ok = (n * p >= 10 && n * (1 - p) >= 10),
       counts = counts)
}
