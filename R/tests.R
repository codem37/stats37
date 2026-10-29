#' z test for a mean
#'
#' @description Tests H0: mu = mu0 using z = (xbar - mu0) / (sigma / sqrt(n)),
#'   and gives the critical value and a confidence interval.
#'
#' @details **When to use:** sigma is known, or the question says to treat the
#'   sample standard deviation as the known value (Dec 2022 weights, Nov 2023
#'   difference sample). If sigma is estimated and n is small, use `t.test`.
#'   The alternative is decided by the wording: "reduction" is one-sided,
#'   "changed/different" is two-sided.
#'
#' @param x Numeric sample.
#' @param mu0 Hypothesised mean.
#' @param sigma Known standard deviation. If `NULL`, `sd(x)` is used (treated
#'   as known).
#' @param alternative `"two.sided"`, `"less"` or `"greater"`.
#' @param alpha Significance level.
#' @param conf.level Confidence level for the interval.
#' @return A `cs1b_result` list: `z`, `p_value`, `critical`, `conf_int`,
#'   `hypotheses`, `decision`.
#' @examples
#' set.seed(1)
#' cs1b_z_test(rnorm(30, 1300, 200), mu0 = 1375, sigma = 200, alternative = "less")
#' @export
cs1b_z_test <- function(x, mu0, sigma = NULL,
                        alternative = c("two.sided", "less", "greater"),
                        alpha = 0.05, conf.level = 0.95) {
  alternative <- match.arg(alternative)
  n <- length(x); xbar <- mean(x)
  sig <- if (is.null(sigma)) sd(x) else sigma
  se <- sig / sqrt(n)
  z <- (xbar - mu0) / se
  p <- switch(alternative, less = pnorm(z), greater = pnorm(z, lower.tail = FALSE),
              two.sided = 2 * pnorm(-abs(z)))
  crit <- switch(alternative, less = qnorm(alpha), greater = qnorm(1 - alpha),
                 two.sided = c(-1, 1) * qnorm(1 - alpha / 2))
  zq <- qnorm(1 - (1 - conf.level) / 2)
  .res(method = "z test for a mean",
       hypotheses = sprintf("H0: mu = %g vs H1: mu %s %g", mu0, .alt_symbol(alternative), mu0),
       n = n, xbar = xbar, sigma_used = sig, z = z, p_value = p, critical = crit,
       conf_int = xbar + c(-1, 1) * zq * se, decision = .decide(p, alpha))
}

#' Chi-square test and confidence interval for a standard deviation
#'
#' @description Uses (n - 1) S^2 / sigma0^2 ~ chi-square(n - 1) under H0.
#'
#' @details **When to use:** "test whether the standard deviation (or variance)
#'   is ...". R has no built-in test, so this does the `qchisq`/`pchisq` work.
#'   Requires approximately normal data. The p-value for a two-sided test is
#'   twice the smaller tail.
#'
#' @param x Numeric sample.
#' @param sigma0 Hypothesised standard deviation (not variance).
#' @param alternative `"two.sided"`, `"less"` or `"greater"`.
#' @param alpha Significance level.
#' @param conf.level Level for the interval for sigma.
#' @return A `cs1b_result` list: `statistic`, `df`, `critical`, `p_value`,
#'   `s2`, `sigma_ci`, `hypotheses`, `decision`.
#' @examples
#' x <- qchisq(cs1b_data("chisq15_u"), df = 2)
#' cs1b_chisq_var_test(x, sigma0 = 2.5)   # T = 7.306, p = 0.1554
#' @export
cs1b_chisq_var_test <- function(x, sigma0,
                                alternative = c("two.sided", "less", "greater"),
                                alpha = 0.05, conf.level = 0.95) {
  alternative <- match.arg(alternative)
  n <- length(x); df <- n - 1; s2 <- var(x)
  T <- df * s2 / sigma0^2
  lo <- pchisq(T, df); hi <- pchisq(T, df, lower.tail = FALSE)
  p <- switch(alternative, less = lo, greater = hi, two.sided = 2 * min(lo, hi))
  crit <- switch(alternative, less = qchisq(alpha, df), greater = qchisq(1 - alpha, df),
                 two.sided = qchisq(c(alpha / 2, 1 - alpha / 2), df))
  .res(method = "Chi-square test for a standard deviation",
       hypotheses = sprintf("H0: sigma = %g vs H1: sigma %s %g", sigma0,
                            .alt_symbol(alternative), sigma0),
       n = n, s2 = s2, statistic = T, df = df, critical = crit, p_value = p,
       sigma_ci = sqrt(df * s2 / qchisq(c((1 + conf.level) / 2, (1 - conf.level) / 2), df)),
       decision = .decide(p, alpha))
}

#' Confidence interval for sigma and sigma squared
#'
#' @description Interval from (n - 1) S^2 / sigma^2 ~ chi-square(n - 1).
#'
#' @details **When to use:** "construct a 90 percent confidence interval for the
#'   standard deviation". The *larger* chi-square quantile gives the lower
#'   limit. Assumes normal data.
#'
#' @param x Numeric sample.
#' @param conf.level Confidence level (0.9 for a 90 percent interval).
#' @return Named list with `sigma` and `variance` intervals (length-2 vectors).
#' @examples
#' cs1b_sd_ci(cs1b_data("delivery"), conf.level = 0.90)
#' @export
cs1b_sd_ci <- function(x, conf.level = 0.95) {
  n <- length(x); s2 <- var(x)
  q <- qchisq(c((1 + conf.level) / 2, (1 - conf.level) / 2), n - 1)
  v <- (n - 1) * s2 / q
  list(sigma = sqrt(v), variance = v)
}

#' Paired versus independent t tests on the same data
#'
#' @description Runs the paired t test, the two-sample t test and `var.test`,
#'   and tabulates them side by side.
#'
#' @details **When to use:** to show how the design changes the conclusion
#'   (Feb 2025 Q3: same paper marked twice is paired; different papers are
#'   independent). Use *paired* when each x has a natural partner (before/after,
#'   same subject, same paper); otherwise independent. For a one-sided claim,
#'   `alternative` refers to x minus y (a "reduction" from before to after is
#'   `"greater"` with x = before).
#'
#' @param x,y Numeric vectors (equal length if paired).
#' @param alternative `"two.sided"`, `"less"` or `"greater"`.
#' @param mu Hypothesised difference in means (default 0).
#' @param var.equal If `TRUE` (default) pooled-variance t test; if `FALSE`
#'   Welch. Check with the returned `var_test`.
#' @param conf.level Confidence level.
#' @param alpha Significance level for the decision wording.
#' @return A `cs1b_result` list: `summary` (data frame), `paired`,
#'   `independent`, `var_test` (all `htest` objects), `decision_paired`,
#'   `decision_independent`.
#' @examples
#' ae <- cs1b_data("ae_scores")
#' cs1b_paired_vs_indep(ae$AE_1, ae$AE_2)$summary
#' @export
cs1b_paired_vs_indep <- function(x, y, alternative = c("two.sided", "less", "greater"),
                                 mu = 0, var.equal = TRUE, conf.level = 0.95, alpha = 0.05) {
  alternative <- match.arg(alternative)
  if (length(x) != length(y)) stop("x and y must be the same length for a paired comparison")
  pt <- t.test(x, y, paired = TRUE, alternative = alternative, mu = mu, conf.level = conf.level)
  it <- t.test(x, y, var.equal = var.equal, alternative = alternative, mu = mu, conf.level = conf.level)
  vt <- var.test(x, y)
  tab <- data.frame(
    design = c("paired", "independent"),
    t = c(pt$statistic, it$statistic), df = c(pt$parameter, it$parameter),
    p_value = c(pt$p.value, it$p.value),
    ci_low = c(pt$conf.int[1], it$conf.int[1]), ci_high = c(pt$conf.int[2], it$conf.int[2]))
  .res(method = "Paired vs independent t tests",
       hypotheses = sprintf("H0: difference in means = %g vs H1: %s %g", mu,
                            .alt_symbol(alternative), mu),
       summary = tab, paired = pt, independent = it, var_test = vt,
       decision_paired = .decide(pt$p.value, alpha),
       decision_independent = .decide(it$p.value, alpha))
}

#' Chi-square goodness-of-fit test with automatic cell merging
#'
#' @description Compares observed counts with expected counts from specified
#'   probabilities. Merges cells with expected count below `min_expected` into a
#'   neighbour, and sets degrees of freedom to cells - 1 - parameters estimated.
#'
#' @details **When to use:** "does the data fit a Binomial(3, 0.2) / Poisson /
#'   specified model". Pass probabilities that already include the tail
#'   (they must sum to 1). If a parameter was *estimated from the data*, say
#'   so via `params_estimated`, since `chisq.test` cannot do this for you.
#'
#' @param obs Observed counts.
#' @param prob Model probabilities, summing to 1.
#' @param params_estimated Number of parameters estimated from the data.
#' @param min_expected Minimum expected count per cell (default 5).
#' @param merge Logical: merge small cells automatically.
#' @param alpha Significance level.
#' @return A `cs1b_result` list: `table` (data frame of merged cells with
#'   observed, expected, contribution), `statistic`, `df`, `p_value`,
#'   `hypotheses`, `decision`.
#' @examples
#' # Nov 2023 Q4: X-squared = 6.737, df = 2, p = 0.0344
#' cs1b_chisq_gof(c(61, 71, 15, 3), dbinom(0:3, 3, 0.2))
#' @export
cs1b_chisq_gof <- function(obs, prob, params_estimated = 0, min_expected = 5,
                           merge = TRUE, alpha = 0.05) {
  if (length(obs) != length(prob)) stop("obs and prob must have the same length")
  if (abs(sum(prob) - 1) > 1e-6) stop("prob must sum to 1; add a tail cell")
  O <- obs; E <- sum(obs) * prob; lab <- as.list(seq_along(O))
  while (merge && any(E < min_expected) && length(O) > 2) {
    i <- which.min(E); j <- if (i == length(E)) i - 1 else i + 1
    keep <- min(i, j); drop <- max(i, j)
    O[keep] <- O[keep] + O[drop]; E[keep] <- E[keep] + E[drop]
    lab[[keep]] <- c(lab[[keep]], lab[[drop]])
    O <- O[-drop]; E <- E[-drop]; lab <- lab[-drop]
  }
  df <- length(O) - 1 - params_estimated
  if (df < 1) stop("Non-positive degrees of freedom after merging")
  stat <- sum((O - E)^2 / E)
  p <- pchisq(stat, df, lower.tail = FALSE)
  .res(method = "Chi-square goodness of fit",
       hypotheses = "H0: the data follow the specified distribution vs H1: they do not",
       table = data.frame(cells = vapply(lab, paste, "", collapse = "+"),
                          observed = O, expected = E, contribution = (O - E)^2 / E),
       statistic = stat, df = df, p_value = p, decision = .decide(p, alpha))
}

#' Chi-square test of independence for a table (with Fisher for 2 x 2)
#'
#' @description Runs `chisq.test` (with and without Yates correction for a 2 x 2
#'   table), reports the expected counts, standardised residuals and a warning
#'   when any expected count is below 5, and adds `fisher.test` for 2 x 2.
#'
#' @details **When to use:** "is outcome independent of treatment/attendance/
#'   activity level". Remember R fills matrices by column, so build the matrix
#'   carefully (rows are one variable, columns the other). The chi-square test
#'   works for any r x c table; Fisher's exact test is intended for 2 x 2. Cells
#'   with `|stdres| > 2` drive any dependence.
#'
#' @param tab A matrix or table of counts.
#' @param alpha Significance level.
#' @return A `cs1b_result` list: `statistic`, `df`, `p_value`, `expected`,
#'   `stdres`, `min_expected`, `expected_ok`, and for 2 x 2 tables
#'   `uncorrected` and `fisher_p`.
#' @examples
#' cs1b_chisq_independence(cs1b_data("exam_success"))
#' @export
cs1b_chisq_independence <- function(tab, alpha = 0.05) {
  tab <- as.matrix(tab)
  ct <- suppressWarnings(chisq.test(tab))
  out <- list(method = "Chi-square test of independence",
              hypotheses = "H0: the two variables are independent vs H1: they are associated",
              statistic = unname(ct$statistic), df = unname(ct$parameter),
              p_value = ct$p.value, expected = ct$expected, stdres = ct$stdres,
              min_expected = min(ct$expected), expected_ok = all(ct$expected >= 5))
  if (!out$expected_ok)
    out$warning <- "Some expected counts are below 5: merge categories or use Fisher (2 x 2)."
  if (all(dim(tab) == 2)) {
    u <- suppressWarnings(chisq.test(tab, correct = FALSE))
    out$uncorrected <- list(statistic = unname(u$statistic), p_value = u$p.value)
    out$fisher_p <- fisher.test(tab)$p.value
  }
  out$decision <- .decide(ct$p.value, alpha)
  do.call(.res, out)
}

#' One-way ANOVA with assumption checks
#'
#' @description Fits `aov(y ~ g)` and returns group means and standard
#'   deviations, the ratio of largest to smallest sd (rule of thumb: below 2),
#'   the F test, and optionally Tukey's pairwise comparisons.
#'
#' @details **When to use:** comparing the means of three or more groups.
#'   Assumptions: independence, normal residuals (check with `cs1b_qq`
#'   on `result$residuals`), and equal variances (sd ratio below 2). H0: all
#'   group means equal; H1: at least one differs.
#'
#' @param y Numeric response.
#' @param g Grouping variable (converted to a factor).
#' @param tukey Logical: also compute `TukeyHSD`.
#' @param alpha Significance level.
#' @return A `cs1b_result` list: `means`, `sds`, `sd_ratio`, `F`, `df`,
#'   `p_value`, `anova` (table), `fit`, `residuals`, optionally `tukey`.
#' @examples
#' d <- cs1b_data("clinic")
#' cs1b_oneway_anova(d$thickness, d$clinic)$F   # 15.14
#' @export
cs1b_oneway_anova <- function(y, g, tukey = FALSE, alpha = 0.05) {
  g <- factor(g)
  fit <- aov(y ~ g)
  s <- summary(fit)[[1]]
  sds <- tapply(y, g, sd)
  p <- s[["Pr(>F)"]][1]
  out <- list(method = "One-way ANOVA",
              hypotheses = "H0: all group means are equal vs H1: at least one differs",
              means = tapply(y, g, mean), sds = sds, sd_ratio = max(sds) / min(sds),
              equal_var_ok = (max(sds) / min(sds) < 2),
              F = s[["F value"]][1], df = s[["Df"]], p_value = p, anova = s,
              fit = fit, residuals = residuals(fit))
  if (tukey) out$tukey <- TukeyHSD(fit)
  out$decision <- .decide(p, alpha)
  do.call(.res, out)
}

#' Test a stated regression coefficient value
#'
#' @description `summary()` tests coefficient = 0. This tests coefficient =
#'   `value` (for instance slope = 1, intercept = 100) with
#'   t = (estimate - value) / se on the residual degrees of freedom (a z test
#'   for a GLM).
#'
#' @details **When to use:** "the researcher claims BP = 100 + age": test
#'   intercept = 100 and slope = 1 separately. The `anova()` p-value only tests
#'   slope = 0 and is not an answer to this.
#'
#' @param model A fitted `lm` or `glm`.
#' @param term Coefficient name exactly as in `names(coef(model))`.
#' @param value Hypothesised value.
#' @param alternative `"two.sided"`, `"less"` or `"greater"`.
#' @param alpha Significance level.
#' @return A `cs1b_result` list: `estimate`, `se`, `statistic`, `df`,
#'   `p_value`, `hypotheses`, `decision`.
#' @examples
#' b <- cs1b_data("bp")
#' m <- lm(bp ~ age, data = b)
#' cs1b_test_coef(m, "age", 1)           # p = 0.505
#' cs1b_test_coef(m, "(Intercept)", 100) # p = 0.672
#' @export
cs1b_test_coef <- function(model, term, value = 0,
                           alternative = c("two.sided", "less", "greater"), alpha = 0.05) {
  alternative <- match.arg(alternative)
  est <- coef(summary(model))
  if (!term %in% rownames(est)) stop("term must be one of: ", paste(rownames(est), collapse = ", "))
  b <- est[term, "Estimate"]; se <- est[term, "Std. Error"]
  stat <- (b - value) / se
  if (inherits(model, "glm")) {
    df <- Inf
    p <- switch(alternative, less = pnorm(stat), greater = pnorm(stat, lower.tail = FALSE),
                two.sided = 2 * pnorm(-abs(stat)))
  } else {
    df <- df.residual(model)
    p <- switch(alternative, less = pt(stat, df), greater = pt(stat, df, lower.tail = FALSE),
                two.sided = 2 * pt(-abs(stat), df))
  }
  .res(method = "Test of a regression coefficient",
       hypotheses = sprintf("H0: %s = %g vs H1: %s %s %g", term, value, term,
                            .alt_symbol(alternative), value),
       estimate = b, se = se, statistic = stat, df = df, p_value = p,
       decision = .decide(p, alpha))
}
