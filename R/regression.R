#' Prediction with confidence and prediction intervals
#'
#' @description For an `lm`, returns the fit, the confidence interval for the
#'   *mean* response and the (wider) prediction interval for a *new*
#'   observation. For a `glm`, returns the fitted mean on the response scale
#'   with a Wald interval transformed by the inverse link.
#'
#' @details **When to use:** any "predict ... and give an interval" request.
#'   Use the confidence interval when asked about the average response (for
#'   example mean crime rate) and the prediction interval for a single new
#'   case. The column names in `newdata` must match the formula variable names
#'   exactly, and factors need the same levels as in the fitted data.
#'
#' @param model A fitted `lm` or `glm`.
#' @param newdata Data frame of new predictor values.
#' @param level Confidence level.
#' @return Data frame: `newdata` plus `fit`, `ci_lwr`, `ci_upr` and for `lm`
#'   `pi_lwr`, `pi_upr`.
#' @examples
#' b <- cs1b_data("bp")
#' m <- lm(bp ~ age, data = b)
#' cs1b_predict(m, data.frame(age = 45), level = 0.99)
#' @export
cs1b_predict <- function(model, newdata, level = 0.95) {
  if (inherits(model, "glm")) {
    p <- predict(model, newdata, type = "link", se.fit = TRUE)
    z <- qnorm(1 - (1 - level) / 2); li <- family(model)$linkinv
    return(data.frame(newdata, fit = li(p$fit), ci_lwr = li(p$fit - z * p$se.fit),
                      ci_upr = li(p$fit + z * p$se.fit)))
  }
  ci <- predict(model, newdata, interval = "confidence", level = level)
  pi <- predict(model, newdata, interval = "prediction", level = level)
  data.frame(newdata, fit = ci[, "fit"], ci_lwr = ci[, "lwr"], ci_upr = ci[, "upr"],
             pi_lwr = pi[, "lwr"], pi_upr = pi[, "upr"])
}

#' Compare linear models
#'
#' @description Tabulates R-squared, adjusted R-squared, residual standard
#'   error, AIC and BIC for several `lm` fits.
#'
#' @details **When to use:** choosing between models (dropping the least
#'   significant term, raw vs per-capita predictor). Prefer the higher
#'   adjusted R-squared and lower residual standard error / AIC. R-squared
#'   alone always rises with more terms.
#'
#' @param ... Two or more fitted `lm` objects.
#' @return Data frame with one row per model.
#' @examples
#' b <- cs1b_data("bp"); b$sq <- b$age^2
#' cs1b_compare_lm(m1 = lm(bp ~ age, b), m2 = lm(bp ~ age + sq, b))
#' @export
cs1b_compare_lm <- function(...) {
  ms <- list(...)
  nm <- names(ms)
  if (is.null(nm)) nm <- paste0("model", seq_along(ms))
  nm[nm == ""] <- paste0("model", seq_along(ms))[nm == ""]
  data.frame(model = nm,
    r_squared = vapply(ms, function(m) summary(m)$r.squared, 0),
    adj_r_squared = vapply(ms, function(m) summary(m)$adj.r.squared, 0),
    sigma = vapply(ms, function(m) summary(m)$sigma, 0),
    AIC = vapply(ms, AIC, 0), BIC = vapply(ms, BIC, 0), row.names = NULL)
}

#' Solve for the x that gives a target fitted value
#'
#' @description For a model that is linear in `var`, finds the value of `var`
#'   at which the fitted value equals `target`, holding other predictors fixed.
#'
#' @details **When to use:** "at what unemployment level is the fitted crime
#'   rate zero, holding density at 500" or "what area gives the target
#'   output". If the answer lies outside the observed range it is
#'   extrapolation; say so.
#'
#' @param model A fitted `lm`.
#' @param var Name of the predictor to solve for.
#' @param target Target fitted value (default 0).
#' @param fixed Named list of values for the other predictors.
#' @return Numeric value of `var`.
#' @examples
#' d <- data.frame(u = c(1, 2, 3, 4, 5), h = c(5, 3, 4, 1, 2), y = c(2, 4, 5, 8, 9))
#' m <- lm(y ~ u + h, d)
#' cs1b_solve_x(m, "u", target = 0, fixed = list(h = 3))
#' @export
cs1b_solve_x <- function(model, var, target = 0, fixed = list()) {
  mk <- function(v) as.data.frame(c(fixed, stats::setNames(list(v), var)))
  f0 <- unname(predict(model, mk(0))); f1 <- unname(predict(model, mk(1)))
  (target - f0) / (f1 - f0)
}

#' Compare two GLMs
#'
#' @description Reports AIC, BIC, log-likelihood and deviance for both models,
#'   and the chi-square drop-in-deviance test when they share a family and one
#'   is nested in the other.
#'
#' @details **When to use:** choosing between GLMs (drop Gender and its
#'   interaction; add interactions to a logistic model). Lower AIC is better
#'   and works across families. Deviances can be compared *only* for nested
#'   models of the same family and link, because each is measured against its
#'   own saturated model; a normal and a Poisson fit are not on the same scale.
#'
#' @param m1,m2 Fitted `glm` objects (order does not matter).
#' @param alpha Significance level for the likelihood-ratio decision.
#' @return A `cs1b_result` list: `table`, and when comparable `lrt`
#'   (`deviance_drop`, `df`, `p_value`) and `decision`.
#' @examples
#' set.seed(2)
#' d <- data.frame(x = rnorm(60), z = rnorm(60)); d$y <- rpois(60, exp(0.3 + 0.5 * d$x))
#' cs1b_glm_compare(glm(y ~ x, poisson, d), glm(y ~ x + z, poisson, d))
#' @export
cs1b_glm_compare <- function(m1, m2, alpha = 0.05) {
  tab <- data.frame(model = c("m1", "m2"),
    AIC = c(AIC(m1), AIC(m2)), BIC = c(BIC(m1), BIC(m2)),
    logLik = c(as.numeric(logLik(m1)), as.numeric(logLik(m2))),
    deviance = c(deviance(m1), deviance(m2)),
    df_residual = c(df.residual(m1), df.residual(m2)))
  out <- list(method = "GLM comparison", table = tab)
  same <- identical(family(m1)$family, family(m2)$family) &&
          identical(family(m1)$link, family(m2)$link)
  if (same && df.residual(m1) != df.residual(m2)) {
    big <- if (df.residual(m1) < df.residual(m2)) m1 else m2
    small <- if (df.residual(m1) < df.residual(m2)) m2 else m1
    drop <- deviance(small) - deviance(big)
    ddf <- df.residual(small) - df.residual(big)
    p <- pchisq(drop, ddf, lower.tail = FALSE)
    out$lrt <- list(deviance_drop = drop, df = ddf, critical = qchisq(1 - alpha, ddf), p_value = p)
    out$decision <- .decide(p, alpha)
  } else {
    out$note <- "Deviance test not shown (different family/link or same size). Compare AIC."
  }
  do.call(.res, out)
}

#' Log-likelihood from a printed AIC
#'
#' @description AIC = -2 logL + 2k, so logL = k - AIC / 2.
#'
#' @details **When to use:** a question prints the AIC and asks for the
#'   log-likelihood. `k` counts every estimated parameter (for a Gaussian
#'   `glm` or `lm`, include the variance).
#'
#' @param aic The AIC.
#' @param k Number of estimated parameters.
#' @return Numeric log-likelihood.
#' @examples
#' cs1b_loglik_from_aic(59.403, 3)   # -26.7015
#' @export
cs1b_loglik_from_aic <- function(aic, k) k - aic / 2

#' Poisson pricing: expected-claims premium and modified benefit
#'
#' @description Price = benefit per claim x expected number of claims lambda
#'   (from `predict(type = "response")`). Optionally prices a modified product
#'   paying `first` for the first claim and `later` for each further claim, and
#'   finds the lambda where the two products cost the same.
#'
#' @details **When to use:** Dec 2022 Q3. Modified price =
#'   later x lambda + (first - later) x P(N >= 1), with P(N >= 1) =
#'   1 - exp(-lambda). Low-risk customers (small lambda) pay more under the
#'   modification because the extra first-claim payment dominates; high-risk
#'   customers (lambda above the crossover) pay less because the lower later
#'   payment dominates.
#'
#' @param model A fitted Poisson `glm`.
#' @param newdata Data frame of customers.
#' @param amount Benefit per claim in the original product.
#' @param first,later Benefit for the first claim and each later claim in the
#'   modified product (both or neither).
#' @return A `cs1b_result` list: `table` (lambda, price and, if requested,
#'   price_modified and change) and `crossover_lambda`.
#' @examples
#' set.seed(3)
#' d <- data.frame(Age = runif(80, 20, 70)); d$Claim <- rpois(80, exp(-1 + 0.02 * d$Age))
#' m <- glm(Claim ~ Age, poisson, d)
#' cs1b_poisson_price(m, data.frame(Age = c(30, 50)), 5000, first = 6000, later = 4000)
#' @export
cs1b_poisson_price <- function(model, newdata, amount = 5000, first = NULL, later = NULL) {
  lam <- as.numeric(predict(model, newdata, type = "response"))
  tab <- data.frame(lambda = lam, price = amount * lam)
  cross <- NA_real_
  if (!is.null(first) && !is.null(later)) {
    tab$price_modified <- later * lam + (first - later) * (1 - exp(-lam))
    tab$change <- tab$price_modified - tab$price
    f <- function(l) (first - later) * (1 - exp(-l)) - (amount - later) * l
    cross <- tryCatch(uniroot(f, c(1e-6, 50))$root, error = function(e) NA_real_)
  }
  .res(method = "Poisson pricing", table = tab, crossover_lambda = cross)
}

#' Principal components analysis summary
#'
#' @description Runs `prcomp`, and returns standard deviations, proportion and
#'   cumulative proportion of variance, loadings, and the number of components
#'   needed to reach a variance threshold.
#'
#' @details **When to use:** "how much variance does PC1 capture", "how many
#'   components for 90 percent", "which group of variables is most strongly
#'   correlated". Use `scale = TRUE` when variables are on different scales
#'   (equivalent to PCA on the correlation matrix). A group whose PC1 alone is
#'   near 90 percent is the most strongly correlated. Same-sign PC1 loadings
#'   give a "general level" component; mixed signs give a contrast.
#'
#' @param data Data frame or matrix of numeric variables.
#' @param scale Logical, standardise the variables (default `TRUE`).
#' @param threshold Cumulative variance proportion target (default 0.90).
#' @param plot Logical, draw a scree plot.
#' @return A `cs1b_result` list: `sdev`, `proportion`, `cumulative`,
#'   `n_components`, `loadings`, `pca`.
#' @examples
#' cs1b_pca(iris[, 1:4])
#' @export
cs1b_pca <- function(data, scale = TRUE, threshold = 0.90, plot = FALSE) {
  pc <- prcomp(data, scale. = scale)
  prop <- pc$sdev^2 / sum(pc$sdev^2)
  cum <- cumsum(prop)
  if (plot) screeplot(pc, type = "lines", main = "Scree plot")
  .res(method = "Principal components analysis", sdev = pc$sdev, proportion = prop,
       cumulative = cum, n_components = which(cum >= threshold - 1e-12)[1],
       loadings = pc$rotation, pca = pc)
}
