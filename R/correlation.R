#' Pearson, Spearman and Kendall correlation with a confidence interval
#'
#' @description Computes all three coefficients, the Pearson test (t, df,
#'   p-value) and its Fisher-transformation confidence interval.
#'
#' @details **When to use:** any "calculate the correlation" or "is the
#'   relationship strong" question. Pearson measures linear association (use
#'   when the scatter is a straight band); Spearman and Kendall use ranks and
#'   suit non-normal data, outliers and ordinal data. Kendall's tau is smaller
#'   in magnitude by construction, so do not compare it directly with the
#'   other two. `cor.test` tests rho = 0, not rho = 1: to judge a claim of
#'   *perfect* correlation, check whether the interval excludes +1 or -1 (it
#'   always does unless r is exactly +-1).
#'
#' @param x,y Numeric vectors of equal length.
#' @param conf.level Confidence level for the Pearson interval.
#' @return A `cs1b_result` list: `coefficients` (named vector), `pearson_test`
#'   (`htest`), `conf_int`, `excludes_perfect`.
#' @examples
#' b <- cs1b_data("bp")
#' cs1b_cor_all(b$age, b$bp)$coefficients   # 0.807, 0.818, 0.611
#' @export
cs1b_cor_all <- function(x, y, conf.level = 0.95) {
  pt <- cor.test(x, y, conf.level = conf.level)
  co <- c(pearson = unname(pt$estimate),
          spearman = suppressWarnings(cor(x, y, method = "spearman")),
          kendall = suppressWarnings(cor(x, y, method = "kendall")))
  ci <- as.numeric(pt$conf.int)
  .res(method = "Correlation summary", coefficients = co, pearson_test = pt,
       conf_int = ci, excludes_perfect = (ci[2] < 1 && ci[1] > -1))
}

#' Portfolio standard deviation from two assets
#'
#' @description sd = sqrt(w^2 s1^2 + 2 r w (1-w) s1 s2 + (1-w)^2 s2^2).
#'
#' @details **When to use:** the standard deviation of a two-asset portfolio
#'   with weight `w` on asset 1. The value is increasing in the correlation
#'   `r`, so plugging in the lower and upper limits of a correlation
#'   confidence interval gives an interval for the portfolio sd.
#'
#' @param w Weight on asset 1 (asset 2 gets 1 - w).
#' @param sd1,sd2 Standard deviations of the two assets.
#' @param r Correlation between them (may be a vector, e.g. both CI limits).
#' @return Numeric vector of portfolio standard deviations.
#' @examples
#' cs1b_portfolio_sd(0.5, 11.01, 10.46, c(-0.446, 0.256))
#' @export
cs1b_portfolio_sd <- function(w, sd1, sd2, r) {
  sqrt(w^2 * sd1^2 + 2 * r * w * (1 - w) * sd1 * sd2 + (1 - w)^2 * sd2^2)
}

#' Portfolio mean, risk interval and risk-adjusted return
#'
#' @description For one strategy, builds the return series, takes its mean, a
#'   confidence interval for the sd (through the correlation CI) and the
#'   risk-adjusted return mean / sd.
#'
#' @details **When to use:** Dec 2022 Q2 style "which strategy is better by
#'   metric 1 (mean), 2 (risk) and 3 (return per unit risk)". Run it once per
#'   strategy and compare.
#'
#' @param a1,a2 Return vectors of asset 1 and asset 2.
#' @param w Weight on asset 1.
#' @param conf.level Confidence level for the correlation interval.
#' @return A `cs1b_result` list: `mean`, `sd_actual`, `cor_ci`, `sd_interval`
#'   (lower, upper), `risk_adjusted_interval`.
#' @examples
#' s <- cs1b_data("stocks")
#' cs1b_portfolio_compare(s$abc, s$xyz, 0.5)   # strategy A
#' cs1b_portfolio_compare(s$pqr, s$lmn, 0.75)  # strategy B
#' @export
cs1b_portfolio_compare <- function(a1, a2, w, conf.level = 0.95) {
  ret <- w * a1 + (1 - w) * a2
  ci <- as.numeric(cor.test(a1, a2, conf.level = conf.level)$conf.int)
  sdi <- cs1b_portfolio_sd(w, sd(a1), sd(a2), ci)
  .res(method = "Portfolio comparison", mean = mean(ret), sd_actual = sd(ret),
       cor_ci = ci, sd_interval = sdi,
       risk_adjusted_interval = mean(ret) / rev(sdi))
}
