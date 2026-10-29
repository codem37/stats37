#' Empirical Bayes credibility, Model 1 (equal exposure each year)
#'
#' @description Estimates `E[m(theta)]` (grand mean), `E[s^2(theta)]` (average
#'   within-risk variance) and `Var[m(theta)]` (variance of the row means minus
#'   `E[s^2]/n`), then Z = n / (n + `E[s^2]/Var[m]`) and the credibility premiums
#'   P_i = Z xbar_i + (1 - Z) m.
#'
#' @details **When to use:** a table of claims with one row per risk and one
#'   column per year, all risks having the same volume each year (Nov 2023,
#'   May 2024, May 2025). Rows are risks: if your data were entered by column
#'   in `matrix()`, check orientation. If the `Var[m(theta)]` estimate is
#'   negative, Z is set to 0. Z rises towards 1 as n grows.
#'
#' @param x Matrix or data frame, rows = risks, columns = years.
#' @param next_exposure Optional multiplier for next year's premium (default
#'   1).
#' @return A `cs1b_result` list: `m`, `s2`, `v`, `Z`, `xbar_i`, `premium`.
#' @examples
#' cs1b_ebct1(cs1b_data("ebct1_travel"))   # Z = 0.9444
#' @export
cs1b_ebct1 <- function(x, next_exposure = 1) {
  x <- as.matrix(x); n <- ncol(x)
  xbar <- rowMeans(x); m <- mean(x)
  s2 <- mean(apply(x, 1, var))
  v <- var(xbar) - s2 / n
  Z <- if (v <= 0) 0 else n / (n + s2 / v)
  prem <- Z * xbar + (1 - Z) * m
  .res(method = "Empirical Bayes credibility, Model 1", n_years = n,
       m = m, s2 = s2, v = v, Z = Z, xbar_i = xbar, premium = prem * next_exposure)
}

#' Empirical Bayes credibility, Model 2 (different exposures each year)
#'
#' @description Buhlmann-Straub estimates with risk volumes P_ij: claims per
#'   policy X_ij = claims / volumes, scaling constant P*, `E[m]` = total claims /
#'   total volume, `E[s^2]` the volume-weighted within-risk variance, `Var[m]` the
#'   between-risk variance after removing noise, and Z_i = P_i / (P_i + s^2/v).
#'
#' @details **When to use:** the exposure (number of policies) differs between
#'   risks and years, and the question supplies it (Nov 2024 Q2). The extra
#'   information needed compared with Model 1 is the volume per risk per year.
#'   Rows are risks, columns are years, in both inputs.
#'
#' @param claims Matrix/data frame of total claims (rows = risks, columns = years).
#' @param volumes Matrix/data frame of volumes (same shape).
#' @param next_volumes Optional vector of next-year volumes per risk; if given,
#'   the total premium is volumes x premium per unit volume.
#' @return A `cs1b_result` list: `Pstar`, `m`, `s`, `v`, `Z`, `premium_per_unit`,
#'   and `total_premium` if `next_volumes` was supplied.
#' @examples
#' d <- cs1b_data("ebct2")
#' cs1b_ebct2(d$claims, d$nopols, next_volumes = c(5000, 4800, 4200))
#' @export
cs1b_ebct2 <- function(claims, volumes, next_volumes = NULL) {
  claims <- as.matrix(claims); volumes <- as.matrix(volumes)
  if (!all(dim(claims) == dim(volumes))) stop("claims and volumes must have the same dimensions")
  N <- nrow(claims); n <- ncol(claims)
  X <- claims / volumes
  Pi <- rowSums(volumes); Pt <- sum(Pi)
  Xi <- rowSums(claims) / Pi
  Pstar <- sum(Pi * (1 - Pi / Pt)) / (N * n - 1)
  m <- sum(claims) / Pt
  s <- mean(rowSums(volumes * (X - Xi)^2) / (n - 1))
  v <- (sum(rowSums(volumes * (X - m)^2)) / (n * N - 1) - s) / Pstar
  Z <- if (v <= 0) rep(0, N) else Pi / (Pi + s / v)
  prem <- Z * Xi + (1 - Z) * m
  out <- list(method = "Empirical Bayes credibility, Model 2", Pstar = Pstar,
              m = m, s = s, v = v, Z = Z, premium_per_unit = prem)
  if (!is.null(next_volumes)) out$total_premium <- next_volumes * prem
  do.call(.res, out)
}
