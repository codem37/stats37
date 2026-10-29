#' Find the right function for an exam task
#'
#' @description A searchable index of the package. Matches your keyword against
#'   the task description, topic and function name.
#'
#' @details **When to use:** at the start of a sub-part when you are not sure
#'   which tool to reach for. Tasks without a wrapper point to the base R call
#'   (for example `binom.test`).
#'
#' @param topic Optional keyword such as `"paired"`, `"bayes"`, `"pca"`. If
#'   `NULL`, the whole index is shown.
#' @return A data frame (invisibly) with columns Topic, Task, Function and
#'   Chapter (the chapter of the CS1B R notes).
#' @examples
#' cs1b_guide("bayes")
#' cs1b_guide("chi")
#' @export
cs1b_guide <- function(topic = NULL) {
  g <- data.frame(
    Topic = c(
      "Distributions", "Distributions", "Distributions", "Distributions",
      "Distributions", "Simulation", "Simulation", "Simulation", "Simulation",
      "Tests", "Tests", "Tests", "Tests", "Tests", "Tests", "Tests", "Tests",
      "Tests", "Tests", "Tests",
      "Correlation", "Correlation", "Regression", "Regression", "Regression",
      "Regression", "GLM", "GLM", "GLM", "GLM", "PCA", "Bayes", "Bayes",
      "Credibility", "Credibility", "Bootstrap", "MLE", "MLE", "Helpers",
      "Helpers"),
    Task = c(
      "Mean, variance, cdf of a discrete table (outcomes + probabilities)",
      "Linear combination of independent normals, e.g. P(M >= 2K)",
      "Skewness of a Beta(a,b)",
      "Hypergeometric vs binomial: mean, variance, finite-population correction",
      "Compare hypergeometric and binomial probabilities (lottery)",
      "Inverse transform: uniforms to any distribution via q-function",
      "Simulate a sampling distribution / CLT (sum, mean, variance)",
      "Normal QQ plot to check normality",
      "Simulate binomial counts and overlay the normal approximation",
      "z test for a mean (sigma known or treated as known)",
      "Chi-square test and CI for a standard deviation / variance",
      "CI for sigma and sigma^2 only",
      "Paired t vs independent t on the same data (+ var.test)",
      "Test for one proportion / two proportions / Poisson rate",
      "Chi-square test of independence in a table (+ Fisher for 2x2)",
      "Chi-square goodness of fit with automatic cell merging",
      "One-way ANOVA with assumption checks",
      "Write the decision sentence", "Test a stated regression coefficient value",
      "Wording of every test (hypotheses, decision)",
      "Pearson, Spearman, Kendall + CI + 'perfect correlation' claim",
      "Portfolio sd, mean and risk-adjusted return across correlation CI",
      "Prediction: confidence AND prediction interval together",
      "Compare linear models (adjusted R2, sigma, AIC)",
      "Solve for the x giving a target fitted y (e.g. fitted crime = 0)",
      "Residual-style diagnostics: see plot(model); use cs1b_qq(residuals(m))",
      "Compare two GLMs: AIC, BIC, deviance, chi-square drop-in-deviance test",
      "Recover the log-likelihood from a printed AIC",
      "Poisson pricing, modified-benefit pricing and crossover lambda",
      "Prediction with CI on the response scale for a GLM",
      "Principal components: sd, proportion of variance, components for 90 percent",
      "Gamma prior + Poisson data: posterior, estimates under each loss function",
      "Beta prior + binomial data: posterior and credible interval",
      "Empirical Bayes credibility, Model 1 (equal exposure)",
      "Empirical Bayes credibility, Model 2 (different exposures)",
      "Non-parametric and parametric bootstrap of a statistic",
      "Poisson log-likelihood on a grid, MLE",
      "Maximise any log-likelihood numerically",
      "Reading a 'Poisson null model' output: lambda = exp(intercept)",
      "Load the worked-example data from the past papers"),
    Function = c(
      "cs1b_discrete", "cs1b_norm_lincomb", "cs1b_beta_skew",
      "cs1b_hyper_moments", "cs1b_hyper_vs_binom", "cs1b_inverse_transform",
      "cs1b_clt_sim", "cs1b_qq", "cs1b_binom_normal", "cs1b_z_test",
      "cs1b_chisq_var_test", "cs1b_sd_ci", "cs1b_paired_vs_indep",
      "binom.test / prop.test / poisson.test (base R)",
      "cs1b_chisq_independence", "cs1b_chisq_gof", "cs1b_oneway_anova",
      "cs1b_decision", "cs1b_test_coef", "print.cs1b_result",
      "cs1b_cor_all", "cs1b_portfolio_compare", "cs1b_predict",
      "cs1b_compare_lm", "cs1b_solve_x", "cs1b_qq", "cs1b_glm_compare",
      "cs1b_loglik_from_aic", "cs1b_poisson_price", "cs1b_predict",
      "cs1b_pca", "cs1b_gamma_poisson", "cs1b_beta_binomial", "cs1b_ebct1",
      "cs1b_ebct2", "cs1b_bootstrap", "cs1b_loglik_grid", "cs1b_mle",
      "exp(coef(m)) (base R)", "cs1b_data"),
    Chapter = c(3, 3, 3, 3, 3, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 8, 6, 7,
                7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 11, 11, 12, 12, 13, 13, 13, 9, 1),
    stringsAsFactors = FALSE
  )
  if (!is.null(topic)) {
    hit <- apply(g, 1, function(r) any(grepl(topic, r, ignore.case = TRUE)))
    g <- g[hit, , drop = FALSE]
  }
  print(g, row.names = FALSE, right = FALSE)
  invisible(g)
}

#' Worked-example data from the past papers
#'
#' @description Returns the data sets that the CS1B notes use as worked
#'   examples, so you can test your own run against the verified numbers.
#'
#' @details **When to use:** to practise, or to check that a function gives the
#'   figure quoted in the notes. In the real exam you type the data in from
#'   `Codes.docx`.
#'
#' @param name One of the names listed below, or `NULL` for the full list.
#'   Available: `stocks` (Dec 2022 Q2), `exam_success` (Nov 2024 Q4),
#'   `treat`, `exercise`, `clinic` (Nov 2025 Q3 ANOVA), `cholesterol`
#'   (May 2025 Q2), `ae_scores` (Feb 2025 Q3), `bp` (May 2024 Q2),
#'   `expressway` (Feb 2025 Q4), `claims8` (Nov 2025 Q2), `ebct1_travel`
#'   (Nov 2023 Q4), `ebct2` (Nov 2024 Q2), `delivery` (Feb 2025 Q1),
#'   `chisq15_u` (Nov 2024 Q1 uniforms).
#' @return A list (all data) or the single requested object.
#' @examples
#' names(cs1b_data())
#' cs1b_data("stocks")
#' @export
cs1b_data <- function(name = NULL) {
  d <- list(
    stocks = data.frame(
      abc = c(-6, -10, 3, 18, -10, 1, 3, -13, -14, 13),
      xyz = c(8, 0, -4, 10, 20, 8, 0, 10, 5, -19),
      pqr = c(-20, 19, 3, 13, 20, 7, -3, 13, 20, 1),
      lmn = c(14, 4, 5, 15, 19, 9, 6, 10, 7, 16)),
    exam_success = matrix(c(132, 120, 27, 51), ncol = 2, nrow = 2,
      dimnames = list(c("Attended", "Did not attend"), c("Passed", "Failed"))),
    treat = matrix(c(20, 32, 8, 52, 9, 72, 8, 32, 16, 64, 30, 12), nrow = 4,
      dimnames = list(c("Injection", "Tablet", "Laser", "Herbal"),
                      c("None", "Partial", "Full"))),
    exercise = matrix(c(38, 232, 48, 15, 120, 357, 29, 34, 42, 73, 4, 8),
      nrow = 4,
      dimnames = list(c("Sedentary", "Moderately Active", "Very Active",
                        "Extremely Active"), c("Light", "Moderate", "High"))),
    clinic = {
      thickness <- c(
        c(93, 120, 65, 105, 115, 82, 99, 87, 100, 90, 78, 95, 93, 88, 110),
        c(85, 45, 80, 28, 75, 70, 65, 55, 50, 40, 45, 55),
        c(100, 75, 65, 40, 73, 65, 50, 30, 45, 50),
        c(96, 58, 95, 90, 65, 80, 85, 95, 82))
      data.frame(thickness = thickness,
        clinic = factor(rep(c("I", "II", "III", "IV"), times = c(15, 12, 10, 9))))
    },
    cholesterol = data.frame(
      before = c(9.1, 8.0, 7.7, 10.0, 9.6, 7.9, 9.0, 7.1, 8.3, 9.6),
      after = c(8.2, 6.4, 6.6, 8.5, 8.0, 5.8, 7.8, 7.2, 6.7, 9.8)),
    ae_scores = data.frame(AE_1 = c(4, 1, 5, 1, 4, 6, 4, 5, 3, 6),
                           AE_2 = c(3, 2, 4, 0, 3, 4, 2, 3, 3, 6)),
    bp = data.frame(
      age = c(28, 37, 41, 52, 57, 49, 38, 25, 23, 48, 60, 55, 29, 43, 36, 50,
              34, 40, 26, 33),
      bp = c(132, 140, 155, 160, 167, 148, 128, 131, 118, 139, 149, 154, 117,
             146, 142, 168, 144, 156, 114, 133)),
    expressway = c(5,5,6,2,4,10,2,5,5,2,5,3,7,4,4,5,4,6,7,2,8,4,6,4,3,6,6,6,5,7),
    claims8 = c(1, 6, 0, 3, 5, 2, 1, 0),
    ebct1_travel = matrix(c(455, 251, 309, 400, 458, 322, 246, 426,
                            587, 292, 217, 470, 531, 340, 120, 547),
      ncol = 4, nrow = 4,
      dimnames = list(c("Make", "Ease", "Go", "Clear"), paste0("Year", 1:4))),
    ebct2 = list(
      claims = data.frame(Year1 = c(14.2, 58.6, 123), Year2 = c(15.8, 63.1, 132),
                          Year3 = c(22.7, 81.0, 161), Year4 = c(19, 64.2, 133)),
      nopols = data.frame(Year1 = c(163, 4435, 16184), Year2 = c(189, 4761, 17443),
                          Year3 = c(252, 5576, 20102), Year4 = c(199, 4581, 18000))),
    delivery = c(14.8, 17.6, 11.2, 13.5, 12.9, 10.8, 10.5, 12.3),
    chisq15_u = c(0.07991847, 0.82064314, 0.33683219, 0.93005953, 0.31919393,
                  0.92695533, 0.76263949, 0.51740370, 0.49224880, 0.46354694,
                  0.89832157, 0.21920729, 0.20471780, 0.19055074, 0.69137537)
  )
  if (is.null(name)) return(d)
  if (!name %in% names(d)) stop("Unknown data set. Choose from: ",
                                paste(names(d), collapse = ", "))
  d[[name]]
}
