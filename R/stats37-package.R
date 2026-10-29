#' stats37: helper functions for the IAI CS1B practical paper
#'
#' One function per exam topic. Start with [cs1b_guide()] to find the function
#' for a task, and [cs1b_data()] for the worked-example data from the past
#' papers.
#'
#' @section Answer template for every test:
#' 1. State H0 and H1 in terms of the parameter. 2. Run the test and print the
#' output. 3. Quote the statistic and p-value. 4. Decide: reject or *fail to
#' reject* H0 (never "accept"). 5. Conclude in words about the real situation.
#' The test functions in this package return `hypotheses` and `decision`
#' fields that follow this template.
#'
#' @section Exam habits:
#' Put `set.seed()` at the top of the script, re-run from the top before
#' copying output, use the object names the question asks for, and print
#' everything that is asked.
#'
#' @keywords internal
#' @import stats
#' @import graphics
#' @importFrom utils head
"_PACKAGE"

# internal: build a classed result list ------------------------------------
.res <- function(...) structure(list(...), class = "cs1b_result")

# internal: wording of the decision ----------------------------------------
.decide <- function(p, alpha) {
  if (is.na(p)) return(NA_character_)
  if (p < alpha) {
    sprintf("Reject H0 at the %g%% level (p = %s < %g): there is sufficient evidence for H1.",
            alpha * 100, format(signif(p, 4)), alpha)
  } else {
    sprintf("Fail to reject H0 at the %g%% level (p = %s >= %g): insufficient evidence for H1.",
            alpha * 100, format(signif(p, 4)), alpha)
  }
}

.alt_symbol <- function(alternative) {
  c(two.sided = "!=", less = "<", greater = ">")[[alternative]]
}

#' Print method for stats37 results
#'
#' Prints the method, the hypotheses, every computed element and the decision.
#'
#' @param x An object returned by a `cs1b_*` function.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.cs1b_result <- function(x, ...) {
  if (!is.null(x$method)) cat(x$method, "\n", sep = "")
  if (!is.null(x$hypotheses)) cat(x$hypotheses, "\n", sep = "")
  skip <- c("method", "hypotheses", "decision")
  rest <- unclass(x)[setdiff(names(x), skip)]
  if (length(rest)) print(rest)
  if (!is.null(x$decision)) cat("\nDecision:", x$decision, "\n")
  invisible(x)
}

#' Write the decision sentence for a hypothesis test
#'
#' @description Produces the standard exam sentence: "Reject H0 ..." or
#'   "Fail to reject H0 ...".
#'
#' @details **When to use:** at the end of any hand-computed test (z test,
#'   chi-square, regression coefficient) to word the conclusion correctly. It
#'   never says "accept H0".
#'
#' @param p The p-value.
#' @param alpha Significance level as a proportion (0.05 = 5 percent).
#' @return A character string.
#' @examples
#' cs1b_decision(0.0187, 0.05)
#' cs1b_decision(0.247, 0.05)
#' @export
cs1b_decision <- function(p, alpha = 0.05) .decide(p, alpha)

# `x` is the dummy variable inside curve() expressions
utils::globalVariables("x")
