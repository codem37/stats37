# stats37

Helper functions for the IAI **CS1B** (Actuarial Statistics, Paper B) practical exam, built from seven past papers (Dec 2022 – Nov 2025). One function per exam topic; every function documents **when to use it** and **what each parameter means**. Test functions print the hypotheses and a decision in exam wording ("fail to reject", never "accept").

## Install

### From GitHub Release using `install.packages()`
```r
install.packages(
  "https://github.com/codem37/stats37/releases/download/v0.1.0/stats37_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
library(stats37)
```

### Or using `remotes`
```r
remotes::install_github("codem37/stats37")
library(stats37)
```

## Start here

```r
cs1b_guide()            # full index: task -> function -> notes chapter
cs1b_guide("paired")    # search the index
?cs1b_z_test            # every function has a help page with examples
names(cs1b_data())      # worked-example data from the papers
```

## Question-type guide (base R only)

A separate guide lists each CS1B question type with sample wording, when to use the method and plain base R code that does not need this package:

```r
file.show(system.file("docs", "CS1B_question_types.md", package = "stats37"))
```

## Function map

| Topic | Functions |
|---|---|
| Distributions | `cs1b_discrete`, `cs1b_norm_lincomb`, `cs1b_beta_skew`, `cs1b_hyper_moments`, `cs1b_hyper_vs_binom` |
| Simulation | `cs1b_inverse_transform`, `cs1b_clt_sim`, `cs1b_qq`, `cs1b_binom_normal` |
| Tests and CIs | `cs1b_z_test`, `cs1b_chisq_var_test`, `cs1b_sd_ci`, `cs1b_paired_vs_indep`, `cs1b_chisq_gof`, `cs1b_chisq_independence`, `cs1b_oneway_anova`, `cs1b_test_coef`, `cs1b_decision` |
| Correlation | `cs1b_cor_all`, `cs1b_portfolio_sd`, `cs1b_portfolio_compare` |
| Regression | `cs1b_predict`, `cs1b_compare_lm`, `cs1b_solve_x` |
| GLM | `cs1b_glm_compare`, `cs1b_loglik_from_aic`, `cs1b_poisson_price` |
| PCA | `cs1b_pca` |
| Bayes | `cs1b_gamma_poisson`, `cs1b_beta_binomial` |
| Credibility | `cs1b_ebct1`, `cs1b_ebct2` |
| Bootstrap / MLE | `cs1b_bootstrap`, `cs1b_loglik_grid`, `cs1b_mle` |

Tasks best done with base R (no wrapper): `binom.test`, `prop.test`, `poisson.test`, `t.test`, `var.test`, `lm`, `glm`, `predict`.

## Verified

`tests/run-tests.R` reproduces the figures quoted in the CS1B notes (for example Z = 0.9444 for EBCT Model 1, posterior Gamma(20, 13) mean 1.5385, one-way ANOVA F = 15.14).

## Exam tips

Set `set.seed()` first, re-run from the top before copying output, use the object names the question asks for, and print every result.
