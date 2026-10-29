# CS1B question types: what to recognise, what to type

Plain base R only. No package functions are needed. Each section gives the
kind of question, when to use the method, code you can paste, and what to
write. Contact: example@gmail.com

## Answer template for every test

1. State H0 and H1 in terms of the parameter.
2. Run the test and print the output.
3. Quote the test statistic and the p-value.
4. Decide: reject or **fail to reject** H0 at the stated level (never "accept").
5. Conclude in words about the real situation.

Direction: "reduction/increase/higher" is one-sided; "changed/different" is
two-sided. For paired and two-sample tests the direction refers to the first
argument minus the second.

## Quick finder

| The question says | Go to |
|---|---|
| outcomes with probabilities, mean, sd | 1 |
| probability / quantile of a normal, difference of normals | 2 |
| P(X > k), P(X >= k) Poisson or binomial | 3 |
| Beta shape, skewness, histograms of samples | 4 |
| lottery, without replacement, hypergeometric | 5 |
| use these uniforms to simulate | 6 |
| simulate 1000 samples, central limit theorem | 7 |
| check normality, QQ plot | 8 |
| test a mean, sigma known | 9 |
| test a mean, sigma unknown, confidence interval | 10 |
| test or interval for a standard deviation or variance | 11 |
| before/after, two groups, compare variances | 12 |
| proportion, Poisson rate | 13 |
| association in a table | 14 |
| does the data fit a distribution | 15 |
| compare three or more means | 16 |
| correlation | 17 |
| one predictor, line, prediction | 18 |
| several predictors | 19 |
| claims counts, Poisson GLM, pricing | 20 |
| yes/no outcome, logistic | 21 |
| positive skewed outcome, Gamma, AIC comparison | 22 |
| principal components | 23 |
| prior + data, posterior, credible interval | 24 |
| credibility premium, equal exposure | 25 |
| credibility premium, different exposures | 26 |
| bootstrap | 27 |
| maximum likelihood, log-likelihood plot | 28 |

## R habits that cost marks if missed

- R indexes from 1. Matrices fill by column unless `byrow = TRUE`.
- `rexp`, `dexp` take a **rate**; `rgamma` takes a rate unless `scale =` is named.
- A column coded 1, 2, 3 that means categories needs `factor()` before a GLM.
- `set.seed()` first, and re-run the whole script from the top before copying output.
- Use the object names the question asks for. Print everything that is asked.
- Label every plot: `main`, `xlab`, `ylab`.

---

## 1. Discrete distribution from a table

**Looks like:** a game or lottery gives pay-outs and probabilities; find the
mean, standard deviation, cumulative probabilities, draw the distribution.
**Use when:** values come with probabilities. Never use `mean()` or `sd()` on the outcomes; they treat all outcomes as equally likely.

```r
outcomes <- c(-400, 0, 100, 800)
prob     <- c(0.32, 0.48, 0.15, 0.05)
cumul    <- cumsum(prob)
cumul                                         # 0.32 0.80 0.95 1.00
mu  <- sum(outcomes * prob)                   # -73
v   <- sum(outcomes^2 * prob) - mu^2
sdX <- sqrt(v)                                # 281.73
c(mean = mu, sd = sdX)
barplot(prob, names.arg = outcomes, xlab = "Amount (Rs)",
        ylab = "Probability", main = "Distribution of X")
plot(outcomes, cumul, type = "s", xlab = "x", ylab = "P(X <= x)",
     main = "Cumulative distribution of X")
```

**Write:** mean = sum x p(x); variance = sum x^2 p(x) minus mean squared.

## 2. Normal probabilities, quantiles, linear combinations

**Looks like:** weights are N(80.2, 1.1^2); find P(80.5 < X < 81.5) or the weight below which the lightest 20% lie; "price in city M is at least twice city K".
**Use when:** normal model. `sd` is a standard deviation, not a variance. For a linear combination of independent normals the variance adds with the squared coefficient.

```r
pnorm(81.5, 80.2, 1.1) - pnorm(80.5, 80.2, 1.1)   # 0.2739
qnorm(0.20, 80.2, 1.1)                            # 79.274

# M ~ N(5400, 900^2), K ~ N(3600, 1500^2); event M - 2K >= 0
w.mean <- 5400 - 2 * 3600                         # -1800
w.sd   <- sqrt(900^2 + 2^2 * 1500^2)              # 3132.09
1 - pnorm(0, w.mean, w.sd)                        # 0.2827
# difference M - K
dif.mean <- 5400 - 3600
dif.sd   <- sqrt(900^2 + 1500^2)                  # 1749.29
```

## 3. Poisson and binomial tail probabilities

**Looks like:** cars in 120 minutes average 107; P(more than 100)? Exactly 0?
**Use when:** counts. For a discrete variable `ppois(k, ...)` is P(Y <= k).

```r
ppois(100, 107, lower.tail = FALSE)   # P(Y > 100) = P(Y >= 101) = 0.7319
1 - ppois(99, 107)                    # P(Y >= 100)
dpois(0, 107)                         # P(Y = 0)
dbinom(3, size = 10, prob = 0.2)      # P(X = 3)
pbinom(3, 10, 0.2)                    # P(X <= 3)
y <- 60:150
plot(y, dpois(y, 107), type = "h", xlab = "Cars", ylab = "P(Y = y)",
     main = "Poisson(107) mass function")
```

## 4. Beta distribution, skewness, histograms

**Looks like:** simulate 12,000 values from Beta(5,1), Beta(1,5), Beta(3,3); compute P(0.2 < X < 0.8); explain the shapes.

```r
pbeta(0.8, 5, 1) - pbeta(0.2, 5, 1)             # 0.32736
qbeta(0.65, 5, 1, lower.tail = FALSE)           # 0.8106
skew <- function(a, b) 2 * ((b - a) / (a + b + 2)) * sqrt((a + b + 1) / (a * b))
skew(5, 1); skew(1, 5); skew(3, 3)              # -1.1832 1.1832 0

set.seed(421967)
x1 <- rbeta(12000, 5, 1); x2 <- rbeta(12000, 1, 5); x3 <- rbeta(12000, 3, 3)
par(mfrow = c(1, 3))
hist(x1, main = "Beta(5,1)", xlab = "x1")
hist(x2, main = "Beta(1,5)", xlab = "x2")
hist(x3, main = "Beta(3,3)", xlab = "x3")
```

**Write:** Beta(5,1) is left-skewed (mass near 1), Beta(1,5) right-skewed (mass near 0), Beta(3,3) symmetric. Histograms of many sample means are bell-shaped whatever the parent shape (central limit theorem).

## 5. Hypergeometric versus binomial (lottery)

**Looks like:** five balls drawn from 69, you hold five numbers; expected pay-out; why is the binomial wrong?
**Use when:** drawing without replacement from a small finite pool. `dhyper(x, m, n, k)`: x matches, m successes in the pool, n failures, k draws.

```r
w_draw <- c(5, 5, 4, 4, 3, 3, 2, 1, 0)     # white matches, cases A to I
r_draw <- c(1, 0, 1, 0, 1, 0, 1, 1, 1)     # power ball matched
pw <- dhyper(w_draw, m = 5, n = 64, k = 5)
pr <- dhyper(r_draw, m = 1, n = 25, k = 1)
prize <- c(20000000, 1000000, 50000, 100, 100, 7, 7, 4, 4)
EV <- sum(pw * pr * prize)                 # 0.3883
0.50 - EV                                  # profit per ticket

pwb <- dbinom(w_draw, 5, 5/69); prb <- dbinom(r_draw, 1, 1/26)
sum(pwb * prb * prize)                     # 3.909, ten times too high

5 * 5/69                                   # mean (same) 0.3623
5 * (5/69) * (64/69)                       # binomial variance 0.3361
5 * (5/69) * (64/69) * (69 - 5) / (69 - 1) # hypergeometric 0.3163
```

**Write:** the binomial assumes replacement, so it overstates the jackpot chance. The means agree; the hypergeometric variance is smaller by (N - k)/(N - 1).

## 6. Inverse transform (use these uniforms)

**Looks like:** "the following 15 uniform random numbers are given; simulate chi-square(2) values".
**Use when:** you must turn U(0,1) numbers into another distribution: apply its q-function.

```r
u <- c(0.07991847, 0.82064314, 0.33683219, 0.93005953, 0.31919393,
       0.92695533, 0.76263949, 0.51740370, 0.49224880, 0.46354694,
       0.89832157, 0.21920729, 0.20471780, 0.19055074, 0.69137537)
x <- qchisq(u, df = 2)
y <- sum(x)                                 # 30.98
all.equal(x, -2 * log(1 - u))               # TRUE
```

**Write:** chi-square(2) is Gamma(1, 1/2); the sum of 15 is Gamma(15, 1/2), which is chi-square(30) (mean 30, variance 60).

## 7. Simulation and the central limit theorem

**Looks like:** generate 1,000 samples of size 15, record each sum; repeat with samples of 10,000; comment on shape.

```r
set.seed(47)
y <- replicate(1000, sum(rchisq(15, df = 2)))
sum(y)
hist(y, freq = FALSE, main = "Histogram of y", xlab = "y")
lines(density(y))
curve(dchisq(x, df = 30), add = TRUE, lty = 2)

set.seed(47)
x_bar <- numeric(200); s_squared <- numeric(200)
for (i in 1:200) { x <- rchisq(10000, df = 2)
                   x_bar[i] <- mean(x); s_squared[i] <- var(x) }
mean(x_bar)        # near 2
mean(s_squared)    # near 4
par(mfrow = c(1, 2))
hist(x_bar, main = "x_bar", xlab = "x_bar")
hist(s_squared, main = "s_squared", xlab = "s_squared")
```

**Write:** small n stays skewed; large n gives a bell shape. The centres sit at the true mean and variance, so both estimators look unbiased.

## 8. QQ plot (checking normality)

**Use when:** "check the normality assumption" for data, regression residuals or ANOVA residuals. Points on the line mean normal; a curve means skewness; an S-shape means tails.

```r
set.seed(1234)
dif.sample <- rnorm(50, mean = 1800, sd = 1749.29)
qqnorm(dif.sample, main = "Normal Q-Q plot")
qqline(dif.sample)
# for a model:  qqnorm(residuals(model)); qqline(residuals(model))
```

## 9. z test for a mean (sigma known or treated as known)

**Looks like:** "assume the standard deviation is known to be ...", or the question treats the sample sd as the population value.

```r
set.seed(1234)
dif.sample <- rnorm(50, 1800, 1749.29)
n <- length(dif.sample)
z <- (mean(dif.sample) - 1375) / (1749.29 / sqrt(n))   # H0: mu = 1375
pnorm(z)                       # one-sided H1: mu < 1375
2 * pnorm(-abs(z))             # two-sided
qnorm(0.05)                    # critical value -1.645
```

## 10. t test for one mean and confidence interval

**Use when:** sigma unknown. `alternative` is `"less"`, `"greater"` or `"two.sided"`.

```r
weights1 <- c(79.1, 80.4, 78.8, 79.9, 80.6, 78.5, 79.2, 80.1)
t.test(weights1, mu = 80, alternative = "less")
t.test(weights1, mu = 80, alternative = "less", conf.level = 0.99)
t.test(weights1)$conf.int          # 95% interval for the mean
```

## 11. Standard deviation or variance: test and interval

**Looks like:** test H0: sigma = 2.5; give a 90% interval for sigma.
**Use when:** a statement about spread. R has no built-in test, so use (n - 1)s^2/sigma0^2 ~ chi-square(n - 1). Needs normal data.

```r
x <- qchisq(c(0.07991847, 0.82064314, 0.33683219, 0.93005953, 0.31919393,
              0.92695533, 0.76263949, 0.51740370, 0.49224880, 0.46354694,
              0.89832157, 0.21920729, 0.20471780, 0.19055074, 0.69137537), 2)
n  <- length(x)
s2 <- var(x)                                                   # 3.2614
T  <- (n - 1) * s2 / 2.5^2                                     # 7.306
qchisq(c(0.025, 0.975), n - 1)                                 # 5.629 26.119
2 * min(pchisq(T, n - 1), 1 - pchisq(T, n - 1))                # 0.1554
# one-sided H1: sigma < 2.5   -> pchisq(T, n - 1)

# 90% CI for sigma: the LARGER quantile goes first
sqrt((n - 1) * s2 / qchisq(c(0.95, 0.05), n - 1))
```

## 12. Two groups, paired data, comparing variances

**Looks like:** same exam marked twice (paired); different papers (independent); before and after a programme.
**Use when:** paired if each value has a natural partner; otherwise independent. Check variances with `var.test` before pooling.

```r
AE_1 <- c(4, 1, 5, 1, 4, 6, 4, 5, 3, 6)
AE_2 <- c(3, 2, 4, 0, 3, 4, 2, 3, 3, 6)
t.test(AE_1, AE_2, paired = TRUE)               # t = 2.862, p = 0.0187
var.test(AE_1, AE_2)                            # F = 1.3136, p = 0.691
t.test(AE_1, AE_2, var.equal = TRUE)            # t = 1.197, p = 0.247
t.test(AE_1, AE_2, var.equal = TRUE)$conf.int   # (-0.680, 2.480)

before <- c(9.1, 8.0, 7.7, 10.0, 9.6, 7.9, 9.0, 7.1, 8.3, 9.6)
after  <- c(8.2, 6.4, 6.6, 8.5, 8.0, 5.8, 7.8, 7.2, 6.7, 9.8)
t.test(before, after, paired = TRUE)                           # two-sided
t.test(before, after, paired = TRUE, alternative = "greater")  # claim of a reduction
t.test(males, females, paired = TRUE, mu = 5)   # H0: mean difference = 5 (if given)
```

**Write:** pairing removes between-item variation, so a paired test can find a difference an independent test misses (same data, different design).

## 13. Proportions and Poisson rates

```r
binom.test(252, 330, p = 0.6)             # one proportion, exact
prop.test(c(40, 25), c(100, 90))          # two proportions
poisson.test(34, T = 20, r = 1.5)         # total 34 events, 20 units, H0 rate 1.5
```

For a Poisson null model with log-link intercept b0, the fitted mean is exp(b0) (the sample mean), and the total is exp(b0) times the number of policies.

## 14. Association in a table (chi-square, Fisher)

**Looks like:** tutorial attendance against exam result; treatment against outcome.
**Use when:** two categorical variables. Chi-square works for any r by c table; Fisher is for 2 by 2. Check expected counts are at least 5.

```r
exam.success <- matrix(c(132, 120, 27, 51), ncol = 2, nrow = 2)  # by column!
test <- chisq.test(exam.success)
test$expected                               # check all >= 5
test                                        # X-squared = 6.835, p = 0.00894
chisq.test(exam.success, correct = FALSE)   # 7.530, p = 0.00607
fisher.test(exam.success)                   # p = 0.00654

treat <- matrix(c(20, 32, 8, 52, 9, 72, 8, 32, 16, 64, 30, 12), nrow = 4,
  dimnames = list(c("Injection", "Tablet", "Laser", "Herbal"),
                  c("None", "Partial", "Full")))
chisq.test(treat)                           # X-squared = 66.17, df = 6
chisq.test(treat)$stdres                    # |value| > 2 drives dependence
```

## 15. Goodness of fit

**Looks like:** do 150 memberships with 0 to 3 cancellations follow Binomial(3, 0.2)?
**Use when:** testing a specified distribution. Merge cells with expected count below 5. df = cells - 1 - parameters estimated from the data.

```r
obs  <- c(61, 71, 15, 3)
prob <- dbinom(0:3, 3, 0.2)
150 * prob                                   # 76.8 57.6 14.4 1.2: merge last two
obs2  <- c(61, 71, 18)
prob2 <- c(prob[1], prob[2], prob[3] + prob[4])
chisq.test(obs2, p = prob2)                  # X-squared = 6.737, df = 2, p = 0.0344
```

## 16. One-way ANOVA

**Use when:** three or more group means. Assumptions: independence, normal residuals (QQ plot), equal variances (largest sd under twice the smallest).

```r
c1 <- c(93,120,65,105,115,82,99,87,100,90,78,95,93,88,110)
c2 <- c(85,45,80,28,75,70,65,55,50,40,45,55)
c3 <- c(100,75,65,40,73,65,50,30,45,50)
c4 <- c(96,58,95,90,65,80,85,95,82)
thickness <- c(c1, c2, c3, c4)
clinic <- factor(rep(c("I","II","III","IV"), times = c(15, 12, 10, 9)))
tapply(thickness, clinic, mean)
sds <- tapply(thickness, clinic, sd); max(sds) / min(sds)   # 1.509
fit <- aov(thickness ~ clinic)
summary(fit)                                 # F = 15.14, p = 7.99e-07
qqnorm(residuals(fit)); qqline(residuals(fit))
TukeyHSD(fit)                                # which pairs differ
boxplot(thickness ~ clinic, xlab = "Clinic", ylab = "Thickness",
        main = "Skin thickness by clinic")
points(1:4, tapply(thickness, clinic, mean), pch = 17)
```

**Write:** H0 all means equal; H1 at least one differs.

## 17. Correlation

**Use when:** strength of association. Pearson for straight-line data; Spearman and Kendall for ranks, outliers, ordinal data. `cor.test` tests rho = 0, not rho = 1: a "perfect correlation" claim is judged by whether the 95% interval contains 1 (it never does unless r is exactly 1).

```r
age <- c(28,37,41,52,57,49,38,25,23,48,60,55,29,43,36,50,34,40,26,33)
bp  <- c(132,140,155,160,167,148,128,131,118,139,149,154,117,146,142,168,144,156,114,133)
cor(age, bp)                          # 0.807
cor(age, bp, method = "spearman")     # 0.818
cor(age, bp, method = "kendall")      # 0.611
cor.test(age, bp)                     # t, df, p and 95% interval
cor.test(age, bp)$conf.int

# portfolio sd at a given correlation r, weight p1 on asset 1
sdfun <- function(p1, sd1, sd2, r) sqrt(p1^2*sd1^2 + 2*r*p1*(1-p1)*sd1*sd2 + (1-p1)^2*sd2^2)
abc <- c(-6,-10,3,18,-10,1,3,-13,-14,13); xyz <- c(8,0,-4,10,20,8,0,10,5,-19)
ciA <- cor.test(abc, xyz)$conf.int
sdfun(0.5, sd(abc), sd(xyz), ciA)     # (3.05, 8.51): sd rises with r
A <- 0.5 * abc + 0.5 * xyz; mean(A) / rev(sdfun(0.5, sd(abc), sd(xyz), ciA))
```

## 18. Simple regression

**Looks like:** regress y on x, plot with the line, test the slope, predict, give the ANOVA table, test a claimed line "BP = 100 + age".

```r
x <- c(28,37,41,52,57,49,38,25,23,48,60,55,29,43,36,50,34,40,26,33)
y <- c(132,140,155,160,167,148,128,131,118,139,149,154,117,146,142,168,144,156,114,133)
plot(x, y, xlab = "Age (years)", ylab = "Max systolic BP", main = "BP against age")
model <- lm(y ~ x); abline(model, lwd = 2)
summary(model)          # intercept 96.499, slope 1.1331, R-squared
anova(model)            # F test of slope = 0
confint(model)

# test a CLAIMED value (summary only tests 0)
est <- coef(summary(model))
t_b <- (est["x","Estimate"] - 1)   / est["x","Std. Error"]
2 * pt(-abs(t_b), df = 18)                           # p = 0.505
t_a <- (est["(Intercept)","Estimate"] - 100) / est["(Intercept)","Std. Error"]
2 * pt(-abs(t_a), df = 18)                           # p = 0.672

# prediction: newdata column name must equal the formula name (x)
newd <- data.frame(x = 45)
predict(model, newd, interval = "confidence", level = 0.99)  # mean response
predict(model, newd, interval = "prediction", level = 0.99)  # one new case (wider)

plot(fitted(model), residuals(model), xlab = "Fitted", ylab = "Residuals",
     main = "Residuals against fitted"); abline(h = 0, lty = 2)
```

**Write:** the slope p-value from `summary` only says slope differs from 0. A funnel in the residuals means non-constant variance; a curve means non-linearity.

## 19. Multiple regression

**Use when:** several predictors. Drop the least significant term, compare adjusted R-squared (rises when a useless term is removed).

```r
set.seed(1)
planning <- data.frame(Unemployment = runif(30, 100, 400),
                       Armspermit = rnorm(30, 50, 10),
                       Urbandensity = runif(30, 200, 800))
planning$Crimerate <- -69 + 0.285 * planning$Unemployment +
  0.0136 * planning$Urbandensity + rnorm(30)
cor(planning)
fit1 <- lm(Crimerate ~ Unemployment + Armspermit + Urbandensity, data = planning)
summary(fit1)
fit2 <- update(fit1, . ~ . - Armspermit)
summary(fit2)
c(summary(fit1)$adj.r.squared, summary(fit2)$adj.r.squared)
anova(fit2, fit1); AIC(fit1, fit2)
newd <- data.frame(Unemployment = 300, Urbandensity = 500)
predict(fit2, newd, interval = "confidence", level = 0.99)

# x giving a target fitted value, others fixed (model linear in x)
b <- coef(fit2)
-(b[1] + b[3] * 500) / b[2]          # unemployment where fitted crime = 0
```

Formula shortcuts: `y ~ x1*x2` (with interaction), `y ~ (x1+x2+x3)^2` (all two-way), `y ~ x - 1` (no intercept), `y ~ x + I(x^2)`, `y ~ .` (all columns), `log(y) ~ x`.

## 20. Poisson GLM, interpretation and pricing

**Looks like:** number of claims by gender, health, age; interpret output; choose between models; price a policy at Rs 5,000 per claim.

```r
# PA_Data <- read.csv("PA_Data.csv")      # needs your file; check names()
PA_Data$Gender <- factor(PA_Data$Gender); PA_Data$Health <- factor(PA_Data$Health)
levels(PA_Data$Health)                     # first level is the reference
Model1 <- glm(Claim ~ Gender * Health + Age, family = poisson, data = PA_Data)
Model2 <- glm(Claim ~ Health + Age,         family = poisson, data = PA_Data)
summary(Model1); logLik(Model1); deviance(Model1)
AIC(Model1, Model2)                        # smaller is better
anova(Model2, Model1, test = "Chisq")      # p > 0.05: drop Gender terms
exp(coef(Model2))                          # multiplicative effects (log link)

newc <- data.frame(Age = c(30, 50), Health = c("Diabetic", "Non-Diabetic"),
                   Gender = c("Male", "Female"))
lam   <- predict(Model2, newdata = newc, type = "response")
price <- 5000 * lam
# Rs 6000 first claim, Rs 4000 each later claim
price_mod <- 4000 * lam + 2000 * (1 - exp(-lam))   # P(N >= 1) = 1 - exp(-lam)
cbind(lam, price, price_mod)
```

**Write:** the price change is 2000(1 - e^-lambda) - 1000 lambda, negative when lambda > 1.59. Low-risk customers pay more (the extra first-claim payment dominates); high-risk customers pay less.

From printed output only: log-likelihood = k - AIC/2; deviance drop 30.147 - 14.436 = 15.71 against `qchisq(0.95, 2)` = 5.99 shows the bigger model is significantly better. Deviances compare only for nested models of the same family; use AIC across families.

## 21. Logistic regression (yes/no outcome)

```r
# ev <- read.csv("ev_purchase.csv")        # needs your file
ev$AvgDailyCommute <- factor(ev$AvgDailyCommute)   # codes 1,2,3 are categories
model_a <- glm(PurchaseEV ~ AvgDailyCommute + ChargingAccess + HouseholdAdults,
               family = binomial(link = "logit"), data = ev)
summary(model_a)
cust <- data.frame(AvgDailyCommute = factor(3, levels = 1:3),   # 20 km is code 3
                   ChargingAccess = 4, HouseholdAdults = 2)
predict(model_a, newdata = cust, type = "response")   # P(purchase); > 0.5 means buy
model_b <- glm(PurchaseEV ~ (AvgDailyCommute + ChargingAccess + HouseholdAdults)^2,
               family = binomial, data = ev)
AIC(model_a, model_b); anova(model_a, model_b, test = "Chisq")
exp(coef(model_a))                          # odds ratios
```

**Write:** a numeric variable gets one slope; a factor with k levels gets k - 1 dummies relative to the reference level.

## 22. Gamma and Gaussian GLMs, AIC comparison

```r
# crop <- read.csv("crop_yield.csv")        # needs your file
m_gam <- glm(Yield ~ Area + Rainfall + Groundwater + pH,
             family = Gamma(link = "inverse"), data = crop)   # capital G
m_lm  <- lm(Yield ~ Area + Rainfall + Groundwater + pH, data = crop)
AIC(m_gam, m_lm)
avg <- as.data.frame(t(colMeans(crop)))     # one-row data frame of means
predict(m_gam, newdata = avg, type = "response")
predict(m_lm,  newdata = avg)               # equals mean(crop$Yield) exactly
mean(crop$Yield)
# yield at zero groundwater = intercept of the simple regression
pred0 <- predict(lm(Yield ~ Groundwater, data = crop), data.frame(Groundwater = 0))
(pred0 - mean(crop$Yield)) / mean(crop$Yield) * 100
```

## 23. Principal components

**Use when:** "variance explained by PC1", "components needed for 90%", "which group is most correlated". Use `scale. = TRUE` when variables are on different scales.

```r
run <- iris[, 1:3]                          # replace with your columns
pc <- prcomp(run, scale. = TRUE)
summary(pc)                                 # sd, proportion, cumulative
prop <- pc$sdev^2 / sum(pc$sdev^2)
prop[1]; cumsum(prop)
which(cumsum(prop) >= 0.90)[1]              # components for 90%
pc$rotation                                 # loadings
screeplot(pc, type = "lines", main = "Scree plot")
```

**Write:** same-sign PC1 loadings mean a general level component. The group whose PC1 alone is near 90% is the most strongly correlated.

## 24. Bayesian conjugate analysis

**Use when:** a prior and data. Poisson data with Gamma(alpha, lambda) prior gives Gamma(alpha + total, lambda + n). Exponential(r) prior is Gamma(1, r).
Losses: squared error gives the mean, absolute error the median, all-or-nothing the mode (a - 1)/l.

```r
x <- c(1, 6, 0, 3, 5, 2, 1, 0)
a1 <- 2 + sum(x); l1 <- 5 + length(x)       # prior Gamma(2, 5): posterior Gamma(20, 13)
a1 / l1                                     # mean 1.5385
(a1 - 1) / l1                               # mode 1.4615
qgamma(0.5, a1, l1)                         # median
qgamma(c(0.025, 0.975), a1, l1)             # 95% interval (0.9397, 2.2824)
dgamma(c(1, 5), a1, l1)                     # density at 1 and 5
pgamma(1, a1, l1); pgamma(5, a1, l1, lower.tail = FALSE)
set.seed(1234)
mu_sim <- rgamma(2000, shape = a1, rate = l1)
tail(mu_sim)
hist(mu_sim, freq = FALSE, xlab = "mu", main = "Posterior of mu"); lines(density(mu_sim))

# prior vs posterior plot: set ylim so the taller curve fits
y <- c(5,5,6,2,4,10,2,5,5,2,5,3,7,4,4,5,4,6,7,2,8,4,6,4,3,6,6,6,5,7)
a <- 1 + sum(y); b <- 0.5 + length(y)       # Exp(0.5) prior: Gamma(149, 30.5)
curve(dgamma(x, 1, 0.5), from = 3.2, to = 6.8, ylim = c(0, 1),
      xlab = "theta", ylab = "Density", main = "Prior and posterior")
curve(dgamma(x, a, b), add = TRUE, lty = 2)
legend("topright", legend = c("Prior", "Posterior"), lty = 1:2)
a / b                                       # 4.885
qgamma(c(0.05, 0.95), a, b)                 # 90% interval (4.246, 5.562)
pgamma(5, a, b, lower.tail = FALSE)         # P(theta > 5) = 0.3776
```

Beta prior with binomial data: Beta(a + x, b + n - x), e.g. `qbeta(c(0.025, 0.975), a + x, b + n - x)`.

## 25. Credibility, equal exposure (Model 1)

**Looks like:** claims for several companies/states over n years; credibility factor and premiums.
**Use when:** every risk has the same volume each year. Rows must be risks.

```r
q4 <- matrix(c(455,251,309,400, 458,322,246,426,
               587,292,217,470, 531,340,120,547), ncol = 4, nrow = 4)
n <- ncol(q4)
xbar_i <- rowMeans(q4)
m_hat  <- mean(q4)                           # E[m(theta)] = 373.19
s2_hat <- mean(apply(q4, 1, var))            # E[s^2(theta)] = 3967.85
v_hat  <- var(xbar_i) - s2_hat / n           # Var[m(theta)] = 16843.2
Z <- n / (n + s2_hat / v_hat)                # 0.9444 (set Z = 0 if v_hat < 0)
prem <- Z * xbar_i + (1 - Z) * m_hat         # 500.27 305.25 231.35 455.88
```

**Write:** Z rises towards 1 as n grows, since a risk's own mean becomes more reliable.

## 26. Credibility, different exposures (Model 2)

**Use when:** the number of policies differs by risk and year. You need the volumes as well as the claims.

```r
claims <- data.frame(Year1 = c(14.2, 58.6, 123), Year2 = c(15.8, 63.1, 132),
                     Year3 = c(22.7, 81.0, 161), Year4 = c(19, 64.2, 133))
nopols <- data.frame(Year1 = c(163, 4435, 16184), Year2 = c(189, 4761, 17443),
                     Year3 = c(252, 5576, 20102), Year4 = c(199, 4581, 18000))
n <- ncol(claims); N <- nrow(claims)
X     <- claims / nopols                              # claims per policy
Xibar <- rowSums(claims) / rowSums(nopols)
Pibar <- rowSums(nopols); Pbar <- sum(Pibar)
Pstar <- sum(Pibar * (1 - Pibar / Pbar)) / (N * n - 1)          # 2891.6
m <- sum(claims) / Pbar                                         # E[m] 0.009660
s <- mean(rowSums(nopols * (X - Xibar)^2) / (n - 1))            # E[s^2] 0.002750
v <- (sum(rowSums(nopols * (X - m)^2)) / (n * N - 1) - s) / Pstar   # Var[m]
Zi <- Pibar / (Pibar + s / v)                                   # 0.9813 0.9992 0.9998
prem <- Zi * Xibar + (1 - Zi) * m                               # per policy
c(5000, 4800, 4200) * prem                                      # 438.99 66.18 32.15
```

**Write:** Z is near 1, so the premium rests almost entirely on each company's own experience.

## 27. Bootstrap

**Use when:** approximate a sampling distribution by resampling. Non-parametric resamples the data (never exceeds the observed maximum). Parametric fits a model (exponential, rate 1/mean) and simulates from it.

```r
data <- c(14.8, 17.6, 11.2, 13.5, 12.9, 10.8, 10.5, 12.3)
mean(data); sd(data)                                  # 12.95 2.374
set.seed(2025)
boot_np <- replicate(1000, mean(sample(data, size = 8, replace = TRUE)))
hist(boot_np, freq = FALSE, xlab = "Bootstrap mean", main = "Non-parametric")
lines(density(boot_np)); mean(boot_np); sd(boot_np)   # sd near 0.8

set.seed(2025)
lam_hat <- 1 / mean(data)                             # 0.07722 (MLE and moments)
boot_p <- replicate(1000, mean(rexp(8, rate = lam_hat)))
hist(boot_p, freq = FALSE, xlab = "Bootstrap mean", main = "Parametric")
lines(density(boot_p)); mean(boot_p); sd(boot_p)      # sd near 4.6
1 / mean(boot_np); 1 / mean(boot_p)                   # estimates of lambda
```

**Write:** the parametric spread is far larger because an exponential with mean 12.95 has sd 12.95, so the data (sd 2.37) fit it poorly.

## 28. Maximum likelihood and log-likelihood plot

**Use when:** estimate a parameter from a likelihood. Poisson: lambda-hat = S/n. Exponential: 1/mean.

```r
n <- 150; S <- 280
lambda <- seq(0.1, 3.3, by = 0.1)
loglik <- S * log(lambda) - n * lambda
plot(lambda, loglik, xlab = "lambda", ylab = "Log_likelihood",
     main = "Log-likelihood of lambda")
lambda[which.max(loglik)]                    # 1.9 (grid)
S / n                                        # 1.8667 (exact)
optimise(function(l) S * log(l) - n * l, c(0.01, 5), maximum = TRUE)$maximum
```

For several parameters minimise the negative log-likelihood with `optim`.

---

## Last-hour checklist

- `getwd()`; confirm the data file loads and `names()` match your formulas.
- Copy the data-entry lines from Codes.docx first.
- Every sub-part: code, printed output, one sentence of interpretation.
- About one minute per mark; leave ten minutes to re-run the script from the top.
- Plots labelled. Tests: H0, H1, statistic, p-value, decision, conclusion.
