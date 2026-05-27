---

# Bonus — For independent study

The following exercises extend the lab in different directions. Each is self-contained; pick those that interest you. The companion script `Session3_Lab1_OutOfSample_OJ_script.R` contains the code skeletons.

## ⚠️ B1: Diagnostics on the full model — did adding `deal` help?

In Step 1b we added `deal` to address the A1 violation. A natural follow-up question: do the residuals of the **full model** show a cleaner pattern than those of the baseline? Specifically, did adding `deal` reduce the heteroscedasticity (B2) and the heavy tails (B4) observed in Step 1a?

:::{.callout-important icon=false}
## ⚠️ B1: Re-run the diagnostics for the full model

Repeat the empirical diagnostics from Step 1a — but this time for `mod_full_114` (the model that includes `deal`):

```{r b1-diagnostics}
#| eval: false
# 1. Six-panel diagnostic
library(ggfortify)
autoplot(mod_full_114, which = 1:6, ncol = 2, label.size = 3) + theme_minimal()

# 2. Formal tests
car::durbinWatsonTest(mod_full_114)
shapiro.test(residuals(mod_full_114))

# 3. VIF
car::vif(mod_full_114)
```

Compare side by side with the diagnostics from Step 1a. For each of the following, write a one-sentence verdict:

a. Did the heteroscedasticity pattern (Scale-Location plot) change?
b. Did the Q-Q plot tails improve, worsen, or stay the same?
c. Are the same observations (weeks 23, 28) still flagged as influential?
d. Did the VIF change for `feat` and `log(price)` after adding `deal`?

**Interpretation:** If the diagnostics did *not* visibly improve, what does that tell you about the A1 / C1 violation? Is `deal` the only relevant omitted variable, or might there be others?
:::

:::{.callout-note collapse="true" icon=false color="success"}
### 🟢 Expected reasoning (Click to unfold)

If `deal` were the only relevant omitted variable, adding it should reduce systematic structure in the residuals. In practice, expect the diagnostics to look broadly similar — mild heteroscedasticity and heavy tails often persist. This is consistent with the data-generating process: weekly retail demand is driven by many factors (holidays, weather, competing chains' promotions, stock-outs) that no model with four regressors can fully capture.

The lesson: addressing one A1 violation does not eliminate all structural patterns in the residuals. The diagnostic toolbox identifies *symptoms*; it does not enumerate all possible causes.

The VIF for `feat` will increase after adding `deal` (because `feat` and `deal` are highly correlated by construction). The VIF for `log(price)` may decrease slightly. Neither change indicates a new problem — it reflects the now-explicit correlation structure that was previously invisible.
:::

---

## ⚠️ B2: Robust standard errors in action (B2 first aid)

The diagnostics in Step 1a flagged mild heteroscedasticity. In the lab we noted that `sandwich::vcovHC` can provide robust standard errors. This bonus task puts that into practice.

:::{.callout-important icon=false}
## ⚠️ B2: Compare classical and heteroscedasticity-robust SE

```{r b2-robust-se}
#| eval: false
library(sandwich)
library(lmtest)

# Classical (OLS-assumed) standard errors
classical_se <- summary(mod_full_114)$coefficients[, "Std. Error"]

# Heteroscedasticity-consistent SE (White, HC3 — preferred for finite samples)
robust_vcov  <- sandwich::vcovHC(mod_full_114, type = "HC3")
robust_test  <- lmtest::coeftest(mod_full_114, vcov = robust_vcov)
robust_se    <- robust_test[, "Std. Error"]

# Compare side by side
tibble::tibble(
  term         = names(coef(mod_full_114)),
  estimate     = coef(mod_full_114),
  se_classical = classical_se,
  se_robust    = robust_se,
  ratio        = robust_se / classical_se
) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4)))
```

a. How do the robust SE differ from the classical SE for the `feat` coefficient?
b. Does the conclusion about statistical significance change?
c. Under what conditions would you report the robust SE rather than the classical ones in Sarah's memo?
:::

:::{.callout-note collapse="true" icon=false color="success"}
### 🟢 What to expect (Click to unfold)

For mild heteroscedasticity, the robust SE will typically be slightly larger than the classical ones (ratio between 1.0 and 1.2), but the qualitative conclusion (significant / not significant at α = 0.05) usually does not change.

**Reporting rule of thumb in applied work:** when the diagnostics show *any* evidence of heteroscedasticity — even mild — report the robust SE. The cost is negligible; the gain in defensibility is real. Many journals in econometrics now require robust SE by default.

**Important limitation:** robust SE only fix B2. They do **not** address A1 or C1. The coefficient itself is still biased if `deal` is omitted. Robust SE without a correctly specified model deliver more precise statements about the wrong parameter.
:::

---

## ⚠️ B3: Bootstrap confidence intervals — a B4-free alternative

The Shapiro-Wilk test rejected B4 (normality). In Step 1a we argued that the CLT keeps t-tests approximately valid at n = `r nrow(tbl_114)`. But there is also a distribution-free alternative you already know from Session 2: the **bootstrap**. This bonus task lets you compare classical and bootstrap CIs for the `feat` coefficient.

:::{.callout-important icon=false}
## ⚠️ B3: Bootstrap a confidence interval for β_feat

Using the `infer` workflow from Session 2:

```{r b3-bootstrap}
#| eval: false
library(infer)

# Bootstrap distribution of the feat coefficient
set.seed(42)
boot_dist <- tbl_114 %>%
  specify(formula = logmove ~ feat + deal + log(price) + brand) %>%
  generate(reps = 2000, type = "bootstrap") %>%
  fit()

# 95 % percentile-based bootstrap CI for the feat coefficient
boot_ci_feat <- boot_dist %>%
  dplyr::filter(term == "feat") %>%
  get_confidence_interval(level = 0.95, type = "percentile")

boot_ci_feat

# Compare with classical CI
broom::tidy(mod_full_114, conf.int = TRUE) %>%
  dplyr::filter(term == "feat") %>%
  dplyr::select(term, estimate, conf.low, conf.high)
```

a. How close are the two intervals?
b. Which assumption does the bootstrap *not* require, that classical inference does?
c. In what situation would you switch from classical to bootstrap CIs in practice?
:::

:::{.callout-note collapse="true" icon=false color="success"}
### 🟢 What to expect (Click to unfold)

With n = `r nrow(tbl_114)`, the bootstrap and classical intervals will be very similar — typically within a few percent of each other. This is the CLT at work: the sampling distribution of $\hat\beta_{\text{feat}}$ is approximately normal, regardless of the residual distribution.

The bootstrap does not require B4 (normality of errors). It also does not assume any specific parametric form for the sampling distribution — it estimates it directly from the data. This makes it especially valuable in three situations:

1. **Small samples** where the CLT approximation is uncertain
2. **Complex statistics** (e.g. ratios, medians) where no closed-form variance exists
3. **Heavy-tailed distributions** where the convergence rate of the CLT is slow

In our setting, with a clean large-sample situation, the bootstrap is a useful cross-check rather than a necessity. But it is a tool worth having in the box.
:::

---

## ⚠️ B4: Clustered standard errors on the full panel

In Step 3 we will pool all 83 stores for prediction. But what if we want to make *inferential* statements on the full panel — say, the average $\beta_{\text{feat}}$ across the chain? The classical SE then become misleading, because observations within a store are not independent (same operational decisions, same local trends).

:::{.callout-important icon=false}
## ⚠️ B4: Cluster SE by store on the full-panel regression

```{r b4-cluster-se}
#| eval: false
library(sandwich)
library(lmtest)

mod_full_panel <- lm(logmove ~ feat + deal + log(price) + brand, data = tbl_full)

# Classical SE
classical_test <- lmtest::coeftest(mod_full_panel)

# Cluster-robust SE (clustering at the store level)
cluster_vcov <- sandwich::vcovCL(mod_full_panel, cluster = ~ store)
cluster_test <- lmtest::coeftest(mod_full_panel, vcov = cluster_vcov)

# Compare side by side
tibble::tibble(
  term            = names(coef(mod_full_panel)),
  estimate        = coef(mod_full_panel),
  se_classical    = classical_test[, "Std. Error"],
  se_clustered    = cluster_test[, "Std. Error"],
  ratio           = cluster_test[, "Std. Error"] / classical_test[, "Std. Error"]
) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4)))
```

a. How much larger are the clustered SE compared to the classical ones?
b. Why does ignoring within-store correlation lead to *underestimated* SE in the classical case?
c. What does this imply for the credibility of the t-statistics reported by `summary(mod_full_panel)`?
:::

:::{.callout-note collapse="true" icon=false color="success"}
### 🟢 What to expect (Click to unfold)

The clustered SE are typically 2–4× larger than the classical SE on this kind of retail panel. The reason: classical SE treat the ≈ 28 000 observations as independent draws. In reality, the 351 observations within Store 114 share many unobserved factors (local manager, neighbourhood trends, holiday patterns) — they carry far less independent information than 351 truly independent observations would.

Cluster-robust SE correct for this by treating each store as the unit of independent variation. The effective sample size in the cluster-robust calculation is closer to the **number of clusters (83)** than to the number of observations (28 000) — which is why the SE inflate substantially.

**Operational rule:** whenever your data have a natural grouping structure (stores, regions, schools, firms, individuals over time), report SE clustered at the level of that grouping. The single most common inferential mistake in panel data analysis is to ignore clustering.

In Lab 2 we will work with multilevel models (`lme4::lmer`) that handle this in a more general way.
:::

---

## ⚠️ B5: Drop-one sensitivity — how much do single observations matter?

The diagnostics in Step 1a flagged weeks 23 and 28 as high-leverage points. The Cook's-distance plot quantified their influence, but it did not show what happens if we actually leave them out. This bonus task answers that question directly: re-fit the model without each suspicious observation and see how much $\hat\beta_{\text{feat}}$ moves.

:::{.callout-important icon=false}
## ⚠️ B5: Quantify the influence of weeks 23 and 28

```{r b5-drop-one}
#| eval: false
# Identify the suspicious observations
tbl_114 %>%
  dplyr::mutate(row = dplyr::row_number()) %>%
  dplyr::filter(row %in% c(23, 28))

# Re-fit on subsets that exclude each suspicious week
mod_no_23  <- lm(logmove ~ feat + deal + log(price) + brand,
                 data = tbl_114 %>% dplyr::slice(-23))
mod_no_28  <- lm(logmove ~ feat + deal + log(price) + brand,
                 data = tbl_114 %>% dplyr::slice(-28))
mod_no_both <- lm(logmove ~ feat + deal + log(price) + brand,
                  data = tbl_114 %>% dplyr::slice(-c(23, 28)))

# Compare the feat coefficient across specifications
tibble::tibble(
  specification = c("Full model (all weeks)",
                    "Drop week 23",
                    "Drop week 28",
                    "Drop both"),
  feat_estimate = c(coef(mod_full_114)["feat"],
                    coef(mod_no_23)["feat"],
                    coef(mod_no_28)["feat"],
                    coef(mod_no_both)["feat"]),
  n             = c(nrow(tbl_114),
                    nrow(tbl_114) - 1,
                    nrow(tbl_114) - 1,
                    nrow(tbl_114) - 2)
) %>%
  dplyr::mutate(
    delta_feat = feat_estimate - coef(mod_full_114)["feat"],
    dplyr::across(where(is.numeric), \(x) round(x, 4))
  )
```

a. How much does $\hat\beta_{\text{feat}}$ change when week 23 is dropped? When week 28 is dropped? When both are dropped?
b. Does the qualitative conclusion (positive effect of `feat` on `logmove`) change in any specification?
c. What does this tell you about the **robustness** of the lab's main finding to individual observations?
d. If you had to defend the original estimate in Sarah's memo, would you mention this sensitivity check? Why or why not?
:::

:::{.callout-note collapse="true" icon=false color="success"}
### 🟢 What to expect (Click to unfold)

Typical findings for this kind of retail data: dropping week 23 or 28 changes $\hat\beta_{\text{feat}}$ by a few hundredths of a log unit. The qualitative conclusion remains intact. The model is **not** driven by these two observations alone — they are influential but not decisive.

The didactic point is twofold:

1. **The diagnostics flag, the sensitivity check quantifies.** Cook's distance tells you *which* observations have leverage; the drop-one re-fit tells you *how much* it actually matters. Both are useful; neither replaces the other.

2. **Robustness reporting is good practice.** Including a one-line sensitivity check in any inferential memo strengthens its defensibility. The standard format: *"The advertising coefficient is robust to dropping the two highest-leverage weeks (delta < 0.05 log units)."*

**An advanced variant** for very engaged students: the `car::influencePlot()` function combines leverage, Cook's distance, and studentized residuals into a single visual. The `car::influence.measures()` function returns a full table of influence diagnostics for every observation.
:::

---

## Notes on the bonus tasks

- B1 is the **direct continuation** of the diagnostic work in Step 1a. Do this one if you want to deepen your understanding of what the diagnostics tell you (and what they don't).
- B2 is the most **operationally useful**: heteroscedasticity-robust SE are the default in most modern applied econometrics.
- B3 reconnects to the **resampling toolkit** from Session 2 and prepares you for situations where classical inference becomes shaky.
- B4 is the **bridge to Lab 2** and to all subsequent panel-data work. It is also the most consequential in practice — most analysts who handle panel data without clustering produce overconfident results.
- B5 demonstrates how to convert a diagnostic flag into a **quantitative robustness statement** — the kind of practice that distinguishes credible from glib applied work.

If you complete all five, you have a complete picture of the inference-side toolbox: diagnostics → robust SE → bootstrap → clustering → sensitivity. The prediction-side toolbox (regularisation, cross-validation) follows in Lab 2.
