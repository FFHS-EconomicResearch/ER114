# ============================================================
# Store selection for ER114 — multi-criteria analysis
#
# Goal: Find the most didactically useful store for ER114 across
# all three sessions, plus an appropriate replication partner.
#
# Criteria (per store):
#   1. OVB-visibility:  difference in feat-coefficient with/without deal
#   2. Diagnostic-cleanness: no severe heteroscedasticity / multicollinearity
#   3. Wrangling richness: n missing weeks, n feat anomalies
#   4. Brand heterogeneity: SD-spread between brand-specific logmove
#   5. Sample size: n ≥ 300
#   6. Plausibility: feat coefficient in [0.5, 1.0]
#   7. Demographics: not in extreme quantile
#   8. Replication partner: demographically similar second store exists
# ============================================================

library(tidyverse)
library(broom)
library(car)        # vif()
library(lmtest)     # bptest()

tbl_full <- read_rds(xfun::from_root("data", "raw", "dominicks_full.rds"))
tbl_demo <- read_rds(xfun::from_root("data", "raw", "dominicks_demo.rds"))


# ============================================================
# 1. Per-store model fitting
# ============================================================

# Filter to stores with enough observations
stores <- tbl_full %>%
  count(store) %>%
  filter(n >= 300) %>%
  pull(store)

cat("Number of stores with n >= 300:", length(stores), "\n")


# Helper: fit naive and full model, extract metrics
fit_store_models <- function(store_id) {
  tbl_s <- tbl_full %>% filter(store == store_id)
  if (nrow(tbl_s) < 200) return(NULL)

  mod_naive <- tryCatch(
    glm(logmove ~ feat + log(price) + brand, data = tbl_s),
    error = function(e) NULL
  )
  mod_full <- tryCatch(
    glm(logmove ~ feat + deal + log(price) + brand, data = tbl_s),
    error = function(e) NULL
  )
  if (is.null(mod_naive) || is.null(mod_full)) return(NULL)

  # OVB metrics
  feat_naive <- coef(mod_naive)["feat"]
  feat_full  <- coef(mod_full)["feat"]
  ovb_diff   <- feat_naive - feat_full

  # Multicollinearity
  vif_max <- tryCatch(max(car::vif(mod_full)), error = function(e) NA)

  # Heteroscedasticity (Breusch-Pagan)
  bp_p <- tryCatch(
    lmtest::bptest(mod_naive)$p.value,
    error = function(e) NA
  )

  # Brand heterogeneity (SD of logmove per brand, then variance of these SDs)
  brand_sd_spread <- tbl_s %>%
    group_by(brand) %>%
    summarise(sd_lm = sd(logmove), .groups = "drop") %>%
    pull(sd_lm) %>%
    sd()

  # Wrangling richness — feat anomalies (raw feat between 0 and 1, before rounding)
  # We work with rounded feat already, so we use deal-feat inconsistencies instead
  n_feat1 <- sum(tbl_s$feat == 1)

  # Missing weeks
  expected_weeks <- seq(min(tbl_s$week), max(tbl_s$week))
  n_missing_weeks <- length(setdiff(expected_weeks, unique(tbl_s$week)))

  tibble(
    store        = store_id,
    n_obs        = nrow(tbl_s),
    feat_naive   = round(feat_naive, 3),
    feat_full    = round(feat_full,  3),
    ovb_diff     = round(ovb_diff,   3),
    vif_max      = round(vif_max,    2),
    bp_pvalue    = round(bp_p,       4),
    brand_sd_spread = round(brand_sd_spread, 3),
    n_feat1      = n_feat1,
    n_missing_weeks = n_missing_weeks
  )
}


# Iterate over all stores
cat("Fitting models for all", length(stores), "stores...\n")
results <- map_dfr(stores, fit_store_models)
cat("Done.\n\n")


# ============================================================
# 2. Add scoring criteria
# ============================================================

results_scored <- results %>%
  mutate(
    # OVB-visibility: larger is better, but should be positive
    score_ovb         = ovb_diff,
    # Plausibility of feat-coef (in [0.5, 1.0] is ideal)
    score_plausible   = case_when(
      feat_naive >= 0.5 & feat_naive <= 1.0 ~ 1,
      feat_naive >= 0.3 & feat_naive <= 1.2 ~ 0.5,
      TRUE ~ 0
    ),
    # Clean diagnostics: low VIF
    score_clean_vif   = if_else(vif_max < 5, 1, 0),
    # Brand heterogeneity: larger spread is more interesting for EDA
    score_brand_het   = brand_sd_spread,
    # Wrangling richness: more missing weeks = more teachable
    score_wrangling   = n_missing_weeks,
    # Sample size
    score_n           = n_obs / 400
  )


# ============================================================
# 3. Find candidate "main stores"
# ============================================================

cat("=== Top 10 stores by OVB visibility (positive ovb_diff) ===\n")
results_scored %>%
  filter(ovb_diff > 0) %>%
  arrange(desc(ovb_diff)) %>%
  select(store, n_obs, feat_naive, feat_full, ovb_diff,
         brand_sd_spread, n_missing_weeks) %>%
  head(10) %>%
  print()

cat("\n=== Top 10 by brand heterogeneity ===\n")
results_scored %>%
  arrange(desc(brand_sd_spread)) %>%
  select(store, n_obs, feat_naive, ovb_diff,
         brand_sd_spread, n_missing_weeks) %>%
  head(10) %>%
  print()

cat("\n=== Top 10 by Wrangling richness (missing weeks) ===\n")
results_scored %>%
  arrange(desc(n_missing_weeks)) %>%
  select(store, n_obs, feat_naive, ovb_diff,
         brand_sd_spread, n_missing_weeks) %>%
  head(10) %>%
  print()


# ============================================================
# 4. Composite score for the "best all-round" store
# ============================================================

results_composite <- results_scored %>%
  filter(
    feat_naive > 0.5 & feat_naive < 1.2,   # plausible effect
    ovb_diff > 0.005,                       # OVB visible
    vif_max < 10,                           # no severe multicollinearity
    n_obs >= 300                            # enough data
  ) %>%
  mutate(
    # rescale all scores to [0, 1]
    z_ovb       = (ovb_diff       - min(ovb_diff))       / (max(ovb_diff)       - min(ovb_diff)),
    z_plausible = score_plausible,
    z_brand_het = (brand_sd_spread - min(brand_sd_spread))/ (max(brand_sd_spread)- min(brand_sd_spread)),
    z_n         = pmin(n_obs / 400, 1),
    composite_score = (
      0.40 * z_ovb +
      0.20 * z_plausible +
      0.20 * z_brand_het +
      0.20 * z_n
    )
  ) %>%
  arrange(desc(composite_score))

cat("\n=== Top 10 best all-round stores (weighted composite score) ===\n")
cat("Weights: 40% OVB, 20% plausibility, 20% brand heterogeneity, 20% sample size\n\n")
results_composite %>%
  select(store, n_obs, feat_naive, ovb_diff,
         brand_sd_spread, n_missing_weeks, composite_score) %>%
  head(10) %>%
  print()


# ============================================================
# 5. Find replication partner candidates
# ============================================================
#
# Strategy: for each candidate main store, find demographically nearest
# neighbour. Use Euclidean distance in standardised demographic space.

# Standardise demographics
demo_std <- tbl_demo %>%
  mutate(across(-store, ~ as.numeric(scale(.))))


find_replication_partner <- function(main_store_id, n = 3) {
  if (!main_store_id %in% demo_std$store) return(NULL)

  ref <- demo_std %>% filter(store == main_store_id) %>% select(-store)
  others <- demo_std %>% filter(store != main_store_id)

  others %>%
    rowwise() %>%
    mutate(distance = sqrt(sum((c_across(-store) - as.numeric(ref))^2))) %>%
    ungroup() %>%
    select(store, distance) %>%
    arrange(distance) %>%
    head(n)
}


# For the top 3 stores, find their replication partners
cat("\n=== Replication partner candidates for top 3 all-round stores ===\n\n")

top3_stores <- results_composite %>% head(3) %>% pull(store)
for (s in top3_stores) {
  cat("Main store:", s, "\n")
  partners <- find_replication_partner(s, n = 3)
  # Merge partner OVB info
  partner_info <- partners %>%
    left_join(results_scored, by = "store") %>%
    select(store, distance, n_obs, feat_naive, ovb_diff,
           brand_sd_spread) %>%
    mutate(distance = round(distance, 2))
  print(partner_info)
  cat("\n")
}


# ============================================================
# 6. Save results for later inspection
# ============================================================

results_scored %>%
  write_rds(xfun::from_root("data", "raw", "Sondage",
                            "store_selection_results.rds"))
results_composite %>%
  write_rds(xfun::from_root("data", "raw", "Sondage",
                            "store_selection_composite.rds"))

cat("Results saved.\n")
