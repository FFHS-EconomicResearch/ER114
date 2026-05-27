# ============================================================
# Data creation: Dominick's OJ — for ER114 FS26 Session 3
#
# Input:  bayesm::orangeJuice
# Output: data/raw/
#           dominicks_full.rds — 83 stores × 3 brands × weeks (~28k rows)
#           dominicks_demo.rds — 83 stores × 11 demographic variables
#
# In Session 3 labs, subsets (Store 51, Store 50) are filtered on-the-fly
# from tbl_dominicks_full using dplyr::filter(). This keeps the data
# architecture lean and makes the filtering operation explicit as a
# teaching step.
#
# Conventions:
#   - tidy syntax throughout (mutate() pipelines, no $ assignments)
#   - write_rds() / read_rds() for serialisation
#   - tbl_ prefix for tibble variables used in lab materials
#   - brand as readable factor (ordered: dominicks → minute.maid → tropicana)
#   - price scaled to container price ($) via factor 64 (all three brands 64 oz)
# ============================================================

library(tidyverse)
library(bayesm)


# ============================================================
# 1. Load raw bayesm data
# ============================================================

data(orangeJuice)
tbl_yx_raw   <- as_tibble(orangeJuice$yx)
tbl_demo_raw <- as_tibble(orangeJuice$storedemo)

cat("=== Source data ===\n")
cat("yx (panel):     ", nrow(tbl_yx_raw),   " rows × ", ncol(tbl_yx_raw),   " cols\n", sep = "")
cat("storedemo:      ", nrow(tbl_demo_raw), " rows × ", ncol(tbl_demo_raw), " cols\n\n", sep = "")


# ============================================================
# 2. Build tbl_dominicks_full — panel data, top-3 brands, tidy
#
# Brand IDs from ?bayesm::orangeJuice:
#   1  = Tropicana Premium 64 oz
#   5  = Minute Maid       64 oz
#   10 = Dominick's        64 oz
# All three are 64-oz containers → uniform container price = per-oz × 64.
# ============================================================

tbl_dominicks_full <- tbl_yx_raw %>%
  filter(brand %in% c(1, 5, 10)) %>%
  mutate(
    # pick the price column corresponding to the brand of each row
    price_per_oz = case_when(
      brand == 1  ~ price1,
      brand == 5  ~ price5,
      brand == 10 ~ price10
    ),
    # container price ($) — uniform conversion for 64 oz brands
    price = price_per_oz * 64,
    # readable brand factor, ordered from store-brand to premium
    brand = factor(
      recode(as.character(brand),
             "10" = "dominicks",
             "5"  = "minute.maid",
             "1"  = "tropicana"),
      levels = c("dominicks", "minute.maid", "tropicana")
    ),
    # binary advertising indicator (round transition values)
    feat  = round(feat),
    # sales in raw units (for use in EDA / non-log models)
    sales = exp(logmove)
  ) %>%
  select(
    store, brand, week,
    logmove, sales,
    price, price_per_oz,
    feat, deal,
    profit
  ) %>%
  arrange(store, brand, week)

cat("=== tbl_dominicks_full ===\n")
cat("Rows:           ", nrow(tbl_dominicks_full), "\n", sep = "")
cat("Stores:         ", n_distinct(tbl_dominicks_full$store), "\n", sep = "")
cat("Brands:         ", n_distinct(tbl_dominicks_full$brand), "\n", sep = "")
cat("Weeks (range):  ",
    min(tbl_dominicks_full$week), " – ", max(tbl_dominicks_full$week), "\n", sep = "")

cat("\nFirst 3 rows:\n")
print(head(tbl_dominicks_full, 3))


# ============================================================
# 3. Build tbl_dominicks_demo — store demographics, tidy
# ============================================================

tbl_dominicks_demo <- tbl_demo_raw %>%
  rename(store = STORE) %>%
  # all column names to lower case for consistency
  rename_with(tolower, .cols = -store)

cat("\n=== tbl_dominicks_demo ===\n")
cat("Rows:           ", nrow(tbl_dominicks_demo), "\n", sep = "")
cat("Variables:      ", paste(names(tbl_dominicks_demo), collapse = ", "), "\n", sep = "")

cat("\nFirst 3 rows:\n")
print(head(tbl_dominicks_demo, 3))


# ============================================================
# 4. Sanity check — does Store 51 exist? Store 50?
# ============================================================

stores_full <- tbl_dominicks_full %>% distinct(store) %>% pull(store)
stores_demo <- tbl_dominicks_demo %>% pull(store)

cat("\n=== Sanity checks ===\n")
cat("Store 50 in full panel:  ", 50 %in% stores_full, "\n", sep = "")
cat("Store 50 in demographics:", 50 %in% stores_demo, "\n", sep = "")
cat("Store 51 in full panel:  ", 51 %in% stores_full, "\n", sep = "")
cat("Store 51 in demographics:", 51 %in% stores_demo, "\n", sep = "")

stores_missing_demo <- setdiff(stores_full, stores_demo)

cat("\nStores in panel without demographics:", length(stores_missing_demo), "\n")
if (length(stores_missing_demo) > 0) {
  cat("MISSING demographics for stores:\n")
  print(stores_missing_demo)
}


# ============================================================
# 5. Save the two .rds files
# ============================================================

tbl_dominicks_full %>%
  write_rds(xfun::from_root("data", "raw", "dominicks_full.rds"))

tbl_dominicks_demo %>%
  write_rds(xfun::from_root("data", "raw", "dominicks_demo.rds"))

cat("\n=== Files written ===\n")
cat("  data/raw/dominicks_full.rds\n")
cat("  data/raw/dominicks_demo.rds\n\n")


# ============================================================
# 6. Verification — reload and check
# ============================================================

cat("=== Reload verification ===\n")

tbl_check <- read_rds(xfun::from_root("data", "raw", "dominicks_full.rds"))
cat("dominicks_full reloaded — rows:", nrow(tbl_check), "\n")
cat("brand levels:", paste(levels(tbl_check$brand), collapse = " / "), "\n")

tbl_demo_check <- read_rds(xfun::from_root("data", "raw", "dominicks_demo.rds"))
cat("dominicks_demo reloaded — rows:", nrow(tbl_demo_check), "\n")


# ============================================================
# 7. Reference numbers — what will appear inline in Lab 1
# ============================================================

cat("\n=== Reference numbers for Lab 1 (Phase 1a — Store 51) ===\n")

tbl_51 <- tbl_check %>% filter(store == 51)

mod_naive_51 <- glm(logmove ~ feat + log(price) + brand, data = tbl_51)
cat("Store 51 — n =", nrow(tbl_51), "\n")
cat("  feat coefficient (no deal):  ",
    round(coef(mod_naive_51)["feat"], 3), "\n")

mod_full_51 <- glm(logmove ~ feat + deal + log(price) + brand, data = tbl_51)
cat("  feat coefficient (with deal):",
    round(coef(mod_full_51)["feat"], 3), "\n")
cat("  difference (OVB size):       ",
    round(coef(mod_naive_51)["feat"] - coef(mod_full_51)["feat"], 3), "\n")


cat("\n=== Reference numbers for Lab 1 (Phase 3a — Store 50 prediction) ===\n")

tbl_50 <- tbl_check %>% filter(store == 50)
pred_50 <- predict(mod_naive_51, newdata = tbl_50)
rmse_50 <- sqrt(mean((pred_50 - tbl_50$logmove)^2))
cat("Store 50 — n =", nrow(tbl_50), "\n")
cat("  RMSE when predicting from Store 51 model:",
    round(rmse_50, 3), "\n")


cat("\n=== Reference numbers for Lab 1 (Phase 3b — all 83 stores) ===\n")

mod_naive_full <- glm(logmove ~ feat + log(price) + brand, data = tbl_check)
cat("Full panel — n =", nrow(tbl_check), "\n")
cat("  feat coefficient (no deal):  ",
    round(coef(mod_naive_full)["feat"], 3), "\n")

# p-value comparison: same effect, much smaller p-value in big data
p_51   <- summary(mod_naive_51)$coefficients["feat", "Pr(>|t|)"]
p_full <- summary(mod_naive_full)$coefficients["feat", "Pr(>|t|)"]
cat("\np-value comparison (the 'big data' point):\n")
cat("  p(feat) in Store 51:    ", format.pval(p_51, digits = 3), "\n")
cat("  p(feat) on 83 stores:   ", format.pval(p_full, digits = 3), "\n")

cat("\n=== End of data creation ===\n")







tbl_50 <- tbl_check %>% filter(store == 50)

mod_naive_50 <- glm(logmove ~ feat + log(price) + brand, data = tbl_50)
cat("Store 50 — n =", nrow(tbl_50), "\n")
cat("  feat coefficient (no deal):  ",
    round(coef(mod_naive_50)["feat"], 3), "\n")

mod_full_50 <- glm(logmove ~ feat + deal + log(price) + brand, data = tbl_50)
cat("  feat coefficient (with deal):",
    round(coef(mod_full_50)["feat"], 3), "\n")
cat("  difference (OVB size):       ",
    round(coef(mod_naive_50)["feat"] - coef(mod_full_50)["feat"], 3), "\n")

