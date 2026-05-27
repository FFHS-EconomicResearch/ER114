# ============================================================
# Session 3 · Lab 1: BLUE Bridge — Predict or Explain?
# Companion script for the live session.
# Full explanations and tasks: Session3_Lab1_OutOfSample_OJ.qmd
# Navigate: Ctrl+Shift+O (RStudio Document Outline)
# ============================================================
#
# This is Part 1 of the companion script:
#   - Setup
#   - Phase 0: Translator briefing (no code)
#   - Phase 1a: Naive model + diagnostics
#
# Parts 2-4 (Phase 1b, Phase 2, Phase 3, Phase 4) will follow once
# this part is verified against the real data.
# ============================================================


# Setup ----

library(tidyverse)        # data wrangling and plotting
library(gt)               # publication-quality tables
library(broom)            # tidy model output
library(patchwork)        # combine ggplots
library(car)              # VIF for multicollinearity diagnostics

# shared visual identity
theme_set(theme_minimal(base_size = 11))
col_primary   <- "#D50006"
col_secondary <- "#502479"
col_neutral   <- "#686868"

# Load data ----

# Adjust paths if your project structure differs.
# Expected location: data/raw/ (relative to project root)
tbl_full <- readRDS(xfun::from_root("data", "raw", "dominicks_full.rds"))
tbl_demo <- readRDS(xfun::from_root("data", "raw", "dominicks_demo.rds"))

tbl <- tbl_full

# Quick sanity check ----

# Print structure to confirm expected variables
cat("Data dimensions:\n")
cat("  Rows:", nrow(tbl), "\n")
cat("  Cols:", ncol(tbl), "\n\n")

cat("Variable names:\n")
cat(paste0("  ", names(tbl)), sep = "\n")
cat("\n")

cat("Distinct stores:", n_distinct(tbl$store), "\n")
cat("Distinct brands:", n_distinct(tbl$brand), "\n")
cat("Distinct weeks:",  n_distinct(tbl$week), "\n")
cat("Total observations:", nrow(tbl), "\n\n")


# Phase 1a — Question 2: Explaining the Advertising Effect ----

## Naive model — all 83 stores ----

mod_naive <- lm(logmove ~ feat + log(price) + brand, data = tbl)

cat("Naive model summary:\n")
mod_naive |>
  tidy(conf.int = TRUE) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

cat("\nfeat coefficient:", round(coef(mod_naive)["feat"], 3), "\n")
cat("(this is what Sarah expects — about 25%)\n\n")


## C2: Multicollinearity (VIF) ----

vif_naive <- car::vif(mod_naive)
cat("VIF values for the naive model:\n")
print(vif_naive)
cat("\nMax VIF:", round(max(vif_naive), 2), "\n")
cat("(unproblematic if < 5; warning if 5-10; severe if > 10)\n\n")


## B-Assumptions: Residual plots ----

# Open a 2x2 plot window manually if running interactively
par(mfrow = c(2, 2))
plot(mod_naive)
par(mfrow = c(1, 1))


# ============================================================
# End of Part 1
#
# Verification questions for the team:
#   1. Does tbl_full load correctly from data/raw/?
#   2. Is the feat coefficient close to 0.25 (or 0.2-0.3 range)?
#   3. Are the VIF values all below 5?
#   4. Do the residual plots look reasonable for a real-data set
#      with N ~ 28,000?
#
# Once verified, we proceed to Part 2:
#   - Phase 1b: The feat × deal stumbling block
#   - Phase 2: BLUE bridge becomes operative
#   - Phase 3: Question 1 (Prediction)
#   - Phase 4: Synthesis + bonus
# ============================================================


# Phase 1b — The feat × deal Stumbling Block ----

## Joint distribution ----

cat("\n\n=== feat × deal joint distribution ===\n")
tbl |>
  count(feat = round(feat), deal) |>
  pivot_wider(names_from = deal, values_from = n, names_prefix = "deal=") |>
  print()


## Model with deal added ----

mod_full <- lm(logmove ~ feat + deal + log(price) + brand, data = tbl)

cat("\n=== Model with deal added ===\n")
mod_full |>
  tidy(conf.int = TRUE) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()


## Coefficient comparison ----

cat("\n=== Coefficient comparison (naive vs. with deal) ===\n")
bind_rows(
  tidy(mod_naive) |> mutate(model = "Naive (without deal)"),
  tidy(mod_full)  |> mutate(model = "Full (with deal)")
) |>
  filter(term %in% c("feat", "deal")) |>
  select(model, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

cat("\nfeat in naive model:", round(coef(mod_naive)["feat"], 3), "\n")
cat("feat in full model: ", round(coef(mod_full)["feat"], 3), "\n")
cat("→ If these differ substantially, omitted variable bias is real\n\n")


# Phase 3 — Question 1: Out-of-Sample Evaluation ----

## Train Store 51, predict Store 50 ----

mod_51 <- lm(logmove ~ feat + log(price) + brand,
             data = tbl |> filter(store == 51))

tbl_50 <- tbl |> filter(store == 50)
pred_50 <- predict(mod_51, newdata = tbl_50)

rmse_50 <- sqrt(mean((pred_50 - tbl_50$logmove)^2))

cat("\n=== Predict Store 50 from Store 51 model ===\n")
cat("RMSE on Store 50:", round(rmse_50, 3), "\n")
cat("(should be relatively low — similar demographics)\n\n")


## Predict on all other stores ----

other_stores <- setdiff(unique(tbl$store), 51)

store_rmses <- tibble(store = other_stores) |>
  rowwise() |>
  mutate(
    rmse = {
      d <- tbl |> filter(store == !!store)
      sqrt(mean((predict(mod_51, newdata = d) - d$logmove)^2))
    }
  ) |>
  ungroup() |>
  arrange(desc(rmse))

cat("=== RMSE distribution across all other stores ===\n")
store_rmses |>
  summarise(
    min_rmse    = round(min(rmse),    3),
    median_rmse = round(median(rmse), 3),
    max_rmse    = round(max(rmse),    3),
    n_stores    = n()
  ) |>
  print()

cat("\nWhich 5 stores have highest RMSE (most distant from Store 51)?\n")
store_rmses |> head(5) |> print()


## RMSE distribution plot ----

ggplot(store_rmses, aes(x = rmse)) +
  geom_histogram(bins = 25, fill = col_primary, alpha = 0.8, colour = "white") +
  geom_vline(xintercept = rmse_50, linetype = "dashed",
             colour = col_secondary, linewidth = 1) +
  annotate("text", x = rmse_50, y = Inf,
           label = paste0("Store 50: RMSE = ", round(rmse_50, 3)),
           hjust = -0.1, vjust = 2, colour = col_secondary) +
  labs(
    title    = "Out-of-sample RMSE: Store-51 model on other stores",
    subtitle = "Generalises poorly to demographically different stores",
    x        = "RMSE on out-of-sample store",
    y        = "Number of stores"
  )


## Train/test split across stores ----

set.seed(42)
stores <- unique(tbl$store)
test_stores <- sample(stores, size = floor(0.20 * length(stores)))

tbl_train <- tbl |> filter(!store %in% test_stores)
tbl_test  <- tbl |> filter(store  %in% test_stores)

mod_train <- lm(logmove ~ feat + log(price) + brand, data = tbl_train)
pred_test <- predict(mod_train, newdata = tbl_test)

rmse_test  <- sqrt(mean((pred_test - tbl_test$logmove)^2))
rmse_train <- sqrt(mean((mod_train$residuals)^2))

cat("\n\n=== 80/20 train-test split (across stores) ===\n")
cat("In-sample RMSE  (training stores):", round(rmse_train, 3), "\n")
cat("Out-of-sample RMSE (test stores):", round(rmse_test, 3), "\n\n")


## K-fold cross-validation ----

set.seed(42)
K <- 5
stores_shuffled <- sample(unique(tbl$store))
folds <- cut(seq_along(stores_shuffled), breaks = K, labels = FALSE)

cv_rmses <- map_dbl(1:K, \(k) {
  test_st  <- stores_shuffled[folds == k]
  train_st <- stores_shuffled[folds != k]

  mod_k <- lm(logmove ~ feat + log(price) + brand,
              data = tbl |> filter(store %in% train_st))
  pred_k <- predict(mod_k,
                    newdata = tbl |> filter(store %in% test_st))
  actual_k <- tbl |> filter(store %in% test_st) |> pull(logmove)
  sqrt(mean((pred_k - actual_k)^2))
})

cv_score <- mean(cv_rmses)

cat("=== 5-fold CV (across stores) ===\n")
cat("Per-fold RMSE:", paste(round(cv_rmses, 3), collapse = ", "), "\n")
cat("Mean CV-RMSE: ", round(cv_score, 3), "\n\n")


# ============================================================
# End of Lab 1 main content
#
# Verification questions:
#   1. Naive feat coefficient ≈ 0.25? (Sarah's expectation)
#   2. With deal added: does feat coefficient change substantially?
#   3. Predict Store 50 from Store 51: RMSE realistic (well below
#      out-of-sample stores)?
#   4. RMSE distribution across stores: shows clear spread?
#   5. CV-RMSE: roughly between Store-50-RMSE and median RMSE of
#      all other stores?
# ============================================================
