# ============================================================
# DataImport.R — Data Import and Local Storage
# Session 1 · Lab 1: R Projects & Quarto
#
# Purpose:  Import all datasets, save raw snapshots to data/raw/,
#           document each dataset in data/raw/README.md.
#
# Workflow:
#   Case 1 — Data fetched by R (packages, URLs, APIs):
#             load into tibble → write_rds() with date-stamped filename
#   Case 2 — Local files:
#             place manually in data/raw/ → read with original filename
#
# After cleaning/transformation: save to data/tidy/ → DataWrangling.R (Lab 3+)
#
# Navigate sections: Ctrl+Shift+O (RStudio Document Outline)
# ============================================================


# Setup ----
library(tidyverse)    # data wrangling & ggplot2 (includes magrittr pipe %>%)
library(glue)         # glue() for readable date-stamped filenames
library(xfun)         # from_root() for portable project-relative paths
library(openintro)    # ames dataset
library(ISLR2)        # Carseats dataset
library(gt)           # formatted tables (used in Quarto presentation)


# Section 1: Package / URL / API Data ----
# R fetches data directly into memory — no local file exists yet.
# Save immediately to data/raw/ with a date-stamped filename.
# Rule: only my_out_file changes between datasets; the rest stays identical.

## A2a: Import from packages ----
data(ames)                                # housing dataset (demo in this lab)
tbl_carseats <- tibble(Carseats)          # retail dataset — used in Labs 2–4

# Check Environment panel: tbl_carseats should appear under "Data" with dimensions.
# ames may appear as <Promise> — run glimpse(ames) to force loading.

## A2b: Save to data/raw/ with date-stamped filename ----
# format(Sys.Date(), "%Y%m%d") → "20240315"  (no hyphens, safe on all OS)

# Save tbl_carseats
my_out_file <- glue("carseats_{format(Sys.Date(), '%Y%m%d')}.rds")
tbl_carseats %>%
  write_rds(from_root("data", "raw", my_out_file))
# Result: e.g. data/raw/carseats_20240315.rds

# Save ames  (A2b task — same routine, only my_out_file changes)
my_out_file <- glue("ames_{format(Sys.Date(), '%Y%m%d')}.rds")
ames %>%
  write_rds(from_root("data", "raw", my_out_file))
# Result: e.g. data/raw/ames_20240315.rds

## URL / API template (same routine as above) ----
# Uncomment to run — downloads from the internet:
# url <- "http://data.insideairbnb.com/switzerland/vd/geneva/2024-09-18/data/listings.csv.gz"
# tbl_airbnb <- read_csv(url)
# my_out_file <- glue("airbnb_geneva_{format(Sys.Date(), '%Y%m%d')}.rds")
# tbl_airbnb %>% write_rds(from_root("data", "raw", my_out_file))


# Section 2: Local Files ----
# Files placed manually in data/raw/ — keep the original filename.
# No write_rds() needed: the original file IS the local raw copy.
# Rule: only my_in_file changes between datasets; the rest stays identical.

## A3: Import local files ----
# First: download files from Moodle and place in data/raw/ (see Lab instructions)

my_in_file <- "autos_(StockerUIBK)_20240414.csv"   # semicolon-separated CSV
tbl_autos  <- read_csv2(from_root("data", "raw", my_in_file))

my_in_file <- "buli_raw_2023-09-06.rds"            # R binary format
tbl_buli   <- read_rds(from_root("data", "raw", my_in_file))

# Verify: tbl_autos and tbl_buli should appear in the Environment panel.

## File without a date in its name? ----
# Document the download date in data/raw/README.md instead.
# Example: my_in_file <- "data.csv"   <- no version info in name -> README!


# A4: Explore Datasets ----
# Quick structural check — full EDA comes in Lab 2.

## Dimensions ----
dim(tbl_autos);   nrow(tbl_autos);   ncol(tbl_autos)
dim(tbl_buli);    nrow(tbl_buli);    ncol(tbl_buli)

# What does one row represent?
# tbl_autos:  one car listing
# tbl_buli:   one ___?

## Structure ----
glimpse(tbl_autos)
glimpse(tbl_buli)

# What variable types appear? (<chr>, <dbl>, <date>, <fct> ...)

## First rows ----
head(tbl_autos)
head(tbl_buli)


# Document in data/raw/README.md ----
# After running this script, update data/raw/README.md.
# Template (copy into the file and fill in actual values):
#
# | File                             | Source                                    | Obtained   | Description                 |
# |----------------------------------|-------------------------------------------|------------|-----------------------------|
# | carseats_YYYYMMDD.rds            | ISLR2 R package (v1.0)                    | YYYY-MM-DD | Retail: 400 stores, 11 vars |
# | ames_YYYYMMDD.rds                | openintro R package                       | YYYY-MM-DD | Housing: 2930 obs, 82 vars  |
# | autos_(StockerUIBK)_20240414.csv | Provided by course instructor, via Moodle | 2024-04-14 | Car listings                |
# | buli_raw_2023-09-06.rds          | Provided by course instructor, via Moodle | 2023-09-06 | Bundesliga match data       |


# Bonus B1: Variable Types as a Table ----
tibble(
  Variable = names(tbl_carseats),
  Type     = map_chr(tbl_carseats, class)
) %>%
  gt() %>%
  tab_header(title = "Variables: Carseats")


# Bonus B2: Missing Values ----
tbl_carseats %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "Variable",
               values_to = "Missing") %>%
  filter(Missing > 0) %>%
  gt() %>%
  tab_header(title = "Missing values: Carseats")
# Expected: no missing values — Carseats is a clean teaching dataset.


# Bonus B3: Visualisation with ames ----
ames %>%
  ggplot(aes(x = area, y = price)) +
  geom_point(alpha = 0.4, color = "#502479") +        # --- colour ---
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title    = "Living Area vs. Sale Price",
    subtitle = "Ames, Iowa 2006-2010",
    x        = "Living area (sqft)",
    y        = "Sale price (USD)"
  ) +
  theme_minimal()


# Bonus B5: Airbnb Data from the Web ----
# url_airbnb <- "http://data.insideairbnb.com/switzerland/vd/geneva/2024-09-18/data/listings.csv.gz"
# tbl_airbnb <- read_csv(url_airbnb)
# dim(tbl_airbnb)
# glimpse(tbl_airbnb)
# How many listings? What variables? What business question could this answer?
