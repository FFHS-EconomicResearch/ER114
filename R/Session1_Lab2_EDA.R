# =============================================================================
# Session 1 · Lab 2: Data Understanding & Exploratory Data Analysis
# CRISP-DM Phase 2 --- Carseats Case
#
# Companion R script for the live lab session.
# Full explanations, background, and tasks: Session1_Lab2_EDA.qmd
#
# Navigation: Ctrl+Shift+O (RStudio Document Outline) to jump between sections.
# =============================================================================


# Setup ----
library(tidyverse)     # data wrangling & ggplot2
library(skimr)         # skim()
library(gt)            # formatted tables
library(GGally)        # ggpairs()
library(patchwork)     # combine plots side by side
library(moments)       # skewness(), kurtosis()
library(datasauRus)    # Datasaurus Dozen dataset
library(janitor)       # tabyl(), adorn_*()
library(DataExplorer)  # plot_intro()
library(psych)         # describe() with skew & kurtosis
library(corrr)         # correlate(), shave(), focus(), fashion()

library(ISLR2)
tbl_carseats <- tibble(Carseats)   # convert to tibble for tidyverse compatibility


# Icebreaker: Never Trust a Statistic You Haven't Plotted ----

# Summary statistics look virtually identical across all 13 datasets
datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(
    n      = n(),
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x   = sd(x),
    sd_y   = sd(y),
    cor_xy = cor(x, y)
  )

# The plots are completely different
datasaurus_dozen %>%
  ggplot(aes(x = x, y = y, color = dataset)) +
  geom_point(alpha = 0.8, size = 1.0) +
  facet_wrap(~dataset, ncol = 4) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "none",
        strip.text       = element_text(face = "bold", size = 9),
        panel.grid.minor = element_blank()) +
  labs(title    = "The Datasaurus Dozen",
       subtitle = "Same summary statistics --- completely different structures.",
       x = "x", y = "y")


# Step 1: Getting to Know the Dataset ----

## 1.1 Structural overview ----
glimpse(tbl_carseats)   # rows, columns, data types, first values

## 1.2 Summary with skim() ----
tbl_carseats %>% skim()   # type-split: missings, quantiles, sparkline histograms

## 1.3 Frequency tables for categorical variables ----

# Count per ShelveLoc category
tbl_carseats %>% count(ShelveLoc, sort = TRUE)

# With proportions
tbl_carseats %>%
  tabyl(ShelveLoc) %>%
  adorn_pct_formatting(digits = 1)

# Cross-tabulation Urban x US (counts and proportions)
tbl_carseats %>%
  tabyl(Urban, US) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

## 1.4 Extended descriptives: skewness & kurtosis ----
tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, Population) %>%
  psych::describe() %>%
  as_tibble(rownames = "variable") %>%
  select(variable, n, mean, sd, median, min, max, skew, kurtosis)

## 1.5 Data quality overview ----
plot_intro(tbl_carseats)   # types, missings, complete rows --- at a glance

# A1: Explore the Dataset ----
# Using the outputs above, answer in the .qmd:
# 1. How many stores and variables?
# 2. What is the range of Sales? What does one unit represent?
# 3. What proportion of stores have "Good" shelf placement?
# 4. Which numeric variable has the highest skewness?
# 5. Are there any missing values?


# Step 2: Univariate Distributions ----

## 2.1 Non-Metric Variables: Bar Charts ----

# First look: default (alphabetical) level order --- spot the problem!
p_shelf <- tbl_carseats %>%
  ggplot(aes(x = ShelveLoc, fill = ShelveLoc)) +
  geom_bar() +                                          # height = frequency
  scale_fill_manual(values = c("Bad"    = "#D50006",   # --- colour ---
                               "Medium" = "#686868",
                               "Good"   = "#502479")) +
  labs(title = "ShelveLoc (default order)", x = NULL, y = "Count") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

p_urban <- tbl_carseats %>%
  ggplot(aes(y = Urban, fill = Urban)) +
  geom_bar() +                                          # horizontal bar
  scale_fill_manual(values = c("Yes" = "#502479", "No" = "#686868")) +
  labs(title = "Urban", x = "Count", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

p_us <- tbl_carseats %>%
  ggplot(aes(y = US, fill = US)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "#502479", "No" = "#686868")) +
  labs(title = "US", x = "Count", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

p_shelf + p_urban + p_us

# Diagnose the ShelveLoc ordering problem
class(tbl_carseats$ShelveLoc)
levels(tbl_carseats$ShelveLoc)

# Fix: recode to natural ordinal order (Bad < Medium < Good)
tbl_carseats <- tbl_carseats %>%
  mutate(ShelveLoc = factor(ShelveLoc, levels = c("Bad", "Medium", "Good")))

# Convenience alias + shared colour palette (reused throughout Steps 4 and 5)
tbl_cs    <- tbl_carseats
col_shelf <- c("Bad" = "#D50006", "Medium" = "#686868", "Good" = "#502479")

# Corrected bar chart
tbl_carseats %>%
  ggplot(aes(x = ShelveLoc, fill = ShelveLoc)) +
  geom_bar() +                                          # height = frequency
  scale_fill_manual(values = col_shelf) +               # --- colour ---
  labs(title = "ShelveLoc (corrected: Bad < Medium < Good)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

## 2.2 Metric Variables: Histogram and Kernel Density ----

# A2a: First Impression ----
# Look at the histogram before reading on.
# Is Sales symmetric, right-skewed, or left-skewed?
# Does the log transformation improve distributional shape?

# Pre-compute summary values for plot labels
skew_sales <- tbl_carseats %>%
  summarise(skew = round(skewness(Sales), 3)) %>% pull(skew)
mean_sales  <- tbl_carseats %>% summarise(m = round(mean(Sales),   2)) %>% pull(m)
med_sales   <- tbl_carseats %>% summarise(m = round(median(Sales), 2)) %>% pull(m)

# Histogram of raw Sales
tbl_carseats %>%
  ggplot(aes(x = Sales)) +
  geom_histogram(bins = 30, fill = "#D50006",              # bar fill
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean_sales,                      # mean line
             color = "#502479", linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = med_sales,                       # median line
             color = "#502479", linewidth = 1.2, linetype = "dotted") +
  annotate("text", x = mean_sales + 0.4, y = 37,          # mean label
           label = "Mean",   color = "#502479", size = 3.2, hjust = 0) +
  annotate("text", x = med_sales  - 0.4, y = 32,          # median label
           label = "Median", color = "#502479", size = 3.2, hjust = 1) +
  labs(title    = "Sales: raw distribution",
       subtitle = paste0("Skewness = ", skew_sales,
                         "  |  Mean = ", mean_sales,
                         "  |  Median = ", med_sales),
       x = "Sales (thousands of units)", y = "Count") +
  theme_minimal(base_size = 11)

# Kernel density of raw Sales
tbl_carseats %>%
  ggplot(aes(x = Sales)) +
  geom_density(color = "#502479", fill = "#502479",        # kernel density curve
               linewidth = 1.1, alpha = 0.5) +
  geom_vline(xintercept = mean_sales,                      # mean line
             color = "#686868", linewidth = 0.9, linetype = "dashed") +
  labs(title    = "Sales: kernel density estimate",
       subtitle = "Purple area = kernel density (bandwidth by Silverman's rule)",
       x = "Sales (thousands of units)", y = "Density") +
  theme_minimal(base_size = 11)

# log(Sales): histogram + KDE overlay
stats_log <- tbl_carseats %>%
  filter(Sales > 0) %>%
  mutate(log_sales = log(Sales)) %>%
  summarise(
    skew   = round(skewness(log_sales), 3),
    mean   = mean(log_sales),
    median = median(log_sales),
    n      = n()
  )

tbl_carseats %>%
  filter(Sales > 0) %>%
  mutate(log_sales = log(Sales)) %>%
  ggplot(aes(x = log_sales)) +
  geom_histogram(aes(y = after_stat(density)),              # rescale to density
                 bins = 30, fill = "#686868", color = "white", alpha = 0.6) +
  geom_density(color = "#502479", linewidth = 1.1) +
  geom_vline(xintercept = stats_log$mean,
             color = "#D50006", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = stats_log$median,
             color = "#502479", linetype = "dotted", linewidth = 1) +
  annotate("text", x = stats_log$mean   - 0.05, y = 0.45,
           label = "Mean",   color = "#D50006", angle = 90, vjust = 1) +
  annotate("text", x = stats_log$median + 0.05, y = 0.45,
           label = "Median", color = "#502479", angle = 90, vjust = 0) +
  labs(title    = "log(Sales): histogram + kernel density",
       subtitle = paste0("Skewness = ", stats_log$skew,
                         "  |  Mean = ",   round(stats_log$mean,   2),
                         "  |  Median = ", round(stats_log$median, 2),
                         "  |  zeros excluded (n = ", stats_log$n, ")"),
       x = "log(Sales)", y = "Density") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(color = "#686868"))

## 2.3 Target Variable: Advertising (A2b) ----

# Part 1: raw skewness
tbl_carseats %>%
  summarise(skew_adv = skewness(Advertising))

# Part 2: naive log -> NaN problem
tbl_carseats %>%
  mutate(log_adv = log(Advertising)) %>%
  summarise(skew = skewness(log_adv))

# Diagnose the NaN
tbl_carseats %>% filter(Advertising == 0) %>% nrow()   # any zeros?
log(0)                                                   # what does log(0) return?
skewness(c(1, 2, 3, -Inf))                              # how does skewness() handle -Inf?

# Part 3: fix --- two approaches
# Option A: filter zeros, then log
tbl_carseats %>%
  filter(Advertising > 0) %>%
  mutate(log_adv = log(Advertising)) %>%
  summarise(skew = skewness(log_adv))

# Option B: log1p --- no filtering needed
tbl_carseats %>%
  mutate(log_adv = log1p(Advertising)) %>%
  summarise(skew = skewness(log_adv))


# Step 3: Group-Level Descriptives ----

## 3.1 Sales by ShelveLoc ----
tbl_carseats %>%
  group_by(ShelveLoc) %>%          # split by shelf location
  summarise(
    n            = n(),
    mean_sales   = round(mean(Sales),   2),
    median_sales = round(median(Sales), 2),
    sd_sales     = round(sd(Sales),     2),
    min_sales    = round(min(Sales),    2),
    max_sales    = round(max(Sales),    2)
  ) %>%
  arrange(desc(mean_sales))        # sort: highest sales first

## 3.2 Sales by Urban x US ----
tbl_carseats %>%
  group_by(Urban, US) %>%          # split by two categorical variables
  summarise(
    n          = n(),
    mean_sales = round(mean(Sales), 2),
    sd_sales   = round(sd(Sales),   2),
    .groups = "drop"
  )

## 3.3 Price by ShelveLoc ----
tbl_carseats %>%
  group_by(ShelveLoc) %>%
  summarise(
    mean_price = round(mean(Price), 2),
    sd_price   = round(sd(Price),   2),
    n          = n()
  )

# A3: Group Comparisons ----
# 1. Difference in average sales Good vs. Bad?
# 2. Do better shelves command higher prices? (foreshadows OVB in Lab 3)
# 3. Extend: mean Sales and Advertising for Urban x US:
tbl_carseats %>%
  group_by(Urban, US) %>%
  summarise(
    mean_sales = mean(Sales),
    mean_adv   = mean(Advertising),
    n          = n(),
    .groups = "drop"
  )


# Step 4: Bivariate Relationships ----

## 4.1 Categorical: geom_count and heatmap ----

# geom_count: bubble area proportional to joint frequency
p_count <- tbl_cs %>%
  ggplot(aes(x = Urban, y = ShelveLoc)) +
  geom_count(color = "#502479") +             # bubble area ~ count
  scale_size_area(max_size = 12) +
  labs(title    = "geom_count(): Urban x ShelveLoc",
       subtitle = "Bubble area = joint frequency",
       size = "Count") +
  theme_minimal(base_size = 10)

# Heatmap: tile shade proportional to joint proportion
p_heat <- tbl_cs %>%
  count(Urban, ShelveLoc) %>%                 # joint frequencies
  mutate(f = n / sum(n)) %>%                  # -> proportions
  ggplot(aes(x = Urban, y = ShelveLoc, fill = f)) +
  geom_tile(color = "white", linewidth = 0.5) +  # tile colour ~ proportion
  scale_fill_gradient(low = "white", high = "#502479",
                      labels = scales::percent) +
  labs(title    = "Heatmap: f(Urban, ShelveLoc)",
       subtitle = "Tile shade = joint proportion",
       fill = "f(x,y)") +
  theme_minimal(base_size = 10)

p_count + p_heat

## 4.2 Categorical: conditional distribution plots ----

# Panel 1: joint distribution f(ShelveLoc, Urban) as heatmap
p_joint <- tbl_carseats %>%
  count(ShelveLoc, Urban) %>%                      # joint frequencies
  mutate(f = n / sum(n)) %>%                       # joint proportions
  ggplot(aes(x = Urban, y = ShelveLoc, fill = f)) +
  geom_tile(color = "white", linewidth = 0.5) +    # tile shade ~ joint proportion
  scale_fill_gradient(low = "white", high = "#502479",
                      labels = scales::percent) +
  labs(title    = "f(ShelveLoc, Urban)",
       subtitle = "Tile shade = joint proportion",
       fill = "f(x,y)") +
  theme_minimal(base_size = 9)

# Panel 2: conditional f(ShelveLoc | Urban)
# For each Urban value: how is ShelveLoc distributed?
p_cond1 <- tbl_carseats %>%
  count(Urban, ShelveLoc) %>%                      # joint frequencies
  group_by(Urban) %>%                              # condition on Urban
  mutate(f_cond = n / sum(n)) %>%                  # conditional proportions
  ggplot(aes(x = Urban, y = f_cond, fill = ShelveLoc)) +
  geom_col(color = "white", linewidth = 0.3) +     # stacked bar = conditional dist.
  scale_fill_manual(values = col_shelf) +          # --- colour ---
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "f(ShelveLoc | Urban)",
       subtitle = "Each bar sums to 100%",
       y = NULL, x = "Urban") +
  theme_minimal(base_size = 9) + theme(legend.position = "none")

# Panel 3: conditional f(Urban | ShelveLoc)
# For each ShelveLoc value: how is Urban distributed?
p_cond2 <- tbl_carseats %>%
  count(ShelveLoc, Urban) %>%                      # joint frequencies
  group_by(ShelveLoc) %>%                          # condition on ShelveLoc
  mutate(f_cond = n / sum(n)) %>%                  # conditional proportions
  ggplot(aes(x = ShelveLoc, y = f_cond, fill = Urban)) +
  geom_col(color = "white", linewidth = 0.3) +     # stacked bar = conditional dist.
  scale_fill_manual(values = c("Yes" = "#502479", "No" = "#686868")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "f(Urban | ShelveLoc)",
       subtitle = "Each bar sums to 100%",
       y = NULL, x = "ShelveLoc") +
  theme_minimal(base_size = 9) + theme(legend.position = "none")

p_joint + p_cond1 + p_cond2

## 4.3 Metric: Price vs. Sales scatter ----
tbl_carseats %>%
  ggplot(aes(x = Price, y = Sales)) +
  geom_point(alpha = 0.4, color = "#7d0a52", size = 1.8) +       # raw observations
  geom_smooth(method = "lm",    color = "#D50006",                # OLS line
              se = FALSE, linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#502479",                # LOESS (linearity check)
              se = FALSE, linewidth = 1, linetype = "dashed") +
  labs(title    = "Price vs. Sales",
       subtitle = paste0(
         "Correlation r = ",
         tbl_carseats %>% summarise(r = round(cor(Price, Sales), 3)) %>% pull(r),
         "  |  Red = OLS line  |  Purple dashed = LOESS"
       ),
       x = "Price (USD)", y = "Sales (thousands of units)") +
  theme_minimal(base_size = 11)

## 4.4 Pearson correlation matrix ----

# Compute (returns a matrix, not a tibble)
cor_mat <- tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, Population) %>%
  cor() %>%          # Pearson correlation matrix -> returns a matrix, not a tibble
  round(3)

cor_mat

# Ranked by association with Sales
cor_mat %>%
  as_tibble(rownames = "variable") %>%
  select(variable, Sales) %>%
  filter(variable != "Sales") %>%          # remove self-correlation
  arrange(desc(abs(Sales)))                # sort by absolute value

## 4.5 corrr package: tidy correlation analysis ----

# cor() returns a matrix --- this breaks the pipe workflow
cor_mat %>% class()     # "matrix" "array" --- not a tibble!

# correlate() returns a tibble directly
tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, Population) %>%
  corrr::correlate() %>%                   # Pearson by default, result is a tibble
  corrr::shave() %>%                       # remove upper triangle (redundant)
  corrr::fashion()                         # replace NA with blank, round to 2 dp

# focus(): correlations with Sales only
tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, Population) %>%
  corrr::correlate() %>%
  corrr::focus(Sales) %>%                  # one column: correlations with Sales only
  arrange(desc(abs(Sales)))                # sort by absolute value

# A4: Bivariate Analysis ----
# 1. Strongest / weakest linear correlation with Sales?
# 2. Strongly correlated predictor pairs -> multicollinearity risk?
# 3. Look at ggpairs below: does the Price-Sales slope differ by ShelveLoc?

## 4.6 Scatterplot matrix: putting it all together ----
# Diagonal = KDE (Step 2.2) | Lower = scatter (4.3) | Upper = Pearson r (4.4)
tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, ShelveLoc) %>%
  ggpairs(
    aes(color = ShelveLoc, alpha = 0.4),
    upper = list(continuous = wrap("cor", size = 2.8)),            # correlations
    lower = list(continuous = wrap("points",                       # scatterplots
                                   alpha = 0.25, size = 0.7)),
    diag  = list(continuous = wrap("densityDiag", alpha = 0.5))   # densities
  ) +
  scale_color_manual(values = col_shelf) +
  scale_fill_manual( values = col_shelf) +
  theme_minimal(base_size = 8) +
  labs(title    = "Scatterplot matrix: Carseats variables",
       subtitle = "Coloured by shelf location (ShelveLoc)")


# Step 5: Boxplots and Conditional Distributions ----

## 5.1 Sales and Price by ShelveLoc ----

# Core plots (no colours/labels yet --- Option 2 teaching split)
p1 <- tbl_cs %>%
  ggplot(aes(x = ShelveLoc, y = Sales, fill = ShelveLoc)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +              # distribution per group
  stat_summary(fun = mean, geom = "point",                      # add mean as diamond
               shape = 23, size = 3, fill = "white")

p2 <- tbl_cs %>%
  ggplot(aes(x = ShelveLoc, y = Price, fill = ShelveLoc)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white")

p1 + p2   # side by side (patchwork)

# With colours, labels, and theme
p1 <- tbl_cs %>%
  ggplot(aes(x = ShelveLoc, y = Sales, fill = ShelveLoc)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = col_shelf) +      # --- colour ---
  labs(title    = "Sales by shelf location",   # --- labels ---
       subtitle = "Diamond = mean | line = median",
       x = NULL, y = "Sales (thousands)") +
  theme_minimal(base_size = 10) +              # --- theme ---
  theme(legend.position = "none")

p2 <- tbl_cs %>%
  ggplot(aes(x = ShelveLoc, y = Price, fill = ShelveLoc)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = col_shelf) +      # --- colour ---
  labs(title    = "Price by shelf location",   # --- labels ---
       subtitle = "Do premium shelves command higher prices?",
       x = NULL, y = "Price (USD)") +
  theme_minimal(base_size = 10) +              # --- theme ---
  theme(legend.position = "none")

p1 + p2

## 5.2 Advertising vs. Sales by ShelveLoc ----
tbl_cs %>%
  ggplot(aes(x = Advertising, y = Sales, color = ShelveLoc)) +
  geom_point(alpha = 0.35, size = 1.4) +                        # raw observations
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +     # OLS trend per group
  scale_color_manual(values = col_shelf) +                       # --- colour ---
  labs(title    = "Advertising vs. Sales by shelf location",
       subtitle = paste0("Overall r = ",
                         tbl_cs %>%
                           summarise(r = round(cor(Advertising, Sales), 3)) %>%
                           pull(r)),
       x = "Advertising (thousands USD)", y = "Sales (thousands)",
       color = "ShelveLoc") +
  theme_minimal(base_size = 10) +                                # --- theme ---
  theme(legend.position = "bottom")

## 5.3 Violin plot (alternative to boxplot) ----
tbl_cs %>%
  ggplot(aes(x = ShelveLoc, y = Sales, fill = ShelveLoc)) +
  geom_violin(alpha = 0.6, trim = FALSE) +         # mirrored density per group
  geom_boxplot(width = 0.15, fill = "white",        # narrow boxplot overlay
               outlier.alpha = 0.5) +
  scale_fill_manual(values = col_shelf) +
  labs(title = "Sales by shelf location --- violin plot",
       x = NULL, y = "Sales (thousands)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")


# Step 6: Formulating Hypotheses (A5) ----
# Formulate three hypotheses in the structure:
# "I expect [X] to have a [positive/negative] association with Sales because
#  [reasoning]. This is visible in [specific EDA finding: table/plot]."
#
# Additionally:
# 1. ShelveLoc is associated with both Price and Sales. What does this imply
#    for a simple regression of Sales ~ Price? (-> OVB, Lab 3)
# 2. Which variables to include in the regression? Which are redundant?
# 3. Which variables are missing from the dataset? (-> Data Drought)
# 4. Induction trap: what would you need to do before this exploration
#    to run valid statistical tests on these hypotheses?


# Bonus B1: Log Transformation for Sales ----

# Step 1: raw skewness
tbl_carseats %>%
  summarise(skew_sales = skewness(Sales))

# Step 2: naive log -> NaN problem
tbl_carseats %>%
  mutate(log_sales = log(Sales)) %>%
  summarise(skew = skewness(log_sales))

# Step 3: fix with filter()
tbl_carseats %>%
  filter(Sales > 0) %>%
  mutate(log_sales = log(Sales)) %>%
  summarise(skew = skewness(log_sales))

# Step 4: compare with log1p()
tbl_carseats %>%
  mutate(log_sales = log1p(Sales)) %>%
  summarise(skew = skewness(log_sales))

# Step 5: inspect zero-sales stores
tbl_carseats %>%
  filter(Sales == 0) %>%
  select(Sales, Price, Advertising, ShelveLoc, Urban, US, Income, Age)


# Bonus B2: Beyond Pearson --- Spearman, Kendall, Cramer's V ----

# Part 1: Spearman vs. Pearson
tbl_carseats %>%
  summarise(
    pearson  = cor(Price, Sales, method = "pearson"),
    spearman = cor(Price, Sales, method = "spearman")
  )

tbl_carseats %>%
  summarise(
    pearson  = cor(Advertising, Sales, method = "pearson"),
    spearman = cor(Advertising, Sales, method = "spearman")
  )

# Part 2: Spearman matrix via corrr
tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, Population) %>%
  corrr::correlate(method = "spearman") %>%   # switch method
  corrr::shave() %>%
  corrr::fashion()

# Part 3: Kendall's tau
tbl_carseats %>%
  summarise(
    spearman = cor(Price, Sales, method = "spearman"),
    kendall  = cor(Price, Sales, method = "kendall")
  )

# Part 4: Cramer's V for nominal variables
# install.packages("rcompanion")   # if not yet installed
library(rcompanion)

tbl_carseats %>%
  select(Urban, US, ShelveLoc) %>%
  corrr::colpair_map(rcompanion::cramerV) %>%
  corrr::shave() %>%
  corrr::fashion()

# Part 5: Network plot (visual correlation structure)
tbl_carseats %>%
  select(Sales, Price, Advertising, Income, Age, Population) %>%
  corrr::correlate() %>%
  corrr::network_plot(min_cor = 0.15,
                      colors = c("#D50006", "white", "#502479"))


# Bonus B3: Automated EDA Report ----
# install.packages("DataExplorer")   # if needed

create_report(tbl_carseats,
              output_file  = "EDA_Carseats_report.html",
              output_dir   = ".",
              report_title = "Carseats EDA Report")


# Bonus B4: Missing Data Visualisation ----
# install.packages(c("visdat", "naniar"))   # if needed
library(visdat)
library(naniar)

vis_dat(airquality)             # data types and missing values as heatmap
miss_var_summary(airquality)    # missing count and % per variable
gg_miss_upset(airquality)       # co-occurrence pattern of missings


# Bonus B5: Segmented EDA ----
tbl_carseats %>%
  ggplot(aes(x = Price, y = Sales, color = Urban)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +   # OLS per Urban group
  facet_wrap(~US) +                                            # separate panels for US/non-US
  scale_color_manual(values = c("Yes" = "#502479", "No" = "#D50006")) +
  labs(title = "Price vs. Sales by location (Urban) and region (US)",
       x = "Price (USD)", y = "Sales (thousands)") +
  theme_minimal()
