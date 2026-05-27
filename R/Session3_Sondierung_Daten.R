# =============================================================================
# Dominick's OJ — Daten-Sondage Teil 1: Überblick
# =============================================================================
# Zweck:
#   - Datenstruktur des bayesm::orangeJuice-Datensatzes verstehen
#   - Variablen, Skalentypen, Wertebereiche dokumentieren
#   - Anzahl Wochen pro Store ermitteln (für Single-Store-Wahl)
#
# Voraussetzung: bayesm bereits installiert
# Ausführen: Skript komplett laufen lassen oder zeilenweise
# =============================================================================

library(bayesm)
library(tidyverse)

# -----------------------------------------------------------------------------
# 1. DATENSTRUKTUR — was steckt in orangeJuice?
# -----------------------------------------------------------------------------
data(orangeJuice)
str(orangeJuice, max.level = 1)
# Erwartung: Liste mit zwei Elementen — yx (Wochen-Daten) und storedemo

# -----------------------------------------------------------------------------
# 2. yx-Tabelle — die Wochen-Beobachtungen
# -----------------------------------------------------------------------------
tbl_yx <- as_tibble(orangeJuice$yx)

cat("\n--- yx (Wochen-Daten) ---\n")
cat("Zeilen:", nrow(tbl_yx), "\n")
cat("Spalten:", ncol(tbl_yx), "\n\n")

cat("Variablen mit Skalentyp und Wertebereich:\n")
tbl_yx %>%
  summarise(across(everything(),
                   list(class = ~class(.)[1],
                        min   = ~ifelse(is.numeric(.), min(., na.rm = TRUE), NA),
                        max   = ~ifelse(is.numeric(.), max(., na.rm = TRUE), NA),
                        n_unique = ~length(unique(.))))) %>%
  pivot_longer(everything(),
               names_to = c("var", ".value"),
               names_pattern = "(.+)_(class|min|max|n_unique)") %>%
  print(n = Inf)

# -----------------------------------------------------------------------------
# 3. storedemo-Tabelle — die Demografie pro Store
# -----------------------------------------------------------------------------
tbl_demo <- as_tibble(orangeJuice$storedemo)

cat("\n--- storedemo (Demografie pro Store) ---\n")
cat("Zeilen (Stores):", nrow(tbl_demo), "\n")
cat("Spalten:", ncol(tbl_demo), "\n\n")

cat("Demografie-Variablen mit Wertebereich:\n")
tbl_demo %>%
  summarise(across(everything(),
                   list(class = ~class(.)[1],
                        min   = ~ifelse(is.numeric(.), round(min(., na.rm = TRUE), 3), NA),
                        max   = ~ifelse(is.numeric(.), round(max(., na.rm = TRUE), 3), NA),
                        median = ~ifelse(is.numeric(.), round(median(., na.rm = TRUE), 3), NA)))) %>%
  pivot_longer(everything(),
               names_to = c("var", ".value"),
               names_pattern = "(.+)_(class|min|max|median)") %>%
  print(n = Inf)

# -----------------------------------------------------------------------------
# 4. Demografie-Korrelationen — was ist korreliert mit was?
# -----------------------------------------------------------------------------
cat("\n--- Korrelationsmatrix der Demografie-Variablen ---\n")
tbl_demo %>%
  select(where(is.numeric)) %>%
  select(-any_of("STORE")) %>%
  cor() %>%
  round(2) %>%
  print()

# -----------------------------------------------------------------------------
# 5. Wochen pro Store — wie viele Beobachtungen pro Filiale?
# -----------------------------------------------------------------------------
cat("\n--- Wochen-Coverage pro Store ---\n")
weeks_per_store <- tbl_yx %>%
  group_by(store) %>%
  summarise(n_weeks_total = n() / 3,  # 3 Marken pro Woche
            n_weeks_unique = n_distinct(week),
            week_min = min(week),
            week_max = max(week)) %>%
  arrange(desc(n_weeks_unique))

# Verteilung der Coverage
weeks_per_store %>% summary() %>% print()

# Top-10 Stores mit größter Coverage
cat("\nTop 10 Stores mit größter Wochen-Abdeckung:\n")
weeks_per_store %>% slice_head(n = 10) %>% print()

# -----------------------------------------------------------------------------
# 6. Marken-Verteilung — sind alle drei Marken in jedem Store präsent?
# -----------------------------------------------------------------------------
cat("\n--- Marken-Coverage pro Store ---\n")
brand_coverage <- tbl_yx %>%
  count(store, brand) %>%
  pivot_wider(names_from = brand, values_from = n, names_prefix = "brand_")

cat("Verteilung der Beobachtungen nach Marke (über alle Stores):\n")
brand_coverage %>% select(starts_with("brand_")) %>% summary() %>% print()

# -----------------------------------------------------------------------------
# 7. Speichere die Sondage-Ergebnisse
# -----------------------------------------------------------------------------
tbl_yx %>%
  write_rds(xfun::from_root("data","raw","dominicks.rds"))

tbl_demo %>%
  write_rds(xfun::from_root("data","raw","Sondage","sondage_demo.rds"))

weeks_per_store %>%
  write_rds(xfun::from_root("data","raw","Sondage","sondage_weeks_per_store.rds"))

cat("\n=== Sondage Teil 1 abgeschlossen ===\n")
cat("Ergebnisse in: sondage_yx.rds, sondage_demo.rds, sondage_weeks_per_store.rds\n")
cat("Nächster Schritt: sondage_store_selection.R\n")



# =============================================================================
# Dominick's OJ — Daten-Sondage Teil 2 (KORRIGIERT)
# =============================================================================
# Korrektur: yx hat KEINE Spalte 'price', sondern price1..price11.
# Der korrekte Preis für die fokale Marke ist price[brand], also brand-abhängig.
# =============================================================================

library(tidyverse)

tbl_yx <- read_rds(xfun::from_root("data","raw","Sondage","sondage_yx.rds"))
tbl_demo        <- read_rds(xfun::from_root("data","raw","Sondage","sondage_demo.rds"))
weeks_per_store <- read_rds(xfun::from_root("data","raw","Sondage","sondage_weeks_per_store.rds"))

# -----------------------------------------------------------------------------
# 0. STRUKTUR-CHECK: ist priceK der Preis der fokalen Marke?
# -----------------------------------------------------------------------------
# Wenn ja, dann ist für jede Zeile mit brand=k der "eigene Preis" price_k.
# Wir prüfen das durch eine Plausibilitätsprüfung: der Preis der fokalen Marke
# müsste mit logmove negativ korrelieren (Nachfragegesetz).
# -----------------------------------------------------------------------------

# Extrahiere für jede Beobachtung den Preis der fokalen Marke
tbl_yx_with_own_price <- tbl_yx %>%
  rowwise() %>%
  mutate(price_focal = c_across(starts_with("price"))[brand]) %>%
  ungroup() %>%
  select(store, brand, week, logmove, price_focal, feat, deal, profit)

# Korrelation logmove ~ log(price_focal) je Marke
cat("Korrelation log(price_focal) und logmove je Marke (sollte negativ sein):\n")
tbl_yx_with_own_price %>%
  group_by(brand) %>%
  summarise(
    n         = n(),
    mean_p    = round(mean(price_focal), 4),
    cor_logp_logmove = round(cor(log(price_focal), logmove), 3),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# Wenn alle cor < 0: priceK ist tatsächlich der Preis von Marke K. Plausibel.

# -----------------------------------------------------------------------------
# 1. KANDIDATEN-AUSWAHL — wir nehmen Store 51 (aus dem ersten Skript)
# -----------------------------------------------------------------------------
chosen_store <- 51

# Welche 3 Marken sind die "großen"? Diejenigen mit den höchsten Sales-Volumina
brand_volume <- tbl_yx_with_own_price %>%
  group_by(brand) %>%
  summarise(total_logmove = sum(logmove),
            mean_logmove  = mean(logmove),
            mean_price    = mean(price_focal),
            .groups = "drop") %>%
  arrange(desc(total_logmove))

cat("\nBrand-Ranking nach Gesamt-logmove (= log(units * size)):\n")
brand_volume %>% print()

# Vermutung: brand 1, 2, 3 sind Tropicana / Minute Maid / Dominick's
# (Reihenfolge der Brand-Codes in bayesm: 1=Tropicana, 2=Minute Maid, 3=Citrus Hill,
#  4=Florida's Natural, ... — ABER: das muss verifiziert werden)
# Wir nehmen die Top 3 nach Volumen:
top3_brands <- brand_volume %>% slice_head(n = 3) %>% pull(brand)
cat("\nTop 3 Marken nach Volumen:", top3_brands, "\n")

# -----------------------------------------------------------------------------
# 2. STORE 51, TOP 3 MARKEN — die Lehr-Welt
# -----------------------------------------------------------------------------
tbl_store_top3 <- tbl_yx_with_own_price %>%
  filter(store == chosen_store, brand %in% top3_brands) %>%
  arrange(week, brand)

cat("\nBeobachtungen für Store", chosen_store, "× Top 3 Marken:", nrow(tbl_store_top3), "\n")
cat("Verteilung über Marken:\n")
tbl_store_top3 %>% count(brand) %>% print()

# Deskriptive Statistiken
cat("\nDeskriptive Statistik je Marke:\n")
tbl_store_top3 %>%
  group_by(brand) %>%
  summarise(
    n           = n(),
    mean_logmove = round(mean(logmove), 2),
    sd_logmove   = round(sd(logmove), 2),
    mean_price   = round(mean(price_focal), 4),
    feat_share   = round(mean(feat), 2),
    .groups = "drop"
  ) %>%
  print()

# -----------------------------------------------------------------------------
# 3. AUTOKORRELATION — pro Marke
# -----------------------------------------------------------------------------
acf_results <- tbl_store_top3 %>%
  group_by(brand) %>%
  arrange(brand, week) %>%
  group_split() %>%
  map(function(df) {
    mod <- lm(logmove ~ log(price_focal) + feat, data = df)
    res <- resid(mod)
    a   <- acf(res, plot = FALSE, lag.max = 10)
    tibble(
      brand = df$brand[1],
      lag   = a$lag[, , 1],
      acf   = a$acf[, , 1]
    )
  }) %>%
  bind_rows()

cat("\n--- Autokorrelation der Residuen je Marke ---\n")
acf_results %>%
  pivot_wider(names_from = brand, values_from = acf, names_prefix = "brand_") %>%
  print(n = Inf)

n_per_brand <- nrow(tbl_store_top3) / 3
crit_value  <- 1.96 / sqrt(n_per_brand)
cat("\nKritischer ACF-Wert (n =", n_per_brand, "):", round(crit_value, 3), "\n")

sig_lags <- acf_results %>%
  filter(lag > 0) %>%
  group_by(brand) %>%
  summarise(
    n_sig_lags   = sum(abs(acf) > crit_value),
    max_acf_abs  = round(max(abs(acf)), 3),
    .groups = "drop"
  )

cat("\nSignifikante Autokorrelations-Lags pro Marke:\n")
sig_lags %>% print()

# -----------------------------------------------------------------------------
# 4. AMPEL
# -----------------------------------------------------------------------------
cat("\n=== INTERPRETATION ===\n")
max_lags <- max(sig_lags$n_sig_lags)
if (max_lags <= 1) {
  cat("GRÜNES LICHT — wenig Autokorrelation (max", max_lags, "signifikante Lags).\n")
  cat("Klassische Inferenz und naiver Bootstrap unproblematisch.\n")
} else if (max_lags <= 3) {
  cat("GELBES LICHT — moderate Autokorrelation (max", max_lags, "Lags).\n")
  cat("Permutationstest didaktisch vorzuziehen.\n")
} else {
  cat("ROTES LICHT — starke Autokorrelation (max", max_lags, "Lags).\n")
  cat("Block-Bootstrap oder Aggregation auf Monate erwägen.\n")
}

# -----------------------------------------------------------------------------
# 5. BONUS: Demografie-Variablen — die EDUC-INCOME-HVAL150-Multikollinearität
# -----------------------------------------------------------------------------
cat("\n--- Multikollinearitäts-Vorausschau für S3 ---\n")
cat("Stark korrelierte Demografie-Paare (|r| > 0.6):\n")
cor_demo <- tbl_demo %>%
  select(where(is.numeric)) %>%
  select(-any_of("STORE")) %>%
  cor()

high_cor <- which(abs(cor_demo) > 0.6 & abs(cor_demo) < 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  pairs <- data.frame(
    var1 = rownames(cor_demo)[high_cor[, 1]],
    var2 = colnames(cor_demo)[high_cor[, 2]],
    cor  = round(cor_demo[high_cor], 2)
  )
  pairs <- pairs[!duplicated(t(apply(pairs[, 1:2], 1, sort))), ]
  print(pairs)
}

# -----------------------------------------------------------------------------
# 6. SPEICHERN
# -----------------------------------------------------------------------------
tbl_store_top3 %>%
  write_rds(xfun::from_root("data","raw","Sondage", "sondage_chosen_store_top3.rds"))
acf_results %>%
  write_rds(xfun::from_root("data","raw","Sondage","sondage_acf.rds"))

sig_lags %>%
  write_rds(xfun::from_root("data","raw","Sondage","sondage_sig_lags.rds"))

brand_volume %>%
  write_rds(xfun::from_root("data","raw","Sondage","sondage_brand_volume.rds"))

cat("\n=== Sondage Teil 2 abgeschlossen ===\n")
cat("Gewählter Store: 51, Top 3 Marken:", top3_brands, "\n")


# =============================================================================
# Dominick's OJ — Daten-Sondage Teil 3: feat-Variable und Marken-Identität
# =============================================================================
# Zwei offene Fragen:
#   (1) Was bedeutet `feat` genau? — binär (Werbung ja/nein) oder kontinuierlich?
#   (2) Welche realen Marken sind brand=1, brand=5, brand=10?
# =============================================================================

library(bayesm)
library(tidyverse)

tbl_yx <- read_rds(xfun::from_root("data","raw","Sondage","sondage_yx.rds"))

# -----------------------------------------------------------------------------
# (1a) feat — Verteilung der Werte über alle Beobachtungen
# -----------------------------------------------------------------------------
cat("=== FEAT-VARIABLE: Wertverteilung ===\n\n")

cat("Histogramm-artige Übersicht (Anteile der Werte):\n")
tbl_yx %>%
  count(feat) %>%
  arrange(feat) %>%
  mutate(share = round(n / sum(n), 3)) %>%
  filter(share >= 0.001 | feat %in% c(0, 1)) %>%
  print(n = 30)

cat("\nAnteil exakt 0:", round(mean(tbl_yx$feat == 0), 3), "\n")
cat("Anteil exakt 1:", round(mean(tbl_yx$feat == 1), 3), "\n")
cat("Anteil dazwischen (0 < feat < 1):", round(mean(tbl_yx$feat > 0 & tbl_yx$feat < 1), 3), "\n")
cat("Anzahl unique values:", length(unique(tbl_yx$feat)), "\n")

# -----------------------------------------------------------------------------
# (1b) feat im gewählten Store + Top-3-Marken
# -----------------------------------------------------------------------------
cat("\n=== FEAT für Store 51, Brands 1/5/10 ===\n\n")

tbl_yx %>%
  filter(store == 51, brand %in% c(1, 5, 10)) %>%
  group_by(brand) %>%
  summarise(
    n          = n(),
    feat_zero  = mean(feat == 0),
    feat_one   = mean(feat == 1),
    feat_other = mean(feat > 0 & feat < 1),
    feat_mean  = round(mean(feat), 3),
    feat_unique = n_distinct(feat),
    .groups = "drop"
  ) %>%
  print()

# Wenn feat_zero + feat_one ≈ 1.0: praktisch binär
# Wenn feat_other > 0.1: substanziell kontinuierlich

# -----------------------------------------------------------------------------
# (1c) feat — Beziehung zu deal (vermutlich: feat=Werbeanzeige, deal=Preisaktion)
# -----------------------------------------------------------------------------
cat("\n=== feat × deal Kreuztabelle (Store 51, Brand 1) ===\n\n")
tbl_yx %>%
  filter(store == 51, brand == 1) %>%
  mutate(feat_class = case_when(
    feat == 0 ~ "feat=0",
    feat == 1 ~ "feat=1",
    TRUE      ~ "feat in (0,1)"
  )) %>%
  count(feat_class, deal) %>%
  pivot_wider(names_from = deal, values_from = n, names_prefix = "deal=", values_fill = 0) %>%
  print()

# -----------------------------------------------------------------------------
# (2) Marken-Identität — aus der bayesm-Hilfe
# -----------------------------------------------------------------------------
cat("\n=== MARKEN-IDENTITÄT laut bayesm-Dokumentation ===\n\n")

# Die Hilfe als Text extrahieren
help_text <- tryCatch({
  rd_db   <- tools::Rd_db("bayesm")
  oj_rd   <- rd_db[["orangeJuice.Rd"]]
  if (!is.null(oj_rd)) {
    tmp <- tempfile()
    tools::Rd2txt(oj_rd, out = tmp)
    paste(readLines(tmp), collapse = "\n")
  } else {
    NULL
  }
}, error = function(e) NULL)

if (!is.null(help_text)) {
  cat(help_text)
  cat("\n\n")
} else {
  cat("Hilfetext konnte nicht automatisch extrahiert werden.\n")
  cat("Bitte manuell ausführen: ?bayesm::orangeJuice\n\n")
}

# -----------------------------------------------------------------------------
# (2b) Plausibilitätscheck: typische OJ-Marken-Reihenfolge
# -----------------------------------------------------------------------------
# Bronnenberg/Hartmann/Tsay-Konvention im bayesm-Datensatz:
# Häufig dokumentiert ist die Reihenfolge:
#   1=Tropicana 64oz, 2=Minute Maid 64oz, 3=Citrus Hill 64oz,
#   4=Florida's Natural 64oz, 5=Dominick's 64oz,
#   6-11 weitere (kleinere Größen, Eigenmarken-Varianten)
#
# Das ist KONSISTENT mit unseren Befunden:
#   Brand 1 (höchster Preis ø 0.0462) = Tropicana (Premium)
#   Brand 5 (mittlerer Preis ø 0.0361) = Dominick's-Eigenmarke 64oz
#   Brand 10 (niedrigster Preis ø 0.0273) = vermutlich kleinere Eigenmarke
#
# Aber: das verifiziert nur die Plausibilität, nicht die exakte Zuordnung.
# Für Sicherheit: Bronnenberg/Hartmann/Tsay 2008 Marketing Science.

cat("Plausibilitätscheck — mittlere Preise je Brand:\n")
tbl_yx %>%
  group_by(brand) %>%
  summarise(mean_price = round(mean(price1[1:1]), 4), .groups = "drop")  # placeholder

# Bessere Variante: nutze price_focal aus Sondage 2
tbl_yx_with_own_price <- tbl_yx %>%
  rowwise() %>%
  mutate(price_focal = c_across(starts_with("price"))[brand]) %>%
  ungroup()

tbl_yx_with_own_price %>%
  group_by(brand) %>%
  summarise(
    mean_price_per_oz = round(mean(price_focal), 4),
    rank_by_price     = NA_integer_,
    .groups = "drop"
  ) %>%
  arrange(desc(mean_price_per_oz)) %>%
  mutate(rank_by_price = row_number()) %>%
  arrange(brand) %>%
  print()

cat("\n=== Sondage Teil 3 abgeschlossen ===\n")



# =============================================================================
# Dominick's OJ — Daten-Sondage Teil 4: Datenqualität, Anomalien, Kuration
# =============================================================================
# Zweck:
#   - Welche Variablen bieten Lehrmomente für Data Governance?
#   - Wo gibt es Messfehler, Ausreißer, Strukturbrüche?
#   - Welcher Store ist für die Kuratierungs-Lehre am ergiebigsten?
# =============================================================================

library(tidyverse)
tbl_yx <- read_rds(xfun::from_root("data","raw","Sondage","sondage_yx.rds"))

# -----------------------------------------------------------------------------
# (1) feat in (0,1) — wo treten die Übergangswerte auf?
# -----------------------------------------------------------------------------
cat("=== feat-Übergangswerte ===\n\n")

feat_anomalies <- tbl_yx %>%
  filter(feat > 0 & feat < 1) %>%
  count(store, brand, feat) %>%
  arrange(desc(n))

cat("Verteilung der feat-Übergangswerte über Stores und Marken:\n")
feat_anomalies %>% slice_head(n = 20) %>% print()

cat("\nWelche Stores haben am meisten feat-Anomalien?\n")
feat_anomalies %>%
  group_by(store) %>%
  summarise(n_anomalies = sum(n), .groups = "drop") %>%
  arrange(desc(n_anomalies)) %>%
  slice_head(n = 10) %>%
  print()

# Spezifisch: hat Store 51 (unsere Wahl) feat-Anomalien?
cat("\nfeat-Anomalien in Store 51:\n")
tbl_yx %>%
  filter(store == 51, feat > 0 & feat < 1) %>%
  select(store, brand, week, feat) %>%
  print(n = 20)

# -----------------------------------------------------------------------------
# (2) logmove-Ausreißer — sehr hohe oder sehr niedrige Sales
# -----------------------------------------------------------------------------
cat("\n=== logmove-Ausreißer ===\n\n")

logmove_stats <- tbl_yx %>%
  group_by(brand) %>%
  summarise(
    median_lm = median(logmove),
    iqr_lm    = IQR(logmove),
    .groups   = "drop"
  )

# Robuste Ausreißer-Definition: > median + 3*IQR oder < median - 3*IQR
outliers <- tbl_yx %>%
  left_join(logmove_stats, by = "brand") %>%
  mutate(z_robust = (logmove - median_lm) / iqr_lm) %>%
  filter(abs(z_robust) > 3)

cat("Anzahl robuste Ausreißer (>3*IQR vom Median, je Marke):\n")
outliers %>% count(brand) %>% print()

cat("\nExtremste Ausreißer (Top 10):\n")
outliers %>%
  arrange(desc(abs(z_robust))) %>%
  select(store, brand, week, logmove, z_robust) %>%
  slice_head(n = 10) %>%
  print()

# Stores mit den meisten Ausreißern
cat("\nStores mit den meisten Ausreißern:\n")
outliers %>%
  count(store, sort = TRUE) %>%
  slice_head(n = 10) %>%
  print()

# -----------------------------------------------------------------------------
# (3) Preis-Anomalien
# -----------------------------------------------------------------------------
cat("\n=== Preis-Anomalien ===\n\n")

# Sehr niedrige oder unrealistisch hohe Preise
tbl_yx_with_own_price <- tbl_yx %>%
  rowwise() %>%
  mutate(price_focal = c_across(starts_with("price"))[brand]) %>%
  ungroup() %>%
  select(store, brand, week, logmove, price_focal, feat, deal)

cat("Preis-Quantile je Marke:\n")
tbl_yx_with_own_price %>%
  group_by(brand) %>%
  summarise(
    p01 = round(quantile(price_focal, 0.01), 4),
    p50 = round(quantile(price_focal, 0.50), 4),
    p99 = round(quantile(price_focal, 0.99), 4),
    min_p = round(min(price_focal), 4),
    max_p = round(max(price_focal), 4),
    .groups = "drop"
  ) %>%
  print()

# Sehr niedrige Preise — könnten Promo-Aktionen oder Datenfehler sein
cat("\nNiedrigste 5 Preise je Marke (im Lehr-Subset):\n")
tbl_yx_with_own_price %>%
  filter(brand %in% c(1, 5, 10)) %>%
  group_by(brand) %>%
  arrange(price_focal) %>%
  slice_head(n = 5) %>%
  print(n = 20)

# -----------------------------------------------------------------------------
# (4) profit-Variable — was ist das eigentlich?
# -----------------------------------------------------------------------------
cat("\n=== profit-Variable: Verteilung ===\n\n")

cat("profit-Statistik insgesamt:\n")
tbl_yx %>%
  summarise(
    min    = round(min(profit), 2),
    p25    = round(quantile(profit, 0.25), 2),
    median = round(median(profit), 2),
    mean   = round(mean(profit), 2),
    p75    = round(quantile(profit, 0.75), 2),
    p99    = round(quantile(profit, 0.99), 2),
    max    = round(max(profit), 2),
    n_zero = sum(profit == 0)
  ) %>%
  print()

cat("\nprofit = 0 — wie viele Beobachtungen?\n")
cat("Anteil profit == 0:", round(mean(tbl_yx$profit == 0), 4), "\n")

# Wenn profit als Marge in % interpretiert wird, sind 0-Werte Verluste
# Wenn als absoluter $-Profit, sind sie Cost-Recovery
# Die Variable braucht semantische Klärung — perfekter Lehrpunkt!

# -----------------------------------------------------------------------------
# (5) Wochen-Lücken — strukturelle Fehlstellen?
# -----------------------------------------------------------------------------
cat("\n=== Wochen-Lücken in Store 51 ===\n\n")

weeks_in_store51 <- tbl_yx %>% filter(store == 51) %>% pull(week) %>% unique()
expected_weeks <- 40:160
missing_weeks <- setdiff(expected_weeks, weeks_in_store51)

cat("Erwartete Wochen 40-160 (n=", length(expected_weeks), "):\n", sep = "")
cat("Vorhandene Wochen in Store 51 (n=", length(weeks_in_store51), "):\n", sep = "")
cat("Fehlende Wochen:", missing_weeks, "\n")

# -----------------------------------------------------------------------------
# (6) deal — was ist das?
# -----------------------------------------------------------------------------
cat("\n=== deal-Variable ===\n\n")

cat("deal-Verteilung insgesamt:\n")
tbl_yx %>% count(deal) %>% mutate(share = round(n / sum(n), 3)) %>% print()

cat("\ndeal × feat-Kombinationen (insgesamt, alle Stores):\n")
tbl_yx %>%
  count(deal, feat_class = case_when(
    feat == 0 ~ "feat=0",
    feat == 1 ~ "feat=1",
    TRUE      ~ "feat in (0,1)"
  )) %>%
  pivot_wider(names_from = deal, values_from = n,
              names_prefix = "deal=", values_fill = 0) %>%
  print()

# -----------------------------------------------------------------------------
# (7) Geheimer Bonus: Wochen mit fehlender Werbe-Aktivität bei aktivem deal
# -----------------------------------------------------------------------------
# Wenn feat=1 IMMER deal=1 nach sich zieht, dann ist diese Verschränkung
# strukturell — perfekt für Auftrag 3 (Eingreifen / A/B).
cat("\n=== Verschränkung feat × deal (Confounding-Demo) ===\n\n")

cat("Wenn feat=1: P(deal=1):\n")
tbl_yx %>%
  filter(feat == 1) %>%
  summarise(p_deal_when_feat = mean(deal == 1)) %>%
  print()

cat("Wenn feat=0: P(deal=1):\n")
tbl_yx %>%
  filter(feat == 0) %>%
  summarise(p_deal_when_no_feat = mean(deal == 1)) %>%
  print()

# Wenn die zwei stark unterschiedlich sind: feat und deal sind gekoppelt
# → klassisches Confounder-Problem für Beobachtungsdaten

cat("\n=== Sondage Teil 4 abgeschlossen ===\n")




# ============================================================
# Sondage: bayesm::orangeJuice vs. Taddy oj.csv
#
# Ziel: Prüfen, ob unsere Filterung der bayesm-Daten auf die Top-3
# Marken die Taddy-Daten reproduziert. Insbesondere:
#   - Anzahl Beobachtungen pro Marke
#   - Verteilung von price und sales (Schiefe)
#   - Preis-Niveau pro Marke (Boxplot wie bei Taddy)
#   - Regressions-Koeffizienten (Vergleich mit Taddy-Output)
#   - Existenz von `deal` in bayesm (nicht in Taddy)
# ============================================================

library(tidyverse)
library(bayesm)
library(moments)   # für skewness / kurtosis
library(patchwork)

# Brand-IDs in bayesm:
#   1  = Tropicana    (Premium)
#   5  = Minute Maid  (Mainstream)
#   10 = Dominick's   (Eigenmarke)
# Diese Zuordnung steht in ?bayesm::orangeJuice

data(orangeJuice)
tbl_yx   <- as_tibble(orangeJuice$yx)
tbl_demo <- as_tibble(orangeJuice$storedemo)

cat("=== Roh-Stand bayesm ===\n")
cat("yx rows:", nrow(tbl_yx), "  cols:", ncol(tbl_yx), "\n")
cat("storedemo rows:", nrow(tbl_demo), "  cols:", ncol(tbl_demo), "\n\n")

cat("Verteilung der Marken in tbl_yx (Roh, alle 11):\n")
tbl_yx |> count(brand) |> print()
cat("\n")


# ============================================================
# Schritt 1: Filter auf Top-3 Marken
# ============================================================

top3 <- c(1, 5, 10)
brand_names <- c("1" = "tropicana", "5" = "minute.maid", "10" = "dominicks")

tbl_top3 <- tbl_yx |>
  filter(brand %in% top3)

cat("=== Nach Filter auf Marken 1, 5, 10 ===\n")
cat("Rows:", nrow(tbl_top3), "\n")
cat("(Taddy oj.csv hat 28947 — übereinstimmung?)\n\n")

cat("Pro Marke:\n")
tbl_top3 |> count(brand) |> print()
cat("\n")


# ============================================================
# Schritt 2: Wide -> Long-Form für Preis
# Wir wollen *eine* price-Variable: den Preis der jeweiligen Marke
# der jeweiligen Zeile. price1..price11 enthalten Preise aller 11
# Marken — wir picken den passenden.
# ============================================================

tbl_oj <- tbl_top3 |>
  mutate(
    price = case_when(
      brand == 1  ~ price1,
      brand == 5  ~ price5,
      brand == 10 ~ price10
    ),
    brand_name = recode(as.character(brand), !!!brand_names),
    brand_name = factor(brand_name,
                        levels = c("dominicks", "minute.maid", "tropicana")),
    feat_binary = round(feat),                     # round 2% transition values
    sales = exp(logmove)                           # Taddy zeigt 'sales' direkt
  ) |>
  select(store, brand, brand_name, week, logmove, sales,
         price, feat = feat_binary, feat_raw = feat,
         deal, profit)

cat("=== Aufbereitet (Lehr-Form) ===\n")
print(head(tbl_oj, 3))
cat("\n")


# ============================================================
# Schritt 3: Vergleich mit Taddy — Verteilung price pro Marke
# Taddy-Boxplot: dominicks niedrig (~1-2), minute.maid mittel
# (~2-2.5), tropicana hoch (~3)
# ============================================================

cat("=== Price je Marke (Vergleich zu Taddy-Boxplot) ===\n")
tbl_oj |>
  group_by(brand_name) |>
  summarise(
    n      = n(),
    min    = round(min(price), 2),
    q25    = round(quantile(price, 0.25), 2),
    median = round(median(price), 2),
    q75    = round(quantile(price, 0.75), 2),
    max    = round(max(price), 2),
    skew   = round(skewness(price), 3)
  ) |>
  print()
cat("\n")


# ============================================================
# Schritt 4: Schiefe von sales und log(sales)
# Taddy plottet log(sales), also unterstellen wir Right-Skew bei
# rohen sales und einigermaßen symmetrisch bei log(sales)
# ============================================================

cat("=== Schiefe von sales (raw vs. log) ===\n")
sales_stats <- tbl_oj |>
  summarise(
    sales_min       = round(min(sales), 0),
    sales_max       = round(max(sales), 0),
    sales_skew      = round(skewness(sales), 3),
    sales_kurt      = round(kurtosis(sales), 3),
    log_sales_min   = round(min(logmove), 2),
    log_sales_max   = round(max(logmove), 2),
    log_sales_skew  = round(skewness(logmove), 3),
    log_sales_kurt  = round(kurtosis(logmove), 3)
  )
print(sales_stats)
cat("\n")
cat("Interpretation: raw sales rechtsschief (positive Skew),\n")
cat("log(sales) wesentlich näher an symmetrisch.\n\n")


# ============================================================
# Schritt 5: Schiefe von price und log(price)
# ============================================================

cat("=== Schiefe von price (raw vs. log) ===\n")
price_stats <- tbl_oj |>
  summarise(
    price_skew       = round(skewness(price), 3),
    log_price_skew   = round(skewness(log(price)), 3)
  )
print(price_stats)
cat("\n")


# ============================================================
# Schritt 6: Existenz von deal in bayesm
# Taddy hat es nicht — wir wollen es behalten als Stolperstein-Hebel
# ============================================================

cat("=== deal in bayesm (nicht in Taddy oj.csv) ===\n")
tbl_oj |> count(feat, deal) |> print()
cat("\n")

cat("Anteil feat=1: ",
    round(mean(tbl_oj$feat == 1), 3), "\n")
cat("Anteil deal=1: ",
    round(mean(tbl_oj$deal == 1), 3), "\n")
cat("Anteil feat=1 UND deal=1: ",
    round(mean(tbl_oj$feat == 1 & tbl_oj$deal == 1), 3), "\n")
cat("Wenn feat=1, ist deal immer 1? ",
    all(tbl_oj$deal[tbl_oj$feat == 1] == 1), "\n\n")


# ============================================================
# Schritt 7: Taddys naive Regression (zur Reproduktion)
# Taddy ohne feat: log(sales) ~ log(price) + brand
#   log(price)        -3.13869
#   brandminute.maid   0.87017
#   brandtropicana     1.52994
# ============================================================

cat("=== Reproduktion Taddys naive Regression ===\n")
tbl_oj$brand_name <- relevel(tbl_oj$brand_name, ref = "dominicks")
reg_taddy <- glm(logmove ~ log(price) + brand_name, data = tbl_oj)
cat("Koeffizienten (sollten Taddy matchen):\n")
print(round(coef(reg_taddy), 4))
cat("\n")


# ============================================================
# Schritt 8: Mit feat — Werbe-Effekt
# ============================================================

cat("=== Mit feat ===\n")
reg_feat <- glm(logmove ~ log(price) + brand_name + feat, data = tbl_oj)
cat("Koeffizienten:\n")
print(round(coef(reg_feat), 4))
cat("\nfeat-Koeffizient (Sarahs 25%-Erwartung):\n")
cat(round(coef(reg_feat)["feat"], 3), "\n\n")


# ============================================================
# Schritt 9: Mit feat + deal — der Stolperstein-Vergleich
# ============================================================

cat("=== Mit feat + deal (Stolperstein) ===\n")
reg_full <- glm(logmove ~ log(price) + brand_name + feat + deal, data = tbl_oj)
cat("Koeffizienten:\n")
print(round(coef(reg_full), 4))
cat("\nfeat-Koeffizient ohne deal: ",
    round(coef(reg_feat)["feat"], 3), "\n")
cat("feat-Koeffizient mit deal:   ",
    round(coef(reg_full)["feat"], 3), "\n")
cat("Differenz: ", round(coef(reg_feat)["feat"] - coef(reg_full)["feat"], 3),
    " — das ist die Omitted-Variable-Bias-Größenordnung\n\n")


# ============================================================
# Schritt 10: Visuelle Vergleiche zu Taddy-Plots
# ============================================================

# Boxplot price pro Marke (analog zu Taddys Plot)
p1 <- tbl_oj |>
  ggplot(aes(x = price, y = brand_name, fill = brand_name)) +
  geom_boxplot() +
  scale_fill_manual(values = c("green", "red", "gold")) +
  labs(title = "Price per brand", x = "Price ($)", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Scatter log(price) vs. log(sales) wie Taddy
p2 <- tbl_oj |>
  ggplot(aes(x = logmove, y = log(price), color = brand_name)) +
  geom_point(alpha = 0.3, size = 0.5) +
  scale_color_manual(values = c("green", "red", "gold")) +
  labs(title = "log(sales) vs. log(price)",
       x = "log(sales)", y = "log(price)") +
  theme_minimal()

# Verteilung sales (rechtsschief)
p3 <- tbl_oj |>
  ggplot(aes(x = sales, fill = brand_name)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("green", "red", "gold")) +
  labs(title = "Distribution of raw sales (right-skewed)",
       x = "sales") +
  theme_minimal()

# Verteilung logmove
p4 <- tbl_oj |>
  ggplot(aes(x = logmove, fill = brand_name)) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("green", "red", "gold")) +
  labs(title = "Distribution of log(sales)",
       x = "log(sales)") +
  theme_minimal()

# Speichern für Vergleich mit Taddy
combined <- (p1 + p2) / (p3 + p4)
ggsave("sondage_taddy_vergleich.png", combined, width = 12, height = 8)
cat("Plot gespeichert: sondage_taddy_vergleich.png\n")
cat("Vergleiche mit den Taddy-Plots, die im Chat hochgeladen wurden.\n\n")


# ============================================================
# Summary
# ============================================================

cat("============================================\n")
cat("ERWARTETE BEFUNDE:\n")
cat("============================================\n")
cat("1. Nach Filter Top-3: ~28947 Zeilen (= Taddy)\n")
cat("2. Pro Marke: ~9649 Zeilen\n")
cat("3. price-Boxplot: dominicks niedrig, minute.maid mitte,\n")
cat("   tropicana hoch — wie Taddy-Plot 1\n")
cat("4. logmove vs. log(price): negative Korrelation, drei Cluster\n")
cat("   — wie Taddy-Plot 2\n")
cat("5. sales raw: stark rechtsschief\n")
cat("6. log(sales): fast symmetrisch\n")
cat("7. price raw: mäßig rechtsschief\n")
cat("8. log(price): symmetrischer\n")
cat("9. Taddys naive Regression reproduziert: -3.14, 0.87, 1.53\n")
cat("10. deal existiert in bayesm und ist 1, wenn feat=1\n")
cat("11. feat-Koeffizient sinkt deutlich, wenn deal hinzugefügt wird\n")
cat("============================================\n")


# ============================================================
# Replication of Taddy plots — with bayesm data, correctly scaled
#
# bayesm 'price' is price-per-ounce.
# For brands 1, 5, 10 (all 64 oz containers): container price = price × 64.
#
# Brand IDs in bayesm (from ?bayesm::orangeJuice):
#   1  Tropicana Premium 64 oz
#   5  Minute Maid 64 oz
#   10 Dominicks 64 oz
# (Brands 2, 6, 11 have different sizes — not used here.)
# ============================================================

library(tidyverse)
library(bayesm)

data(orangeJuice)
tbl_yx <- as_tibble(orangeJuice$yx)

# Filter top-3 brands and scale price to per-container ($)
tbl_oj <- tbl_yx |>
  filter(brand %in% c(1, 5, 10)) |>
  mutate(
    # pick the correct price column for the brand of each row
    price_per_oz = case_when(
      brand == 1  ~ price1,
      brand == 5  ~ price5,
      brand == 10 ~ price10
    ),
    # all three brands are 64-oz containers → uniform conversion
    price = price_per_oz * 64,

    # brand as readable factor, ordered as Taddy
    brand_name = factor(
      recode(as.character(brand),
             "1" = "tropicana", "5" = "minute.maid", "10" = "dominicks"),
      levels = c("dominicks", "minute.maid", "tropicana")
    ),

    feat = round(feat),    # binary
    sales = exp(logmove)
  ) |>
  select(store, brand, brand_name, week, logmove, sales,
         price, price_per_oz, feat, deal, profit)


# ============================================================
# Plot 1: Boxplot price per brand  (Taddy reproduction)
# Expected (Taddy):  dominicks median ~1.79, minute.maid ~2.17, tropicana ~2.99
# ============================================================

brandcol <- c(dominicks = "green", minute.maid = "red", tropicana = "gold")

p1 <- tbl_oj |>
  ggplot(aes(x = price, y = brand_name, fill = brand_name)) +
  geom_boxplot() +
  scale_fill_manual(values = brandcol) +
  labs(title    = "Price per brand (bayesm, container price)",
       subtitle = "Compare to Taddy's boxplot",
       x        = "Price ($)",
       y        = "") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)


# ============================================================
# Plot 2: Scatter log(price) vs log(sales)  (Taddy reproduction)
# Expected: three colour-coded clusters, negative slope
# ============================================================

p2 <- tbl_oj |>
  ggplot(aes(x = logmove, y = log(price), color = brand_name)) +
  geom_point(alpha = 0.3, size = 0.5) +
  scale_color_manual(values = brandcol) +
  labs(title = "log(sales) vs. log(price) — bayesm data",
       x     = "log(sales) = logmove",
       y     = "log(price)",
       color = "Brand") +
  theme_minimal()

print(p2)


# ============================================================
# Verification: numerical summaries
# ============================================================

cat("=== Container-price summary (compare to Taddy: 1.79, 2.17, 2.99) ===\n")
tbl_oj |>
  group_by(brand_name) |>
  summarise(
    n      = n(),
    min    = round(min(price),    2),
    q25    = round(quantile(price, 0.25), 2),
    median = round(median(price), 2),
    q75    = round(quantile(price, 0.75), 2),
    max    = round(max(price),    2)
  ) |>
  print()

cat("\n=== Reproduce Taddy's regression on container prices ===\n")
tbl_oj$brand_name <- relevel(tbl_oj$brand_name, ref = "dominicks")
reg <- glm(logmove ~ log(price) + brand_name, data = tbl_oj)
cat("Coefficients:\n")
print(round(coef(reg), 4))
cat("\nExpected (Taddy):\n")
cat("  log(price)        -3.13869\n")
cat("  brandminute.maid   0.87017\n")
cat("  brandtropicana     1.52994\n")
cat("(Intercept will be ~10.83 because of the × 64 scaling shift —\n")
cat(" log(64) ≈ 4.16, so the intercept shifts down by ~13.05.\n")
cat(" The slopes are scale-invariant under log transform.)\n")


# ============================================================
# Fit two regressions: parallel slopes vs interaction
# ============================================================

reg_parallel <- glm(logmove ~ log(price) + brand_name, data = tbl_oj)
reg_interact <- glm(logmove ~ log(price) * brand_name, data = tbl_oj)

cat("=== Parallel slopes model ===\n")
print(round(coef(reg_parallel), 4))

cat("\n=== Interaction model ===\n")
print(round(coef(reg_interact), 4))

cat("\n=== Brand-specific elasticities (interaction model) ===\n")
b <- coef(reg_interact)
cat("dominicks:   ", round(b["log(price)"], 3), "\n")
cat("minute.maid: ", round(b["log(price)"] + b["log(price):brand_nameminute.maid"], 3), "\n")
cat("tropicana:   ", round(b["log(price)"] + b["log(price):brand_nametropicana"], 3), "\n\n")


# ============================================================
# Two side-by-side base-R plots (Taddy style)
# ============================================================

par(mfrow = c(1, 2))

# --- Left plot: parallel slopes ---
plot(logmove ~ log(price), data = tbl_oj, col = brandcol[brand_name],
     cex = 0.1, pch = 20, bty = "n",
     main = "Parallel slopes")

beta <- coef(reg_parallel)
# dominicks is reference: intercept = beta[1], slope = beta[2]
abline(a = beta[1],            b = beta[2], col = brandcol["dominicks"],   lwd = 2)
abline(a = beta[1] + beta[3],  b = beta[2], col = brandcol["minute.maid"], lwd = 2)
abline(a = beta[1] + beta[4],  b = beta[2], col = brandcol["tropicana"],   lwd = 2)

legend("bottomleft", bty = "n", lwd = 2, col = brandcol,
       legend = names(brandcol))


# --- Right plot: interaction ---
plot(logmove ~ log(price), data = tbl_oj, col = brandcol[brand_name],
     cex = 0.1, pch = 20, bty = "n",
     main = "Brand × log(price) interaction")

beta <- coef(reg_interact)
# dominicks (reference)
abline(a = beta[1],
       b = beta[2],
       col = brandcol["dominicks"], lwd = 2)
# minute.maid
abline(a = beta[1] + beta["brand_nameminute.maid"],
       b = beta[2] + beta["log(price):brand_nameminute.maid"],
       col = brandcol["minute.maid"], lwd = 2)
# tropicana
abline(a = beta[1] + beta["brand_nametropicana"],
       b = beta[2] + beta["log(price):brand_nametropicana"],
       col = brandcol["tropicana"], lwd = 2)

legend("bottomleft", bty = "n", lwd = 2, col = brandcol,
       legend = names(brandcol))

par(mfrow = c(1, 1))


# ============================================================
# Optional comparison: three-way interaction (price × brand × feat)
# Taddy's final model — shows how advertising changes elasticities
# ============================================================

tbl_oj$feat_binary <- round(tbl_oj$feat)
reg_3way <- glm(logmove ~ log(price) * brand_name * feat_binary, data = tbl_oj)

cat("=== Brand-specific elasticities — WITHOUT advertising (feat=0) ===\n")
b <- coef(reg_3way)
elas_no_ad <- c(
  dominicks   = b["log(price)"],
  minute.maid = b["log(price)"] + b["log(price):brand_nameminute.maid"],
  tropicana   = b["log(price)"] + b["log(price):brand_nametropicana"]
)
print(round(elas_no_ad, 3))

cat("\n=== Brand-specific elasticities — WITH advertising (feat=1) ===\n")
elas_with_ad <- c(
  dominicks   = b["log(price)"] + b["log(price):feat_binary"],
  minute.maid = b["log(price)"] + b["log(price):brand_nameminute.maid"] +
    b["log(price):feat_binary"] + b["log(price):brand_nameminute.maid:feat_binary"],
  tropicana   = b["log(price)"] + b["log(price):brand_nametropicana"] +
    b["log(price):feat_binary"] + b["log(price):brand_nametropicana:feat_binary"]
)
print(round(elas_with_ad, 3))

cat("\nTaddy's finding: advertising makes consumers *more* price-sensitive\n")
cat("(elasticities become more negative when feat=1)\n")

# ============================================================
# Replication of Taddy regression plots — bayesm data, tidy syntax
#
# Two models on log(sales) ~ log(price) + brand:
#   1) Parallel slopes  → moderndive::geom_parallel_slopes()
#   2) Interaction      → geom_smooth(method = "lm", se = FALSE)
#
# bayesm 'price' is per-ounce. Brands 1, 5, 10 are all 64-oz containers,
# so container price = price × 64.
# ============================================================

library(tidyverse)
library(bayesm)
library(moderndive)   # geom_parallel_slopes()


# ============================================================
# Data preparation — tidy pipeline only
# ============================================================

data(orangeJuice)

tbl_oj <- as_tibble(orangeJuice$yx) %>%
  filter(brand %in% c(1, 5, 10)) %>%
  mutate(
    price_per_oz = case_when(
      brand == 1  ~ price1,
      brand == 5  ~ price5,
      brand == 10 ~ price10
    ),
    price = price_per_oz * 64,                       # container price ($)
    brand_name = factor(
      recode(as.character(brand),
             "1" = "tropicana", "5" = "minute.maid", "10" = "dominicks"),
      levels = c("dominicks", "minute.maid", "tropicana")
    ),
    feat  = round(feat),                             # binary
    sales = exp(logmove)
  ) %>%
  select(store, brand, brand_name, week, logmove, sales,
         price, price_per_oz, feat, deal, profit)


# ============================================================
# Save tidy training data
# ============================================================

# Save the cleaned OJ data for use in subsequent labs/sondage
tbl_oj %>% write_rds(xfun::from_root("data", "raw", "Sondage", "sondage_oj_top3.rds"))


# ============================================================
# Coefficient tables for inspection
# ============================================================

# Parallel slopes model
reg_parallel <- glm(logmove ~ log(price) + brand_name, data = tbl_oj)

# Interaction model
reg_interact <- glm(logmove ~ log(price) * brand_name, data = tbl_oj)

cat("=== Parallel slopes model ===\n")
reg_parallel %>%
  broom::tidy() %>%
  mutate(across(where(is.numeric), \(x) round(x, 4))) %>%
  print()

cat("\n=== Interaction model ===\n")
reg_interact %>%
  broom::tidy() %>%
  mutate(across(where(is.numeric), \(x) round(x, 4))) %>%
  print()

cat("\n=== Brand-specific elasticities (interaction) ===\n")
elasticities <- tibble(
  brand_name = c("dominicks", "minute.maid", "tropicana"),
  elasticity = c(
    coef(reg_interact)["log(price)"],
    coef(reg_interact)["log(price)"] +
      coef(reg_interact)["log(price):brand_nameminute.maid"],
    coef(reg_interact)["log(price)"] +
      coef(reg_interact)["log(price):brand_nametropicana"]
  )
) %>%
  mutate(elasticity = round(elasticity, 3))
print(elasticities)


# ============================================================
# Shared visual settings
# ============================================================

brandcol <- c(dominicks   = "green",
              minute.maid = "red",
              tropicana   = "gold")


# ============================================================
# Plot 1: Parallel slopes via moderndive::geom_parallel_slopes()
# ============================================================

p_parallel <- tbl_oj %>%
  ggplot(aes(x = log(price), y = logmove, color = brand_name)) +
  geom_point(alpha = 0.2, size = 0.4) +
  geom_parallel_slopes(se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = brandcol) +
  labs(
    title    = "Parallel slopes",
    subtitle = "log(sales) ~ log(price) + brand",
    x        = "log(price)",
    y        = "log(sales)",
    color    = "Brand"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_parallel)


# ============================================================
# Plot 2: Interaction (different slope per brand)
# geom_smooth with method = "lm" gives a separate fit per color group
# ============================================================

p_interact <- tbl_oj %>%
  ggplot(aes(x = log(price), y = logmove, color = brand_name)) +
  geom_point(alpha = 0.2, size = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = brandcol) +
  labs(
    title    = "Brand × log(price) interaction",
    subtitle = "log(sales) ~ log(price) * brand",
    x        = "log(price)",
    y        = "log(sales)",
    color    = "Brand"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_interact)


# ============================================================
# Side-by-side comparison (patchwork)
# ============================================================

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined <- p_parallel + p_interact +
    plot_annotation(
      title = "Parallel slopes vs. interaction — bayesm OJ data"
    )
  print(combined)

  # Save for comparison with Taddy's base-R plots
  ggsave("taddy_ggplot_replication.png", combined, width = 12, height = 5)
  cat("\nPlot saved: taddy_ggplot_replication.png\n")
}


# ============================================================
# Optional: three-way interaction with feat — tidy version
# Shows how advertising changes elasticities by brand
# ============================================================

reg_3way <- glm(logmove ~ log(price) * brand_name * feat, data = tbl_oj)

elasticities_3way <- tibble(
  brand_name = rep(c("dominicks", "minute.maid", "tropicana"), 2),
  feat       = c(0, 0, 0, 1, 1, 1)
) %>%
  mutate(
    coef_name = "log(price)",
    # construct the elasticity expression piece by piece
    main      = coef(reg_3way)["log(price)"],
    brand_off = case_when(
      brand_name == "minute.maid" ~ coef(reg_3way)["log(price):brand_nameminute.maid"],
      brand_name == "tropicana"   ~ coef(reg_3way)["log(price):brand_nametropicana"],
      TRUE                         ~ 0
    ),
    feat_off  = if_else(feat == 1, coef(reg_3way)["log(price):feat"], 0),
    inter_off = case_when(
      feat == 1 & brand_name == "minute.maid" ~
        coef(reg_3way)["log(price):brand_nameminute.maid:feat"],
      feat == 1 & brand_name == "tropicana"   ~
        coef(reg_3way)["log(price):brand_nametropicana:feat"],
      TRUE ~ 0
    ),
    elasticity = round(main + brand_off + feat_off + inter_off, 3)
  ) %>%
  select(brand_name, feat, elasticity) %>%
  pivot_wider(names_from = feat,
              values_from = elasticity,
              names_prefix = "feat=")

cat("\n=== Brand-specific price elasticities × advertising state ===\n")
print(elasticities_3way)
cat("\nInterpretation: with advertising (feat=1), customers become *more*\n")
cat("price-sensitive — elasticities are more negative. The premium brand\n")
cat("Tropicana shows the largest jump (less brand-loyal under promotion).\n")
