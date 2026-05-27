# ER114 — Konsolidierte TODO-Liste und Roadmap (v2)

**Erstellt:** 2026-05-22, FS26
**Letzte Aktualisierung:** nach Lab 2 + Lab 3 Patches und Spiess-Integration
**Status:** Lab 1 final, Lab 2 patches integriert und händisch rendering probleme gelöst, Lab 3 v1 erstellt

---

## 1. FS26 — was unmittelbar ansteht

### 1.1 Labs — Restaufgaben

Lab 1: Finale inhaltliche und didaktische Prüfung mit Rückgriff auf Fachliteratur (ausschließlich)

Lab 2 + 3: Werden von Autor geprüft und dann weitere Iteration. Also erstmal abwarten und später nachfragen.

### 1.2 Folien FS26

Die Folien sollen die methodische Landkarte zeichnen, das Lab vertieft
ausgewählte Konzepte beispielhaft.

#### Grobskizze Session 3 Einleitung

| Slide | Inhalt |
|---|---|
| 1 | Memo (Sarah Chen) |
| 2 | Brücke zu Session 2 (DGP unter den Verteilungen) |
| 3 | DGP (Urnen-Metapher, Breiman 2001) |
| 4 | Drei Pfade (DGP-Paths.svg + Pfade E1, E2, P) |
| 5 | Athey Hotel-Beispiel (Sneak-Peek Lab 3) |
| 6 | Big-Data-Regime (Fan/Han/Liu vier Probleme) |
| 7 | BLUE-Brücke (Bias-Variance, wann E, wann P) |
| 8 | Übergang ins Lab |

#### Bridge Lab 1 → Lab 2

- Lab 1: hohe Erklärungskraft ≠ gute Prädiktion
- Lab 2: OLS selbst ist für Prädiktion suboptimal
- BLUE-Brücke wird operativ erlebbar
- Spiess' institutional argument als Verbindung

#### Bridge Lab 2 → Lab 3 — Kausalanalyse-Landkarte (4 Slides)

**Slide A — Experimente und der DGP**

Konzeptueller Frame: alle Methoden der Kausalinferenz sind Antworten auf
*"wie kommen wir an den DGP heran, ohne ihn nur zu beobachten?"*

| Klasse | Definition | Beispiel | DGP-Bezug |
|---|---|---|---|
| **Laborexperiment** | Vollständig kontrollierte Umgebung | Psychologie, klinische Studien Phase 1 | Forscher *konstruiert* den DGP |
| **Feldexperiment** | Randomisierung in realer Umgebung | A/B-Tests im Marketing, Bildungsinterventionen | Forscher *manipuliert* einen Teil des DGP |
| **Natürliches Experiment** | Manipulation durch externe Kräfte | Mindestlohnreform, Lotterien, Grenzwerte | Externe Manipulation, Forscher rekonstruiert die Logik |

**Slide B — Auswertungsstrategien im Überblick**

Tabellarische Übersicht aller gängigen Strategien:

| Strategie | Datenanforderung | Identifizierende Annahme | Wann verwenden |
|---|---|---|---|
| **OLS + Controls** | Beobachtungsdaten, alle Confounder beobachtet | Unconfoundedness | Wenn DAG vollständig bekannt |
| **A/B-Test (RCT)** | Experimentelle Daten | Randomisierung | Wenn Manipulation möglich |
| **DiD** | Panel, Treatment-Variation | Parallel Trends | Politik-Reform, Rollout |
| **Event Study** | Panel mit Treatment-Zeitpunkt | Parallel Trends (testbar) | Wenn Dynamik wichtig |
| **IV** | Beobachtungsdaten + valides Instrument | Exclusion + Relevance | Endogenität, Instrument verfügbar |
| **Synthetic Control** | 1 behandelte Einheit + viele Kontrollen | Kontrollen-Pool gut | Einzelne Politik-Intervention |
| **RDD** | Cutoff-Variable mit Schwellenwert | Stetigkeit am Cutoff | Schwellenwert-Politik |
| **Matching / PS** | Viele Kovariate | Unconfoundedness + Common Support | Plausibles DAG |
| **Double ML** | Beobachtungsdaten + hochdim. Controls | Unconfoundedness + ML-Konvergenz | Sample-Splitting nötig |

**Slide C — DiD versus Event Study: die wichtige Abgrenzung**

| Aspekt | DiD | Event Study |
|---|---|---|
| Anzahl Koeffizienten | 1 (Durchschnitt) | n_periods × 2 |
| Parallel-Trends-Test | indirekt | direkt (pre-period leads) |
| Dynamik des Effekts | aggregiert | sichtbar |
| Staggered Adoption | problematisch (Borusyak et al. 2024) | erfordert moderne Schätzer |
| Empfehlung | einzelner sharp policy change | wenn Timing/Dynamik wichtig |

Borusyak/Jaravel/Spiess (2024): Imputations-Schätzer löst beide Probleme.

**Slide D — Spiess als verbindender Faden**

- Lab 1: Note of Caution — Unbiasedness als credibility scaffold
- Lab 2: Bias-Variance hat zwei Lesarten (statistisch + strategisch)
- Lab 3: Sample-splitting operationalisiert die institutionelle Restriktion

Methodische Disziplin = institutionelle Funktion = Glaubwürdigkeit der
Empfehlung an Sarah.

**Aufwand Folien gesamt:** 8 h (4 h Hauptslides + 1 h Bridge 1→2 + 3 h Bridge 2→3).



### Gesamtaufwand FS26

| Block | Aufwand |
|---|---|
| Lab 1 Rest | 1 h |
| Lab 2 Rest | 30 min |
| Lab 3 Rest | 2.5 h |
| Folien | 8 h |
| Cohort-Tests | 3 h |
| **Gesamt** | **~15 h** |

---

## 2. Sommer-Iteration HS26

### 2.1 Strukturelle Änderungen

| Punkt | Aufwand |
|---|---|
| Breiman 2001 als Kursarchitektur-Grundlage | 4 h |
| DGP-Begriff in Session 1 verankern | 2 h |
| Setup-Lab mit `renv` + `set.seed` | 4 h |
| **Vom Modell zur Entscheidung** (EV-Framework operativ als Lab 2.5) | 8 h |

### 2.2 Inhaltliche Ergänzungen

| Punkt | Wohin |
|---|---|
| Athey Hotel-Beispiel als Standard-Illustration | Session 1 + 3 Folien |
| Lab 2.5 zu EV-Framework | nach Lab 2 |
| Newey-West HAC als Bonus-Lab | Bonus-Lab |
| Mullainathan & Spiess (2017) als Pflichtlektüre | Reading List |
| **Lab 3 erweitern um Synthetic Control-Sektion** | Lab 3 Step 4 oder neuer Step |
| **RDD-Section in Lab 3** | Lab 3 Bonus |
| **Spiess-Argument als Modul-Klammer** | Session 1 Einleitung oder Session 2 Schluss |

### 2.3 Werkzeug-Erweiterungen

| Punkt | Aufwand |
|---|---|
| `ggdag`-Paket etablieren | 2 h |
| Companion-Scripts auf einheitlichen Stil | 4 h |
| Test-Suite für Render-Stabilität | 6 h |
| `DoubleML`-Paket operativ in Lab 3 (in FS26 nur Sketch) | 3 h |
| `didimputation`-Paket operativ in Lab 3 | 2 h |
