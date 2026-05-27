# ER114 — Eckpunktepapier zum Gesamtkurs

*Stand: Mai 2026 (final). Soll-Zustand für die Sommer-Iteration.
Verbindliches Referenzdokument für Kursarchitektur, Lab-Entwicklung und
Datennutzung.*

*Anmerkung: Der Stand des laufenden Semesters (FS26) wird in einem
separaten Dokument festgehalten und ist nicht Gegenstand dieses Papiers.*

---

## 0. Framing — Warum dieser Kurs

In einer zunehmend von Daten und Künstlicher Intelligenz geprägten
Wirtschaft reicht es für Entscheidungsträger nicht mehr aus, analytische
Ergebnisse lediglich passiv zu konsumieren. Während generative KI-Tools
die Erstellung von Code und Modellen radikal vereinfachen
(„Demokratisierung des Prototyping"), steigen die Anforderungen an das
Management, die Qualität dieser Ergebnisse zu **verifizieren**.

Modernes Management erfordert daher eine vertiefte Datenkompetenz
(Data Literacy):

- die Beurteilung statistischer Modelle,
- die Fähigkeit, algorithmischen Output zu hinterfragen,
- die Kompetenz, in einer Welt automatisierter Analysen die Grenzen
  empirischer Evidenz sicher zu navigieren.

ER114 vermittelt diese Datenkompetenz als Brücke zwischen klassischer
Statistik/Ökonometrie und moderner Datenwissenschaft.

---

## 1. Storyline und Architektur

### 1.1 Die Leitfrage

Wann ist welche statistische/datenwissenschaftliche Methode angemessen
— und was unterscheidet die ökonometrische Tradition von der modernen
Datenwissenschaft?

Studierende kommen aus der Statistik-Grundausbildung und gehen weiter
in die Welt von Machine Learning und Data Analytics (ER017, ERx18).
Sie sollen lernen, zwischen beiden Welten zu **übersetzen** und zu
erkennen, dass die zwei Traditionen komplementäre Werkzeuge für
unterschiedliche Erkenntnisinteressen sind.

### 1.2 Das erkenntnistheoretische Fundament — Induktion ↔ Deduktion

Vor jeder methodischen Wahl steht eine epistemologische Setzung:
Schließen wir induktiv aus Daten auf eine Regel, oder deduktiv aus einer
Regel auf einen Einzelfall?

| Induktion | Deduktion |
|---|---|
| von Daten zur Hypothese | von Theorie zur Hypothese |
| Hypothese als *Ergebnis* | Hypothese als *Eingang* |
| Risiko: HARKing | Risiko: starre Theorie-Treue |
| Stärke: datengetrieben | Stärke: kontrolliertes Testen |
| Data Mining, EDA | Hypothesentest, Inferenz |

Diese methodische Klammer ist nicht nur philosophisch — sie strukturiert
das Risiko-Profil jeder Analyse:

- Induktiv erzeugte Hypothesen dürfen nicht an denselben Daten geprüft
  werden, an denen sie gewonnen wurden (HARKing).
- Deduktiv geprüfte Hypothesen sind nur so gut wie die Theorie, die sie
  motiviert.

### 1.3 Drei Sessions, drei Erkenntnisinteressen

| Session | Erkenntnisinteresse | Methodische Operation |
|---|---|---|
| **S1: Beschreiben** | Daten verstehen, induktiv Hypothesen generieren | EDA, deskriptive Regression, Modell als Erkundungs-Werkzeug |
| **S2: Hypothesen prüfen** | Aussagen über den Datengenerierungsprozess machen | Inferenz-Maschine (CI, p-Wert, Bootstrap, infer-Pipe), HARKing-Auflösung |
| **S3: Modellieren mit Folge-Logik** | Vorhersagen oder Eingreifen | Pfad A (Predictive) und Pfad B (Causal), beide auf BLUE-Brücke aufbauend |

S1 und S2 stehen in einer **induktiven Reifekette**: S1 generiert
Hypothesen, S2 prüft sie. Der HARKing-Konflikt zwischen beiden wird
didaktisch durch zwei kombinierte Methoden gelöst (siehe 1.5).

### 1.4 Pfad A und Pfad B in S3 — mit BLUE als gemeinsame Brücke

Bevor Pfad A und Pfad B getrennt werden, steht **BLUE/Bias-Variance** als
*gemeinsame methodische Brücke*. BLUE optimiert Schätzer-Eigenschaften
(unverzerrt, minimale Varianz unter den unverzerrten) — diese
Eigenschaften sind *für beide Pfade* relevant:

- **Für Pfad B (Inferenz / Erklären):** BLUE ist Optimalitätskriterium
  unter der Annahme korrekter Modellspezifikation und Exogenität.
- **Für Pfad A (Prädiktion):** BLUE ist *nicht* das passende Kriterium —
  hier zählt Out-of-Sample-Performance, was zum Bias-Variance-Tradeoff
  und zur Akzeptanz von Bias (Ridge, Lasso) führt.

Die BLUE-Diskussion macht damit *beide Pfade* erst verständlich. Sie
ist die zentrale theoretische Brücke zwischen Statistik-Tradition und
Data-Science-Praxis.

**Pfad A — Predictive Modeling:** „Was wird Y sein bei neuen X?"
- Out-of-Sample-Generalisierung, Lasso, CV
- Lucas-Kritik begrenzt Geltung: das Modell gilt nur unter struktureller
  Stabilität

**Pfad B — Causal Modeling:** „Was wäre mit Y passiert, wenn wir X
manipuliert hätten?"

Innerhalb von Pfad B differenziert S3 zwei Wege zur Exogenität — eine
Differenzierung, die sich an der Hernán-Position orientiert:

- **Exogenität per Annahme** (statistische Perspektive): Beobachtungs-
  daten unter MLR.4 — die Exogenität wird *unterstellt*, nicht garantiert.
  Wooldridge-Welt. Die Klassifikation der Daten (Querschnitt /
  Längsschnitt / Panel / Survey) entscheidet, wie plausibel diese
  Annahme ist. Selbstselektion ist ein Beispiel, wo die Annahme
  systematisch verletzt wird.
- **Exogenität per Studiendesign**: RCT, IV, DiD, RDD — das Design
  *erzeugt* Exogenität, nicht nur die Annahme. Pearl-/Angrist-Welt.

Diese Verfeinerung ist Hernáns Kernaussage in Reinform: *„Causal
inference is just inference with the right data."* — die Methode ist
dieselbe, nur die Datengrundlage entscheidet darüber, ob die Aussage
kausal interpretierbar ist.

### 1.5 Die HARKing-Lösung — Sample-Splitting + Replikation

Der Übergang S1 → S2 birgt einen methodischen Konflikt: induktiv gewonnene
Hypothesen aus S1 dürfen nicht mit denselben Daten in S2 getestet werden.

**Lösung: zwei kombinierte Strategien, die didaktisch aufeinanderfolgen.**

**Strategie A — Sample-Splitting (80/20).** S2 beginnt mit einem
80/20-Split der S1-Daten. Studierende erleben die Logik (Daten teilen
für *Finden* und *Bestätigen*) und die Schwächen (n-Halbierung schmälert
Power; bei Zeitreihen entstehen Leakage-Probleme). Daraus werden zwei
Lehrgewinne extrahiert:
- *Stichprobenumfang und Power* — klassischer ökonometrischer Anker
- *Time-Series-Leakage* — Vorbereitung auf S3 (CV-Strategien)

**Strategie B — Replikation an einer zweiten Filiale.** Anschließend
wird dieselbe Hypothese an einer unabhängigen Filiale geprüft
(Store 50 als Replikations-Partner zu Store 51). In der Sommer-
Iteration als Demo mit aktivierender Reflexions-Aufgabe (Studierende
befüllen einen Vergleichs-Steckbrief und diskutieren die Implikationen
für eine Empfehlung). Lehrpunkte:
- Replikation als die ältere und konzeptionell tiefere Lösung
- externe Validität als operativ erfahrbares Konzept
- Brücke zur Demografie-Diskussion (warum unterscheiden sich Filialen?)
- Anschluss an die Replikationskrise-Debatte

**Bridge zu S3:** Bei großen Daten ist Sample-Splitting wieder
praktikabel — dort kommt CV. Aber Replikation an unabhängigen Daten
bleibt der Goldstandard.

### 1.6 Der Translator als Akteur, der AI Value Creator als Position

Über alle drei Sessions führt der Studierende die Rolle des Translators
aus (Provost & Fawcett 2013). Er übersetzt zwischen:
- Geschäftsfragen ↔ Datenfragen
- methodischer Sprache ↔ Manager-Sprache
- Statistik-Tradition ↔ Data-Science-Tradition
- Korrelation ↔ Kausalität

Über die operative Translator-Rolle legt sich die strategische Position
des AI Value Creator (Thomas et al.) — organisationsbezogen, mit Blick
auf:
- proprietäre Daten als strategischen Asset
- Datenqualität und Metadaten als Wettbewerbsvorteil
- Datenarchitektur und Data Governance

### 1.7 Six Foundations Bridge

Das didaktische Skelett des Kurses ist die Brücken-Folie *Six Statistical
Foundations and their Transfer to Data Science* (Erweiterung der
bestehenden Five-Foundations-Folie aus S2).

| # | Klassische Tradition | Brücke | Data Science | Aktiv in |
|---|---|---|---|---|
| 1 | E[Y\|X] — Conditional Mean | Market benchmark | Residual als Arbitrage | S1 |
| 2 | Sampling Distribution | Measure uncertainty | Bootstrap, infer | S2 |
| 3 | Expected Value & Error Types | ROI Calculus | Expected Value Framework | S2 |
| 4 | p-Wert, α, Multiplicity | Signal vs. Noise | FDR, p-Hacking, HARKing | S2 |
| 5 | Korrelation ≠ Kausalität | Intervention | A/B-Test, Causal ML | S3 |
| **6 (neu)** | **BLUE, Gauss-Markov, Bias-Varianz** | **Komplexitätskontrolle** | **OOS, Cross-Validation, Regularisierung** | **S3** |

Die Brückenfolie wird in jeder Session gezeigt, mit der jeweils aktiven
Zeile hervorgehoben.

---

## 2. Themen-Allokation pro Block (Soll-Zustand)

Die Allokation ist *vorläufig*. Verschiebungen sind möglich, wenn
didaktische oder zeitliche Gründe sie nahelegen.

### Block 1 — Session 1: Beschreiben (Population)

**CRISP-DM:** Phasen 1–4 (Business → Data → Preparation → Modeling)
**Modus:** induktiv, deskriptiv, ohne Inferenz.
**Notation:** Stocker-Realisationen (b₁, b₂, eᵢ).

**Inhalte:**
- AI User vs. AI Value Creator (Thomas et al.)
- Walmart-Pop-Tarts als narratives Eingangsbeispiel
- CRISP-DM-Modell als Roadmap des Kurses
- **Data Governance — fünf Dimensionen** als Wheel-Visualisierung
  (siehe Sektion 5)
- Skalenniveau und Datenkuratierung (Refresher)
- **Wrangling-Demo:** Roh-Datensatz mit chr-Variablen, Locale-Problemen
  und unsauberer Codierung wird in tidy Form gebracht (gestellter Code,
  vollständig kommentiert)
- EDA: deskriptive Statistik, ggpairs, Verteilungen, Faktoren
- Deskriptive Regression als „lineare Approximation an die CEF"
  (Stocker)
- **Multiple Regression** mit Interaktionsterm, Interpretation
- **Ausblick** Overfitting / Underfitting / OVB als deskriptive
  Beobachtung
- Translator-Memo am Ende

Kein p-Wert, kein Konfidenzintervall, `se = FALSE` durchgehend.

**Cliffhanger am Ende:**

1. *„Was, wenn wir Daten einer anderen Filiale gehabt hätten, die nur
   einen Effekt von 5 % statt 25 % zeigen? Sind wir uns sicher? In Block
   2 bekommen wir das Werkzeug — aber Achtung: die in Block 1 gewonnenen
   Hypothesen dürfen nicht mit denselben Daten geprüft werden. Das wäre
   **HARKing** — mehr in Block 2."*
2. *„Außerdem: gilt unser Modell auch für Stores, die wir gar nicht
   beobachtet haben? In Block 3 lernen wir die Werkzeuge der
   Out-of-Sample-Generalisierung."*

**Lab/Demo-Konfiguration S1 (Sommer-Iteration):**
- **Moodle-Vorbereitungsaufgabe (vor S1):** R + RStudio + Quarto
  installieren, Test-Projekt anlegen, Test-Quarto rendern. Screencast als
  Anleitung. Setup-Probleme werden so aus der Live-Session ausgelagert.
- **Demo (20 Min) — R-Projekt und OJ-Import:** Dozent zeigt den fertigen
  Reproducibility-Workflow am OJ-Datensatz, inkl. Codebook-Erstellung.
  Anker für **Data-Governance-Dimension 5 (Lifecycle & Reproducibility)**
  und **Dimension 3 (Metadata)**.
- **Demo (20 Min) — Wrangling roher OJ-Daten:** chr → fct, `brand_tier`
  konstruieren, Anomalien diskutieren. Demo, weil Code gestellt wird und
  Studierende die Logik nachvollziehen, nicht selbst konstruieren.
- **Lab (25 Min) — EDA auf der gewrangelten Datei.**
- **Demo (15 Min) — einfache Regression** als Übergang zur multiplen
  Regression. Demo, weil keine eigene Aha-Erkenntnis, sondern Brücke.
- **Lab (25 Min) — Multiple Regression mit Interaktionsterm.**

### Block 2 — Session 2: Hypothesen prüfen (Sample)

**CRISP-DM:** Phasen 4–5 (Modeling, Evaluation)
**Modus:** stochastisch, inferentiell.
**Notation:** Stocker-Schätzfunktionen (β̂) und Realisationen (b).

**Inhalte:**
- Wechsel von „Population" zu „Sample" — Stocker'sches Reframing:
  derselbe Datensatz, andere Frage
- **Methodische Hierarchie in S2: Resampling im Vordergrund.**
  Der Resampling-Ansatz (Bootstrap, Permutationstest) wird als
  *Primärwerkzeug* der Inferenz eingeführt, weil er ohne
  Verteilungsannahmen auskommt und damit der robuste Standard ist.
  Klassische Tests (t-Test, ANOVA, F-Test) werden über das
  Only-One-Test-Prinzip als *Spezialfälle der Regression* eingeordnet
  — sie funktionieren nur unter Annahmen. Studierende lernen die
  *Prüfung* der Annahmen-Verletzung (Diagnostik), aber **nicht die
  Konsequenzen** der Verletzung — diese kommen erst in S3 im BLUE-
  Kontext. Bei erkannter Verletzung: Rückfall auf Resampling als
  Lösung.
- Sampling Distribution, Bootstrap mit infer-Pipe
- Permutationstest als verteilungsfreie Alternative
- Konfidenzintervalle mit infer-Pipe
- p-Wert-Logik, p-Hacking, **HARKing**
- Type-1- und Type-2-Error
- FDR und Expected Value Framework im Kontext der
  Multiplicity-Korrektur
- Klassische Tests als Spezialfälle der Regression: t-Test = `lm(y ~
  x_binary)`, ANOVA = `lm(y ~ x_factor)` (Only-One-Test, ModernDive)
- **Multiple Regression mit Inferenz: infer-Pipe `fit()` als Workflow
  inklusive Regressionsdiagnostik der A-B-C-Annahmen (Auer) als
  Validitäts-Check.** VIF für C2 (Multikollinearität), plot(lm) für
  die B-Annahmen, Aufmerksamkeit auf A1 (Omitted Variable,
  Domänenwissen). Die Annahmen sind die Voraussetzung für die
  Gültigkeit der *klassischen* Inferenz — Diagnostik gehört konzeptionell
  in S2, weil die Annahmen hier eingeführt werden. **Die Konsequenzen
  einer Annahmenverletzung (Bias, Effizienz-Verlust, etc.) werden erst
  in S3 im BLUE-Kontext behandelt** — in S2 reicht die Erkenntnis:
  *„Annahmen verletzt → Rückfall auf Resampling als robuste
  Alternative."*
- **HARKing-Auflösung in zwei Schritten:**
  - Schritt 1: Sample-Splitting an Store 51 (mit erkannten Schwächen)
  - Schritt 2: Replikation an Store 50 als methodisch tiefere Lösung
- Translator-Empfehlung mit Unsicherheitsquantifizierung

**Cliffhanger am Ende:**

> *„Wir haben jetzt das volle inferenzstatistische Werkzeug — Resampling
> als robusten Standard, klassische Tests als Spezialfall. Bei einigen
> der OJ-Annahmen-Checks ist uns aufgefallen, dass Annahmen verletzt
> sind. Was passiert dann eigentlich mit unseren Schätzungen? Diese
> Frage öffnet sich erst in S3 im BLUE-Kontext. Ebenso bleiben zwei
> Geschäftsfragen offen: Erstens — gilt unser Modell auch für Stores,
> die wir gar nicht beobachtet haben? Zweitens — wenn der Werbe-Effekt
> 0,25 beträgt — was passiert, wenn wir morgen Werbung machen? Hier
> verlässt uns die klassische Statistik. In Session 3 brauchen wir
> neue Werkzeuge."*

**Lab/Demo-Konfiguration S2 (Sommer-Iteration):**
- **Lab (20–25 Min) — Bootstrap auf Werbe-Effekt:** infer-Pipe für CI auf
  Store 51, Konzept der Sampling Distribution operativ erfahren.
- **Demo (10 Min) — Permutationstest** als verteilungsfreie Alternative.
  Demo, weil Code-Analoge des Bootstrap, schnell gezeigt.
- **Demo (15 Min) — Only-One-Test live an OJ:** t-Test = ANOVA = Regression
  geben dieselben Ergebnisse. Demo, weil die didaktische Aussage im
  *Vergleich* der Outputs liegt.
- **Lab (30 Min) — Multiple Regression mit Diagnostik:** infer-Pipe `fit()`
  + VIF + plot(lm). Lab, weil Diagnostik *erlernt* wird (zentrales Lernziel
  von S2 ab Sommer).
- **Demo (15–20 Min) — Replikation Store 50:** dieselbe Regression an
  unabhängigen Daten. Demo, weil Code aus dem vorherigen Lab wiederverwendet
  wird — der Lehrwert liegt in der *Reflexion* (Vergleichs-Steckbrief, was
  bedeutet die Replikation für die Empfehlung?). Aktivierende
  Reflexions-Aufgabe innerhalb der Demo.

### Block 3 — Session 3: Vorhersagen oder Eingreifen

**CRISP-DM:** Phasen 5–6 (Evaluation, Deployment)
**Modus:** generalisierend, mit zwei methodischen Pfaden, die auf einer
gemeinsamen BLUE-Brücke aufbauen.

**Inhalte:**
- Big-Picture-Folie: Komplexität, Generalisierung, Kausalität
- Two Worlds of Regression als Recap aus S2
- **BLUE-Brücke (gemeinsam für beide Pfade) — Auer-Didaktik:**
  - Gauss-Markov-Theorem: Voraussetzungen und BLUE-Eigenschaft
  - Bias-Variance-Tradeoff
  - **Konsequenzen der Annahmenverletzung** (Bias, Effizienz-Verlust)
    — Einlösung des Cliffhangers aus S2: dort wurde die *Prüfung* der
    Annahmen gelernt, hier kommt die *Konsequenz* der Verletzung
  - Wann BLUE relevant ist (Inferenz/Erklären) und wann nicht (Prädiktion)
- **Pfad A — Predictive Modeling:**
  - Out-of-Sample, Train/Test, K-Fold-CV
  - Multikollinearität in der Demografie (eingelöste Vorausschau aus S1)
  - Lasso, Ridge, λ-Wahl per CV (Taddy-Pfad)
  - Lucas-Kritik als Geltungsgrenze
- **Pfad B — Causal Modeling:**
  - Datenherkunft als Identifikationsproblem (Querschnitt, Längsschnitt,
    Panel; Survey, Experiment, natürliches Experiment)
  - Selbstselektion als Data-Governance-Thema (Dimension 4)
  - **Exogenität per Annahme** (statistische Perspektive): MLR.4 in
    Beobachtungsdaten, Wooldridge-Welt
  - **Exogenität per Studiendesign**: RCT, IV, DiD — Hernán-Position
  - Lucas-Kritik als Trennlinie zwischen Vorhersage und Eingriff
  - Korrelation vs. Kausalität, DAGs
  - A/B-Testing als RCT (Cohen's d aus PVA2)
  - IV, DiD, RDD als Outlook
- Translator-Empfehlung: zwei verschiedene Antworten je nach Pfad
- **Outlook ER017:** Logit / GLM, Causal ML, Random Forests, Mehrebenen
- **Outlook Zeitreihen:** als methodische Erweiterung erwähnt
- **Data-Gap-Auflösung:** *„Welche Daten bräuchten wir für Kausalität?
  Antwort: ein A/B-Test."* — explizit als AI-Value-Creator-Botschaft

**Lab/Demo-Konfiguration S3 (Sommer-Iteration):**
- **Lab 1 (35–40 Min) — BLUE-Brücken-Lab:** Translator-Auftrag, Diagnostik-
  Anwendung (aus S2 bekannt), OOS / K-Fold-CV. Lab, weil Studierende den
  Pfad-Wechsel selbst nachvollziehen müssen.
- **Demo (20 Min) — Regularisierung auf Demografie:** Dozent führt durch
  Lasso/Ridge/Elastic Net. Demo, weil Code-Boilerplate hoch und der Lehrwert
  im *Vergleich der Ergebnisse* liegt, nicht im Coding-Üben.
- **Lab 2 (25 Min) — A/B-Test-Design für den Werbe-Effekt:** Studierende
  skizzieren das Design selbst — selbstständiges Denken ist hier das
  Lernziel.

---

## 3. Daten-Architektur

### 3.1 Wahl des Datensatzes: Dominick's OJ (`bayesm::orangeJuice`)

**Begründung:**
- *Realität:* echte Scanner-Daten (Booth Kilts Center)
- *Business-Relevanz:* Marketing-Mix (Marken, Preis, Werbung) — intuitiv
  für alle BWL-Studierenden ohne Detail-Vorwissen
- *Marken-Story:* Tropicana (Premium) / Minute Maid (Mainstream) /
  Dominick's (Eigenmarke)
- *Datenkuratierungs-Lehrmomente* eingebaut
- *Multikollinearität* in den Demografie-Variablen sichtbar
- *Confounding* in `feat × deal` strukturell vorhanden — natürlicher
  Übergang zu Pfad B
- *Drei Auflösungen* möglich

**Bundesliga** wird zur Hausaufgabe und/oder zum Final-Project-Datensatz.
Sport-Bezug bleibt erhalten, aber nicht als Präsenz-Datensatz.

### 3.2 Lehr-Datensätze über die Sessions

**Datenstrategie:** Wir nutzen *einen Hauptstore* (Store 114) als
durchgängigen Anker über S1, S2, S3 — und *einen Kontrast-Store*
(Store 83), der die Replikations- und Inferenz-Frage didaktisch
ausschöpft (siehe 3.7).

| Datensatz | Beschreibung | n | Verwendung |
|---|---|---|---|
| `dominicks_store114_raw.rds` | Store 114, bewusst unsauber (chr, Locale) | ~351 | S1 Lab 1 (Wrangling) |
| `dominicks_store114.rds` | tidy, nach Wrangling-Demo | ~351 | S1 Labs 2–4, S2 Teil 1 |
| `dominicks_store83.rds` | tidy, Kontrast-Store für Replikation | ~333 | S2 Teil 2 (Replikation) |
| `dominicks_full.rds` | alle 83 Stores × Top-3-Marken | ~28k | S3 (Vollpanel-Skalierung, Pfad A Lasso) |
| `dominicks_demo.rds` | Demografie aller 83 Stores | 83 | S3 (Multikollinearität) |
| Bundesliga | Hausaufgabe / Final Project | ~600 | parallel |

**Begründung der Store-Wahl** (siehe 3.7 für die analytische Herleitung):

- **Store 114:** Composite-Score-Rang 1 von 50 (multi-kriterieller Vergleich
  aller 83 Stores). Plausibler Werbe-Effekt (`feat`-Koef ≈ 0.94 naive,
  ≈ 0.79 mit `deal`). **OVB-Differenz +0.145** — der feat × deal-Stolperstein
  ist lokal **sichtbar**, anders als bei Stores 50/51. Saubere Diagnostik
  (VIF < 3), genug Wrangling-Reichtum (4 missing weeks).

- **Store 83:** Kontrast-Store. Werbe-Effekt ≈ 1.07. **OVB-Differenz −0.028**
  — Vorzeichen kehrt sich um gegenüber Store 114! Brand-Heterogenität
  1.5× höher als Store 114 (0.298 vs. 0.194). Damit didaktisch maximal
  ergiebig: Replikation widerspricht erstem Befund.

### 3.3 Roher Datensatz für S1: bewusst unsauber

Der Datensatz für Lab 1 ist absichtlich unsauber formatiert:

- `brand` als chr ("Brand_1", "Brand_5", "Brand_10") statt Faktor
- `feat`, `deal` als chr ("Yes"/"No") statt 0/1
- `week` als chr ("Week 40") statt numerisch
- evtl. Preise im deutschen Locale (Komma) zur Locale-Diskussion

Die Wrangling-Demo putzt diesen Datensatz mit gestelltem Code, der
vollständig kommentiert ausgeliefert wird. Lehrpunkte:
- Faktor-Erkennung und ordinale Skalierung mit *konstruierter* Variable
  `brand_tier` (siehe 3.4)
- Refresher Skalenniveau und R-Datentypen
- Codebook am Ende der Demo
- Daten-Governance als Selbstverständnis

### 3.4 Konstruierte Variable: `brand_tier`

Der Datensatz hat keine native ordinale Variable. Aus didaktischen
Gründen wird eine *konstruierte* Variable `brand_tier` aus `brand`
abgeleitet, die die Marken-Strategien in eine Hierarchie bringt:

| brand | Realer Name | brand_tier (konstruiert) |
|---|---|---|
| 10 | Dominick's 64oz (Eigenmarke) | Discount / Private Label |
| 5 | Minute Maid 64oz | Mainstream / Medium |
| 1 | Tropicana Premium 64oz | Premium / Markenprodukt |

Diese Variable ist *keine native Datensatz-Eigenschaft*, sondern eine
inhaltliche Konstruktion auf Basis der bekannten Markenpositionierung.
Sie wird in der Wrangling-Demo *konstruiert* — und der Lehrpunkt dabei
ist: *„Aus inhaltlicher Sicht wissen wir, dass diese drei Marken eine
Hierarchie haben. Wir kodieren das explizit als ordinale Variable. Das
hat Konsequenzen für Plot-Reihenfolgen und für Polynom-Kontraste in der
Regression."*

### 3.5 Datenkuratierungs-Lehrmomente (im Datensatz strukturell vorhanden)

| Lehrpunkt | Variable | Sektion |
|---|---|---|
| Datentypen-Kuratierung | chr → fct, chr → numeric | Wrangling-Demo S1 |
| Ordinale Codierung (konstruiert) | brand → brand_tier | Wrangling-Demo S1 |
| Messfehler vs. Aggregations-Artefakt | feat ∈ (0,1) (~2% der Daten) | Wrangling-Demo S1 |
| Echte Promo-Spitzen vs. Datenfehler | logmove-Spitzen mit feat=1 | EDA in S1 |
| Semantische Klarheit von Variablen | profit (Einheit unklar) | Wrangling-Demo S1 |
| **Metadaten-Lehrmoment: Einheiten** | `price` per-oz vs. per-container | Wrangling-Demo S1 |
| Strukturelles Missing | Wochen-Lücken (4 in Store 114, 7 in Store 83) | EDA in S1 |
| Strukturelle Konfundierung | feat=1 ⟹ deal=1 (global perfekt) | Vorbereitung S2 → Pfad B in S3 |
| Multikollinearität in Demografie | EDUC ↔ HVAL150 (r=0.89) | Vorausschau S1 → Auflösung S3 |
| Time-Series-Leakage | bei zeitlichem Sample-Splitting | S2 Sample-Splitting-Demo |

**Vertiefung: Metadaten-Lehrmoment zu `price`-Einheiten.**

Die `bayesm::orangeJuice`-Dokumentation gibt für die 11 Marken
unterschiedliche Container-Größen an:

- Marken 1, 5, 10: alle 64 oz
- Marke 2: 96 oz
- Marken 6, 11: 96 oz bzw. 128 oz

Die Roh-`price`-Spalten enthalten Preise *pro Unze*. Wenn wir mit
Container-Preisen rechnen wollen ($1.79 statt $0.028), müssen wir
umrechnen — für unsere drei Top-3-Marken einheitlich `× 64`.

**Lehrpunkt für S1:** *„Was bedeutet `price` eigentlich? Wo steht das?
Wie rechnen wir um?"* — Operationale Anwendung von **Dimension 3
(Metadata)** der Data Governance: ohne Dokumentation der Einheiten ist
*derselbe* Datenpunkt nicht interpretierbar. Skaleninvarianz der
log-Regression (Slope bleibt gleich, nur Intercept verschiebt sich)
ist gleichzeitig ein methodischer Nebenpunkt.

### 3.6 S1 Lab 1 (R-Projekte, Quarto, Reproduzierbarkeit) — Status

S1 Lab 1 *„Arbeiten mit R-Projekten, Dokumentation und Quarto"* steht
weitgehend bereit. Es muss in der Sommer-Iteration nur *um den Import
der OJ-Daten ergänzt* werden — das wird gleichzeitig zum Anker für die
**Dimension 5 (Data Lifecycle / Reproducibility)** der Data Governance:

- R-Projekt einrichten
- OJ-Daten importieren (aus bayesm extrahieren oder gestelltes
  `.rds` laden)
- Quarto-Reproduzibilität: dieser Datenstand ist *auch in 5 Jahren*
  noch reproduzierbar, weil er versioniert vorliegt
- Codebook erstellen (auch ein Anker für **Dimension 3 (Metadata)**)

Das Lab wird damit zu einem zweifachen Data-Governance-Anker: Lifecycle
(Reproduzibilität) und Metadata (Codebook).

**Hinweis:** Die Überarbeitung dieses Labs hat noch Zeit. Im
Vordergrund steht jetzt Session 3 des laufenden Semesters mit Folien
und Labs/Demos.

### 3.7 Multi-kriterielle Store-Auswahl — analytische Herleitung

Statt einer intuitiven Wahl wurde die Hauptstore-Selektion **datenbasiert**
durchgeführt: alle 81 Stores mit n ≥ 300 wurden in einem
Multi-Kriterien-Verfahren bewertet.

**Bewertungs-Dimensionen:**

| Kriterium | Operationalisierung | Gewicht im Composite |
|---|---|---|
| OVB-Sichtbarkeit | Differenz feat-Koef mit/ohne deal | 40% |
| Plausibilität | feat-Koef im Bereich [0.5, 1.0] | 20% |
| Brand-Heterogenität | SD-Spread der logmove zwischen Marken | 20% |
| Sample-Größe | n / 400 | 20% |

Zusätzlich Filter-Kriterien (alle erfüllt sein):
feat-Koef in [0.5, 1.2], OVB-Differenz > 0.005, VIF < 10, n ≥ 300.

**Ergebnis:** 50 von 81 Stores qualifizierten sich. Top 5 Allrounder:

| Rang | Store | n | feat naive | feat full | OVB diff | brand_sd | Composite |
|---|---|---|---|---|---|---|---|
| **1** | **114** | 351 | 0.935 | 0.791 | **+0.145** | 0.194 | **0.841** |
| 2 | 121 | 354 | 0.826 | 0.698 | +0.128 | 0.233 | 0.818 |
| 3 | 131 | 348 | 0.920 | 0.786 | +0.134 | 0.180 | 0.798 |
| 4 | 103 | 345 | 0.895 | 0.758 | +0.137 | 0.132 | 0.775 |
| 5 | 101 | 363 | 0.899 | 0.802 | +0.097 | 0.200 | 0.711 |

Zum Vergleich (Stores aus früheren Iterationen):
- **Store 50:** Rang 41/50 — OVB diff +0.022, composite 0.446
- **Store 51:** **gefiltert raus** — OVB diff −0.006 (Stolperstein lokal unsichtbar)

**Entscheidung:** Store 114 als Hauptstore wegen höchstem
Composite-Score und maximaler OVB-Sichtbarkeit.

### 3.8 Kontrast-Store für Replikation und Inferenz — Store 83

Statt eines "ähnlichen" Replikations-Partners wählen wir bewusst einen
*kontrastierenden* Store. **Store 83** zeigt:

| Metrik | Store 114 | Store 83 |
|---|---|---|
| feat-Koef (naive) | 0.935 | 1.073 |
| feat-Koef (mit deal) | 0.791 | 1.101 |
| **OVB-Differenz** | **+0.145** | **−0.028** |
| brand-Heterogenität | 0.194 | **0.298** (1.5×) |
| missing weeks | 4 | 7 |

**Didaktischer Wert:** Das Vorzeichen des OVB-Effekts **dreht sich**
zwischen den beiden Stores. Das ist nicht zufällig: es zeigt, dass die
Confounding-Struktur *lokal* heterogen ist, obwohl sie *global* (im
Vollpanel) klar positiv ausfällt (OVB +0.038).

**Aha-Sequenz, die das ermöglicht:**

1. *Inferenz an Store 114:* Bootstrap-KI für feat-Koef oder OVB-Effekt.
   Resultat: signifikant von 0 verschieden. Innerhalb von Store 114
   ist der Effekt "sicher".
2. *Replikation in Store 83:* derselbe Bootstrap-Mechanismus, anderer
   Store. Der OVB-Effekt zeigt dort das **Gegenvorzeichen**.
3. *Schlussfolgerung:* Statistische Signifikanz innerhalb eines Samples
   beweist nicht externe Validität. Replikation ist *Stresstest*, nicht
   *Bestätigungsritual*. Das ist die operative Erfahrung der
   **Replikationskrise**.

Brand-Heterogenität in Store 83 (1.5× höher) macht ihn zusätzlich zu
einem reicheren EDA-Datensatz für S1 — eine vertiefte Marken-Story
ist möglich.

### 3.9 Population vs. Sample auch im Vollpanel

Wichtiger methodischer Anker für S3:

> *Auch die 83 Stores sind keine Population im Sinne der
> klassischen Inferenz.*

Es handelt sich um:
- die 83 Stores **eines** Filialnetzes (Dominick's, Chicago)
- in einem **bestimmten** Zeitraum (1989–1994)
- mit **einer** spezifischen Marken-Auswahl (Top-3 von 11)

Wer aus dem Vollpanel auf "OJ-Märkte allgemein" schließt, macht eine
extra-statistische Generalisierung. Inferenz vom Sample 114/83 auf
das Vollpanel ist eine andere Operation als vom Vollpanel auf
"Supermärkte weltweit".

**Lehrpunkt:** *Big data ist nicht automatisch Population.* In S3 wird
diese Grenze explizit thematisiert, bevor wir mit dem Vollpanel als
"unsere beste verfügbare Annäherung an die Realität" weiterarbeiten.

---

## 4. Vernetzung der Blöcke

### 4.1 Roter Faden über die drei Sessions: Population-Sample-Frage operationalisiert

Mit der Wahl Store 114 als Hauptstore und Store 83 als Kontrast-Store
entsteht eine **inhaltliche Längsspannung** über alle drei Sessions:

**S1 — Beschreiben:** Studierende lernen Store 114 deskriptiv kennen.
Sie sehen Werbe-Effekt, feat × deal-Verschränkung, Marken-Heterogenität.
Implizit behandeln sie den Store als "die Realität".

**Cliffhanger S1 → S2:**

> *„Was wir beschrieben haben, sind die Daten EINES Stores — Store 114.
> Wir behandeln ihn implizit als 'repräsentativ' für Dominick's gesamtes
> Filialnetz. Aber: dürfen wir das? Wenn Store 114 nur eine zufällige
> Stichprobe wäre, könnten wir Inferenz machen — KI, Tests. Wenn er
> hingegen eine spezifische Filiale mit eigenen Charakteristika ist,
> sagt uns Inferenz nichts über andere Stores aus."*

**S2 — Hypothesen prüfen:** Inferenz an Store 114. Bootstrap-KI für
feat-Koef, OVB-Differenz. Resultat: Effekte sind signifikant.
**Aber:** Replikations-Test in Store 83 zeigt das **Gegenvorzeichen**.
Studierende erleben Replikation als Stresstest.

**Cliffhanger S2 → S3:**

> *„Eine Replikation reicht nicht für eine Entscheidung. Wenn wir alle
> 83 Stores ansehen — was sagt das große Bild? Und: was, wenn Sarah
> morgen wissen will, was in einem Store passiert, den wir NIE gesehen
> haben? Hier verlässt uns die klassische Inferenz. Wir brauchen das
> BLUE/Bias-Variance-Werkzeug und Out-of-Sample-Validierung."*

**S3 — Vorhersagen oder Eingreifen:** Skalen-Sprung aufs Vollpanel.
BLUE-Brücke. OOS-Validierung. Aber: auch das Vollpanel ist **keine
Population im strengen Sinn** (siehe 3.9) — dort ist die Grenze auch
mit "big data" nicht überschritten.

### 4.2 Die zwei klassischen Cliffhanger als Querverbindungen

Cliffhanger A: **HARKing** (S1 → S2). Daten zum Finden trennen von Daten
zum Bestätigen.

Cliffhanger B: **Out-of-Sample** (S1 → S3). Daten zum Trainieren trennen
von Daten zum Validieren.

Diese zwei Cliffhanger sind strukturell dieselbe Idee aus zwei
Perspektiven. Mit der Hauptstore/Kontrast-Store-Architektur werden
sie operativ greifbar: Store 114 = Trainingsdaten/Finden, Store 83 =
Validierungs-Lokus für externe Gültigkeit.

### 4.3 Multikollinearitäts-Vorausschau

In S1 Lab 2 (EDA) werden die Demografie-Variablen erwähnt, ohne aktiv
im Modell zu sein. Aufgriff in S3.

---

## 5. Data Governance — fünf Dimensionen

Dieser Block ist neu strukturiert nach **DAMA-DMBOK-Anschluss**. DAMA
International ist mit dem *Data Management Body of Knowledge* (DMBOK)
die globale Standard-Referenz für Data Governance. Sie strukturiert
Datenmanagement in 11 Wissensgebiete um Data Governance als Zentrum
(DAMA-Wheel).

Für ER114 wird daraus eine **didaktisch handhabbare Fünf-Dimensionen-
Variante** abgeleitet, die zur konsensuellen Standardliteratur (DAMA,
DataGalaxy, Atlan, Databricks, Informatica) konsistent ist.

### 5.1 Die fünf Dimensionen

| # | Dimension | Lehrfrage | DMBOK-Anschluss |
|---|---|---|---|
| **1** | **Data Strategy / Data Gap** | Welche Daten *bräuchten* wir, um welche Frage zu beantworten? | Knowledge Area 1 (Data Governance / Strategy) |
| **2** | **Data Quality** | Sind unsere Daten *fit for purpose*? | Knowledge Area 9 (Data Quality) |
| **3** | **Metadata** | Verstehen wir, *was* unsere Daten bedeuten? | Knowledge Area 10 (Metadata Management) |
| **4** | **Data Stewardship & Lineage** | Wer ist *verantwortlich*? Woher kommen die Daten? | Knowledge Area 11 (Data Quality + Lineage) |
| **5** | **Data Lifecycle & Reproducibility** | Können wir den Datenstand *rekonstruieren*? | Knowledge Area 6 (Storage), Open Science |

### 5.2 Visualisierung als Wheel

Die fünf Dimensionen werden in den S1-Folien als **Wheel-Visualisierung**
dargestellt — analog zum DAMA-DMBOK-Wheel, aber didaktisch reduziert:

```
                [ Data Strategy / Data Gap ]
                            |
   [ Metadata ]  ─── [ Data Governance ] ─── [ Data Quality ]
                            |
     [ Stewardship & Lineage ]  ─── [ Lifecycle & Reproducibility ]
```

Mit *Data Governance* als Zentrum, fünf Speichen drumherum. In jeder
Session wird die jeweils aktive Dimension hervorgehoben.

### 5.3 Mapping auf OJ-Daten — Status

| Dimension | Was wir mit OJ haben | Wo es im Kurs auftaucht | Status |
|---|---|---|---|
| **1. Data Strategy / Gap** | Beobachtungsdaten reichen nicht für Werbe-Effekt-Identifikation (`feat × deal`-Verschränkung) | S3 Pfad B: *„Welche Daten bräuchten wir für Kausalität? Antwort: A/B-Test."* | ✓ stark verankert |
| **2. Data Quality** | feat ∈ (0,1) als Anomalie, Wochen-Lücken in Store 51, niedrige Preise (Promo vs. Datenfehler) | S1 Wrangling-Demo | ✓ stark verankert |
| **3. Metadata** | `profit`-Variable mit unklarer Einheit, brand-Codes erst per Doku-Lookup verständlich, Codebook-Erstellung am Ende der Wrangling-Demo | S1 Wrangling-Demo + Codebook-Tabelle, S1 Lab 1 (Codebook-Erweiterung) | ✓ stark verankert |
| **4. Data Stewardship & Lineage** | bayesm-Paket → Booth Kilts Center → Dominick's-Scanner-Daten der 1990er — eine echte Datenlieferkette | S1 Wrangling-Demo (Datenherkunft erklären), S3 Pfad B (Selbstselektion vs. Designdaten) | ⚠ zu entwickeln: Datenherkunfts-Folie in S1 mit der bayesm-Lieferkette |
| **5. Data Lifecycle & Reproducibility** | Quarto-Reproduzierbarkeit, fixe `.rds`-Dateien, R-Projekt-Struktur (S1 Lab 1) | S1 Lab 1 (bestehend, mit OJ-Import erweitern), S3 Outlook | ✓ S1 Lab 1 ist der natürliche Anker (siehe 3.6) |

### 5.4 Strategische Sicht: Daten als Asset

In S3, beim Übergang zur Big-Data-Welt (Pfad A): Dominick's hat
proprietäre Scanner-Daten — Asset-Charakter, AI-Value-Creator-Position.
*„Andere Supermarktketten haben diese Daten nicht. Das ist
Wettbewerbsvorteil."*

---

## 6. Zentrale Literatur

### 6.1 Notations- und Konzept-Quellen — Kursbasis

| Werk | Rolle |
|---|---|
| **Stocker, H.** *Angewandte Ökonometrie, Kap. 3* (Innsbruck) | Notations-Standard durchgehend. PRF/SRF, β/β̂/b-Konvention. CEF als bester Prediktor. |
| **Auer, L. von (2023)** *Ökonometrie — Eine Einführung*, 8. Aufl., Springer Gabler, ISBN 978-3-658-42699-6 | Didaktik zu Gauss-Markov / BLUE. Praxisorientiert. |
| **Taddy, M. et al. (2023)** *Modern Business Analytics* | Modern-DS-Pfad. OOS, Lasso, CV, Regularisierung. |
| **Ismay, C., & Kim, A.** *Statistical Inference via Data Science (ModernDive)* | Inferenz-Pfad in S2. Only-One-Test, infer-Pipe. |
| **Provost, F., & Fawcett, T. (2013)** *Data Science for Business* | Translator-Rolle, Data Mining als induktives Lernen. |
| **Thomas, R., Howard, J., et al.** | AI User → AI Value Creator. |

### 6.2 Erweiterung — Causal Inference und Data Governance

Diese Quellen erweitern die Standard-Ökonometrie um die moderne
Causal-Inference-Position. Sie sind für S3 Pfad B maßgeblich.

| Werk | Beitrag |
|---|---|
| **Hernán, M. A., Hsu, J., & Healy, B. (2019)** *A Second Chance to Get Causal Inference Right.* Chance, 32(1) | Drei Modi (Description / Prediction / Counterfactual). Direkter Anschluss für die Pfad-B-Differenzierung. |
| **Hernán, M. A., & Robins, J. M. (2020)** *Causal Inference: What If* | Datenarten und Identifikationsstrategie integriert. Open Access. |
| **Angrist, J. D., & Pischke, J.-S. (2009)** *Mostly Harmless Econometrics* | Kausal-orientierte Ökonometrie. „The distinction between causal and non-causal regression has nothing to do with the regression itself, but with the assumptions about the unobservables." |
| **Pearl, J. (2009)** *Causality* | Mathematische Präzisierung: P(Y\|X) vs. P(Y\|do(X)). DAGs. |
| **Keele, L. (2015)** *The Statistics of Causal Inference: A View from Political Methodology.* Political Analysis, 23(3) | Integrierte Behandlung von Datenstruktur und Identifikationsstrategie. Anschluss für die Datenherkunfts-Folie in S3. |
| **Messing, S., et al.** *Cause and Inference in Practice* | Praxisorientierte Synthese. Datenarten + Selektionsbias + moderne Methoden. |
| **Lucas, R. E. (1976)** *Econometric Policy Evaluation: A Critique.* Carnegie-Rochester Conference | Trennlinie zwischen Vorhersage und Eingriff. |

### 6.3 Erweiterung — HARKing und Replikationskrise

| Werk | Beitrag |
|---|---|
| **Kerr, N. L. (1998)** *HARKing: Hypothesizing After the Results are Known.* Personality and Social Psychology Review | Originalreferenz. |
| **Gelman, A., & Loken, E. (2014)** *The Statistical Crisis in Science* | Garden of forking paths. HARKing als Forschungs-Ethik-Frage. |
| **Munafò, M. R., et al. (2017)** *A Manifesto for Reproducible Science.* Nature Human Behaviour | Replikation als Goldstandard. Anschluss für die HARKing-Lösung in S2. |
| **Open Science Collaboration (2015)** *Estimating the reproducibility of psychological science.* Science | Empirische Datenlage zur Replikationskrise. |
| **Shmueli, G. (2010)** *To Explain or to Predict?* Statistical Science, 25(3) | Theoretisches Fundament der Trennung erklären/vorhersagen. |

### 6.4 Data Governance — Standardliteratur

| Werk | Beitrag |
|---|---|
| **DAMA International** *DAMA-DMBOK: Data Management Body of Knowledge*, 2nd ed. | Globaler Standard für Data Governance. 11 Wissensgebiete um Data Governance als Zentrum. |
| **Ladley, J. (2019)** *Data Governance: How to Design, Deploy, and Sustain an Effective Data Governance Program*, 2nd ed. | Praktische Implementierung von DAMA-DMBOK in Organisationen. |
| **Knight, M.** Diverse Beiträge zu Data Governance | Praktische Operationalisierung der DAMA-Prinzipien. |

### 6.5 Praxis und Strategie

| Werk | Beitrag |
|---|---|
| **Schrage, M. (2014)** *The Innovator's Hypothesis* | A/B-Testing als Innovations-Tool. Anker für S3 Lab 3. |
| **Schrage, M. (2025)** *Philosophy Eats AI* | Strategische Sicht auf KI. |
| **Wooldridge, J. M. (2020)** *Introductory Econometrics*, 7th ed. | MLR.4 (Zero Conditional Mean) als Exogenitätsannahme. |
| **Hastie, T., Tibshirani, R., & Friedman, J. (2009)** *The Elements of Statistical Learning* | Bias-Variance-Tradeoff, Train/Test, CV. |

### 6.6 Konsistenz der Notation

Über den gesamten Kurs wird **Stocker-Notation** verwendet:
- Population (deterministisch, unbekannt): β₁, β₂, εᵢ, σ²
- Schätzfunktionen (Zufallsvariablen, ex ante): β̂₁, β̂₂, ε̂ᵢ
- Realisationen (deterministisch, ex post): b₁, b₂, eᵢ
- PRF: yᵢ = β₁ + β₂xᵢ + εᵢ
- SRF (ex post): yᵢ = b₁ + b₂xᵢ + eᵢ
- CEF: E(y\|x) = m(x) — der bester Prediktor

In Block 1 ausschließlich Realisationen.
In Block 2 wechselt die Perspektive zur ex-ante-Sicht.

---

## 7. Aha-Momente und Anker

### 7.1 Block 1

| Aha-Moment | Hebel im Datensatz |
|---|---|
| Variablen müssen verstanden werden, bevor sie modelliert werden | profit-Einheit, feat-Anomalien |
| **Metadaten entscheiden über die Interpretation** | price per-oz vs. per-container (× 64) |
| Skalenniveau bestimmt die Methodenwahl | brand_tier als ordinaler Faktor (konstruiert) |
| Multiple Regression verschiebt Koeffizienten — auch ohne Inferenz | Werbe-Effekt mit/ohne Brand-Kontrolle |
| log-Transformation ist nicht Reflex, sondern Antwort auf eine Frage | sales rechtsschief (multiplikativer DGP), price symmetrisch (additiv) |
| Daten-Putzen ist methodische Tätigkeit | Wrangling-Demo, Codebook |
| Reproduzierbarkeit beginnt beim R-Projekt | S1 Lab 1: Quarto + R-Project + .rds |
| **Ein Store ist nicht "die Realität"** | Implizite Population-Frage am Ende von Block 1 |

### 7.2 Block 2

| Aha-Moment | Hebel im Datensatz |
|---|---|
| Population kann unbeobachtbar sein — auch wenn die Daten „alle" sind | Stocker-Reframing am Übergang zu Block 2 |
| Resampling braucht keine Verteilungsannahmen — klassische Tests schon | Bootstrap-CI vs. t-Test am Werbe-Effekt |
| t-Test = ANOVA = Regression. *Eine* Maschine. | Werbe-Effekt: t.test, lm, infer geben dasselbe |
| Annahmen-Verletzung erkennen — aber die *Konsequenz* erst in S3 | plot(lm) zeigt Heteroskedastizität, was nun? → S3 |
| HARKing macht naive Hypothesentests ungültig | S1-Hypothese an S1-Daten testen ist zirkulär |
| Sample-Splitting hat Grenzen bei kleinen Stichproben | 80/20 von n=351 ergibt Test-n=70 |
| **Replikation ist Stresstest, nicht Bestätigungsritual** | Store 114 OVB +0.145 vs. Store 83 OVB −0.028 — **Vorzeichen kehrt sich um** |
| **Statistische Signifikanz ≠ externe Validität** | Bootstrap-KI für OVB-Effekt in 114 ist signifikant — und doch widerspricht Store 83 |

### 7.3 Block 3

| Aha-Moment | Hebel im Datensatz |
|---|---|
| Annahmen-Verletzung hat *Konsequenzen* — Einlösung des S2-Cliffhangers | Bias, ineffizient, BLUE-Eigenschaft verloren |
| **Vollpanel mittelt lokale Heterogenität** | Store 114 (+0.145) + Store 83 (−0.028) → Pool +0.038 als strukturelle Wahrheit |
| **Big data ist nicht Population** | Auch 83 Stores sind ein Sample (ein Filialnetz, ein Zeitraum) |
| BLUE optimiert Schätzer-Eigenschaften — nicht Vorhersage-Performance | Bias-Varianz-Tradeoff, Ridge ist *nicht* BLUE |
| Mehr Variablen = besseres In-Sample-R². Aber nicht besseres OOS-R² | Lasso-Demo auf 12 Demografie-Variablen |
| Multikollinearität diagnostizieren *oder* strukturell lösen | EDUC ↔ HVAL150 |
| Beobachtungsdaten reichen nicht für Kausalität | feat=1 ⟹ deal=1 |
| Exogenität per Annahme vs. per Studiendesign | Wooldridge MLR.4 vs. RCT/IV/DiD |
| Lucas-Kritik trennt Vorhersage und Eingriff | Werbe-Effekt-Stabilität bei Eingriff |
| Data Gap als strategische Frage | „Welche Daten bräuchten wir für Kausalität?" |

---

## 8. Didaktische Prinzipien (kursweit)

### 8.1 Inhaltliche Setzungen

1. **Inferenz-frei in S1.** Keine p-Werte, kein Konfidenzintervall,
   `se = FALSE` durchgehend.
2. **Stocker-Notation kursweit.** β/β̂/b-Trennung von Anfang an.
3. **Lehrökonomie durch Datensatz-Konsistenz.** Ein Datensatz, drei
   Auflösungen, durchgehende Story.
4. **Daten-Governance als Selbstverständnis.** Wrangling als Lehrmoment.
5. **Translator-Empfehlungen am Lab-Ende.** Jede Methode endet mit einem
   Memo.
6. **Forward References explizit.** Cliffhanger benannt.
7. **Brücken-Folie als visueller Anker** in jeder Session.
8. **Data-Governance-Wheel** in S1 als visueller Anker, mit Beispielen
   pro Dimension über den Kursverlauf verteilt.

### 8.2 Folien-Prinzipien

9. **Folien primär visuell.** Drei Funktionen:
   - Motivation
   - Big Picture
   - Aufmerksamkeit auf Stolpersteine
   
   Folien sind kein Manuskript. Details gehören ins Lab.
10. **Neutrale Sprache.** Kein „ich/Du", allenfalls „wir".
11. **Action Titles als Ziel** (nicht als Pflicht). Wenn möglich und
    didaktisch sinnvoll: jeder Folientitel macht eine Aussage. Wenn
    nicht durchgehalten werden kann, lieber descriptive Titles als
    halbgar-action.

### 8.3 Lab- und Demo-Prinzipien

12. **Labs als ausführliche Manuskripte** mit klarer Leserführung.
    Lab-Manuskripte sind gleichzeitig **das Nachbereitungs-Material** —
    es gibt *keinen separaten Folienstand* für Nachbereitung. Folien sind
    Live-Werkzeug, Labs sind Nachschlagewerk.

13. **Faltbare Callout-Boxen** für Live-Sessions kompakt, für
    selbstständiges Nacharbeiten vollständig.

14. **Demo-Definition.** Eine *Demo* ist ein vom Dozenten geführter Lab-
    Durchgang, bei dem Studierende mittippen oder zuschauen. Sie kommt zum
    Einsatz, wenn:
    - Code-Boilerplate hoch ist und der Lehrwert *nicht* im Coden, sondern
      im *Ergebnis-Vergleich* oder in der *Workflow-Demonstration* liegt
    - ein Übergangs- oder Wiederholungs-Inhalt vorliegt, der keinen eigenen
      Aha-Moment braucht
    - das Risiko, dass Studierende technisch hängen bleiben, didaktisch zu
      teuer wäre (z.B. Setup-Tätigkeiten)

    Demos sind *nicht* einfach „kürzere Labs". Sie haben ein eigenes
    didaktisches Profil: der Dozent zeigt aktiv auf Stolpersteine, die im
    Manuskript in Callout-Boxen für die Nachbereitung dokumentiert sind.

15. **Setup-Inhalte als Moodle-Vorbereitung.** Reine Setup-Tätigkeiten
    (Installation, Projekt-Anlegen, Test-Renders) gehören *nicht* in die
    Live-Session, weil sie Frustrationsrisiko ohne kompensierenden
    didaktischen Gewinn erzeugen. Sie wandern in eine Moodle-
    Vorbereitungsaufgabe vor der ersten Session, mit Screencast als
    Anleitung.

16. **Lab-Schema-Datei** als verbindliche Referenz für die
    Lab-Entwicklung (separat dokumentiert).

---

## 9. Status

### 9.1 Was steht jetzt fest

- Storyline und Drei-Sessions-Architektur (Beschreiben / Prüfen /
  Vorhersagen-oder-Eingreifen)
- BLUE als gemeinsame Brücke vor Pfad-A/Pfad-B-Trennung in S3
- Verfeinerung in Pfad B: Exogenität per Annahme vs. per Studiendesign
- Datensatz-Wahl Dominick's OJ
- **Hauptstore Store 114** (Composite-Score Rang 1, multi-kriterielle
  Auswahl, OVB-Differenz +0.145 lokal sichtbar)
- **Kontrast-Store Store 83** (OVB-Differenz −0.028, Vorzeichen-Umkehr
  als Stresstest der externen Validität)
- **Population-Sample-Frage als roter Faden über S1→S2→S3** mit konkretem
  Datensatz-Bezug
- **Auch Vollpanel ist keine Population** (83 Stores eines Filialnetzes
  in einem Zeitraum) — Lehrpunkt in S3
- HARKing-Lösung durch Sample-Splitting + Replikation
- Notations-Standard Stocker
- Six Foundations Bridge mit neuer Zeile 6
- Data-Governance: fünf Dimensionen, DAMA-DMBOK-Anschluss
- `brand_tier` als konstruierte Variable kennzeichnet
- S1 Lab 1 als Lifecycle/Reproducibility-Anker (steht weitgehend, nur
  OJ-Import zu ergänzen; wird im Sommer zur Demo + Moodle-Vorbereitung)
- **Regressionsdiagnostik in S2** als Teil der multiplen Regression mit
  `fit()` (neue Setzung nach Sondage der S2-Zeitbalance)
- **Methodische Hierarchie in S2:** Resampling im Vordergrund, klassische
  Tests als Spezialfall mit Annahmen. Studierende lernen Annahmen-
  *Prüfung*, aber nicht die *Konsequenzen* der Verletzung (kommen in S3
  im BLUE-Kontext)
- **Metadaten-Lehrmoment in S1** (Sommer): price per-oz vs. per-container
  als operative Anwendung von Data-Governance-Dimension 3
- **Lab/Demo-Unterscheidung** als didaktisches Prinzip; Demo für
  Workflow-Demonstration, Ergebnis-Vergleich, Übergangs-Inhalte
- **Moodle-Vorbereitung** für Setup-Inhalte als Prinzip
- **Ein Foliensatz pro Session** + Lab-Manuskripte als Nachbereitung
  (kein separater Folienstand für Selbststudium)
- Forward References zwischen den Sessions

### 9.2 Was als nächstes ansteht

- Foliensätze Session 3 (drei Varianten):
  - Variante A: PVA3-übersetzt mit Alternativ-Folien
  - Variante B: kompletter Neu-Foliensatz nach Eckpunktepapier
  - Variante C: Konsolidierung von A+B mit Eckpunktepapier-Storyline
- Labs / Demos für Session 3
- Sommer-Iteration: Sessions 1 und 2 nach diesem Eckpapier
  - inkl. Datenherkunfts-Folie (Dimension 4)
  - inkl. OJ-Import in S1 Lab 1 (Dimension 5)
  - **S3-Foliensätze (Variante C) anpassen:** BLUE-Brücke explizit als
    Einlösung des Cliffhangers aus S2 (Annahmen-Konsequenzen) rahmen.
    Im laufenden Semester (FS26) nicht relevant, da S2 noch keine
    Diagnostik enthielt.

### 9.3 Was offen bleibt

- Detaillierte Lab-Strukturen für S1 und S2 (Sommer)
- Hausaufgaben-Datensätze pro Phase
- Prüfungsformate

---

## 10. Zeitplanung Sommer-Iteration

Verbindlicher Rahmen: **180 Minuten pro Session.** Folgende Verteilung
ergibt sich aus den Lab/Demo-Konfigurationen in §2:

### Session 1 — Beschreiben

| # | Inhalt | Format | Min |
|---|---|---|---|
| 1 | Begrüßung, Pop-Tarts, CRISP-DM, AI-Value-Creator, Data-Governance-Wheel | Folien | 20 |
| 2 | R-Projekt und OJ-Import | Demo | 20 |
| 3 | Skalenniveau-Refresher (integriert in Wrangling) | Folien | 8 |
| 4 | Wrangling roher OJ-Daten | Demo | 20 |
| 5 | EDA als Konzept (ggpairs, Verteilungen) | Folien | 10 |
| 6 | EDA auf der gewrangelten Datei | Lab | 25 |
| | Pause | | 10 |
| 7 | Deskriptive Regression als CEF-Approximation | Folien | 12 |
| 8 | Einfache Regression | Demo | 15 |
| 9 | Multiple Regression, Interaktion, OVB, Overfitting (Ausblick) | Folien | 15 |
| 10 | Multiple Regression mit Interaktionsterm | Lab | 25 |
| 11 | Cliffhanger HARKing + OOS, Translator-Memo | Folien | 5 |
| | **Summe** | | **180** |

**Moodle-Vorbereitung:** R + RStudio + Quarto installieren, Test-Projekt
rendern. Screencast.

### Session 2 — Hypothesen prüfen

| # | Inhalt | Format | Min |
|---|---|---|---|
| 1 | Recap S1, Population → Sample (Stocker) | Folien | 12 |
| 2 | Sampling Distribution & Bootstrap | Folien | 15 |
| 3 | Bootstrap auf Werbe-Effekt | Lab | 20 |
| 4 | Permutationstest | Folien + Demo | 15 |
| 5 | p-Wert, p-Hacking, HARKing, Type-1/2, FDR, EV | Folien | 25 |
| | Pause | | 10 |
| 6 | Only-One-Test (t = ANOVA = Regression) | Folien | 8 |
| 7 | Only-One-Test live an OJ | Demo | 15 |
| 8 | Multiple Regression mit `fit()`, A-B-C-Annahmen | Folien | 10 |
| 9 | Multiple Regression mit Diagnostik | Lab | 30 |
| 10 | HARKing-Auflösung, Sample-Splitting (Konzept) | Folien | 5 |
| 11 | Replikation Store 50 (mit Reflexion) | Demo | 15 |
| 12 | Translator-Memo + Cliffhanger zu S3 | Folien | 5 |
| | **Summe** | | **185 ≈ 180** |

### Session 3 — Vorhersagen oder Eingreifen

| # | Inhalt | Format | Min |
|---|---|---|---|
| 1 | Recap S1+S2, zwei offene Fragen | Folien | 10 |
| 2 | Induktion/Deduktion, zwei Gefahren, AI-User → Value-Creator | Folien | 18 |
| 3 | BLUE als gemeinsame Brücke, Bias-Variance | Folien | 20 |
| 4 | Pfad A — Predictive: Multikollinearität, OOS, Lasso | Folien | 12 |
| 5 | BLUE-Brücken-Lab (Translator + Diagnostik-Anwendung + OOS/CV) | Lab | 35 |
| | Pause | | 10 |
| 6 | Regularisierung auf Demografie (Lasso/Ridge) | Demo | 20 |
| 7 | Pfad B — Causal: Endogenität, Datenherkunft, Exogenität, A/B | Folien | 20 |
| 8 | A/B-Test-Design für Werbe-Effekt | Lab | 25 |
| 9 | Synthese, Outlook (ER017, Zeitreihen), Translator-Memo | Folien | 10 |
| | **Summe** | | **180** |

### Gesamtbild

| | S1 | S2 | S3 |
|---|---|---|---|
| **Labs** | 2 | 2 | 2 |
| **Demos** | 3 | 3 | 1 |
| **Folien-Zeit** | ~80 Min | ~80 Min | ~100 Min |
| **Lab+Demo-Zeit** | ~100 Min | ~100 Min | ~80 Min |

Die Interaktivität ist über alle drei Sessions hoch und ausgeglichen.

---

*Dokument-Ende.*
