# Lab Development Schema v4 (FS26 final)

**Stand:** 2026-05-20
**Vorgänger:** Schema v3 (nach Lab 2 + Lab 1 Sessions)
**Diese Version:** Updates aus Lab 1/2/3 FS26-Iteration mit Spiess-Integration

---

## Was sich gegenüber v3 geändert hat

### Neue Phase 0.0 — Konzeptioneller Klammer-Begriff(e)

Vor der Dataset-Reconnaissance gibt es eine neue Phase 0.0:

**Identifikation der Modul-Klammern.** Vor irgendeiner Lab-Entwicklung
muss klar sein, welche übergreifenden Begriffe das Modul strukturieren.

Ein Modul kann **mehrere** Klammern haben, die *parallel* laufen. In ER114
FS26 sind das:

- **DGP-Klammer** (Data-Generating Process) — Pfad-E/P-Dichotomie
- **Spiess-Klammer** (Unbiasedness als institutional restriction)

Jede Klammer muss:
- in Step 0 eingeführt werden
- mindestens 5× im Lab aktiv aufgegriffen werden
- in der Synthese (Step 4 oder 5) als roter Faden wiederkehren
- explizit als Verknüpfung zum nächsten Lab dienen

Mehrere Klammern dürfen nicht beliebig viele werden. Empfehlung: max. 2-3 pro Modul.

### Erweiterung Phase 1.2 — Modul-Klammer-Integration

Bei jeder Lab-Erstellung in einem Modul mit mehreren Klammern muss
explizit dokumentiert werden, wo jede Klammer wieder aufgegriffen wird.
Tabellenformat:

```
| Lab-N-Element | Z. | Klammer | Anschluss in Lab-N+1 |
```

### Erweiterung Phase 2.4a — Mini-DAGs als Standard-Element

Wenn ein Lab kausale Strukturen behandelt:

- Mini-DAG (3-5 Knoten) in der Subsection, wo das Konzept zum ersten
  Mal sichtbar wird
- Bevorzugt als SVG (volle Kontrolle), Fallback `ggdag`
- Konsistentes Farbschema im gesamten Modul (Pfeile #686868, Boxen
  #2c2c2c, Hintergrund weiß)

**Best Practice DAG-SVG:**

```svg
<svg viewBox="0 0 500 220" font-family="'Courier New', monospace">
  <defs>
    <marker id="arrowhead" viewBox="0 0 10 10" refX="9" refY="5"
            markerWidth="7" markerHeight="7" orient="auto-start-reverse">
      <path d="M 0 0 L 10 5 L 0 10 z" fill="#686868"/>
    </marker>
  </defs>
  <!-- Edges first, with line endpoints positioned at box borders -->
  <line ... marker-end="url(#arrowhead)"/>
  <!-- Nodes last, so they paint over line endpoints -->
  <rect ... fill="white" stroke="#2c2c2c"/>
  <text .../>
</svg>
```

### Erweiterung Phase 5 — Robust-SE als Standard-Background-Box

Für Labs mit OLS-Regression: Background-Box mit folgender Struktur:

1. Warum klassische SE wrong sein können (B2/B3-Verletzung)
2. Huber-White HC3 für B2
3. Newey-West HAC für B2+B3
4. Code-Vergleich mit konkretem Datensatz
5. Tabelle mit Ratio-Spalte
6. 5-Punkte-Interpretation
7. Pfad-P-Note: robust SE irrelevant für Prädiktion
8. HAC-Limit

### Erweiterung Phase 4 — Cluster-CV als Standard-Element

Für jedes Lab mit CV auf Panel-Daten Background-Box vor K-Fold-Code:

- i.i.d.-Annahme von Standard-CV
- Data Leakage bei naivem Random-Split
- Whole-Cluster-Assignment als Lösung
- Erweiterung auf hierarchische Strukturen
- Brücke zu Rolling-Origin-CV (Zeitreihen)

### Refinement Phase 2.5a — Strukturelle Doppelung vermeiden

Vor jedem Lab-Schluss-Render `grep`-basierte Doppelungs-Suche:

```bash
awk 'length($0)>=20' lab.qmd | sort | uniq -d
```

### Refinement Phase 3.2a — Encoding-Sicherheit

Drei wiederkehrende Probleme aus Lab 1 FS26:

| Problem | Symptom | Lösung |
|---|---|---|
| `%>%` zu Unicode-Variante | Render-Fehler | Patches als reines Markdown |
| LaTeX `\f`, `\b` als Python-Control | `^Lrac` in Datei | Raw-Strings oder direkt im Editor |
| Emoji als `\u200b`-Sequenzen | `1390f` statt `ℹ️` | Direkt-Einfügung |

### Neue Phase 5.2a — Multi-Klammer-Folien-Strategie

Bei Modulen mit mehreren Klammern: in den **Folien** muss die Landkarte
vollständig gezeichnet sein, im **Lab** werden Konzepte beispielhaft
vertieft.

Beispiel aus ER114 FS26: Lab 2→3 Bridge-Folien zeichnen die
"Kausalanalyse-Landkarte" mit allen Auswertungsstrategien (OLS, RCT, DiD,
Event Study, IV, Synthetic Control, RDD, Matching, DML). Im Lab 3 werden
nur A/B-Test, DiD, IV + DML beispielhaft umgesetzt. Andere Methoden
bleiben in den Folien als Nachschlage-Ressource.

**Empfehlung:** in Phase 0.4 (Step Sequence Design) explizit festlegen,
welche Methoden im Lab operativ werden und welche nur referenziert werden.

### Neue Phase 5.2b — DiD vs. Event Study explizit abgrenzen

Wenn ein Lab DiD behandelt, ist die Abgrenzung zur Event Study **immer**
zu thematisieren. Tabellenformat:

```
| Aspekt | DiD | Event Study |
|---|---|---|
| Anzahl Koeffizienten | 1 | n_periods × 2 |
| Parallel-Trends-Test | indirekt | direkt |
| Dynamik | aggregiert | sichtbar |
| Staggered Adoption | problematisch | erfordert moderne Schätzer |
```

### Neue Phase 6.1 — Spiess-Argument als institutioneller Rahmen

Wenn ein Modul Pfad-E mit Pfad-P kontrastiert (wie ER114), gehört die
Spiess-Argumentation (Unbiasedness als institutional restriction) als
zweite Modul-Klammer:

- Lab N (Path E1 Lab): "Note of Caution" mit institutional role-Anker
- Lab N+1 (Path P Lab): Bias-Variance-Box mit zwei Lesarten
- Lab N+2 (Path E2 Lab): Sample-Splitting + DML operationalisiert

### Neue Phase 8.3 — Übergabe-Dokumentation als Standard

Am Ende jeder Iteration eines Moduls:

1. **Roadmap-Dokument** (Aufwand, offene Aufgaben)
2. **Übergabeprotokoll** (Konventionen, Klammern, Verknüpfungen)
3. **Schema-Update** (neue Best Practices)

Drei Dokumente werden bei jedem neuen Chat als Eingangsfiles bereitgestellt.

---

## Phasen-Übersicht v4

**Vor Lab-Entwicklung:**
- 0.0 Modul-Klammer(n) identifizieren [NEU]
- 0.1 Dataset Reconnaissance
- 0.2 Learning Objective Derivation
- 0.3 The Central Surprise
- 0.4 Step Sequence Design (mit Methoden-Allokation Lab vs. Folien) [erweitert]
- 0.5 Icebreaker Design

**Phase 1 — Content Foundation**
- 1.2 Modul-Klammer-Integration [NEU]

**Phase 2 — Structure and Navigation**
- 2.0 CRISP-DM Context Block
- 2.0a Instructor Customisation Comment Block
- 2.1 Navigation System
- 2.1a Steps as Tasks (Setup Labs)
- 2.2 Scale-Level-Driven Section Structure
- 2.3 Fold/Unfold Decision Rules
- 2.4 Anchor Links for Callout Boxes
- 2.4a Mini-DAGs as Standard Element [NEU]
- 2.5 Structural Completeness Check
- 2.5a Duplication Check [NEU]

**Phase 3 — Code Quality**
- 3.1 Pipe and Syntax Standardisation
- 3.2 Package Hygiene
- 3.2a Encoding Safety [NEU]
- 3.3 Code Commenting Strategy
- 3.4 LaTeX Safety Rules
- 3.5 Unicode and Emoji Safety Rules
- 3.5a File and Script Naming Conventions
- 3.6 YAML Header Standards
- 3.7 Visual Identity

**Phase 4 — Didactic Sequencing**
- 4.0 The Inductive Principle
- 4.1 Step Micro-Structure
- 4.1a Bridge Sentences as Icebreaker Back-References
- 4.1b Incremental Script Building
- 4.1c Multi-Context Task Instructions
- 4.1d RStudio Navigation
- 4.1e Cluster-CV as Standard Element [NEU]
- 4.2 Data Problem as Discovery Sequence
- 4.3 Factor Recoding Placement Rule
- 4.4 Inferential vs. Descriptive Framing

**Phase 5 — Content Enrichment**
- 5.1 Real Data Problems as Teaching Moments
- 5.2 Background Box Inventory
- 5.2a Robust-SE Background Box [NEU]
- 5.2b DiD vs. Event Study Differentiation [NEU]
- 5.2c Multi-Klammer Folien-Strategie [NEU]

**Phase 6 — Variable Selection**
- 6.1 Spiess Institutional Frame [NEU]

**Phase 7 — Time, Scope, and Bonus Tasks**
- 7.1 Time Audit
- 7.1a Homework Tasks
- 7.2 Bonus Task Design and Ordering
- 7.3 Summary Table

**Phase 8 — Final Consistency and Companion Script**
- 8.1 Programmatic Consistency Check
- 8.2 R Companion Script
- 8.3 Handover Documentation [NEU]

---

## Cross-Cutting Quality Criteria — v4 final

| Kriterium | Frage |
|---|---|
| Klammer-Begriffe aktiv | Werden alle Modul-Klammern ≥ 5× aktiv verwendet? |
| DAG-Konsistenz | Sind alle DAGs im Modul im gleichen Farbschema und Layout? |
| Robust-SE-Box | Hat jedes OLS-Lab eine Background-Box zu robusten SE? |
| Cluster-CV-Box | Hat jedes Lab mit CV auf Panel-Daten eine Cluster-CV-Box? |
| Doppelungs-Frei | Wurden alle Mehrfach-Erwähnungen identischer Sätze entfernt? |
| Übergabe-Dokumentation | Sind Roadmap, Handover und Schema-Update vorhanden? |
| Lab-Verknüpfungen | Werden Bezüge zwischen Labs explizit dokumentiert? |
| Folien vs. Lab | Ist klar, welche Methoden im Lab vertieft und welche nur in den Folien referenziert werden? |
| DiD-Event-Study-Abgrenzung | Wenn DiD vorkommt: ist die Abgrenzung zur Event Study explizit? |
| Spiess-Argument | Wenn Pfad E/P kontrastiert wird: ist Unbiasedness als institutional restriction integriert? |

---

## Konkrete neue Best Practices aus FS26-Iteration

| Erkenntnis | Best Practice |
|---|---|
| Render-Hänger ist RStudio-Eigenheit | Bei Hänger: Stop-Knopf, kein Bug-Hunt |
| Copy-Paste bricht `%>%` | Patches als reines Markdown |
| SVG-DAGs > ggplot-DAGs für Konsistenz | SVG-Template etablieren |
| Background-Boxen brauchen Interpretations-Block | Phase 5.2: Box mit Daten-Beispiel + 3-5 Lese-Punkten |
| Doppelung tritt durch iterative Patches auf | Phase 2.5a: grep-Doppelungs-Check |
| Übergabe in neue Chats braucht Dokumentation | Phase 8.3: drei Dokumente standardmäßig |
| Mehrere Modul-Klammern parallel möglich (DGP + Spiess) | Phase 0.0: explizit identifizieren, in Lab-Verknüpfungs-Tabelle dokumentieren |
| Folien vs. Lab muss explizit allokiert werden | Phase 0.4: Methoden-Allokation im Step Sequence Design |
| Aktuelle Forschung (Borusyak, Spiess, Chernozhukov) als Background-Boxen | Phase 5.2: nicht im Hauptpfad, aber als verfügbares Wissen |
