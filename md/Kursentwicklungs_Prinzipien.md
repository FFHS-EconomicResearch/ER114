# Prinzipien der Kursentwicklung

*Allgemeine Iterationsregeln und Prinzipien für die Entwicklung
universitärer Kurse in der Schnittstelle Statistik / Ökonometrie /
Data Science. Aus der ER114-Entwicklung extrahiert, für andere Kurse
wiederverwendbar.*

*Stand: Mai 2026.*

---

## 1. Vier Phasen der Kursentwicklung

Kursentwicklung läuft in einer wiederkehrenden Sequenz:

### Phase 1 — Vision

Wofür dieser Kurs überhaupt da ist. Welche Zielgruppe, welche
Vorkenntnisse, welche Anschluss-Module. Was Studierende danach können
sollen.

Output: kurze Vision (1 Seite), die das **Erkenntnisinteresse** des
Kurses formuliert.

### Phase 2 — Architektur

Welche Sessions/Blöcke hat der Kurs. Welche Story trägt sie. Welche
Erzählform.

Output: Eckpunktepapier (mehrere Seiten Markdown), das alle
strukturellen Setzungen verbindlich festhält.

### Phase 3 — Materialien

Konkrete Folien, Labs, Hausaufgaben.

Output: ausführbare Materialien (Quarto, R, Python).

### Phase 4 — Iteration

Nach jedem Semester: was hat funktioniert, was nicht. Was braucht
Korrektur.

Output: aktualisiertes Eckpunktepapier, neue Materialien.

**Prinzip: Phase 2 vor Phase 3.** Wer ohne Eckpunktepapier in die
Materialien geht, baut auf Sand. Wer Folien-Reihenfolgen aus dem Bauch
heraus festlegt, riskiert Inkonsistenzen, die teuer werden.

---

## 2. Iterationsregeln

### 2.1 Sondage vor Entscheidung

**Wenn eine konkrete Entscheidung von Daten abhängt** (z.B.: ist dieser
Datensatz für die Lehre geeignet? Hat er die nötigen Lehrmomente?), dann
sollte erst eine **Sondage** (Daten-Probe) gemacht werden, bevor die
Entscheidung getroffen wird.

Beispiel: Bei der Wahl des Lehr-Datensatzes für ER114 wurden *vier*
sukzessive Sondagen durchgeführt, jede mit einem klaren Ziel:
1. Datenstruktur und Variablenliste
2. Store-Auswahl und Autokorrelation
3. `feat`-Charakter und Marken-Identität
4. Datenqualität und Kuratierungs-Lehrmomente

Erst danach war die Entscheidung „Dominick's OJ, Store 51, Brands
1+5+10" datenbasiert begründbar.

**Anti-Muster:** Annahmen über die Datenstruktur treffen, daraufhin eine
Architektur entwerfen, später feststellen dass die Annahmen nicht
stimmten, dann nachbessern. Das produziert Schleifen, die vermeidbar
gewesen wären.

### 2.2 Eckpapier vor Folien

Folien sind teuer in der Herstellung und in der Wartung. Das
Eckpapier sollte **vor** den ersten Folien stehen — sodass jede
Folien-Entscheidung gegen das Eckpapier geprüft werden kann.

**Zwei Indikatoren, dass das Eckpapier reif ist:**
- Storyline lässt sich in 5 Sätzen zusammenfassen.
- Themen-Allokation hat keine *„später entscheiden wir"*-Felder mehr in
  der Hauptstruktur (Detail-Felder dürfen offen sein).

### 2.3 Kontrollierter Stil

Bei der Überarbeitung bestehender Materialien gilt: **keine
eigenmächtigen Änderungen.** Wenn eine Folie aus Sicht des
Bearbeiters überarbeitet werden müsste, dann wird die ursprüngliche
Folie behalten und ein **Alternativvorschlag direkt danach** als neue
Folie eingefügt. Die Entscheidung trifft die inhaltlich verantwortliche
Person.

**Begründung:** Das schützt vor Verlust an konzeptioneller
Konsistenz, wenn ein Bearbeiter den Kontext einer Folie nicht
vollständig kennt.

### 2.4 Stop-and-go

Bei umfangreichen Aufgaben (z.B. mehrere Dokumente parallel) ist
Stop-and-go besser als Durchziehen. Nach jedem abgeschlossenen Schritt
wird Feedback eingeholt, **bevor** der nächste begonnen wird. Das
verhindert, dass Fehlannahmen durch alle Folge-Dokumente propagieren.

### 2.5 Zwei-Versions-Ansatz bei stilistischen Entscheidungen

Wenn eine stilistische Entscheidung unsicher ist (z.B. Action Titles
vs. Descriptive Titles), dann zwei Versionen erstellen und vergleichen.
Die unterlegene Variante wird gelöscht. Das spart die theoretische
Diskussion über Stil-Fragen.

### 2.6 Dokumentation der Setzungen

Jede konzeptionelle Setzung sollte mit einer kurzen Begründung
dokumentiert werden. Im Handover-Dokument sind die Setzungen mit
Datum und Begründung festgehalten. Das schützt vor späterer
Eigenmächtigkeit, weil die Begründungen rekonstruierbar sind.

---

## 3. Didaktische Grundsetzungen

Diese Setzungen sind nicht spezifisch für ER114, sondern gelten für
jede Statistik-/Ökonometrie-/Data-Science-Lehre auf Master-Niveau.

### 3.1 Inferenz-Frage konsequent unterscheiden

In jeder Lehrsituation klar trennen:

- *Beschreiben* (deskriptiv, ohne Wahrscheinlichkeitsaussagen)
- *Erklären/Generalisieren* (Inferenz auf den Datengenerierungsprozess)
- *Vorhersagen* (auf neue Beobachtungen)
- *Eingreifen* (kausal, mit zusätzlichen Annahmen)

Diese Modi sind *nicht* austauschbar. Studierende, die sie nicht
unterscheiden, übertragen Aussagen aus einem Modus auf den anderen
(*„der Effekt ist signifikant, also ist er kausal"*) — das ist die
häufigste Fehlinterpretation in der Praxis.

### 3.2 Methodisch ehrlich

Wenn ein Verfahren in der Lehrsituation simplifiziert wird (z.B.: bei
n=336 ist klassische Inferenz okay, obwohl die Annahmen nicht alle
gelten), dann sollte das **explizit benannt** werden. Studierende sollen
die Vereinfachung erkennen, nicht für Wirklichkeit halten.

Beispiel-Formulierungen:
> *„Wir nehmen Unabhängigkeit der Wochen an. Diese Annahme ist nicht
> ganz unproblematisch — aber für didaktische Zwecke und bei nicht zu
> starkem Trend reicht das. In der echten Anwendung müsstet ihr Block-
> Bootstrap einsetzen."*

> *„Sample-Splitting ist eine Lösung, aber sie hat Grenzen bei kleinen
> Stichproben. Replikation an unabhängigen Daten ist methodisch tiefer."*

### 3.3 Anschlussfähigkeit zur Fachliteratur

Jeder zentrale Begriff sollte mit mindestens einer Lehrbuch-Referenz
verbunden sein, damit Studierende die Lehre selbständig vertiefen
können. Wenn eine Lehre eine *eigene* Setzung über die Standard-
Literatur hinaus macht (z.B. eine Brücken-Konstruktion zwischen zwei
Traditionen), sollte das explizit als solche markiert werden.

### 3.4 Notations-Konsistenz

Innerhalb eines Kurses *eine* Notation verwenden. Wechsel zwischen
β-Schreibweisen oder Index-Konventionen sind didaktisch teuer. Wenn ein
Lehrbuch als Notations-Standard gewählt wird, sollte das im
Eckpunktepapier festgehalten und über alle Materialien durchgezogen
werden.

### 3.5 Daten als Lehrgegenstand

Ein realer Datensatz ist kein Werkzeug, sondern ein Lehrgegenstand. Die
Auswahl ist konstitutiv für die Lehre. Kriterien:

- *Realität:* echte Daten lehren mehr als simulierte
- *Business-Relevanz:* Studierende müssen nicht erst die Domäne
  einarbeiten
- *Lehrmomente:* der Datensatz sollte *strukturell* die Lehrpunkte
  tragen (Multikollinearität, Confounding, etc.), nicht nur als
  konstruiertes Beispiel
- *Drei-Auflösungen-Test:* kann der Datensatz *mehrere* Sessions tragen,
  oder muss er nach einer Session ausgetauscht werden?

### 3.6 Cliffhanger zwischen Sessions

Jede Session sollte mit *expliziten* Forward References enden. Das ist
nicht nur didaktisch motivierend, sondern erzwingt auch konzeptionelle
Konsistenz: wenn ein Cliffhanger in S1 in S2 nicht eingelöst werden
kann, ist die Architektur nicht stimmig.

### 3.7 Translator-Empfehlung am Lab-Ende

Jedes Lab sollte mit einer *Translator-Empfehlung* enden — was raten
wir dem Management auf Basis der gerade gemachten Analyse? Das zwingt
Studierende dazu, die Methode in Geschäftssprache zurückzuübersetzen
und schließt die Brücke zwischen Methodik und Anwendung.

---

## 4. Folien-Prinzipien

### 4.1 Folien sind kein Manuskript

Folien haben drei Funktionen:
- **Motivation** (warum dieses Thema?)
- **Big Picture** (wie hängt es zusammen?)
- **Aufmerksamkeit auf Stolpersteine** (was geht typischerweise schief?)

Details gehören in die Labs. Eine Folie sollte in 60–90 Sekunden lesbar
sein.

### 4.2 Visuell vor textuell

Wenn ein Konzept mit einer Grafik, einem DAG, einer Tabelle, einer
Illustration darstellbar ist, sollte das der Folientext nicht
duplizieren. Doppelung kostet Aufmerksamkeit.

### 4.3 Action Titles als Ziel

Folientitel sollten möglichst eine *Aussage* machen, nicht nur ein
*Thema*:

- ❌ Schwach: *„Multikollinearität"*
- ✅ Stark: *„Multikollinearität verzerrt Inferenz, aber nicht
  Vorhersage"*

Wenn Action Titles nicht durchgehalten werden können, lieber
descriptive Titles als halbgar-action.

### 4.4 Neutrale Sprache

Kein „ich", kein „Du", allenfalls „wir". Studierende sollen sich nicht
direkt angesprochen fühlen, sondern als Mitglieder einer epistemischen
Gemeinschaft. Folien sollen wiederverwendbar sein, ohne dass die
Lehrperson sich in ihnen findet.

### 4.5 Faltbare Boxen für Lab-Manuskripte

Lab-Manuskripte (im Gegensatz zu Folien) sollen ausführlich sein. Sie
funktionieren in zwei Modi:
- *Live-Session:* faltbare Callout-Boxen sind kompakt, der Übungsleiter
  zeigt nur das Wesentliche
- *Selbststudium:* die gleichen Boxen sind ausgeklappt, vollständige
  Erklärungen verfügbar

**Konsequenz:** Es gibt *einen* Foliensatz pro Session + Lab-Manuskripte
als Nachbereitung. Kein zweiter Folienstand für Selbststudium. Lab-
Manuskripte mit faltbaren Boxen sind das Nachbereitungs-Material.
Doppelpflege von Folienständen ist Doppelarbeit ohne didaktischen
Mehrwert.

### 4.6 Lab und Demo als distinkte Formate

Die Unterscheidung zwischen *Lab* und *Demo* ist nicht nur Zeit-Frage,
sondern didaktisches Profil:

| Format | Wer macht was? | Wann sinnvoll? |
|---|---|---|
| **Lab** | Studierende arbeiten selbst mit dem Manuskript | wenn Selbstkonstruktion und Hands-on-Üben das Lernziel sind |
| **Demo** | Dozent führt durch das Manuskript, Studierende tippen mit oder schauen zu | wenn der Lehrwert nicht im Coden, sondern im Workflow oder Ergebnis-Vergleich liegt |

**Demo nicht einfach „kürzeres Lab":** Demos haben ein eigenes
didaktisches Profil. Der Dozent zeigt aktiv auf Stolpersteine, die im
Manuskript in Callout-Boxen für die Nachbereitung dokumentiert sind.
Studierende erleben den Workflow als zusammenhängende Geschichte —
Details vertiefen sie nach der Session.

**Konkrete Entscheidungskriterien Demo vs. Lab:**

| Wähle Demo, wenn... | Wähle Lab, wenn... |
|---|---|
| Code-Boilerplate hoch, Lehrwert im Vergleich der Ergebnisse | Coden ist Teil des Lernens |
| Workflow-Demonstration (Wrangling, Setup) | Selbstständiges Problem-Lösen |
| Übergangs- oder Wiederholungs-Inhalt ohne eigenen Aha-Moment | Eigener Aha-Moment im Lab |
| Setup-Risiko mit Hängenbleiben-Gefahr | Material gut beherrschbar |

**Aktivierende Reflexions-Aufgabe innerhalb der Demo:** Demo muss nicht
passiv sein. Ein 5-Min-Reflexionsblock am Demo-Ende (Vergleichs-
Steckbrief, Translator-Memo, kurze Diskussion) hält die Studierenden
intellektuell aktiv.

### 4.7 Setup-Inhalte als Moodle-Vorbereitung

Reine Setup-Tätigkeiten (Installation, Projekt-Anlegen, Test-Renders)
gehören *nicht* in die erste Live-Session, weil sie Frustrationsrisiko
ohne kompensierenden didaktischen Gewinn erzeugen. Wenn jemand technisch
hängt, hängt die ganze Gruppe.

**Lösung:** Setup wandert in eine Moodle-Vorbereitungsaufgabe vor der
ersten Session, mit Screencast als Anleitung. Die Live-Session zeigt
dann den *fertigen* Workflow als Demo — das, was Studierende zu Hause
nicht selbst nachvollziehen konnten.

### 4.8 Lab/Demo-Mix als Gleichgewichts-Werkzeug

Über eine Session betrachtet, sollte die Lab/Demo-Mischung Interaktivität
und Zeit-Disziplin balancieren:

- **Zu viele Labs** sprengen die Zeit und erzeugen Stress
- **Zu viele Demos** machen die Session passiv
- **Eine ausgewogene Mischung** (2–3 Labs + 1–3 Demos in 180 Min)
  entspricht typischerweise dem realistischen Optimum

**Stellschrauben für Zeit-Disziplin in einer Session:**

| Maßnahme | Gewinn |
|---|---|
| Übergangs-Inhalt → Demo statt Lab | 10–15 Min |
| Reflexion in bestehende Demo integrieren | spart Lab-Zeit |
| Setup in Moodle-Vorbereitung verlagern | 10–15 Min |
| Optionale Vertiefungen in Callout-Boxen | beliebig |

---

## 5. Lab-Prinzipien

### 5.1 Lab-Schema-Datei

Für die Lab-Entwicklung sollte ein *Schema* existieren, das die Phasen
der Lab-Erstellung dokumentiert. Im ER114-Projekt ist das die
Lab-Schema-Datei mit acht Phasen (Phase 0 = Lab-Design, Phase 1 =
Content Foundation, etc.).

Vorteil: jede Lab-Entwicklung folgt derselben Choreographie. Das
verhindert, dass Labs unterschiedlich strukturiert werden.

### 5.2 Sieben Schritte vor dem ersten Code

Bevor das erste Lab-Codestück geschrieben wird, sollten diese Fragen
beantwortet sein:

1. **CRISP-DM-Phase?** (was darf das Lab inhaltlich tun?)
2. **Population oder Sample?** (was darf methodisch verwendet werden?)
3. **Zentrales Lehrmoment?** (was ist *das eine* Aha-Erlebnis?)
4. **Zentrale Überraschung?** (was widerspricht der Intuition?)
5. **Nachfolge-Anschluss?** (worauf zeigt das Lab voraus?)
6. **Setup oder Analyse?** (welcher Lab-Typ?)
7. **Datensatz und Variablen-Wahl?**

Erst wenn alle sieben Fragen beantwortet sind, beginnt die Material-
Erstellung.

### 5.3 Reproduzierbarkeit

Jedes Lab sollte reproduzierbar sein:
- Quarto-Dokument mit Setup-Chunk, der alle Pakete lädt
- Datensatz aus stabiler Quelle (CRAN-Paket oder lokale Datei mit
  klarem Pfad)
- Seeds gesetzt, wo Zufall im Spiel ist
- Versionsangaben in der Setup-Doku

### 5.4 Companion-Skript

Jedes Lab sollte einen Companion-R-Skript haben, der nur den
ausführbaren Code enthält — als Live-Session-Tool. Studierende können
mit dem Skript arbeiten, das Lab-Manuskript dient als Referenz.

### 5.5 Time-Boxing pro Session

Jede Session hat einen festen zeitlichen Rahmen (typischerweise 180 Min
im Master-Bereich). Diesen Rahmen muss die Materialplanung respektieren.

**Vorgehen:**
1. Inhalts-Komponenten auflisten (Folien-Blöcke, Labs, Demos, Pausen)
2. Jeder Komponente eine realistische Minutenzahl zuweisen
3. Aufsummieren — fast immer ist die erste Summe ≥ 15 % über dem Rahmen
4. Kürzen, bis die Summe passt — *nicht* durch Verkürzen aller Blöcke
   gleichmäßig, sondern durch *bewusstes Streichen* einzelner
   Komponenten oder Format-Wechsel (Lab → Demo, Live-Inhalt → Callout)

**Faustregeln für realistische Minutenzahlen:**

| Format | Realistische Zeit |
|---|---|
| Folie mit Konzept-Erklärung | 1–2 Min pro Folie |
| Folie mit Visualisierung und Diskussion | 3–5 Min pro Folie |
| Lab mit klarem Code-Setup | 20–30 Min |
| Lab mit Open-Ended-Aufgabe | 30–40 Min |
| Demo mit Workflow-Demonstration | 15–20 Min |
| Demo mit anschließender Reflexion | 20–25 Min |
| Pause | 10 Min Minimum |

**Niemals** auf-die-Minute-genau planen — Toleranz einbauen.

### 5.6 Modulare Lab-Konstruktion für Migration

Wenn ein Lab inhaltlich an einer „Übergangs-Position" steht (z.B. weil
ein Curriculum im Übergang ist und in einer späteren Iteration neu
verortet wird), sollte es so konstruiert sein, dass die *Bestandteile*
einzeln wiederverwendbar sind.

**Konkret:**
- Setup-Code, Daten-Load, gemeinsame Funktionen als eigene Abschnitte
- Inhaltliche Module mit klaren Phasen-Markern, sodass einzelne Module
  in andere Labs übernommen werden können
- Companion-Skript spiegelt diese Modularität — die Phasen lassen sich
  ausschneiden und in ein anderes Lab einsetzen

**Beispiel:** Wenn Diagnostik im laufenden Semester in Lab X eingebaut
wird, aber in der nächsten Iteration nach Lab Y wandert, sollte der
Diagnostik-Code-Block in Lab X klar abgegrenzt sein — z.B. mit einem
Phase-Header *„### Phase 2: Regressionsdiagnostik (wird in Iteration N+1
nach Session 2 verschoben)"*.

### 5.7 Coding-Standards für Lab-Material

Lab-Code ist Lehr-Code. Studierende lesen ihn, kopieren ihn, lernen
*Konventionen* damit. Inkonsistente Konventionen verwirren — eine
disziplinierte Standardisierung ist daher methodisch.

**Pipe und Syntax:**

- `%>%` (magrittr) statt `|>` (native pipe) — das `tidyverse`-Ökosystem
  ist auf magrittr ausgerichtet; Konsistenz mit dem Standard-Stack
- Variablenerzeugung in `mutate()`-Pipelines, **nicht** durch
  `df$neueVar <- ...`. Begründung: Pipelines sind lesbarer, machen die
  Reihenfolge der Operationen sichtbar und verhindern stille
  Datentyp-Inkonsistenzen.
- `tibble`-Variablen mit Präfix `tbl_` zur klaren Unterscheidung von
  Skalaren, Modellen, Vektoren
- `case_when()`, `if_else()` für bedingte Logik

**Serialisierung:**

- `write_rds()` / `read_rds()` aus readr statt `saveRDS()` / `readRDS()`
  aus Base R — tidyverse-Konsistenz, gleiche Argumentstruktur wie die
  anderen `read_*` / `write_*`-Funktionen
- Datendateien an `xfun::from_root("data", "raw", "name.rds")` lokalisieren
  (Projekt-Wurzel-relativ, plattform-unabhängig)

**Modell-Outputs:**

- `broom::tidy()` für Koeffizienten-Tabellen statt manueller Konstruktion
- `gt()` für die finale Darstellung in Quarto

**Anti-Muster (zu vermeiden):**

- `$`-Zuweisungen in Lehrmaterial außerhalb der `mutate()`-Pipeline
- Mischung von `|>` und `%>%` im selben Dokument
- Implizite Tibble-Konversion ohne `as_tibble()`-Schritt
- Direkte `df$variable`-Zugriffe in `ggplot()`-aes-Argumenten

Diese Standards gehören in das Übergabe-Protokoll, damit nachfolgende
Bearbeitende sie kennen.

### 5.8 Inline-R für alle Zahlen, die aus den Daten kommen

**Prinzip:** Jede Zahl im Lab-Fließtext, die aus den Daten berechnet
wird, steht in einem Inline-R-Ausdruck (`` `r ...` `` in Quarto) —
**nicht** als hardgecodete Konstante.

**Begründung:**

- *Korrektheit:* Wenn sich die Daten ändern (neuer Store, neue Marken,
  neue Aufbereitung), aktualisieren sich die Zahlen automatisch
- *Konsistenz:* Lab-Manuskript und Code-Output zeigen garantiert dieselben
  Zahlen — keine Drift zwischen Beispielwert im Text und Tatsache
- *Reproduzierbarkeit:* andere Bearbeitende können den Code laufen lassen
  und sehen exakt die Zahlen, die das Manuskript zitiert

**Konkret zu vermeiden:**

| Anti-Muster | Korrekte Variante |
|---|---|
| *„Der Werbe-Effekt ist etwa 0.25"* (hardgecodet) | *„Der Werbe-Effekt beträgt `r round(coef(mod)["feat"], 2)`"* |
| Plausible-Zahl-aus-Erinnerung im Fließtext | Inline-R oder verifizierter Wert aus laufendem Sondage-Skript |
| Beispiel-Wert *zur Illustration* neben Inline-R-Ausdruck | Nur Inline-R-Ausdruck — keine Beispielwerte nebenbei |

**Workflow-Disziplin:**

1. Wenn eine Zahl noch nicht aus der Sondage bekannt ist, im Lab
   schreiben: `` `r [TBD: from Sondage]` `` mit expliziter Notiz
2. Vor dem Finalisieren des Labs: Sondage-Skript laufen lassen,
   alle TBD-Marker durch echte Inline-Ausdrücke ersetzen
3. Nie eine Zahl im Lab-Fließtext nennen, die nicht entweder aus
   einem Inline-R-Ausdruck stammt *oder* in einem laufenden
   Sondage-Skript verifiziert wurde

**Lehrgewinn nebenbei:** Studierende sehen Inline-R im Lab und lernen
diese Quarto-Kapazität als selbstverständliches Werkzeug für ihre
eigenen Berichte und Analysen.

### 5.9 Frage-vor-Antwort-Konvention bei Interpretationen

Eine Interpretation soll nie ungefragt geliefert werden. Das didaktische
Muster ist immer dreischrittig:

1. **Frage an die Studierenden** in einer `callout-important` (⚠️) Box
   — formuliert als konkrete, beobachtbare Frage. Die Studierenden sollen
   selbst überlegen, was sie sehen.
2. **Plot oder Ausgabe** unmittelbar darüber oder darunter
3. **Auflösung / Interpretation** in einer **aufklappbaren**
   `callout-note collapse="true"` Box mit Titel-Präfix 🟢 für „Solution".

Diese Konvention gilt für:
- Diagnostik-Readouts (was sehen wir in den Residuen?)
- Modellvergleiche (warum unterscheiden sich die Koeffizienten?)
- Befund-Diskussionen (was bedeutet dieser Wert?)

Das Anti-Muster: Plot anzeigen und sofort den Befund deklarieren. Damit
geht die Eigenleistung der Studierenden verloren.

### 5.10 Sprachliche Neutralität

Lab-Texte und Folien-Texte verwenden eine **neutrale, fachlich präzise
Sprache**. Metaphern werden nur dann eingesetzt, wenn sie *inhaltlichen
Mehrwert* bringen — nicht als rhetorisches Stilmittel.

**Anti-Muster:**

- *„Wir sind an Wand gestoßen"* — fügt der Aussage „Annahmen sind verletzt"
  nichts hinzu
- *„Aha-Moment", „Stolperstein", „Knall"* — implizit emotional, oft
  unpräzise
- *„opposite criteria", „opposite paths"* — suggeriert Trennung, wo
  asymmetrische Gemeinsamkeit korrekter wäre
- *„crash"* (für ein Modell) — technisch unscharf

**Bessere Formulierungen:**

- „Die Annahmen-Diagnostik zeigt eine Verletzung von B2"
- „Step 1b liefert ein Resultat, das die naïve Interpretation aus Step 1a
  in Frage stellt"
- „Beide Pfade ruhen auf demselben Fundament, reagieren aber asymmetrisch
  auf Annahmen-Verletzungen"
- „Das Modell konvergiert nicht / produziert Singularität in $X^\top X$"

**Ausnahme:** Wenn eine Metapher *präzise* einen Sachverhalt erfasst, ist
sie zulässig. „BLUE-Brücke" ist ein gutes Beispiel: sie bezeichnet
*sachlich* das gemeinsame Fundament der zwei Pfade. „Wir sind an die
Wand gestoßen" ist ein schlechtes — es ersetzt eine sachliche Aussage
durch eine emotionale.

**Begründung:** Lab-Material ist Lehrmaterial. Studierende lernen
fachliche Präzision *mit*, wenn sie sie im Material vorfinden. Saloppe
Sprache schadet der Lernkultur, besonders wenn sie Methodisches überlagert.

---

## 6. Übergangs-Logik (Cliffhanger)

### 6.1 Drei Cliffhanger-Typen

| Typ | Wann eingesetzt | Beispiel |
|---|---|---|
| **Methodisch** | „Wir haben Werkzeug X, aber Frage Y bleibt offen" | „Inferenz funktioniert, aber wenn wir eingreifen?" |
| **Inhaltlich** | „Wir haben Phänomen X gesehen, aber warum?" | „Werbung ↔ Sales korrelieren, ist es kausal?" |
| **Praktisch** | „Wir haben es im Kleinen gemacht, gilt es im Großen?" | „Store 51 funktioniert — auch alle 83?" |

### 6.2 Drei-Cliffhanger-Regel

Eine Session sollte *nicht mehr* als drei Cliffhanger setzen. Mehr
verwirren, weniger ergeben kein Spannungsfeld.

### 6.3 Cliffhanger müssen einlösbar sein

Jeder Cliffhanger in einer Session N muss in Session N+1 explizit
eingelöst werden. Wenn das nicht möglich ist, war der Cliffhanger
falsch gesetzt.

---

## 7. Erkenntnistheoretische Setzungen

Diese Setzungen sind für Statistik-/DS-Kurse besonders wichtig, weil
viele Studierenden-Fehlinterpretationen aus epistemischen Lücken
kommen.

### 7.1 Induktion und Deduktion explizit machen

Studierende verstehen oft nicht, dass Modellbildung ein induktiver
Prozess ist (von Daten zu Hypothese), Modellanwendung aber deduktiv
(von Hypothese zu Einzelfall). Diese Trennung sollte früh im Kurs
benannt werden.

### 7.2 HARKing als ehrliche Lehrgeschichte

HARKing — Hypothesizing After the Results are Known — ist nicht nur
eine Forschungs-Sünde, sondern ein didaktisches Risiko: jeder Kurs,
der erst Daten exploriert und dann an *denselben* Daten Hypothesen
testet, betreibt im Kleinen HARKing. Das sollte explizit benannt und
durch Sample-Splitting oder Replikation gelöst werden.

### 7.3 Lucas-Kritik als Trennungsanker

Die Lucas-Kritik (1976) — Verhaltensgleichungen sind nicht stabil über
Politik-Wechsel — ist ein didaktisch starkes Argument für die
Notwendigkeit kausaler Inferenz. Sie verdeutlicht, *warum* Vorhersage-
Modelle bei Eingriffen versagen.

### 7.4 „Causal inference is just inference with the right data"

Hernán et al. (2019). Dieser Satz ist die kürzestmögliche Auflösung des
Kausal-Inferenz-Diskurses: *„einfache" und kausale Inferenz
unterscheiden sich nicht in der Methode, sondern in der Datengrundlage.*

Studierende, die das verstehen, machen weniger kausale Fehlschlüsse.

---

## 8. Wenn Sie als externe Person an einem Kurs arbeiten

### 8.1 Erst Verstehen, dann Vorschlagen

Bevor Vorschläge gemacht werden, ist es hilfreich, alle vorhandenen
Materialien zu lesen — *vollständig*, nicht selektiv. Konzeptionelle
Vorschläge auf Basis halben Lesens produzieren oft Reibung mit
Setzungen, die schon getroffen wurden.

### 8.2 Klärungsfragen vor Vorschlägen

Wenn etwas unklar ist, eine *Klärungsfrage* stellen, bevor ein
Vorschlag gemacht wird. Vorschläge auf Basis vermuteter Setzungen
führen zu Mehrarbeit, wenn die Vermutung falsch war.

### 8.3 Klare Trennung von Vorschlag und Setzung

Wenn ein Vorschlag gemacht wird, sollte er klar als *Vorschlag* (nicht
als „so ist es") formuliert sein. Die inhaltlich verantwortliche
Person entscheidet.

### 8.4 Spannungen benennen, nicht versöhnen

Wenn zwei Setzungen miteinander in Konflikt stehen, sollte das benannt
werden, nicht verschwiegen oder durch Kompromiss-Formulierungen
versöhnt. Konflikte sind oft produktiv — sie zeigen, dass eine echte
Entscheidung gefällt werden muss.

---

## 9. Anti-Muster — was nicht funktioniert

Aus der ER114-Entwicklung extrahierte Negativ-Lektionen:

- **Datenarchitektur ohne Sondage festlegen.** Führt zu späten
  Korrekturen.
- **Folien überarbeiten ohne Eckpapier.** Führt zu Inkonsistenzen, die
  später gefunden und ausgebessert werden müssen.
- **Englische und deutsche Materialien parallel pflegen.**
  Inkonsistenzen schleichen sich ein. Besser: eine Sprache als
  Wahrheits-Standard, die andere als Übersetzung.
- **Verschiedene Notations-Konventionen mischen.** Selbst kleine
  Inkonsistenzen (β vs. b, ε vs. e) werfen Studierende.
- **Erkenntnis-Modi mischen.** „Beschreiben" ohne Inferenz und
  „Erklären" mit Inferenz dürfen nicht in derselben Folie stehen, ohne
  klare Markierung.
- **Stilistische Entscheidungen unter Diskussion treffen.** Action
  Titles oder nicht? Lieber zwei Versionen erstellen und vergleichen.
- **Sich auf Erinnerung verlassen statt zu suchen.** Bei Datensatz-Fragen
  die Daten direkt prüfen, nicht aus dem Bauch heraus annehmen.
- **Zu viele Labs in einer Session.** Vier Labs in 180 Min sind
  unrealistisch — die Studierenden hetzen, die Reflexion bleibt auf der
  Strecke. Lieber 2–3 Labs + 1–2 Demos.
- **Setup-Inhalte in der Live-Session.** Wer technisch hängt, blockiert
  die ganze Gruppe. Setup in Moodle-Vorbereitung auslagern.
- **Zwei parallele Folienstände pflegen (Live + Nachbereitung).**
  Doppelarbeit ohne didaktischen Mehrwert. Ein Foliensatz +
  Lab-Manuskripte mit faltbaren Callouts erfüllen beide Zwecke.
- **Inhalt zu früh festklopfen, ohne Time-Boxing zu prüfen.** Eckpapier
  ohne Zeitschätzung produziert übervolle Sessions. Time-Boxing gehört
  in die Eckpapier-Phase, nicht erst in die Material-Phase.
- **Inline-R syntaktisch nutzen, aber neben den Inline-Ausdrücken
  *Beispielwerte zur Illustration* nennen.** Die Beispielwerte sind
  schnell Erinnerungs-Schätzungen, nicht verifiziert. Studierende sehen
  am Ende: Lab-Output ≠ Lab-Text. Vertrauen geht verloren. Korrektur:
  *entweder* nur Inline-Ausdruck (Zahl wird beim Rendern eingesetzt)
  *oder* Wert aus laufendem Sondage-Skript zitieren — nie beides
  vermischen.
- **Konventionen pro Lab anders setzen** (`%>%` vs. `|>`, `saveRDS()`
  vs. `write_rds()`, `tbl_`-Präfix mal ja, mal nein). Studierende lernen
  Konventionen mit. Inkonsistenz wird zur Lehre. Korrektur: einmal
  festlegen, in den Coding-Standards (§5.7) verankern, überall halten.
- **Intuitive Hauptstore-Wahl** ohne datenbasierte Multi-Kriterien-
  Prüfung. Führt zu Lab-Designs, die auf scheinbar plausiblen Annahmen
  beruhen, die in der Sondage nicht halten (Beispiel: ER114 → Store 51
  zeigt kein OVB lokal; Store 114 ist klar überlegen). Korrektur: vor
  der Lab-Entwicklung *systematische* Store/Variablen-Auswahl mit
  Bewertungs-Skript.

---

## 10. Checklist für die Eckpunktepapier-Erstellung

Wenn ein Eckpunktepapier neu erstellt wird, sollten diese Punkte
beantwortet sein:

- [ ] Framing: warum dieser Kurs?
- [ ] Storyline: in 5 Sätzen
- [ ] Drei Sessions/Blöcke: Erkenntnisinteresse pro Block
- [ ] Erzählform: Translator? Narrative? Datensatz-getrieben?
- [ ] Datensatz-Wahl mit Begründung
- [ ] Datenkuratierungs-Lehrmomente
- [ ] HARKing-Lösung (falls induktive→konfirmatorische Sequenz)
- [ ] Cliffhanger zwischen den Sessions
- [ ] Notations-Standard
- [ ] Zentrale Literatur (mit ausdrücklicher Anschluss-Markierung)
- [ ] Aha-Momente pro Block
- [ ] Didaktische Prinzipien
- [ ] **Lab/Demo-Verteilung pro Session realistisch (2–3 Labs + 1–3 Demos)**
- [ ] **Time-Boxing pro Session geprüft (180 Min eingehalten?)**
- [ ] **Setup-Inhalte als Moodle-Vorbereitung ausgelagert?**
- [ ] Was steht fest, was als nächstes ansteht, was offen bleibt

---

*Dokument-Ende.*
