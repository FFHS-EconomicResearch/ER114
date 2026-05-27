# ER114 Übergabeprotokoll FS26 (v2)

**Stand:** 2026-05-20
**Status:** Lab 1 final, Lab 2 patches integriert, Lab 3 v1 erstellt

---

## 0. Projekt-Kontext

**ER114** ist ein Master-Level-Bridge-Modul der FFHS, Vorbereitung auf
ER017 (Supervised und Unsupervised ML mit Zeitreihen, Panel und Kausalanalyse).

**Curriculum:**
- 3 Live-Sessions × 180 min
- Session 1: Deskriptive Statistik im Tidyverse, Quarto, CRISP-DM und Explorative Datenanalyse mit Grundsätzen der Modellierung (Regression mit Interaktionsterm)
- Session 2: Stichproben, Bootstrap, p-Hacking, FDR, Expected Value, R-Pakete (moderndive, infer)
- Session 3: lineare Modellierung (Lab 1), Regularisierung (Lab 2), Kausale Inferenz (Lab 3)

**Aktueller Stand:** FS26.

---

## 1. Etablierte Konventionen

### 1.1 Konzeptuelle Konventionen

**Pfad E/P-Dichotomie** (Shmueli 2010 + Breiman 2001):
- Pfad E1: klassische statistische Inferenz
- Pfad E2: kausale Erklärung via Studiendesign
- Pfad P: prädiktive Modellierung

**DGP-Klammer** als roter Faden — Lab 1 + Lab 2 + Lab 3.

**Spiess-Argument als zweite Modul-Klammer** (NEU):
- Lab 1 "Note of Caution": Unbiasedness als credibility scaffold
- Lab 2 Bias-Variance: zwei Lesarten (statistisch + strategisch)
- Lab 3: Sample-splitting operationalisiert (DML), Ausblick auf Konvergenztendenzen von Pfad E/P (Machine Learning for Causal Analysis, J. Spiess und weitere Forscher in diesem Gebiet - Recherche offen)

**Sarah Chen Narrative:**
- VP Marketing bei Dominick's Finer Foods
- Question 1 (Pfad E): Werbeeffekt
- Question 2 (Pfad P): Bestandsplanungs-Tool
- Drei Memos: T4 Lab 1, T7 Lab 2, T_final Lab 3

### 1.2 Datenkonventionen

**Datensätze:**
- `tbl_s114`: Store 114 baseline (~351 Beob., Lab 1 Pfad E)
- `tbl_stores`: 83 Stores (~28 947 Beob., Lab 1/2/3)
- `tbl_demo`: 12 Demographics (Lab 2)
- `tbl_full`: Join `tbl_stores` + `tbl_demo` (Lab 2/3)

**Naming-Konvention:** mit date_stamp <- "YYYYMMDD" und glue-Paket
- `dominicks_s114_{data_stamp}.rds`
- `dominicks_stores_{data_stamp}.rds`
- `dominicks_demo_{data_stamp}.rds`

**Variablen:**
- `logmove`: log weekly sales (für Sommer-Iteration Session 1 zunächst als weekly sales mit schiefer Verteilung als Übung für Anwendungsbereiche log-Transformation)
- `feat`: feature advertising indicator
- `deal`: deal indicator (Confounder)
- `price` / `log(price)` (für Sommer-Iteration Session 1 - zusammenspiel mit logmove und Interpretation als Preiselastizität der Nachfrage - Box aus Session 3 Lab 1 vorziehen)
- `brand`: Faktor (dominicks/minute.maid/tropicana), Dominick's = Referenz

### 1.3 R-Code-Konventionen

- `%>%` durchgängig
- `read_rds`/`write_rds`
- `tbl_`-Präfix
- `moderndive::get_regression_table()`
- Inline-R für alle datengetriebenen Zahlen. (für Interpretation stets konkrete Plots und Kennzahlen anfragen, statt zu raten)
- `xfun::from_root()` mit Mehrargument-Notation

### 1.4 Workflow-Konventionen

- Stop-and-go
- Adv. diabolis bei großen Entscheidungen
- Patches als reines Markdown
- Halluzinationsvermeidung (vgl. für Interpretation stets konkrete Plots und Kennzahlen anfragen, statt zu raten)
- Sources immer verifiziert (z.B. Spiess (2025) Econometrica 93(5), 1779-1810)
- bei Quellen immer .bib Einträge liefern (soweit möglich mit DOI)
- Erstellung Companionscript: nach Finalisierung (vorher fragen!) der Labs R-Code-Extract (Bereitstellung .R-Datei mit Lab-Code, den Studierende in Live Session brauchen, ohne gt-Formatierungspipes oder ggplot-Layout etc.)


---

## 2. Lab 1 FS26 — Status abgeschlossen

**Datei:** `Session3_Lab1.qmd` (1948 Zeilen inkl YAML)

**Schlüssel-Bausteine:** Pfad-Dichotomie, OVB-Demo via `deal`, Mini-DAG (SVG),
BLUE-Brücke, Cluster-CV-Box, Newey-West-Box, Note of Caution mit Spiess-Anker.

**Schlüssel-Verknüpfungen für Lab 2/3:**

| Lab-1-Element | Z. | Lab-2/3-Anschluss |
|---|---|---|
| Mini-DAG `OVB-DAG.svg` | 1105 | Lab 3 Step 1 |
| Newey-West Box `#robust-se` | 785 | Lab 2 (Erinnerung), HS26 als eigenständige Box |
| BLUE-Brücke "Two distinct repair strategies" | 1232-1245 | Lab 2 Step 5 als operative Einlösung |
| Cluster-CV Box `#cluster-cv` | 1378 | Lab 2 Step 4 |
| "Note of Caution" Spiess-Argument | 451 | Lab 2 Bias-Variance + Lab 3 DML |
| Mini-DAG (deal als Confounder) | 1105 | Lab 3 Step 1 als Lehrbuch-Beispiel |
| T4 Memo "A/B-Test erforderlich" | 1306 | Lab 3 Step 2 als Antwort |
| Lucas-Caveat | 1234 | Lab 2 + Lab 3 |

---

## 3. Lab 2 FS26 — Status: Patches integriert

**Datei:** `Session3_Lab2_v2.qmd` → erweitert auf 1240 Zeilen.

**Integrierte Patches:**

| # | Patch | Wirkung |
|---|---|---|
| L1 | DGP-Klammer Step-0-Eröffnung | DGP als roter Faden aktiv |
| L3 | Cluster-CV-Anker auf Lab 1 | Lab-1-Hook |
| L4 | Bias-Variance-Box | Formale Grundlage für Lasso |
| L5 | Fan/Han/Liu Background-Box | Vier-Probleme operativ |
| L8 | Regularization-Bias-Warnung | Lasso ≠ kausal |
| L9 | Lucas-Caveat | Lab-1-Hook |
| L10 | Lab-3-Forward-Reference ausgebaut | Brücke zu Lab 3 |
| L11 | Spiess: zwei Lesarten der Bias-Variance | Institutional argument |
| L12 | Sample-Splitting-Brücke zu DML | Operative Brücke |
| L13 | händische Reparatur Datenimport und ```{r lasso-coefficients} (get_regression moderndive produzierte Fehler)|

**Schlüssel-Verknüpfungen für Lab 3:**

| Lab-2-Element | Lab-3-Anschluss |
|---|---|
| Bias-Variance-Box `#bias-variance` (zwei Lesarten) | Lab 3 Step 5 (Human-in-the-Loop) |
| Fan/Han/Liu Box `#fhl-four-problems` | Lab 3 Step 4 (DML) |
| Regularization-Bias-Warnung (Lasso ≠ kausal) | Lab 3 Step 2 (A/B-Test als Lösung) |
| Lucas-Caveat | Lab 3 Step 3 (DiD) |
| Sample-Splitting-Brücke zu DML | Lab 3 Step 4 (DML-Sneak-Peek) |

---

## 4. Lab 3 FS26 — Status: v1 erstellt

**Datei:** `Session3_Lab3_v1.qmd` (827 Zeilen)

**Bausteine:**

| Step | Inhalt | Lab-1/2-Verknüpfungen |
|---|---|---|
| Step 0 | Memo, Roadmap | Lab 1 OVB, Lab 2 Lasso |
| ML/Ökonometrie-Frame | drei Pfade als Antworten | Spiess, DML als Versprechen |
| Step 1 | DAGs: Confounder, Mediator, Collider | Mini-DAG aus Lab 1 |
| Step 2 | A/B-Test mit Simulation | Lab 1 T4-Memo |
| Step 3 | DiD + Borusyak et al. (Event Study Box) | Lab 1 Lucas-Caveat |
| Step 4 | IV + DML-Sneak-Peek + FE-Verweis | Lab 1 Note of Caution, Lab 2 Spiess |
| Step 5 | Synthese + Human-in-the-Loop + Memo T_final | Alle drei Labs |
| Bonus B1-B5 | Collider-Sim, Power, Event Study, Hausman, Sensitivity | — |

---

## 5. Verwendete bibtex-Keys

**In allen drei Labs:**
- `shmueli_2010`
- `breiman_2001`
- `mullainathan_MLApplied_2017`
- `fan_bigdata_2014`
- `vonAuer_2023`
- `angrist_pischke_2009`
- `wooldridge_2010`
- `belkin_2019`
- `spiess_optimalEstimation_2025` (NEU — durchgängig)

**Lab 1 spezifisch:**
- `white_1980`, `newey_west_1987`, `long_ervin_2000`

**Lab 2 spezifisch:**
- `tibshirani_1996` (Lasso)
- `chernozhukov_DoubleML_2018` (DML-Vorgriff)

**Lab 3 spezifisch:**
- `chernozhukov_DoubleML_2018`
- `borusyak_etal_2024` (DiD-Imputation)
- `pearl_2009` (DAGs)
- `athey_imbens_2019` (ML-Econometrics-Interface)
- `rambachan_roth_2023` (Sensitivity in DiD)

---

## 6. Folien


**Allgemeine Funktion:** 
  * Kompakte, möglichst visuelle Orientierung mit Anspruch auf Breite
  * Einordnung und Motivation der Labs (dort dann die Tiefe in Gruppenarbeit sowie Selbststudium - vgl. Navigationshilfe in den Labs, bspw. Background Boxen für Selbststudium, Task-Boxen für Live-Sessions)

**Referenzen zur Berücksichtigung:**
  * Huntington-Klein (https://theeffectbook.net/) zur "Anmoderation von Lab 3"
  * Cunningham (https://mixtape.scunning.com/), (https://github.com/Mixtape-Sessions)

**Format:** für FS26 noch .Rmd, für Sommeriteration dann Umstellung auf .qmd

**Vorhandene Bausteine**:

**Drei englische Variantenentwürfe vorhanden** (Stand 11. Mai 2026):
- `Session3_FS26_VarianteA.Rmd` (1796 Zeilen) — ausführlichste Variante,
  vermutlich überladen
- `Session3_FS26_VarianteB.Rmd` (1021 Zeilen) — Titel: "Predict or 
  Intervene? Two Paths Beyond Inference"
- `Session3_FS26_VarianteC.Rmd` (1162 Zeilen) — selbe Headline wie B

**Deutscher Erstentwurf** `PVA3_slides.Rmd` (alte Modulstruktur ER014) — 
nicht als Basis verwenden, aber als Pool für selektive Übernahmen:
- Induktion/Deduktion in Data Science
- AI User vs. AI Value Creator
- Andere konzeptuelle Folien

**Vorgehensweise (Best Practice):**
1. Eine der drei Varianten als Basis wählen (Sichtung + Auswahl, 1 h)
2. Anpassung an aktuellen Lab-Stand: DGP-Klammer, Spiess-Argument, 
   Lab-Verweise (3 h)
3. Selektive Übernahmen aus PVA3 (1-2 h)
4. Bridge-Slides Lab 1→2 und Lab 2→3 (2 h)
5. Athey-Hotel-Anker als wiederkehrendes Beispiel (1 h)

**Nicht empfohlen:** PVA3 komplett übersetzen, dann umstrukturieren — 
das wäre 12-15 h Doppelarbeit, weil die englischen Varianten bereits 
existieren und PVA3 die alte Modulstruktur enthält.

**Gesamtaufwand Foliensatz:** 8-9 h (mit den drei Varianten als 
Startpunkt).

**Stilreferenz:** `Session1_slides_FS26_final.Rmd` und 
`Session2_slides_FS26.Rmd` — gleicher Stil, gleiche Themes, gleiche 
Visual Identity sollte übernommen werden.

**Ausblick** Pfad-Konvergenz: Hierzu dieses Material aus Mixtape with Scott (https://causalinf.substack.com/p/s4e8-jann-spiess-machine-learning?utm_source=publication-search) prüfen: "Well, thanks a lot. And I should also say that some of those papers still have to be written. So for some of them, you may have to use some more patience. And we didn't even get into two-way fixed effects.
I know. We didn't even get to it. That was going to be the main one. Everybody's going to be really upset that I didn't do it. But when you started talking, I am kind of curious, though. When did that project first kind of start getting on your radar? Sure.
So, I mean, this started a project by Kirilla and Savir, who were my roommates in graduate school. Oh, y'all roommates? Y'all live together? No, sorry, like office mates.
Office mates. Okay.
Yeah, office mates. So, you know... We continue talking about econometrics. And actually, Xavier was important for the paper with Alberto. He asked a question about what he should do with his standard errors after metric. So we always had this conversation where I think all three of us, and Xavier and Kirill in particular,
come more from an applied micro world and are not primarily econometricians. But I think that's what makes the econometrics work so influential. You know, I would say the same thing about people like John Roth and Peter Hall, for example, who really like embrace kind of a very kind of applied economist mindset and say, oh,
here's an important thing that people actually do. Can we create some clarity? Yeah. And so I think they had this finding that there are some funny things happening and that there are these negative weights in these two way fixed effect regression. So.
Were you surprised to hear that there were negative weights in the regression since you had all that probability background and stats background?
I had never really understood what these fixed effects really do. So I wasn't necessarily surprised because I didn't have a strong prior. To me, that just looked very complicated, like the thing I had never touched because I look at it and I can't really make
sense of it through the perspective of kind of a very agnostic point of view. So I think for us, then what we did is we just tried to, like my language, like try to take everything apart before putting it together in exactly the right way. So for me, this was really...
about curiosity about what's actually going on in a frequent case and trying to understand what it is. And so I actually think, you know, this is more a comment on this two way fixed effect literature. You can see the different papers in this area and ours as like proposing different estimators that like magically fix something.
For me, the main point of our paper, the main aha was really to say, Here is the general structure of what we find when we take those things apart, and specifically that there is a problem where you get bias, which represents these negative weights, but in general just weights that you didn't quite anticipate,
because you're really mixing multiple things when you write down a linear regression specification. You're mixing, here is an estimate I care about, which is usually some average. So you think, okay, I put in a linear regression, it's going to give me some average. But you've also made some strong assumptions about the structure of what's going on.
In this case, you have made a strong assumption about the structure of treatment effects. Now, if you take that away and you start at least with a clean slate, instead with a clean slate where you don't impose that structure on treatment effects, then it turns out that you just have to approach your regression in a different way.
And so for me, this is like some insider baseball now, but I think you know this literature pretty well. So for me, the main insight there was to say that no matter how you fix that problem, it always has the structure that you kind of have this distinction between learning the model for the control units
then imputing them for the treated units and estimating the treatment effects where you don't want to mix those two things. Right. Estimating a model for the controls is a different exercise from estimating treatment effects. It's really like related to sample splitting and machine learning. I was just about to say that.
So that you have to separate the different ones in the right way. So now for me, it's less about exactly the way that you do that first step of imputing. It more like says, okay, we now have a tool. So what is it we can hope as econometricians to do?
the best we can hope to do is to make transparent to an applied researcher what assumption they have to put in, what it gets them, and where the weakness of that could be. Now we have a framework where you can clearly say, okay, if I don't want to take a stance on heterogeneity of treatment effects,
I really have to separate those two things. And then I have to take a stance about how I'm willing to impute my control outcomes. And, you know, To be totally honest with you, don't tell it to Savi and Kirill. I don't have the strongest view about what the right way is of doing an imputation
because that really depends on what you believe in. What do you think is a good control group? Is it really the case that they're never treated? Are the best control group? Maybe they're very different because they're never treated. Maybe we should not use them. Maybe we should use them because they're super clean.
What I think of us or what I try to understand is as part of this project was really what's the structure of that exercise and how we can represent to an applied researcher what assumptions they're making and what it gets them. And so, yeah, so that's where we came to that. And for me,
that was very much about understanding something that I found really complex because when I look at a fixed effect regression, I don't know what's going on. So just taking this apart, putting it again together in a right way. So yeah, that's my read on it.
I actually had not, you know, I'm so dim. I had not really thought until you just started talking And because we had talked about the sample splitting earlier, I hadn't really thought, you know. I wonder if Jan is thinking about the sample splitting and it makes this imputation feel really like a natural thing. I mean, it's not...
it's not that no one had, you know, in the Heckmanicomera and Todd, the way that I, the way I think about that paper is imputation off of the control group too. And Abadie and Imbuds estimate the selection bias under unconfoundedness doing just for the ATT, just doing it.
But it does have a lot of the, of your fingerprints too. The imputation estimator has a lot of your fingerprints because the, You've got all these high dimensions. You've got all these fixed effects that you're going to like just use this. You're going to use just the control group sample and you're going to, you know,
use those to impute these missing counter these missing potential outcomes. Was that is that part of the early conversations that you guys are having that you sort of see that this might be the way to go about it?
It's an interesting question. So I would actually say it's not the starting point, it was the result. So what I really like doing is trying to write on a model and then not just saying, oh, here's somewhere we could do it, let's evaluate it, but more like saying, is there a systematic way where we can say,
what is the structure of the general answer to this problem? Sometimes, you know, that doesn't lead anywhere. Sometimes in this case, It came out of that. So I... It's how you do your Lego.
It's how you do your Legos.
You're like very, very... I want to take it all apart and then put it together. So sometimes this takes forever, right? And even this one took maybe too long. But sometimes it's very satisfying. And so what I would characterize as the main result in our paper is not you have to
do the imputation one way or the other. It's like no matter which of those estimators you use, like use one that Pedro Santana has worked on, You're still doing imputation. You're just doing the imputation, maybe by a different way. So it just gives us a frame to compare what's going on. And in my eyes, you know,
about that literature is like, to me, that's a starting point to say, how should we do this imputation? It doesn't really make the parallel trends assumption any weaker or stronger. You still have to take a stand. So it's not like now magically we have a box that we should all believe in more. Yeah, yeah, yeah.
If anything, it raises the question what the right way of imputing is. Is it really using these parallel trends? Or should we maybe sometimes use something less parametric? Should we go the way of matrix completion in the same way that Hideo has worked on? Is synthetic control maybe a better way of imputing?
How can we do that in like a staggered adoption setting? So for me, that's a starting point. It's interesting that the literature has partly embraced that as, oh, we now have an estimator we can somewhat trust more. But, you know, like in my eyes,
the main thing here is that it allows us to be a bit cleaner about the mapping from assumptions to conclusions. But of course, that doesn't reduce at all our necessity to be very careful about assessing those assumptions.
Yeah, yeah, yeah. We think pretty similar. Well, I recognize my thoughts and what you said. That's very cool. That's very cool. Yeah,
actually, I was curious how you see the overall trajectory of that difference in difference literature, because it is interesting how I had not anticipated how quickly applied empirical researches would take up what kind of apply to the conditions I've been doing in this space.
Yeah, right. I mean, I think that... I think the stuff, Kirill and... Is it Xavier? Is that how you say his first name? Kirill, Xavier?
Sorry, I missed you for a moment because I managed to... It's Xavier...
I think them identifying early on this, these weird properties. And then as that, you know, way before the estimators, I think like I think the fact that the weird property rumor starts kind of like emerging, you know, and then spreads throughout the applied community. I think like that just freaked everybody out because,
and then it just created its own demand for solutions. So they continue to use it, you know, and, uh, There, if you look, there's like, Paul Goldsmith Pinkham has this interesting extension of that Janet Curry thing where she and her co-authors like scraped all the NBRs, you know, top fives.
And the Diff and Diff stuff has just exploded. You know, it's just continued rising. It was already rising, but it has just gone up and up and up. And it was kind of interesting that Synth has actually, and Paul's, uh, Paul's figure's synth has pivoted and gone back down right after these diff and diff stuff went up.
I always thought this new heterogeneity robust diff and diffs would actually cause an increase in synth because I thought when I used to have this one synth paper when we would present it, so this was like 2014 to 2017, when we would present me and Manisha Shaw's paper on illegalization of sex work,
we would constantly be told to do diff and diff, which basically meant a regression instead of synthetic control. People didn't like synthetic control when we were presenting it because it felt like a big black box to them. Yeah, yeah. So what I thought was going to happen was that realizing that these two-way fixed
effects were black boxes was going to actually reduce the price of using synthetic control. But now I sort of wonder if there just really has always been a very strong preference For the different death, maybe it's because there's just still so many active older people like me and they
just have a significant amount of human capital and just doing something called different death. Or if it was just all the availability of the code, you know, I mean, I think amongst applied people, the reasons for adoption are oftentimes like. Who explains it really well?
And, you know, what's the least amount of human capital that I have to give up?
Great point. So, I mean, going back to our discussion about using some more econ informed models to discuss that, I think there's probably also one aspect where A different diff, there was one way of doing it. So if somebody reported that, we probably say, okay, that's a standard way of doing that. That's fine.
Now you have all these other methods, like which of those should you even use? Interestingly, in a different diff that now has led to this strange thing where people report these three, four different estimators, that honestly, to a first order, all do exactly the same thing.
So it's hard to see that as true robustness checks in the sense of systematically varying the assumptions. It's really like variations on a theme. But I think It's exactly because people want to know what's an okay way to do it. Yeah. And I have bad news because as an accommodation, I can't answer that question.
Like that's a question about mapping the assumption to the estimator. I can only make it more transparent by explaining to you what the implicit assumptions are. Yeah. But I think it's a bit of an equilibrium story. And I mean, I just hope that overall we find protocols for moving that into a space where we
are better able to choose the right tool for the method, which sometimes could be this. And of course, just if Kirill or Savi ever see this, of course, our estimator is the one that you should use it. But, you know, like, sometimes maybe the assumptions are not quite the right one.
And you should then use the other one. And I believe that this imputation structure still, you know, is still there. So that's, for me, kind of one of the main insights here, that this general structure is probably behind, like, I mean, it makes sense exposed, like, if you don't impose any structural treatment effects,
you can't really learn about the imputation from the internal structure of the treatment group. You probably don't want to do that, right? You want to separate that. That's our point. But how to best do that, I think, really depends on the application. And I hope that
This doesn't lead to us being more narrow in that because we now think that we know how to do that because I think it really depends on which setting we're in. But I hope instead that this will lead to more experimentation in this direction of what's the best way of imputing.
So yeah, this is why I kind of confide myself in a way that you just described that.
Well, I think one of the positive things that has come from it, aside from thinking about heterogeneity and how it changes how you go about specifying even a regression model. But one of the things that I think is something you just sort of hinted at, which is parallel trends is with respect to a comparison group.
It's not the property of anything other than The proper control. And, you know, thinking back to the conversation we were having about, you know, you're doing work thinking carefully about the types of selection that, you know, human beings engage in it. Like you sort of said, you know,
maybe the never treated is not the right control because why didn't they get treated?
Yeah.
Why did they why did this state not even pass this thing? And, you know, it's like and so one of the things I think has been really valuable is to force people to say they've got this terminology now that they didn't have before, which is just as common. I think now is parallel trends,
if not more is to say never treated, not yet treated, you know, and all of a sudden you've got. You're thinking you're and already treated. You're thinking now you have to make all these decisions. And the only way to answer not yet treated, never treated is to start thinking about those kinds of things like why these
people get treated? What are all the things that are different about these groups?
You can now have a conversation about the economics of it without hiding the econometrics, but using the econometrics to make it visible.
And I think that's like really, really, really a special surprise gift that the whole thing can do, which is to start making more bridges to thinking about, hey, I am the expert on minimum wages or I am the expert on, you know, birth rates or whatever.
And so let me think really carefully about why these places get it. And it's not just... But it's possible that we're in a bit of a corner solution because if you're showing event studies with six different estimators, it's kind of saying, like, I'm a little overwhelmed.
Yeah, but I think the other thing this points to is like you mentioned before this earlier work by Kirill and Saville where they pointed out the problems, which we then evolved into this paper. I also think it shows a little bit that a good blueprint for econometrics that builds those bridges, right? Like from both sides.
And for example, this like negative weights result, it was already, I think, I don't know about the exact chronology, like my course, this will know better. But I think we were right somewhere that there is actually an appendix of the like work by Savier, D'Orforteux and Clément de Chasse-Martin,
where they also kind of saw these negative weights in a more, from a more kinetics point. But for them, it was kind of an appendix, right? When Kirill and Savier stumbled across that, they saw it more from the applied perspective. And I think there is what I would observe overall in all this work,
like including the work by Clément Savier, but like more broadly also in everything about machine learning. And that's why I wanted to bring this up as an example. I think there's more of a convergence where like the econometrics is sitting more in this middle ground and is taking the empirical exercise more seriously. Like you mentioned,
Viktor Chernazukhov writing a paper with machine learning in the title is also has Esther on it, right? Like that there's more and more of that and that econometrics can really be in this middle ground where it translates for an empirical audience between the assumptions, what they thought they were doing, what they're actually doing,
how they can maybe improve it. And that this just shows how rich that space is, if you're kind of willing to engage both sides. And I mean, I think like, maybe as a summary, you know, I see myself a little bit as trying to be a translator between those things by
building models that allow us to navigate exactly that communication a little bit better. And it's a very abstract thing of saying it, but maybe just saying that, you know, I do have a taste for econometrics that takes that empirical exercise very seriously. And if I can, I think during graduate school,
I probably presented more in front of an applied micro cloud than a pure econometrics cloud. And now it's a bit harder to, you know, go to too many seminars. But I do think it's important that we take both sides of this seriously when we want to build successful empirical tools.
And I think that's been embraced very much by this literature.
Yeah, I see that in you too. I see you as having become this hub between lots of different ideas. The pure math, the econometrics, the behavioral science, the computer science. Now you're talking about OR. It seems like you've put yourself, maybe it was just purely serendipitous,
but you've put yourself at this place where you can be this bridge between a lot of people and a lot of ideas. I see that a lot at Stanford. It seems like uh, Muhammad Oscar barn that I didn't pronounce the last name correctly, but I interviewed him weeks ago and, um, he,
he's pretty similar in a lot of ways to you in that, like he kind of took the long way. And, um, and then he also kind of has these little spokes, you know, spoken wheel kind of thing like you do.
I mean, first of all, this is a great compliment to be compared to Mohammed. I do agree that here at Sanford, you know, it can be confusing at times when you arrive here as a junior because there's all these groups working on all kinds of interesting things.
Every second person here probably does something with AI, so it can be a bit overwhelming. But I do think the one thing I'm really proud of here, what we've done at Stanford over the last few years, is really build an econometrics community that sits in this interdisciplinary space. Yeah.
And I wouldn't just count the kind of obvious, if you want, econometricians like Hito Imbert's. like Lee Hualey, who actually comes from statistics, or Kevin Chen, but also I would count Stefan Wager among them, who comes from stats as an operations group, Vasilis Sekanes, who was in Microsoft Research before,
and is here in the Management Science and Engineering Department. But I also think of as one of the most interesting, kind of more technical econometricians. So I think having a community here that is... not just narrowly focused on one view on econometrics, but brings all these into a room. And of course, you know,
this also involves my more senior econometrics colleagues like Han Hong, Joe Romano, who also bring their own perspectives in. I think having all this in one room is a big strength of being here and a big joy for me. So I think that's definitely like taking a bet that is paying off in that way.
I'm so happy that you're there. They're very lucky to have you. And I know you're saying that you're lucky to have them, but you seem like a really special colleague and scholar. I really appreciate you being on the show and talking to me and being so generous with your time. It's really, really been nice talking.
Thanks a lot. I really enjoyed the conversation. I hope we can continue at some point." und Fachliteratur konsultieren/einbinden!

---

## 7. Offene Aufgaben FS26

```
Lab 1:
[ ] finale Prüfung auf inhaltliche korrektheit (Fachliteratur) und didaktische Stringenz

Lab 2:
[ ] Companion-Script aktualisieren
[ ] Qualitätsprüfung mit next Iteration durch Human Feedback

Lab 3:
[ ] Companion-Script erstellen
[ ] Qualitätsprüfung mit next Iteration durch Human Feedback (insbesondere Schwerpunkte)
[ ] ggdag-Plots ggf. als svg in einheitlicher und publikationsfähiger Optik


Folien:
[ ] Weiterentwicklung Foliensatz unter Berücksichtigung #6 Folien
[ ] Bridge Lab 1 → Lab 2
[ ] Bridge Lab 2 → Lab 3 (Kausalanalyse-Landkarte, 4 Slides)
```

---

## 7. Bekannte technische Eigenheiten

### 7.1 Render-Hänger
RStudio markiert Background Jobs nicht als beendet. Stop-Knopf ausreichend.

### 7.2 Copy-Paste `%>%`
Patches IMMER als reines Markdown liefern.

### 7.3 LaTeX-Sicherheit
`\f`, `\b` werden in Python-Strings als Control-Char interpretiert.
Raw-Strings `r'...'` oder direkt im Editor.

### 7.4 Unicode-Sicherheit
Emojis direkt einfügen, niemals via Python-Escapes.

---

## 8. Eröffnungs-Prompts für neuen Chat

```
Ich arbeite an ER114, einem Master-Bridge-Modul der FFHS.

Stand: Lab 1 für FS26 inhaltlich fertig. Für Lab 2 und 3 sowie die Folien
gibt es mehr oder weniger fortgeschritten Entwürfe. Nach Finalisierung der Labs 
sind noch die Companionscripts zu erstellen.

Während ich noch die Labs 2 und 3 prüfe, soll jetzt der 
Foliensatz für Session 3 Priorität haben. Lies zuerst das ER113_Handover_FS26.md 
und erledige dann folgende Schritte:

Schritt 1: Drei englische Variantenentwürfe vergleichen — 
Session3_FS26_VarianteA.Rmd (1796 Zeilen), VarianteB.Rmd (1021), 
VarianteC.Rmd (1162). Eine als Basis wählen.

Schritt 2: Anpassung an aktuellen Stand der Labs (DGP-Klammer, 
Spiess-Argument, Kausalanalyse-Landkarte).

Schritt 3: Selektive Übernahmen aus PVA3_slides.Rmd (deutscher 
Erstentwurf mit konzeptuell wertvollen Folien wie Induktion/Deduktion).

Schritt 4: Bridge-Slides Lab 1→2 und Lab 2→3 ergänzen.

[Datei-Uploads:]
- Session3_FS26_VarianteA.Rmd
- Session3_FS26_VarianteB.Rmd
- Session3_FS26_VarianteC.Rmd
- PVA3_slides.Rmd
- Session1_slides_FS26_final.Rmd (als Stil-Referenz)
- Session2_slides_FS26.Rmd (als Stil-Referenz)
- Session3_Lab1_fin.qmd (als Lab-Referenz)
- Session3_Lab2_v2.qmd
- Session3_Lab3_v1.qmd
- ER114_Roadmap_FS26.md
- ER114_Handover_FS26.md
```
