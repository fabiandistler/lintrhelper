# ROOMBA - Wartungs-Katalog

<!--
Quelle: Prompt-Idee Fabian Distler, 2026-09-01, verarbeitet mit Skill `idee-zu-artefakt`.
Ausfuehrung: Plugin `roomba`, Skill `roomba-run`.
Pruefdatum: 2026-12-01 - siehe Abbaubedingung unten.
-->

## Status

| Feld | Wert |
|---|---|
| Letzter Lauf | 2026-09-01 |
| Letzter Job | `doc-drift` (PR, nur Doku - README, Vignette, roxygen-Kommentare) |
| Naechster faelliger Job | `dead-exports` (nie gelaufen -> Score unendlich, sofort faellig) |
| Offene roomba-PRs | `roomba/doc-drift-2026-09-01` |

## Regeln

1. **Genau ein Job pro Lauf.**
2. **Jobauswahl nach relativer Ueberfaelligkeit:** `score = (heute - zuletzt) / cooldown`.
   Hoechster Score gewinnt, `-` zaehlt als unendlich, Gleichstand nach Katalogreihenfolge.
   Grund: ein 7-Tage-Job wuerde bei Auswahl nach absolutem Datum alle Slots fressen.
3. **Job mit offenem roomba-PR wird uebersprungen** und gilt als in Arbeit.
4. **Jeder Lauf endet in genau einem PR** auf `roomba/<job>-<YYYY-MM-DD>`, auch reine
   Report-Jobs. Kein Commit auf den Default-Branch.
5. **Diff-Budget < 300 Zeilen.** Rest unter *Backlog*.
6. **Verhalten wird nie geaendert.** Aenderungen nur an Doku, toten Exporten und
   Testinfrastruktur - und nur bei identischem Check-Status vor und nach dem Lauf.

## Was NICHT in diesen Katalog gehoert

Alles, was ein Werkzeug abschliessend beantwortet, gehoert ins CI-Gate, nicht in einen
Agentenlauf. Ein Job, der bei gruenem CI regelmaessig nichts findet, verbrennt nur
Rotationsslots.

| Frueher als Job gedacht | Laeuft stattdessen im CI |
|---|---|
| security-footguns | gitleaks, semgrep, bandit + pre-commit |
| dead-code (lokale Vars/Imports) | `ruff F401/F841`, `lintr::object_usage_linter` |

Im Katalog bleibt nur die Rest-Frage: `dead-exports` (Export ueber die Paketgrenze).

### Gate-Zuschnitt in diesem Repo

`.github/workflows/roomba-gate.yml` und `.pre-commit-config.yaml` weichen bewusst von
den Plugin-Assets ab:

- **Blockend nur `lintr::object_usage_linter()`** - aktuell 0 Lints. Das Asset liess
  zusaetzlich `lintr::lint_package()` mit dem vollen Default-Satz hart fehlschlagen;
  das sind hier 36 Lints (`line_length`, `indentation`, `object_name`,
  `commented_code`) und damit ein dauerhaft rotes Gate, das ausserdem dem eigenen
  Step-Namen und der Tabelle oben widerspricht.
- **`lintr::unused_import_linter()` berichtet, blockt nicht.** Die 3 Treffer sind
  `library(lintrhelper)` in `tests/testthat.R` (testthat-Boilerplate) sowie
  `library(lintr)` und `library(xml2)` in `vignettes/getting-started.Rmd`
  (Lehrcode, der so gelesen werden soll). Keiner ist ohne Verhaltensaenderung behebbar.
- **Keine `hashFiles()`-Guards auf Job-Ebene, kein `python-lint`-Job.** Das Asset
  guardet beide Sprachjobs mit `if: hashFiles(...) != ''` auf Job-Ebene. Das ist von
  GitHub Actions nicht erlaubt (`hashFiles` gibt es nur in `jobs.<id>.steps.*`) und
  liess den kompletten Workflow schon vor dem Scheduling scheitern - kein Job lief.
  Da dieses Repo ein reines R-Paket ist, sind die Sprach-Guards hier ohnehin
  gegenstandslos: `python-lint` ist entfernt, `r-lint` laeuft ungeguarded.
  Gegenprobe: `actionlint` ueber alle drei Workflows ist sauber.
- **`local::.` in `setup-r-dependencies`.** `object_usage_linter()` loest
  Cross-File-Referenzen ueber die *installierte* Paket-Namespace auf. Ohne das
  installierte Paket meldet der Linter auf CI vier falsche
  "no visible global function definition" (`create_function_call_linter`,
  `create_assignment_linter`, `mcp_tools`, `create_simple_linter`) - lokal, wo
  `lintrhelper` installiert ist, sind es 0. Das Asset installierte nur `any::lintr`.
- **Kein `.lintr` im Repo-Root.** Dieses Paket ist `lintrhelper`; eine automatisch
  gefundene `.lintr` wuerde auch die eigene Testsuite beeinflussen. Der
  Linter-Zuschnitt steht deshalb inline im Workflow.
- **`.pre-commit-config.yaml` ohne `ruff`/`ruff-format`** (kein Python im Repo) und ohne
  den `lintr`-Hook, der denselben Default-Satz faehrt und jeden Commit an `R/` blocken
  wuerde.

Die vorhandenen Workflows `R-CMD-check.yaml` und `pkgdown.yaml` blieben unangetastet.

## Vorbedingungen je Lauf

- Working Tree sauber, auf dem Default-Branch, `git fetch` gelaufen.
- Baseline erfasst: `R CMD check` / `pytest` **vor** dem Lauf.
- Rote oder fehlende Baseline -> nur Report-Jobs.

## Jobs

| # | Job | Vorstufe | Output | Cooldown | Zuletzt gelaufen |
|---|---|---|---|---|---|
| 1 | `deps-audit` | ja | Report | 7d | 2026-09-01 |
| 2 | `doc-drift` | nein | PR | 14d | 2026-09-01 |
| 3 | `dead-exports` | ja | PR | 14d | - |
| 4 | `error-edges` | nein | Report | 14d | - |
| 5 | `test-flakiness` | ja | PR | 30d | - |
| 6 | `perf-quickwins` | nein | Report | 30d | - |

Rest-Frage je Job (Details im Skill unter `references/jobs.md`):

1. **deps-audit** - Machen mich diese Updates kaputt? Scanner liefert die Liste, der Lauf
   liefert Breaking-Change-Risiko aus gelesenen Changelogs und eine Empfehlung.
2. **doc-drift** - Beschreibt die Doku noch, was der Code tut? Belegt durch Ausfuehren der
   Beispiele. Nur Doku wird angefasst.
3. **dead-exports** - Ist dieser Export ueber die Paketgrenze wirklich tot? Nachweis pro
   Streichung: git grep, NAMESPACE/`__all__`, Vignetten, Reverse-Deps, `git log -S`.
4. **error-edges** - Wo schluckt der Code einen Fehler still? Report, kein PR.
5. **test-flakiness** - Ist die Zeit-/Zufalls-/Netzabhaengigkeit Absicht? Nur die Quelle
   des Nichtdeterminismus wird ersetzt, nie die Assertion.
6. **perf-quickwins** - Messbar langsam oder nur unschoen? Ohne Messung kein Befund.

## Backlog

- **Doku wird nirgends ausgefuehrt.** 13 von 13 `man/*.Rd` liegen komplett in
  `\dontrun{}`, 21 von 24 Vignetten-Chunks sind `eval=FALSE`, ein `README.Rmd` gibt es
  nicht. `checking examples ... OK` und `checking re-building of vignette outputs ... OK`
  laufen deshalb gruen, ohne etwas zu pruefen - genau darum sind im Lauf `doc-drift`
  2026-09-01 zwei nicht existierende `lintr`-Funktionen in der Doku gefunden worden.
  Aufmachen ist kein Doku-Edit: mehrere Beispiele greifen auf `my_script.R` zu, das nicht
  existiert, ein Ausfuehren wuerde die Check-Baseline veraendern. Braucht einen eigenen
  Vorgang mit Fixtures.
- **`linter_name` wird ignoriert.** `create_simple_linter()`,
  `create_function_call_linter()` und `create_assignment_linter()` nehmen das Argument
  entgegen, reichen es aber nicht an `create_linter_factory()` weiter - das dort gar
  keinen solchen Parameter hat. Die roxygen-Doku (`R/simple_linter.R:92`) behauptet
  "used for the lint type". Behebung ist eine Verhaltensaenderung, gehoert deshalb nicht
  in einen roomba-Lauf. Beleg im Report vom 2026-09-01.
- **`.Rbuildignore`:** `roomba` und `skills-lock.json` fehlen und erzeugen die einzige
  offene `R CMD check`-NOTE (`checking top-level files`). `roomba/` hat der
  Wartungsmechanismus mit PR #48 selbst angelegt. Nicht im Lauf behoben, weil das den
  Nachher-Status von der Baseline abweichen liesse und den PR nach eigener Regel
  verworfen haette.
- **Automatisierung:** minimale `.github/dependabot.yml` mit *nur* dem
  `github-actions`-Oekosystem. Anlass: `JamesIves/github-pages-deploy-action@v4.5.0` in
  `.github/workflows/pkgdown.yaml:45` ist exakt gepinnt und veraltet still. Fuer die
  `DESCRIPTION`-Abhaengigkeiten bringt weder Dependabot noch Renovate etwas - beide haben
  keinen R-/CRAN-Manager. Beleg im Report vom 2026-09-01.

## Lauf-Historie

| Datum | Job | Output | PR |
|---|---|---|---|
| 2026-09-01 | `deps-audit` | [Report](roomba/reports/2026-09-01-deps-audit.md) | [#48](https://github.com/fabiandistler/lintrhelper/pull/48) |
| 2026-09-01 | `doc-drift` | [PR + Report](roomba/reports/2026-09-01-doc-drift.md) | `roomba/doc-drift-2026-09-01` |

## Abbaubedingung

Stichtag 2026-12-01. Weniger als vier Laeufe oder kein einziger gemergter roomba-PR ->
zurueckbauen auf eine manuelle Checkliste, Plugin deinstallieren.
