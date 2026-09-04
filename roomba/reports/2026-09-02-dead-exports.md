# roomba: dead-exports - 2026-09-02

| Feld | Wert |
|---|---|
| Job | `dead-exports` (Katalog #3, Cooldown 14d) |
| Score | unendlich (`Zuletzt gelaufen = -`) |
| Uebersprungen | `deps-audit` (Score 0.14), `doc-drift` (offener PR [#49](https://github.com/fabiandistler/lintrhelper/pull/49)) |
| Baseline vorher | 0 errors, 0 warnings, 1 NOTE |
| Baseline nachher | 0 errors, 0 warnings, 1 NOTE (identisch) |
| Streichungen | keine |
| Diff | 1 Zeile Code (`scripts/roomba-scan.sh`) + Report + ROOMBA.md |

## Rest-Frage

> Ist ein Export ueber die Paketgrenze hinweg wirklich tot?

**Antwort: nein - keiner der 13 Exporte ist tot.** Alle 13 sind belegt lebendig, und
zwar nicht nur per Textsuche, sondern durch Ausfuehrung.

## Befund 1 - keine toten Exporte (13 von 13 belegt)

`NAMESPACE` exportiert 13 Objekte. Fuer jedes liegen mehrere unabhaengige Belege vor.

### Staerkster Beleg: die Beispiele laufen

`R CMD check` meldet `* checking examples ... OK` (Zeile 86 des Check-Logs). Jeder der
13 Exporte hat einen `\examples{}`-Block in seiner `.Rd`, und `R CMD check` **fuehrt
diese Bloecke aus**. Das ist kein Grep-Treffer, sondern der Nachweis, dass jeder Export
existiert, aufrufbar ist und ohne Fehler durchlaeuft.

### Referenzen je Export

Zaehlung ueber Wortgrenzen (`\bname\b`), ohne `man/` (Begruendung siehe Befund 2):

| Export | R/ | tests/ | vignettes/ | README | NEWS | `\examples` |
|---|---|---|---|---|---|---|
| `create_assignment_linter` | 3 | 2 | 1 | 1 | 3 | ja |
| `create_function_call_linter` | 3 | 4 | 2 | 1 | 3 | ja |
| `create_simple_linter` | 4 | 6 | 3 | 2 | 2 | ja |
| `enforce_assignment_operator` | 4 | 4 | 4 | 3 | 2 | ja |
| `forbid_functions` | 5 | 6 | 7 | 5 | 1 | ja |
| `forbid_symbols` | 3 | 2 | 7 | 5 | 3 | ja |
| `limit_line_length` | 3 | 2 | 5 | 2 | 2 | ja |
| `quick_test` | 2 | 14 | 3 | 4 | 2 | ja |
| `require_function_arguments` | 2 | 2 | 5 | 2 | 2 | ja |
| `require_function_naming_pattern` | 3 | 2 | 5 | 2 | 3 | ja |
| `require_naming_pattern` | 4 | 4 | 7 | 4 | 3 | ja |
| `start_mcp_server` | 4 | 5 | **0** | 4 | 2 | ja |
| `test_linter` | 9 | 47 | 16 | 20 | 1 | ja |

Kein Export hat eine leere Zeile. Alle 13 stehen ausserdem im Referenz-Index von
`_pkgdown.yml` (vier Gruppen, kein Export fehlt) und haben eine eigene `man/*.Rd`.

### Pflichtbelege aus `references/jobs.md`

- **`git grep -n <name>`** - fuer alle 13 durchgefuehrt, Ergebnis in der Tabelle oben.
- **NAMESPACE / re-exports** - 13 `export()`-Eintraege, ein `importFrom(utils, capture.output)`.
  Keine Re-Exports, keine `export()`-Zeile ohne zugehoerige Funktion.
- **Vignetten, Tests, `inst/`, Beispiele** - `inst/` existiert in diesem Repo nicht.
  Tests referenzieren alle 13; Vignetten alle bis auf `start_mcp_server`.
- **Reverse-Dependencies** - `tools::dependsOnPkgs()` ist hier **nicht anwendbar**:
  `lintrhelper` ist nicht auf CRAN (`available.packages()` gegen cloud.r-project.org:
  `FALSE`), sondern GitHub-only. Konsumenten sind daher nicht ueber die
  CRAN-Abhaengigkeitsgraphen auffindbar. Das ist ein Argument *gegen* Streichungen,
  nicht dafuer: unbekannte externe Nutzer koennen jeden Export aufrufen.
- **`git log -S<name>`** - gegenstandslos bei null Streichungen. Gegenprobe fuer die
  schwaechste Zeile `start_mcp_server` (0 Vignetten-Referenzen) trotzdem gefahren:
  eingefuehrt in `94f4b4b Add MCP server exposing lint_file to coding agents`, also
  ein bewusst hinzugefuegtes Feature mit 4 README- und 5 Test-Referenzen. Nicht tot,
  nur nicht in einer Vignette dokumentiert - und Doku ist Sache von Job `doc-drift`.
- **Dynamische Aufrufe** - `grep` ueber `R/` nach `do.call`, `getExportedValue`,
  `match.fun`, `get(`, `getFunction`: **null Treffer**. Es gibt kein Registry-Muster
  und keine dynamische Dispatch-Ebene, die eine Nutzung vor der Textsuche verstecken
  koennte. Die Grep-Belege sind hier also vollstaendig, nicht nur indikativ.

**Konsequenz: keine Streichung.** Der Job liefert in diesem Lauf keine Codeaenderung am
Paket.

## Befund 2 - die Vorstufe konnte gar nichts finden (behoben)

`scripts/roomba-scan.sh dead-exports` durchsuchte `tests`, `vignettes`, `inst`, **`man`**
und `demo` nach jedem Exportnamen und meldete alles ohne Treffer als Kandidat.

`man/` wird von roxygen2 aus genau diesen Exporten generiert - pro Export entsteht
`man/<name>.Rd` mit `\name{<name>}` und `\alias{<name>}` darin. Ein dokumentierter
Export findet sich damit **immer selbst**.

Beleg, gefahren gegen dieses Repo:

```
dirs=man                        -> ohne Referenz: keine
dirs=tests,vignettes,inst,demo  -> ohne Referenz: keine
dirs=vignettes                  -> ohne Referenz: start_mcp_server
```

Die erste Zeile ist der Nachweis: `man/` allein deckt bereits alle 13 Exporte ab. Fuer
jedes roxygen2-dokumentierte R-Paket ist die Kandidatenliste dadurch strukturell leer -
der Scanner haette einen wirklich toten Export nicht melden koennen.

**Behoben:** `man` aus dem `dirs`-Vektor entfernt (`scripts/roomba-scan.sh`, 1 Zeile).

Wichtig fuer die Bewertung dieses Laufs: **das Ergebnis haengt nicht am Defekt.** Der
korrigierte Scanner meldet ebenfalls `keine` - alle 13 Exporte sind bereits in `tests/`
referenziert. Der Fix veraendert die heutige Antwort nicht, er stellt nur her, dass der
naechste Lauf in 14 Tagen eine Aussage treffen kann.

`scripts/` steht in `.Rbuildignore` und ist nicht Teil des Paket-Builds. Die Aenderung
kann Paketverhalten nicht beruehren; Regel 6 des Katalogs ist gewahrt.

Derselbe Defekt steckt im Plugin-Asset unter
`~/.claude/skills/roomba/assets/roomba-scan.sh` und trifft jedes Repo, das roomba dort
bootstrappt. Das Asset wird aus einem Repo-Lauf heraus bewusst **nicht** angefasst.

## Bewusst nicht getan

- **Keine Streichung.** Es gab keinen belegbaren Kandidaten. Ein Export ohne
  Vignetten-Erwaehnung ist ein Doku-Befund, kein toter Export.
- **Kein Umbau der `man/`-Auswertung.** Denkbar waere, `man/` zu behalten und nur
  `\examples`/`\seealso` auszuwerten statt `\name`/`\alias`. Der Scanner deklariert seine
  Ausgabe selbst als "KANDIDATEN, kein Beweis" - Ueberproduktion ist dort korrektes
  Verhalten, und der minimale Fix reicht. Nicht umgesetzt.
- **Kein Anfassen des Plugin-Assets** (siehe oben).
- **Keine Vignetten-Ergaenzung fuer `start_mcp_server`.** Das ist Doku und gehoert in
  `doc-drift`, der gerade in PR [#49](https://github.com/fabiandistler/lintrhelper/pull/49) liegt.

## Bewertung des Jobs

Erster Lauf, null Befunde am Paket. Das ist noch **kein** Grund, `dead-exports` aus dem
Katalog zu nehmen: bei 13 Exporten und einem Paket, dessen einziger Zweck das Anbieten
dieser 13 Funktionen ist, ist ein leerer Lauf das erwartete Ergebnis. Als Datenpunkt
vormerken - bleibt der Job auch beim zweiten und dritten Lauf leer, gehoert der Cooldown
verlaengert oder der Job gestrichen.
