# deps-audit - 2026-09-01

Job 1 aus dem Katalog in `ROOMBA.md`. Rest-Frage: **Machen mich diese Updates kaputt?**
Output laut Katalog: Report, kein Code-Change.

## Baseline

`R CMD build .` + `R CMD check --no-manual --as-cran lintrhelper_0.3.2.tar.gz`

| | |
|---|---|
| Status | **2 NOTEs**, 0 WARNINGs, 0 ERRORs |
| NOTE 1 | `checking CRAN incoming feasibility` - "New submission" |
| NOTE 2 | `checking top-level files` - `skills-lock.json` ist nicht-standard |

Beide NOTEs sind vorbestehend und haben nichts mit Abhaengigkeiten zu tun. Da dieser Job
report-only ist, gab es keine Code-Aenderung und damit auch keinen Nachher-Check.

## Was der Scanner geliefert hat - und was nicht

`scripts/roomba-scan.sh deps-audit` meldete 33 veraltete Pakete. Davon sind **drei**
fuer dieses Repo relevant; die uebrigen 30 sind Fremdpakete in der Dev-Library
(`shiny`, `future`, `igraph`, Recommended-Pakete ...) und tauchen weder in `DESCRIPTION`
noch in einem Workflow auf.

Abgleich der tatsaechlich deklarierten Abhaengigkeiten gegen CRAN:

| Paket | Rolle | Installiert | CRAN | |
|---|---|---|---|---|
| lintr | Imports (>= 3.0.0) | 3.4.0 | 3.4.0 | aktuell |
| rlang | Imports | 1.3.0 | 1.3.0 | aktuell |
| xml2 | Imports | 1.6.0 | 1.6.0 | aktuell |
| tools, utils | Imports | 4.6.0 | base | n/a |
| ellmer | Suggests | 0.4.2 | 0.4.2 | aktuell |
| mcptools | Suggests | 1.0.2 | 1.0.2 | aktuell |
| jsonlite | Suggests | 2.0.0 | 2.0.0 | aktuell |
| testthat | Suggests (>= 3.0.0) | 3.3.2 | 3.3.2 | aktuell |
| withr | Suggests | 3.0.3 | 3.0.3 | aktuell |
| knitr | Suggests | 1.51 | 1.51 | aktuell |
| **rmarkdown** | Suggests, VignetteBuilder | **2.31** | **2.32** | veraltet |
| **roxygen2** | Toolchain, in DESCRIPTION gepinnt | **8.0.0** | **8.1.0** | veraltet |
| **pkgdown** | Toolchain, nur `pkgdown.yaml` | **2.2.0** | **2.2.1** | veraltet |

### Deckungsluecke - ausdruecklich nicht geprueft

Die Vulnerability-Haelfte der Vorstufe hat **null Abdeckung erzeugt**:

- `renv::vulns()` - uebersprungen, das Repo hat keine `renv.lock`.
- `oysteR::audit_description()` - uebersprungen, `oysteR` ist nicht installiert.

Das heisst **"nicht geprueft"**, nicht "keine Vulnerabilities". Wer aus diesem Report
liest, es seien keine gefunden worden, liest ihn falsch.

### Das Scanner-Modell passt hier nur halb

`old.packages()` beschreibt die Library der Entwicklungsmaschine, nicht das, womit das
Paket gebaut wird. `R-CMD-check.yaml` faehrt eine 5er-Matrix
(macOS/Windows/Ubuntu x release, devel, oldrel-1) und installiert ueber
`r-lib/actions/setup-r-dependencies@v2` bei **jedem** Push und PR die aktuellen
CRAN-Versionen. Es gibt keinen Lockfile und damit keine Versionsdrift, die man auditieren
koennte - die Frage "bricht mir ein neues lintr/rlang/xml2 den Build?" beantwortet die
CI-Matrix laufend und schneller als dieser Job.

## Befunde

### 1 - roxygen2 8.0.0 -> 8.1.0 - empirisch geprueft, Diff ist eine Zeile

Relevant, weil `DESCRIPTION:33` mit `Config/roxygen2/version: 8.0.0` pinnt: ein Upgrade
regeneriert `man/` und `NAMESPACE`.

Quelle: <https://roxygen2.r-lib.org/news/index.html>. Zwei Eintraege in 8.1.0 koennen die
generierte Ausgabe veraendern:

- *"Multibyte characters inside Rd tags are now handled correctly; previously a tag like
  `\code{café}` would corrupt the markdown processing of the text that followed it."*
- *"`@importFrom` now generates a single multi-line `importFrom()` directive per package
  instead of one directive per symbol."*

Beide haben in diesem Paket **keinen Ausloeser**:

- Nicht-ASCII kommt nur als Geviertstrich in Fliesstext vor, nie *innerhalb* eines
  Rd-Tags oder Code-Spans. Gegenprobe:
  `grep -rnoP '\\[a-zA-Z]+\{[^{}]*[^\x00-\x7F][^{}]*\}' man/` findet nichts.
- Es gibt genau **ein** `@importFrom` im ganzen Paket
  (`R/testing_utils.R:35`, `@importFrom utils capture.output`), ein Symbol aus einem
  Paket. Die Zusammenfassung pro Paket aendert an einem Einzeleintrag nichts.

Gegenprobe statt Vermutung: roxygen2 8.1.0 in eine Wegwerf-Library installiert und eine
Kopie des Pakets damit neu dokumentiert.

```
diff -r man   <regeneriert>/man        -> identisch
diff NAMESPACE <regeneriert>/NAMESPACE -> identisch
diff DESCRIPTION <regeneriert>/DESCRIPTION
  ~ 33  Config/roxygen2/version: 8.0.0 -> 8.1.0
```

`man/` und `NAMESPACE` sind **byte-identisch**. Der gesamte Upgrade-Diff ist die eine
gepinnte Versionszeile.

Nebenbefund, ausdruecklich **keine** Regression: `roxygenise()` meldet 12
`Could not resolve link`-Warnungen (`explain_rule`, `list_rules`, ab
`R/mcp_tools.R:676`). Kontrolllauf mit 8.0.0: ebenfalls exakt 12. Vorbestehend, gehoert
zu `doc-drift` (Job 2), nicht hierher - siehe *Backlog* in `ROOMBA.md`.

**Migrationsaufwand:** eine Zeile. **Risiko:** keins. **Empfehlung: `jetzt`.**

### 2 - rmarkdown 2.31 -> 2.32 - Suggests, zwei neue Mindestversionen

Aufrufstellen: `DESCRIPTION:28`, sowie beide Vignetten
(`vignettes/getting-started.Rmd:3,6`, `vignettes/no-xpath-guide.Rmd:3,6`) ueber
`rmarkdown::html_vignette` und `%\VignetteEngine{knitr::rmarkdown}`.

Quelle: <https://pkgs.rstudio.com/rmarkdown/news/index.html>. Die einzige Aenderung mit
Bruchpotenzial ist eine angehobene Untergrenze:

- Pandoc-Minimum **1.14 -> 2.8**. Lokal: `pandoc 3.1.3`. CI holt ueber
  `r-lib/actions/setup-pandoc@v2` eine aktuelle Version. Erfuellt.
- knitr-Minimum auf **1.50** angehoben. Installiert: 1.51. Erfuellt.
- `html_vignette` nutzt jetzt einen literalen YAML-Block fuer die Vignetten-Metadaten.
  Betrifft die Metadatenbehandlung, nicht das Ausgabeformat.

Der Rest von 2.32 sind LaTeX-/PDF-/`intermediates_dir`-Fixes und die Umstellung auf MIT -
alles fuer ein Paket ohne PDF-Vignetten gegenstandslos.

**Migrationsaufwand:** null im Repo. `rmarkdown` steht ohne Versionsschranke in
`Suggests`, und die CI installiert ohnehin 2.32. Betroffen ist allein die lokale
Dev-Library. **Risiko:** gering. **Empfehlung: `lassen`** (lokal bei Gelegenheit
`update.packages()`; keine Repo-Aenderung noetig).

### 3 - pkgdown 2.2.0 -> 2.2.1 - beruehrt nur den Website-Workflow

Aufrufstelle: ausschliesslich `.github/workflows/pkgdown.yaml`; `pkgdown` steht nicht in
`DESCRIPTION`.

Quelle: <https://pkgdown.r-lib.org/news/index.html>. 2.2.1 enthaelt drei Eintraege: ein
Testfix fuer `R CMD check`, lokale Vorschau ueber einen HTTP-Server (damit die Suche
funktioniert), und *"Code in a link (href) is no longer autolinked."* Keine Breaking
Changes.

**Migrationsaufwand:** null, die CI installiert bei jedem Lauf die aktuelle Version.
**Risiko:** keins. **Empfehlung: `lassen`.**

## Empfehlung zur Automatisierung

Die Vorstufe stellt korrekt fest, dass weder Renovate noch Dependabot konfiguriert ist.
Die naheliegende Schlussfolgerung waere aber falsch, deshalb zuerst die Pruefung:

- **Dependabot** kennt kein CRAN-/R-Oekosystem.
- **Renovate** ebenfalls nicht - in der Manager-Liste
  (<https://docs.renovatebot.com/modules/manager/>) gibt es keinen Manager fuer R,
  CRAN, `DESCRIPTION` oder `renv.lock`.

Fuer die Abhaengigkeiten in `DESCRIPTION` bringt also **keines der beiden Werkzeuge
etwas**. Eine pauschale Empfehlung "Dependabot einrichten" waere hier schlicht falsch.

Was beide koennen, ist `github-actions`, und dort gibt es einen konkreten Anlass. Die
Workflows pinnen ueberwiegend auf gleitende Major-Tags (`@v2`, `@v4`), die sich selbst
aktualisieren. Eine Ausnahme:

```
.github/workflows/pkgdown.yaml:45  uses: JamesIves/github-pages-deploy-action@v4.5.0
```

Exakter Patch-Pin, der still veraltet und den niemand bemerkt. Genau dafuer ist
Dependabot da.

**Empfehlung:** minimale `.github/dependabot.yml` mit **nur** dem
`github-actions`-Oekosystem, monatlich. Nicht Teil dieses PRs - der Job ist report-only
und das Anlegen einer CI-Konfiguration ist eine eigene Entscheidung.

## Zusammenfassung

| Kandidat | Risiko | Empfehlung |
|---|---|---|
| roxygen2 8.0.0 -> 8.1.0 | keins (Diff empirisch = 1 Zeile) | `jetzt` |
| rmarkdown 2.31 -> 2.32 | gering (Minima erfuellt) | `lassen` |
| pkgdown 2.2.0 -> 2.2.1 | keins | `lassen` |

Drei Kandidaten, keine Breaking Changes, keine Codeaenderung. Der eigentliche Ertrag
dieses Laufs sind die drei Aussagen, die man ohne ihn nicht haette: der roxygen2-Pin ist
gefahrlos loesbar, die Vulnerability-Pruefung hat **gar nicht stattgefunden**, und die
Update-Automatisierung kann fuer R-Abhaengigkeiten grundsaetzlich nichts beitragen.
