# deps-audit — 2026-08-25

Paket: `lintrhelper` 0.3.2 · Job 1 · Output: Report · Cooldown 7d
(erneut fällig ab 2026-09-01)

## Methodik und ihre Grenzen

In dieser Umgebung ist **kein R installiert** und die Netzwerk-Policy
blockiert `cran.r-project.org` sowie `crandb.r-pkg.org`. Der Audit stützt
sich deshalb auf:

- statische Analyse der tatsächlichen Aufrufstellen (`grep` über `R/`,
  `tests/`, `vignettes/`, `NAMESPACE`, `DESCRIPTION`, `.github/workflows/`),
- Versions- und Advisory-Recherche über Websuche.

Nicht ausgeführt wurden: `R CMD check`, `remotes::check_deps()`,
`renv::status()`. **Die Versionsangaben unten sind recherchiert, nicht
gegen eine CRAN-Installation verifiziert** — vor dem Umsetzen von B1/B2
kurz mit `available.packages()` gegenprüfen. Die Befunde B1, B3 und B5
beruhen dagegen auf dem Repo-Inhalt selbst und sind unabhängig davon
belastbar.

## Ist-Stand der Dependencies

`DESCRIPTION` deklariert **keine einzige Versionsuntergrenze außer
`lintr (>= 3.0.0)`** und **kein `Depends: R (>= x.y)`**.

| Paket | Feld | Deklariert | Aktuell (recherchiert) | Aufrufstellen |
|---|---|---|---|---|
| `lintr` | Imports | `>= 3.0.0` | 3.4.0 (2026-07-16) | 22 |
| `rlang` | Imports | — | 1.3.0 (2026-07-05) | 1 |
| `xml2` | Imports | — | 1.6.0 (2026-06-22) | 11 |
| `tools` | Imports | — | (Basis-R) | 4 |
| `utils` | Imports | — | (Basis-R) | 2 + `importFrom` |
| `ellmer` | Suggests | — | 0.4.1 (2026-05-07) | 33 |
| `mcptools` | Suggests | — | 1.0.0 (2026-07-06) | 2 |
| `jsonlite` | Suggests | — | 2.x | nur `tests/` |
| `testthat` | Suggests | `>= 3.0.0` | 3.3.2 | Testsuite |
| `withr` | Suggests | — | 3.0.2 | nur `tests/` |
| `knitr`, `rmarkdown` | Suggests | — | aktuell | Vignetten |

### Was sauber ist

Vorab, damit die Befunde einordbar bleiben — drei Dinge, die dieser Audit
ausdrücklich **nicht** zu beanstanden hat:

- **Suggests sind korrekt bedingt.** `start_mcp_server()` ruft
  `check_mcp_deps()` (`R/mcp_server.R:53`) vor jeder Nutzung von
  `mcptools`/`ellmer`; `mcp_tools()` ist nicht exportiert und nur von dort
  erreichbar. Alle Tests, die `ellmer`, `mcptools` oder `jsonlite`
  berühren, stehen hinter `skip_if_not_installed()`. Das ist genau die
  CRAN-Anforderung an Suggests, und sie ist erfüllt.
- **Kein Basis-R-Ballast.** `tools` und `utils` werden real benutzt
  (`utils::adist`, `utils::head`, `tools::Rd_db`, `tools::Rd2txt`,
  `utils::capture.output`). Kein ungenutzter Import.
- **lintr 3.4.0 trifft dieses Paket nicht.** Die 3.4.0-Entfernungen
  (`with_defaults()`, `closed_curly_linter()` & Co., neue
  `all_linters()`-Signatur, `source_file=` in `ids_with_token()`) haben
  **null** Aufrufstellen im Repo — belegt per Referenzsuche über `R/`,
  `tests/`, `vignettes/`, `README.md`. Das Repo nutzt durchgängig das
  aktuelle `linters_with_defaults()`. Die Linter werden über
  `lintr::Linter()` (`R/simple_linter.R:3`) korrekt klassiert, sind also
  von der Entfernung "Linter per Name übergeben" nicht betroffen.

## Befunde

Sortiert nach Dringlichkeit.

---

### B1 — `actions/checkout@v4` bricht am 16.09.2026 · Risiko: **hoch** · Breaking-Change-Risiko der Behebung: **keins**

**Fund:** `.github/workflows/R-CMD-check.yaml:34` und
`.github/workflows/pkgdown.yaml:24` nutzen `actions/checkout@v4`. Diese
Version läuft auf Node 20.

GitHub entfernt Node 20 am **16.09.2026** vollständig von den Runnern
(Default-Umstellung auf Node 24 war der 16.06.2026). Das ist in **rund drei
Wochen**. Danach schlagen beide Workflows fehl — der Release-Pfad
(`pkgdown`) eingeschlossen.

Ebenfalls betroffen, gleicher Grund:
`.github/workflows/pkgdown.yaml:41` pinnt
`JamesIves/github-pages-deploy-action@v4.5.0`. Aktuell ist 4.8.0
(2026-01-09), das genau die Node-24-Migration nachzieht.

**Empfehlung:**

```yaml
- uses: actions/checkout@v5                          # beide Workflows
- uses: JamesIves/github-pages-deploy-action@v4.8.0  # pkgdown.yaml
```

`r-lib/actions/*@v2` ist ein gleitender Tag und zieht die Node-24-Migration
selbst nach — dort ist nichts zu tun.

**Breaking-Change-Risiko:** keins für dieses Repo. Der Bruch in
`actions/checkout` v5 betrifft den unsicheren PR-Checkout; beide Workflows
hier nutzen den Default-Checkout ohne `ref:`-Akrobatik.

**Dies ist der einzige Befund mit einer Frist.** Er sollte nicht auf den
nächsten Wartungslauf warten.

---

### B2 — `ellmer` ohne Versionsuntergrenze, Code verlangt aber >= 0.3.0 · Risiko: **mittel-hoch** · Breaking-Change-Risiko: **keins**

**Fund:** `DESCRIPTION` listet `ellmer` in `Suggests` **ohne Version**.
Der Code benutzt aber ausschließlich die **nach 0.3.0** eingeführte
`tool()`-Signatur — `R/mcp_tools.R:13`:

```r
ellmer::tool(
  mcp_lint_file,
  name = "lint_file",
  description = ...,
  arguments = list(path = ellmer::type_string(...)),
  annotations = ellmer::tool_annotations(...)
)
```

In ellmer < 0.3.0 hieß das `.name` / `.description` / `.annotations`, und
die Typen wurden als `...` übergeben, nicht als benannte Liste `arguments`.
Wer heute `ellmer` 0.2.x installiert hat, bekommt beim Start des
MCP-Servers keinen sauberen Hinweis, sondern einen Fehler aus dem Inneren
von `ellmer`. `rlang::check_installed()` prüft nur *ob* installiert, nicht
*welche Version*.

Betroffen sind 33 Aufrufstellen in `R/mcp_tools.R`
(`ellmer::tool` ×6, `ellmer::ContentToolResult` ×16,
`ellmer::tool_annotations` ×4, `ellmer::type_*` ×7).

**Empfehlung:** Untergrenze deklarieren und im Guard mitführen.

```r
# DESCRIPTION, Suggests:
ellmer (>= 0.3.0),
mcptools (>= 1.0.0),
```

```r
# R/mcp_server.R
check_mcp_deps <- function() {
  rlang::check_installed(
    c("mcptools (>= 1.0.0)", "ellmer (>= 0.3.0)"),
    reason = "to start the lintrhelper MCP server."
  )
}
```

`rlang::check_installed()` versteht diese Schreibweise und prompted dann
auch auf ein *Upgrade*, nicht nur auf eine Neuinstallation.

Zu `mcptools`: der aufgerufene Einstiegspunkt
`mcp_server(tools = , session_tools = FALSE)` (`R/mcp_server.R:47`)
existiert in 1.0.0 unverändert, `session_tools` weiterhin mit Default
`TRUE`. Es ist also **kein akuter Bruch** — aber der Code wurde gegen 0.x
geschrieben, und `>= 1.0.0` hält das fest, statt es dem Zufall der
Installation zu überlassen.

**Breaking-Change-Risiko:** keins. Eine Untergrenze schränkt nur ein, wen
das Paket als kompatibel bezeichnet; das Verhalten bei ausreichend neuem
`ellmer` bleibt identisch.

**Offen — bewusst nicht entschieden:** ellmer 0.4.0 stellte `tool()` auf
`convert = TRUE` um (JSON-Eingaben werden zu R-Strukturen gecastet) und
lässt `type_string()` bei `NULL` nun `NA` liefern. Ob `mcp_lint_file()` und
`mcp_lint_project()` unter 0.4.x mit `NA` statt `NULL` für ein
weggelassenes optionales `project_dir` / `dir` noch richtig liegen, ist
statisch nicht zu klären — dafür müsste die Testsuite gegen ellmer 0.4.1
laufen. Das gehört in Job 4 (`error-edges`) oder in einen gezielten
Testlauf, nicht in diesen Report.

---

### B3 — Kein `Depends: R (>= x.y)` · Risiko: **mittel** · Breaking-Change-Risiko: **niedrig**

**Fund:** `DESCRIPTION` hat kein `Depends`-Feld. Damit behauptet das Paket
implizit, auf jedem R zu laufen — auch auf R 3.x.

Das ist zumindest teilweise unbelegt: `lintr (>= 3.0.0)` selbst verlangt
R >= 3.6, und `xml2` 1.6.0 hat die eigene Untergrenze inzwischen angehoben.
Die CI testet `release`, `devel` und `oldrel-1`, also faktisch nur die
oberen drei Versionen — die Behauptung "läuft überall" wird nirgends
geprüft.

Der Code selbst ist genügsam: Referenzsuche zeigt **keine** Nutzung der
nativen Pipe `|>` und **keine** Lambda-Kurzform `\(x)`, beides R >= 4.1.
Die Untergrenze wird also von den Dependencies bestimmt, nicht vom eigenen
Code.

**Empfehlung:** `R (>= 4.0)` deklarieren — konservativ, von `lintr` und
`xml2` ohnehin gefordert, und deckt sich mit dem, was die CI tatsächlich
abdeckt.

```
Depends:
    R (>= 4.0)
```

**Breaking-Change-Risiko:** niedrig, aber nicht null — Nutzer auf R 3.x
können nach dieser Änderung nicht mehr installieren. Praktisch trifft das
niemanden, der `lintr` 3.x betreiben kann.

---

### B4 — `xml2` → libxml2-CVEs: gemeldet, aber hier **nicht ausnutzbar** · Risiko: **niedrig** · Breaking-Change-Risiko: **keins**

**Fund:** `xml2` bindet die System-Bibliothek libxml2, für die 2026 mehrere
DoS-CVEs veröffentlicht wurden — u. a. CVE-2026-0989 (Stack-Erschöpfung im
RelaxNG-Parser über externe Schema-Includes), CVE-2026-0990 (unkontrollierte
Rekursion in `xmlCatalogXMLResolveURI` über selbstreferenzierende
Katalog-Delegates) und CVE-2026-6732 (Type Confusion bei
XSD-validierten Dokumenten mit interner Entity-Referenz).

**Bewertung: für dieses Paket ohne praktische Relevanz.** Die
Referenzsuche zeigt genau fünf xml2-Funktionen im Einsatz —
`xml_find_all`, `xml_parent`, `xml_text` (`R/high_level_linters.R`,
`R/simple_linter.R`). Alle drei arbeiten auf dem Parse-Baum, den `lintr`
**in-process aus R-Quelltext** erzeugt. Das Paket parst **kein** externes
XML, validiert **nicht** gegen XSD oder RelaxNG und löst **keine**
XML-Kataloge auf. Genau die drei Angriffsflächen der CVEs werden also nie
betreten.

**Empfehlung:** keine Code-Änderung. Kein Grund, eine `xml2`-Untergrenze
einzuziehen — die Fixes liegen in der System-libxml2, nicht im R-Wrapper.
Für CI und Nutzer gilt schlicht: System-libxml2 gepatcht halten. Der Punkt
steht hier, damit ein späterer Lauf ihn nicht neu "entdeckt" und als
dringend fehldeutet.

---

### B5 — `rlang` als voller Import für einen einzigen Aufruf · Risiko: **niedrig (kosmetisch)** · Breaking-Change-Risiko: **niedrig**

**Fund:** `rlang` steht in `Imports`, hat aber repo-weit **genau eine**
Aufrufstelle: `rlang::check_installed()` in `R/mcp_server.R:54`. Diese
liegt zudem in einem Pfad, der nur erreicht wird, wenn ohnehin gerade der
optionale MCP-Stack hochgefahren wird.

**Empfehlung: so lassen.** `rlang` ist dependency-frei und in jeder
R-Installation, die `lintr` betreibt, bereits vorhanden — der Gewinn eines
Ausbaus ist nahe null. Ein Ersatz durch `requireNamespace()` wäre zwar
machbar, würde aber den brauchbaren interaktiven Installations-Prompt von
`check_installed()` gegen eine nackte Fehlermeldung eintauschen. Das ist
ein schlechter Tausch.

Nach B2 trägt `check_installed()` zusätzlich die Versionsprüfung — dann ist
der Import erst recht gerechtfertigt. Aufgenommen nur zur Vollständigkeit;
**hier ist nichts zu tun.**

---

## Zusammenfassung

| # | Befund | Risiko | Aufwand | Frist |
|---|--------|--------|---------|-------|
| B1 | `actions/checkout@v4` (Node 20) + alte pages-deploy-action | hoch | ~4 Zeilen | **16.09.2026** |
| B2 | `ellmer`/`mcptools` ohne Versionsuntergrenze | mittel-hoch | ~6 Zeilen | — |
| B3 | Kein `Depends: R (>= 4.0)` | mittel | ~2 Zeilen | — |
| B4 | libxml2-CVEs via `xml2` | niedrig | keiner | — |
| B5 | `rlang` für einen Aufruf | kosmetisch | keiner | — |

**Keine verwundbare R-Dependency mit tatsächlicher Angriffsfläche in
diesem Paket.** Der einzige zeitkritische Punkt ist B1 und liegt in der CI,
nicht im Paket.

Vorschlag für die Umsetzung: B1 sofort und separat (reiner CI-Fix, kein
Paket-Bump nötig, da `.github/` per `.Rbuildignore` außerhalb des Pakets
liegt). B2 und B3 gehören sinnvoll in einen gemeinsamen kleinen
`DESCRIPTION`-PR mit `usethis::use_version("patch")` gemäß `AGENTS.md`.

Beides sind **Änderungen am Paket und damit PR-Arbeit** — dieser Job ist
Report-only, deshalb bleibt es hier bei der Empfehlung.

## Quellen

- [Deprecation of Node 20 on GitHub Actions runners](https://github.blog/changelog/2025-09-19-deprecation-of-node-20-on-github-actions-runners/)
- [actions/checkout releases](https://github.com/actions/checkout/releases)
- [JamesIves/github-pages-deploy-action releases](https://github.com/JamesIves/github-pages-deploy-action/releases)
- [ellmer 0.3.0 — Tidyverse](https://tidyverse.org/blog/2025/07/ellmer-0-3-0/)
- [ellmer 0.4.0 — Tidyverse](https://tidyverse.org/blog/2025/11/ellmer-0-4-0/)
- [ellmer Changelog](https://ellmer.tidyverse.org/news/index.html)
- [mcptools 1.0.0 — Posit](https://opensource.posit.co/blog/2026-07-06_mcptools-1-0-0/)
- [mcptools `server` reference](https://posit-dev.github.io/mcptools/reference/server.html)
- [lintr NEWS](https://github.com/r-lib/lintr/blob/main/NEWS.md)
- [xml2 Changelog](https://xml2.r-lib.org/news/index.html)
- [IBM Security Bulletin — libxml2 CVE-2026-0989/0990/0992](https://www.ibm.com/support/pages/security-bulletin-vulnerabilities-libxml2-cve-2026-0989-cve-2026-0990-cve-2026-0992-affect-aix)
- [RHSA-2026:11349](https://access.redhat.com/errata/RHSA-2026:11349)
