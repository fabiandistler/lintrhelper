# FINDINGS

Befunde aus den Test-Backfill-Läufen. Ein Eintrag hier heißt: das Verhalten ist
per Characterization-Test festgeschrieben (`SUSPECT_`-Präfix), **nicht** dass es
korrigiert wurde. Produktivcode wird in diesen Läufen nicht angefasst.

## Ranking 2026-09-02

Änderungsfrequenz aus `git log --since="18 months ago"` (Repo-Historie beginnt
2025-11-07, das Fenster deckt sie also vollständig ab), Abdeckung aus
`covr::package_coverage()`. Score = `commits × (1 − coverage)`.

| # | Datei | Commits | Coverage vorher | Score | Test-Datei vorhanden | bearbeitet |
|---|-------|--------:|----------------:|------:|---|---|
| 1 | `R/mcp_server.R` | 6 | 14.29 % | 5.14 | nein | ja |
| 2 | `R/testing_utils.R` | 3 | 0.00 % | 3.00 | nein | ja |
| 3 | `R/mcp_tools.R` | 7 | 64.82 % | 2.46 | ja | nein — Diff-Deckel |
| 4 | `R/high_level_linters.R` | 6 | 99.19 % | 0.05 | ja | nein |
| 5 | `R/simple_linter.R` | 5 | 100.00 % | 0.00 | ja | nein |

`R/templates.R` und `R/example_linters.R` tauchen im Churn-Fenster auf, existieren
aber nicht mehr; sie sind aus der Wertung.

**Messvorbehalt.** Der Coverage-Lauf fand ohne die Suggests `ellmer` und
`mcptools` statt (beide im Lauf-Container nicht installierbar). Die 64.82 % für
`R/mcp_tools.R` sind deshalb eine Untergrenze: Zeilen 12–145, 162–167, 233–240,
540–545 und 660–665 sind reine `ellmer`-Definitionen bzw. -Wrapper, deren
bestehende Tests hier übersprungen werden. Mit installiertem `ellmer` liegt die
Abdeckung dieser Datei deutlich höher, der Score damit niedriger.

### Nächste Dateien

1. `R/mcp_tools.R` (Score 2.46) — die reinen R-Zweige ohne `ellmer`-Bezug, die der
   Lauf nicht mehr geschafft hat: `normalise_changed_only()` für `NULL` und
   leeren Input (Zeilen 392, 398), `rule_help()` ohne passenden Alias (855),
   `rd_text()` mit leer renderndem Fragment (982), `relative_to()` für einen Pfad
   außerhalb des Verzeichnisses (1104). Alle vier Verhalten sind bereits
   verifiziert und liefern `FALSE` / `NULL` / `NULL` / den unveränderten Pfad.
2. `R/mcp_tools.R`, zweiter Durchgang — die `ellmer`-Wrapper, sobald ein Lauf mit
   installiertem `ellmer` möglich ist. Vorher ist jede Zahl zu dieser Datei
   unvollständig.
3. `R/high_level_linters.R` (Score 0.05) — eine einzige ungedeckte Zeile (51).
   Lohnt sich erst, wenn 1 und 2 erledigt sind.

## Befunde 2026-09-02 — R/testing_utils.R

### F-01 `test_linter(linter = NULL)` lintet mit lintr-Standardlintern
- **Datei/Zeile:** `R/testing_utils.R:48-52`
- **Erwartet:** Fehler — ohne Linter gibt es nichts zu testen.
- **Beobachtet:** `is.function(NULL)` ist `FALSE`, also landet `NULL` über den
  `else`-Zweig als `linters = NULL` bei `lintr::lint()`, das daraufhin auf den
  Standard-Linterset zurückfällt. `test_linter(NULL, "x <- T")` liefert einen
  Lint von `T_and_F_symbol_linter` und der Test ist grün.
- **Risiko:** hoch. Wenn der Linter-Aufbau still `NULL` ergibt (Tippfehler im
  Objektnamen, Funktion gibt nichts zurück), behauptet der Test weiterhin
  Erfolg — und misst dabei lintr statt des eigenen Linters.
- **Vorschlag:** vor dem Lint prüfen, dass `linter_func` eine Funktion der Klasse
  `linter` ist, sonst mit klarer Meldung abbrechen.
- **Test:** `SUSPECT_test_linter runs lintr's default linters when linter is NULL`

### F-02 `n_lints` überschreibt `should_lint` ohne Hinweis
- **Datei/Zeile:** `R/testing_utils.R:60-77`
- **Erwartet:** Ein Widerspruch wie `should_lint = TRUE, n_lints = 0` wird
  gemeldet.
- **Beobachtet:** Sobald `n_lints` gesetzt ist, wird `should_lint` nie ausgewertet
  (`if / else if`-Kette). `test_linter(l, "x <- TRUE", should_lint = TRUE,
  n_lints = 0)` ist grün.
- **Risiko:** mittel. Der Test dokumentiert eine Absicht, die er nicht prüft.
- **Vorschlag:** widersprüchliche Kombinationen als Aufrufer-Fehler abweisen, oder
  `should_lint` bei gesetztem `n_lints` explizit als ignoriert dokumentieren.
- **Test:** `SUSPECT_test_linter lets n_lints silently override should_lint`

### F-03 `message_pattern` wird bei null Lints stillschweigend übersprungen
- **Datei/Zeile:** `R/testing_utils.R:80`
- **Erwartet:** Eine Zusicherung über Lint-Meldungen schlägt fehl, wenn es keine
  Meldungen gibt.
- **Beobachtet:** Die Bedingung `n_found > 0` überspringt die Prüfung; mit
  `should_lint = FALSE` oder `n_lints = 0` ist jedes `message_pattern` grün.
- **Risiko:** mittel. Leerlaufende Zusicherung, gleiche Klasse wie F-02.
- **Vorschlag:** `message_pattern` bei null Lints als Fehler behandeln, da das
  Muster dann nicht erfüllbar ist.
- **Test:** `SUSPECT_test_linter skips message_pattern when no lint was found`

### F-04 Nicht-numerisches `n_lints` bricht die Fehlermeldung selbst ab
- **Datei/Zeile:** `R/testing_utils.R:61-67`
- **Erwartet:** `n_lints = "01"` wird als ungültiges Argument abgewiesen.
- **Beobachtet:** `n_found != n_lints` vergleicht nach Coercion als Zeichenkette,
  also passiert `n_lints = "1"` die Prüfung. Passt der Wert nicht, stirbt der
  Report an `sprintf("%d", ...)` mit `invalid format '%d'; use format %s for
  character objects` — der Aufrufer sieht nie, wie viele Lints gefunden wurden.
- **Risiko:** niedrig. Falscher Aufruf, aber die Diagnose führt in die Irre.
- **Vorschlag:** `n_lints` am Funktionsanfang gegen einen nicht-negativen
  Ganzzahl-Skalar prüfen.
- **Test:** `SUSPECT_test_linter compares n_lints without coercing it`

### F-05 `code = NULL` endet in einer lintr-Interna-Meldung
- **Datei/Zeile:** `R/testing_utils.R:43-45, 55`
- **Erwartet:** Klare Meldung, dass `code` fehlt.
- **Beobachtet:** `length(NULL) > 1` ist `FALSE`, also geht `NULL` unverändert an
  `lintr::lint(text = NULL)`; die Meldung lautet
  `argument "filename" is missing, with no default`.
- **Risiko:** niedrig.
- **Vorschlag:** `code` auf Zeichenkette prüfen, bevor `lintr::lint()` läuft.
- **Test:** `SUSPECT_test_linter surfaces a lintr internal error for NULL code`

### F-06 `quick_test()` meldet einen kaputten XPath als „No lints found."
- **Datei/Zeile:** `R/testing_utils.R:115-131`
- **Erwartet:** Ein nicht parsbarer XPath-Ausdruck ist ein Fehler.
- **Beobachtet:** `xml2` warnt nur (`Invalid expression [1207]`), der Lauf liefert
  null Lints, und `quick_test("//[[", "x <- T")` meldet `No lints found.` — nicht
  unterscheidbar von einem korrekten Ausdruck ohne Treffer.
- **Risiko:** mittel. Genau der Fall, für den `quick_test()` gedacht ist —
  XPath-Ausdrücke ausprobieren — liefert das irreführendste Ergebnis.
- **Vorschlag:** die Warnung abfangen und als Fehler mit dem XPath-Text melden.
- **Test:** `SUSPECT_quick_test reports a broken XPath as 'No lints found.'`

### F-07 `linter_name` wird entgegengenommen, dokumentiert und nie benutzt
- **Datei/Zeile:** `R/simple_linter.R:111-119` (beobachtet über
  `R/testing_utils.R:116-120`)
- **Erwartet:** Laut `@param linter_name` „used for the lint type" taucht der Name
  im Lint auf.
- **Beobachtet:** `create_simple_linter()` reicht `linter_name` nicht an
  `create_linter_factory()` weiter. lintr benennt den Linter stattdessen nach dem
  Aufrufausdruck: `quick_test()` übergibt `linter_name = "temp_test_linter"`, der
  Lint meldet `temp_linter` (den Namen der lokalen Variablen).
- **Risiko:** mittel. Betrifft alle drei `create_*_linter()`-Funktionen; die
  Namensgebung der Lints ist damit vom Variablennamen der Aufrufstelle abhängig.
- **Vorschlag:** `linter_name` durchreichen oder das Argument samt Dokumentation
  entfernen. Gehört in einen eigenen PR — `R/simple_linter.R` ist Produktivcode.
- **Test:** `SUSPECT_quick_test does not use the linter_name it passes on`
