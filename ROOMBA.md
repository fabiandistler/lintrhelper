# ROOMBA — Wartungs-Katalog

Wiederkehrende Wartungsläufe für `lintrhelper`. Ein Lauf bearbeitet **genau
einen** Job aus dem Katalog unten.

## Regeln

- **Ein Job pro Lauf.** Kein Bündeln, kein "wenn ich schon dabei bin".
- **PR-Jobs** landen auf einem eigenen Branch `roomba/[job]-[YYYY-MM-DD]`,
  z. B. `roomba/doc-drift-2026-09-08`.
- **Diff klein halten:** < 300 Zeilen. Passt der Befund nicht hinein, wird er
  als Report geliefert und der PR auf das aufgeteilt, was hineinpasst.
- **Verhalten nie ändern.** Ausgenommen sind ausschließlich die Jobs
  `doc-drift`, `dead-code` und `test-flakiness` — und auch dort nur, soweit
  der Job es verlangt.
- **`security-footguns` ist Report-only.** Nie ein PR, auch nicht bei einem
  trivialen Fix: ein Sicherheitsbefund wird gemeldet, nicht still gepatcht.
- **Nachweispflicht.** Jeder Befund nennt Datei und Zeile. `dead-code`
  belegt Ungenutztheit zusätzlich durch eine Referenzsuche über `R/`,
  `tests/`, `vignettes/`, `man/`, `README.md` und `NAMESPACE`.
- **Jeder Lauf pflegt diese Datei:** "zuletzt gelaufen" des Jobs setzen und
  den nächsten fälligen Job unten eintragen.
- **Repo-Konventionen gelten weiter.** Ändert ein Lauf das Paket selbst,
  greift die Versions-Bump-Regel aus `AGENTS.md` (`usethis::use_version()`).
  Reine Reports ändern das Paket nicht und bumpen daher nicht.

## Katalog

| # | Job | Beschreibung | Output | Cooldown | Zuletzt gelaufen |
|---|-----|--------------|--------|----------|------------------|
| 1 | `deps-audit` | Veraltete oder verwundbare Dependencies; Empfehlung je Fund mit Breaking-Change-Risiko. | Report | 7d | 2026-08-25 |
| 2 | `doc-drift` | README, Vignetten und Roxygen-Docstrings gegen das tatsächliche Verhalten des Codes. | PR | 14d | — |
| 3 | `dead-code` | Ungenutzte Funktionen, Exporte, Imports — Ungenutztheit per Referenzsuche belegt. | PR | 14d | — |
| 4 | `error-edges` | API- und IO-Ränder ohne Fehlerbehandlung oder mit still geschlucktem Fehler. | Report | 14d | — |
| 5 | `test-flakiness` | Tests mit Zeit-, Zufalls- oder Netzabhängigkeit. | PR | 30d | — |
| 6 | `security-footguns` | Hartkodierte Pfade, Secrets-Verdacht, Injection-Ränder, unsichere Defaults. | **Nur Report** | 14d | — |
| 7 | `perf-quickwins` | Offensichtliche N+1-Muster und Kopier-Orgien (in R: unnötige `data.frame`-Kopien statt Referenzsemantik). | Report | 30d | — |

## Reports

Report-Jobs legen ihr Ergebnis unter `.roomba/reports/[job]-[YYYY-MM-DD].md`
ab, damit ein späterer Lauf gegen den vorherigen Stand lesen kann.

- `.roomba/reports/deps-audit-2026-08-25.md`

## Status

- **Zuletzt gelaufen:** `deps-audit` am 2026-08-25 (Report).
- **Nächster fälliger Job:** **`doc-drift`** (#2, PR, Cooldown 14d) — noch nie
  gelaufen, also sofort fällig.
- `deps-audit` ist erneut fällig ab **2026-09-01**.

Die Jobs 3–7 sind ebenfalls noch nie gelaufen und damit gleichermaßen fällig;
die Reihenfolge folgt der Katalognummer, sobald `doc-drift` erledigt ist.
