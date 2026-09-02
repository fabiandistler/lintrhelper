# BLOCKED

Stellen, an denen ein Characterization-Test nach drei Anläufen nicht
zustande kam. Kein Fix, kein `skip` als Ersatz für einen Test — nur die
Notiz, was scheitert.

## 2026-09-02 — `R/mcp_server.R:47-49`, `start_mcp_server()`

**Funktion:**
[`start_mcp_server()`](https://fabiandistler.github.io/lintrhelper/reference/start_mcp_server.md),
konkret die Zeilen nach dem Abhängigkeits-Guard:
`mcptools::mcp_server(tools = mcp_tools(), session_tools = FALSE)` und
das `invisible(NULL)` dahinter.

**Versucht:**

1.  Direkter Aufruf im Testlauf. Erreicht nur `check_mcp_deps()`; ohne
    `mcptools` und `ellmer` bricht der Aufruf dort ab, mit ihnen würde
    er den Testprozess blockieren, weil `mcp_server()` bis zum
    Verbindungsende über stdio bedient. Dieser Teil ist als Test
    festgehalten (`tests/testthat/test-mcp_server.R`).
2.  `testthat::local_mocked_bindings(mcp_server = ..., .package = "mcptools")`.
    Nicht möglich: `local_mocked_bindings()` braucht den Namensraum des
    Pakets, und `mcptools` ist im Lauf-Container nicht installierbar
    (Suggests, kein Binärpaket in den erreichbaren Quellen).
3.  Subprozess-Harness: Server per `callr`/`Rscript` starten und einen
    JSON-RPC-Handshake über stdin/stdout sprechen. Scheitert an
    derselben Abhängigkeit und würde zusätzlich einen
    Timeout-behafteten, damit potenziell flaky Test einführen — genau
    das, was Schritt 3 des Backfills ausschließt.

**Warum es scheitert:** Die Zeile ist per Konstruktion ein blockierender
Protokoll-Loop hinter zwei optionalen Abhängigkeiten. Sie ist ohne
Subprozess-Harness plus installiertes `mcptools` nicht erreichbar; die
Abdeckung von `R/mcp_server.R` bleibt deshalb bei 71.43 % statt 100 %.

**Voraussetzung für einen neuen Anlauf:** ein Lauf mit installiertem
`mcptools` und `ellmer`. Dann wäre ein
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html)-Harness
mit festem Timeout und einem einzigen `initialize`-Roundtrip der nächste
Versuch — vor dem Einbau gegen die Flakiness-Regel abwägen.
