# Runbook

## Standard analysis sequence

Run scripts 01 through 04 in numerical order from the repository root after setting `POSTHARVEST_LOGISTICS_PROJECT_DIR` and `GHS_RAW_DIR`.

| Script | Function | Local products |
|---|---|---|
| `01_build_wave4_postharvest_logistics_panel.R` | Builds the household-crop panel from authorised source files | Derived panel, source-flow and coverage diagnostics |
| `02_audit_wave4_postharvest_logistics_panel.R` | Audits construction and measured components | Construction diagnostics |
| `03_run_registered_wave4_descriptive_analysis.R` | Runs the registered descriptive analysis | Registered models and exploratory community outputs |
| `04_run_final_wave4_validation_and_outputs.R` | Generates final model ledger, tables, figures, and specified sensitivity checks | Local non-row-level outputs in `outputs/final_v01_2026-10-08/` |
| `05_run_psu_influence_diagnostic.R` | Runs one chunk of the optional leave-one-PSU diagnostic | A local chunked influence table |

## Optional PSU influence diagnostic

This check is computationally intensive because it refits the final conditional model after omitting each primary sampling unit. Run it separately for each outcome and chunk.

```r
Sys.setenv(
  PSU_INFLUENCE_OUTCOME = "paid_hire",
  PSU_CHUNK_SIZE = "50",
  PSU_CHUNK_INDEX = "1"
)
source("scripts/05_run_psu_influence_diagnostic.R")
```

Repeat the command with subsequent chunk indices until all observed primary sampling units are covered. Then repeat with `PSU_INFLUENCE_OUTCOME = "worker_hours"`. The outputs remain local and ignored.

## Checks before use

- Script 01 verifies required source filenames.
- Script 04 stops if the registered analytical denominator is not 6,332 records.
- Scripts stop if required design variables, weights, distance values, or crop-group mappings are unavailable.
- Outputs are regenerated from the local source-to-output workflow. Do not copy numerical results manually into public files.
