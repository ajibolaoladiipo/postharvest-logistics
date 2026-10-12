# Replication code

## The Logistics of Post-Harvest Work

This repository contains code and non-sensitive configuration files for the analysis of market access, reported post-harvest labour organisation, paid hiring, and recorded worker-hours in Nigeria General Household Survey Panel Wave 4 data.

It does **not** contain the manuscript, survey microdata, household identifiers, geographic variables, analytical-panel files, model objects, tables, figures, or private research notes. These omissions are deliberate. The source data and all row-level derivatives remain governed by their original access terms.

## What the code reproduces

With authorised local access to the specified Wave 4 source files, the scripts construct a household-crop analytical panel and generate the registered descriptive analysis, validation tables, figures, and sensitivity diagnostics. The analysis is a cross-sectional, survey-weighted descriptive association study. It does not estimate a causal effect of market distance or market access.

The primary unit is an eligible household-crop record. The primary accessibility measure is household distance to the nearest market, entered as log one plus kilometres. Linked community measures of market presence, reported distance, travel time, and one-way cost are retained as distinct supplementary measurements.

## What you need

- R 4.3 or later
- Authorised access to the Nigeria General Household Survey Panel Wave 4 source files listed in [DATA_ACCESS.md](DATA_ACCESS.md)
- Permission to use the household and geographic variables under the source provider's terms
- The R packages listed in `DESCRIPTION` and versioned in [REQUIREMENTS.md](REQUIREMENTS.md)

## Quick start

1. Clone this repository.
2. Install the packages listed in `DESCRIPTION` or use your preferred R environment manager.
3. Put the authorised source files in a local folder outside the repository.
4. Set the two environment variables below in R.
5. Run the scripts in the stated order.

```r
install.packages(c("dplyr", "haven", "readr", "survey", "yaml"))

Sys.setenv(
  POSTHARVEST_LOGISTICS_PROJECT_DIR = normalizePath(getwd()),
  GHS_RAW_DIR = "/absolute/path/to/authorised/GHS/Wave4/files"
)

source("scripts/00_check_environment.R")
source("scripts/01_build_wave4_postharvest_logistics_panel.R")
source("scripts/02_audit_wave4_postharvest_logistics_panel.R")
source("scripts/03_run_registered_wave4_descriptive_analysis.R")
source("scripts/04_run_final_wave4_validation_and_outputs.R")
```

The final script creates local, non-row-level outputs in `outputs/final_v01_2026-10-08/`. The entire `outputs/` directory is excluded from Git by design. Details of the optional leave-one-PSU diagnostic are in [RUNBOOK.md](RUNBOOK.md).

## Repository structure

```text
config/       Registered design and crop-group crosswalk
data/raw/     Empty, ignored local location for authorised source data if desired
data/derived/ Ignored local analytical panel and model objects
scripts/      Source-to-output R workflow
outputs/      Ignored local tables, figures, and diagnostics
*.md          Data-access, run, requirement, and reproducibility documentation
```

## Reproducibility and privacy

The package is reproducible only for users who independently obtain authorised access to the source files. Do not commit, upload, or redistribute microdata, household identifiers, geographic variables, cluster identifiers, row-level analytical files, model objects, or outputs that could reveal protected information. Read [DATA_ACCESS.md](DATA_ACCESS.md) and [REPRODUCIBILITY_SCOPE.md](REPRODUCIBILITY_SCOPE.md) before running or contributing.

## Status

This is a code-only replication package. It does not represent a manuscript submission package and does not state that any journal has accepted or reviewed the analysis. The code is released under the MIT License; the source data remain subject to their provider's terms.
