# Data access and non-redistribution rules

## Source data

The workflow expects the following authorised Nigeria General Household Survey Panel Wave 4 files in the directory specified by `GHS_RAW_DIR`.

| File | Role in workflow |
|---|---|
| `sectaphl1_harvestw4.dta` | Household-member post-harvest work records |
| `sectaphl2_harvestw4.dta` | Paid and unpaid outside-household post-harvest work records |
| `secta_harvestw4.dta` | Household frame, survey weight, state, cluster, and stratum variables |
| `NGA_HouseholdGeovars_Y4.dta` | Household distance to nearest market and related geographic variables |
| `sectc2_harvestw4.dta` | Community-reported market access, distance, travel time, and cost |

The scripts verify the required filenames before reading them. The repository does not provide a copy, download link, mirror, password, access workaround, or derived substitute for these files.

## Strict non-redistribution rule

Do not put any of the following in GitHub, an issue, a release archive, a cloud notebook, or a public data repository.

- Source `.dta` files.
- Household identifiers, geographic variables, cluster identifiers, or coordinates.
- Row-level analytical panels or model objects.
- Unsuppressed output that could expose protected microdata.

All local data and generated output directories are ignored by `.gitignore`. Before every push, use `git status --ignored` and verify that only the tracked code, configuration, and documentation are staged.

## Access responsibility

Each user is responsible for confirming the current terms, citation requirements, and permitted uses with the original data provider. Running this code does not grant data access or publication rights.
