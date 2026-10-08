# Reproducibility scope

## What this package can reproduce

Given authorised source data and a compatible R environment, the code reconstructs the household-crop panel, survey design, registered model sequence, non-row-level tables, and figures used for the Paper 1 analysis.

## What this package cannot provide

The repository cannot reproduce results without source access. It does not include private data, manuscript files, literature reviews, reviewer correspondence, result PDFs, rendered figures, fitted model objects, or any row-level derivative.

## Interpretation boundary

The workflow estimates descriptive associations between the registered household distance measure and two reported post-harvest labour outcomes. It does not estimate the causal effect of distance, road quality, transport cost, market services, labour-market conditions, post-harvest losses, productivity, welfare, or any other unobserved mechanism.

## Version discipline

The scripts preserve the original source-to-output sequence. Any changes to variable construction, source files, estimands, models, or analytical population should be documented in a new release and should not overwrite the registered design silently.
