# MeasureQual

**Claim-scoped qualification of computational measurements**

MeasureQual is an R framework for determining whether a computational measurement
is qualified for a specific scientific claim under a defined design, context,
and reference structure. Analysts declare assumptions and decision rules;
the framework checks prerequisites and constrains interpretation.

It is not a universal signature-ranking tool, biological-truth engine,
leaderboard, automatic claim-validation system or causal-inference engine.

## Installation

```r
remotes::install_github("ZichenSheng/MeasureQual")
# Optional frozen signature/resource registries; independent of MeasureQual:
remotes::install_github("ZichenSheng/MeasureQual", subdir = "companion/gbm201data", force = TRUE)
```

The companion command uses `force = TRUE` to avoid remotes repository-name
cache shortcuts when installing two packages from the same repository.

## Synthetic example

```r
library(MeasureQual)
source(system.file("examples", "toy-workflow.R", package = "MeasureQual"))
toy <- toy_spec()
frozen <- freeze_audit(toy$spec)
check <- check_audit(frozen, toy$data)
run <- run_audit(check, toy$data)
record <- evidence_record(run, render = FALSE)
record
```

The example uses synthetic observations only. Five vignettes explain reference
dependence, truth isolation, unresolved decisions, definitions and constructibility.
An empty decision map yields `UNRESOLVED`; a blocked preflight can itself produce
a record. Terminal states are unordered, not a score:

- `SUPPORTED_WITHIN_DOMAIN`
- `DEFINITION_IMPLIED`
- `DIRECTIONALLY_MISLEADING`
- `NOT_EVALUABLE`
- `INVALID`
- `UNRESOLVED`

## Scope and provenance

MeasureQual supports explicit declarations for informativeness, resolution,
context deformation, detectability, reference/null construction and recovery
or qualification behavior. It evaluates the analyst's specified contract;
it does not supply automatic scientific validity or causal identification.
The terminal states are unordered and scoped to each claim and context.

Glioblastoma transcriptomic signatures are a motivating application, not the
software's scope. The optional `gbm201data` companion supplies frozen resource
definitions for manuscript-associated audits. Raw study databases and real
patient, sample, cell, spot or AOI observations are not included.

R >= 4.1 is required. See RENAME_INTEGRITY_REPORT.md for verification and
MIGRATION.md for migration details. Original software is MIT licensed; no
third-party source dataset is relicensed or redistributed.

## Citation and history

Cite MeasureQual 0.10.0 using [the version DOI](https://doi.org/10.5281/zenodo.22846512)
or CITATION.cff. The [concept DOI](https://doi.org/10.5281/zenodo.22846292)
identifies the continuing software lineage.

Formerly released as evaluably v0.9.0, preserved at
[its historical DOI](https://doi.org/10.5281/zenodo.22846293).
