# evaluably

`evaluably` 0.9.0 executes claim-scoped measurement audits: the analyst specifies
which representation, units, context and claim are being evaluated, and the
engine checks whether the declared evidence can support that claim.

It checks declarations, callback dependencies, constructibility and inferential
support, evaluates declared alternatives, and produces canonical evidence records.
It does not rank signatures, infer biological truth, validate original clinical
models, rescue a failed claim or transfer evidence between records.

## Installation

```r
remotes::install_github("ZichenSheng/evaluably")
# Optional frozen signature/resource registries; independent of evaluably:
remotes::install_github("ZichenSheng/evaluably", subdir = "companion/gbm201data", force = TRUE)
```

With remotes 2.5.0, `force = TRUE` avoids a repository-name cache shortcut
that can incorrectly skip the companion after installing the root package.
Both installations were tested from GitHub in a clean library.

## Synthetic example

```r
library(evaluably)
source(system.file("examples", "toy-workflow.R", package = "evaluably"))
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

The software implements the frozen ACT6 contract supporting the glioblastoma
transcriptomic-signature project. Software verification does not adjudicate
manuscript scientific results. `gbm201data` contains gene memberships, publication
identities, provenance and scoped evaluability declarations; it contains no raw
expression, clinical, patient, sample, cell, spot or AOI observations.

R >= 4.1 is required. The local authority used R 4.4.1 on macOS arm64.
See PROVENANCE.md for current verification status. CI checks both packages on
Linux, macOS and Windows. Configuration alone is not a cross-platform validation.

## Citation and license

Use CITATION.cff for software citation. A Zenodo DOI will be added only after an
actual archival record is assigned. The companion belongs to this software
release and does not receive a separate repository or DOI. Original code is MIT;
source datasets retain their providers' terms and are not redistributed.
