# MeasureQual rename integrity report

## A. PACKAGE NAME CHANGES

MeasureQual 0.10.0 replaces evaluably 0.9.0 as the current package name.
Package imports, namespace lookups, installed template paths and record package
provenance use MeasureQual. Stable condition classes and schema URN retain
their historical identifiers to preserve error/serialization contracts.

## B. METADATA CHANGES

Title and description express claim-scoped qualification of computational
measurements. Public author remains Zichen Sheng with approved GitHub noreply
contact. Repository renamed natively; repository identity 1377225741 preserved.
Companion version is 0.9.1 because its distributed documentation and package
independence test changed; no altered 0.9.0 tarball is published.

## C. DOCUMENTATION CHANGES

README, NEWS and MIGRATION explain the transition. Roxygen2 7.3.3 regenerated
Rd documentation; NAMESPACE remains byte-identical to the historical release.
Historical evaluably 0.9.0 provenance is retained as history.

## D. SCIENTIFIC SOURCE COMPARISON

Baseline: immutable commit 7502778bfddb09301e93a28dd9b82d9162e0dea4.
All 21 exported function signatures and function-body hashes are identical.
All 181 functions have identical signatures and name-normalized bodies.
All non-function constants are identical. Only package identity/template lookup
references change in executable R; other R edits are documentation references.
RENAMING_API_AUDIT.tsv and RENAMING_SOURCE_AUDIT.tsv record comparisons.
SCIENTIFIC_ALGORITHM_CHANGE=NO. SCIENTIFIC_CHANGE=0.
API_CHANGE_OTHER_THAN_PACKAGE_NAMESPACE=NO.

## E. RESOURCE HASH COMPARISON

Every companion inst/extdata file is byte-identical to v0.9.0, including its
12-file resource hash manifest targets. Frozen schema and templates unchanged.
LF checkout protections are retained for Windows.

## F. TEST RESULTS

Present-day local tests: MeasureQual 475/475; gbm201data 61/61 expectations.
Failures=0; errors=0; skips=0; test warnings=0. Total 536/536.

## G. R CMD CHECK

Both packages: 0 errors, 0 warnings, 0 notes on R 4.4.1 / macOS arm64,
using --no-manual. Package builds, vignette rebuilds and examples pass.
Manual PDF compilation is outside the documented check scope.

## H. CROSS-PLATFORM CI

Six checks configured for both packages on Linux, macOS and Windows.
Migration CI must pass before creating v0.10.0; status pending at preparation.

## I. ZENODO LINEAGE

Historical version DOI: 10.5281/zenodo.22846293 (evaluably v0.9.0).
Existing concept DOI: 10.5281/zenodo.22846292.
Rename synchronization and association verification precede new release.
No new DOI is claimed until assigned; no unrelated concept record authorized.
