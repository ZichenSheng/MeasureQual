# Provenance

Source: ACT6_IMPLEMENTATION_RELEASE_CANDIDATE_V2_2026-09-13, packages evaluably
0.9.0 and gbm201data 0.9.0. Public sources were selected from the corresponding
frozen implementation, not copied from the project tree. No raw patient data
or study database is included. Companion rows identify published signatures,
genes, claims, vocabulary or resource provenance, not individual observations.

Scientific R files, NAMESPACE, tests, man pages, examples, vignettes, schema,
resource builder and frozen resource tables are verified byte-for-byte against
the source authority. Publication changes affect DESCRIPTION contact/URLs,
README, citation, NEWS, CI, build exclusions and publication manifests only.
The companion data-raw builder remains in Git; it is excluded from installable
package tarballs to satisfy current R package top-level conventions.
Tarball hashes therefore differ from the internal tarballs; tarball timestamps
and generated vignette metadata may also differ. No algorithm was changed.

Local R 4.4.1 / macOS arm64: evaluably 475/475 and gbm201data 61/61
testthat expectations passed (536 total), with zero failures/errors/skips.
Both packages passed R CMD check with zero errors, warnings or notes;
vignettes and documented examples were built and executed. Clean-library
installation, synthetic workflow, 21/7 exports and all companion file hashes
were independently verified. Git checkout is pinned to LF line endings so
Windows preserves the frozen resource byte hashes. GitHub installations passed for both packages;
remotes 2.5.0 requires force=TRUE for the companion when the same-repository
root package is already installed. CI is pending; no cross-platform claim yet.
Manual PDF compilation is not claimed when checks use --no-manual.

PUBLIC_RELEASE_MANIFEST.tsv records every public source file except itself
(to avoid a recursive hash) and .git internals. DATA_CLASSIFICATION.tsv records
the tabular-data inclusion decisions. SOURCE_MANIFEST.json identifies upstream
local derived authorities; they are not downloaded or redistributed. To rebuild
the companion resources, supply an explicit local project root to
`python companion/gbm201data/data-raw/build_resources.py PROJECT_ROOT`.
