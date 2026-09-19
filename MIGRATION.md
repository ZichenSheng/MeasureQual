# Migrating from evaluably to MeasureQual

`evaluably` 0.9.0 is the final release under the former name. `MeasureQual`
begins at 0.10.0; this is an intentional breaking pre-1.0 package rename.

```r
# Before
library(evaluably)
evaluably::decision_map(list())
# After
library(MeasureQual)
MeasureQual::decision_map(list())
```

The 21 exported functions, signatures, defaults and scientific calculations
are unchanged. Replace package imports and namespace qualifiers in your code.
No compatibility package is provided.

For error handlers and serialized contract compatibility, the existing
`evaluably_error_*`, `evaluably_warning_*`, `evaluably_message_*` and
`evaluably_condition` identifiers are intentionally preserved. The frozen
schema identifier `urn:evaluably:evidence-record:1.0.2` is also unchanged.
These are stable contract identifiers, not current software branding.
Record provenance now correctly identifies MeasureQual 0.10.0; package/version
metadata and associated provenance hashes can therefore differ. Scientific
record payloads, classes and qualification semantics are unchanged.

The optional companion remains `gbm201data`. Its 0.9.1 patch release changes
name references and documentation; all frozen resource bytes are unchanged.
Historical evaluably v0.9.0 assets, tag and DOI remain immutable.
