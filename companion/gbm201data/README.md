# gbm201data

Frozen derived resource tables: nine MVP tables and seven accessors. No raw
patient data, new biological analysis, ranking or evaluably dependency.

Throughout, each published signature is represented by its gene membership evaluated under a single frozen scoring rule. This evaluates the *representation* a signature defines, not the original published model, its trained coefficients, thresholds, or clinical decision rule. Claims about original model performance are outside the scope of this work.

This set is not a claim of exhaustive coverage of the glioblastoma signature literature; eligible publications may have been missed. It is defined by its frozen derivation rules and its full exclusion ledger rather than by completeness.

Versioned resource evidence does not mean that 201 published signatures were independently validated.

The manifest has 201 identities and 1729 canonical membership rows. The
representation and evaluability registries contain eight explicitly scoped
StageB1 records, not 201 inferred judgements. Unrecorded source fields are NA;
NOT_APPLICABLE is distinct. Representation hashes are NA (NOT_COMPUTED_IN_MVP) where the source did
not provide them. No hash is invented for an incomplete declaration.
The exclusion ledger refers to the upstream 271-instance universe.

Use gbm_signature(), gbm_membership(), gbm_representation(), gbm_evaluability(),
gbm_claims(), gbm_provenance() and gbm_vocab(). HASHES.tsv hashes installed data
files other than itself. data-raw/SOURCE_MANIFEST.json records frozen sources;
data-raw/build_resources.py retabulates those local derived sources.
