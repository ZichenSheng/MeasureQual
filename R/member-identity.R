# V1.0.2 Q07: exact ten-field prospective membership identity.
# This primitive accepts an already extracted identity/component-hash list.
# Shared by audit declarations, record verification and denominator contracts.
.member_fields <- c("subject_id", "representation_hash", "claim_contract_hash", "context_id",
  "unit_spec_hash", "reference_spec_hash", "alternative_spec_hash", "inferential_rule_hash",
  "decision_map_hash", "metric_hash")
.audit_member_key <- function(spec) {
  if (!is.list(spec) || !all(.member_fields %in% names(spec))) .abort("Missing prospective membership identity components")
  for (n in c("subject_id", "context_id")) .require_text(spec[[n]], n)
  for (n in setdiff(.member_fields, c("subject_id", "context_id")))
    if (!.sha_valid(spec[[n]])) .abort(paste("Invalid membership component", n))
  .hash(spec[.member_fields])
}
.claim_contract_hash <- function(claim, claim_class) {
  for (n in c("claim_family", "estimand")) .require_text(claim[[n]], n)
  .choice(claim_class, c("BIOLOGICAL_ASSOCIATION", "DOMAIN_INFORMATIVENESS", "MEASUREMENT_CAPABILITY",
    "DIRECT_IDENTIFICATION", "METRIC_BEHAVIOUR"), "claim_class")
  .hash(list(claim_family = claim$claim_family, claim_class = claim_class, estimand = claim$estimand))
}
