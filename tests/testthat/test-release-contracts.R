test_that("literal frozen API and taxonomy are exact", {
 expected<-c("representation_spec","unit_spec","reference_spec","alternative_spec","inferential_rule","decision_map","audit_spec","freeze_audit","amend_audit","verify_audit","check_audit","run_audit","evidence_record","constructibility_census","check_alternative_integrity","stress_response","audit_saturation","apply_decision_map","render_audit","audit_collection","denominator_summary")
 expect_setequal(getNamespaceExports("evaluably"),expected)
 expect_length(.s3_classes,13);expect_length(.condition_classes,29);expect_length(unique(.condition_classes),29);expect_length(.terminal_states,6)
 expect_false(any(c("score","grade","rank","pass_rate")%in%getNamespaceExports("evaluably")))
})
test_that("unit declarations reject malformed DAGs and pseudoreplication", {
 t<-toy_spec();u<-t$spec$unit_spec
 args<-.plain(u)[c("scientific","computational","resampling","blocking","nesting","id_map")]
 args$resampling<-"observation"
 expect_error(do.call(unit_spec,args),class="evaluably_error_illegal_unit")
 args$resampling<-"patient";args$nesting<-rbind(args$nesting,data.frame(child="cohort",parent="patient"))
 expect_error(do.call(unit_spec,args),class="evaluably_error_illegal_unit")
})
test_that("record hashes separate prose, runtime and scientific changes", {
 t<-toy_spec("SUPPORTED_WITHIN_DOMAIN");r<-evidence_record(run_audit(check_audit(freeze_audit(t$spec),t$data),t$data),render=FALSE)
 changed<-render_audit(r,template_version="test-new-template")
 expect_identical(r$provenance$record_scientific_hash,changed$provenance$record_scientific_hash)
 expect_false(identical(r$provenance$record_file_hash,changed$provenance$record_file_hash))
 y<-.plain(r);y$provenance$runtime_data_fingerprint<-strrep("b",64)
 expect_identical(.hash(.record_payload(y,TRUE)),r$provenance$record_scientific_hash)
 expect_identical(.hash(.record_payload(y,FALSE)),r$provenance$record_file_hash)
 y$execution_evidence$scientific_replicates<-99L
 expect_false(identical(.hash(.record_payload(y,TRUE)),r$provenance$record_scientific_hash))
 expect_false(verify_audit(y))
 result<-apply_decision_map(run_audit(check_audit(freeze_audit(toy_spec()$spec),t$data),t$data))
 expect_false(is.ordered(result$terminal_state));expect_type(result$terminal_state,"character")
})
test_that("collections use member keys and refuse heterogeneous counts", {
 ts<-lapply(c("S1","S2"),function(id) toy_spec("SUPPORTED_WITHIN_DOMAIN",subject=id))
 rs<-lapply(ts,function(t) evidence_record(run_audit(check_audit(freeze_audit(t$spec),t$data),t$data),render=FALSE))
 contract<-.record_comparability(rs[[1]],1)
 collection<-audit_collection(rs,"Two declared subjects",contract,prespecified_total=2L)
 summary<-denominator_summary(collection)
 expect_identical(summary$denominator_provenance,"DECLARED_ONLY");expect_equal(summary$entry_qualified_total,2)
 expect_false(any(grepl("proportion|rate|percent|fraction",names(summary))))
 expect_type(as.data.frame(collection)$terminal_state,"character")
 dup<-audit_collection(list(rs[[1]],rs[[1]]),"Duplicate diagnostic",contract)
 expect_error(denominator_summary(dup),class="evaluably_error_duplicate_member_key")
 heterogeneous<-audit_collection(rs,"No comparison contract")
 expect_error(denominator_summary(heterogeneous),class="evaluably_error_incomparable_collection")
 keys<-sort(vapply(ts,function(t) t$spec$audit_member_key,character(1)),method="radix")
 d<-list(contract_id="family",claim_contract_hash=ts[[1]]$spec$claim_contract_hash,expected_member_keys=keys,prespecified_total=2L,comparability=contract,frozen_at="2026-01-01T00:00:00Z")
 # New pre-run instances keep the identical prospective keys.
 ts<-lapply(c("S1","S2"),function(id) toy_spec("SUPPORTED_WITHIN_DOMAIN",subject=id))
 rs<-lapply(ts,function(t) {
  s<-amend_audit(t$spec,"SCIENTIFIC","Add prospective family","author",list(denominator_contract=d))
  evidence_record(run_audit(check_audit(freeze_audit(s),t$data),t$data),render=FALSE)
 })
 expect_identical(denominator_summary(audit_collection(rs,"Frozen family",contract))$denominator_provenance,"CONTRACT_VERIFIED")
})
