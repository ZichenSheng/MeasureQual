.relations <- c("SAME_SUBJECT_DIFFERENT_REPRESENTATION","SAME_SUBJECT_DIFFERENT_CLAIM","SAME_SUBJECT_DIFFERENT_CONTEXT","SAME_SUBJECT_DIFFERENT_DESIGN","SAME_SUBJECT_DIFFERENT_ALPHA_OR_RULE")
.validate_relationships <- function(x) {
 .forbidden_keys(x)
 if(!is.list(x)) .abort("relationships must be a list")
 for(r in x) {
  if(!is.list(r)||!all(c("record_id","relation","representation_hash","record_scientific_hash")%in%names(r))||any(!names(r)%in%c("record_id","relation","representation_hash","record_scientific_hash","note"))) .abort("Relationships carry identifiers/hashes only")
  .require_text(r$record_id,"record_id");.choice(r$relation,.relations,"relation")
  if(!.sha_valid(r$representation_hash)||!.sha_valid(r$record_scientific_hash)) .abort("Invalid related hash","evaluably_error_unsupported_representation_relation")
 }
 invisible(TRUE)
}
.uuid <- function() {
 seed<-strtoi(substr(.hash(list(time=.now(),pid=Sys.getpid(),nonce=tempfile())),1,7),16L)
 bytes<-as.raw(.with_seed(seed,function() sample.int(256L,16L,replace=TRUE)-1L))
 bytes[7]<-as.raw(bitwOr(bitwAnd(as.integer(bytes[7]),15L),64L));bytes[9]<-as.raw(bitwOr(bitwAnd(as.integer(bytes[9]),63L),128L))
 h<-paste(sprintf("%02x",as.integer(bytes)),collapse="")
 paste(substr(h,1,8),substr(h,9,12),substr(h,13,16),substr(h,17,20),substr(h,21,32),sep="-")
}
.record_payload <- function(x,scientific=TRUE) {
 y<-.plain(x);y$provenance$record_scientific_hash<-NULL;y$provenance$record_file_hash<-NULL
 y$provenance$runtime_data_fingerprint<-NULL;y$identity$audit_member_key<-NULL
 if(scientific) {
  y$relationships<-NULL;y$rendered_prose<-NULL;y$identity$record_id<-NULL;y$identity$subject_label<-NULL
  for(n in c("pkg_name","pkg_version","r_version","platform","run_timestamp","frozen_at")) y$provenance[n]<-NULL
  y$declarations$notes<-NULL;y$declarations$reference_spec$source<-NULL
 }
 y
}
.record_hashes <- function(x) {
 x$provenance$record_scientific_hash<-.hash(.record_payload(x,TRUE))
 x$provenance$record_file_hash<-.hash(.record_payload(x,FALSE));x
}
.verify_record <- function(x) {
 .validate_relationships(x$relationships)
 if(!all(c("identity","declarations","preflight","execution_evidence","qualification","interpretation_license","provenance","relationships","rendered_prose")%in%names(x))) return(FALSE)
 if(!as.character(x$qualification$terminal_state)%in%.terminal_states) return(FALSE)
 parts<-c(list(subject_id=x$identity$subject_id,context_id=x$identity$context_id,claim_contract_hash=x$identity$claim_contract_hash),x$declarations[setdiff(.member_fields,c("subject_id","context_id","claim_contract_hash"))])
 if(!identical(x$identity$audit_member_key,.audit_member_key(parts))) return(FALSE)
 identical(x$provenance$record_scientific_hash,.hash(.record_payload(x,TRUE)))&&identical(x$provenance$record_file_hash,.hash(.record_payload(x,FALSE)))
}
.record_assemble <- function(x,result,relationships) {
 spec<-x$spec;executed<-inherits(x,"audit_run")
 unit<-x$unit_details
 execution<-if(executed) c(list(executed=TRUE),.plain(x)[c("outer_worlds","inner_resamples","dataset_settings","scientific_replicates","raw_ladders","raw_by_severity","per_unit_slopes","direction_consistency_conditional","recovery","calibration","saturation","frozen_failure_mode","null_dispersion","reference_values","effect_relative_to_null","discrimination","ledger")]) else list(executed=FALSE)
 preflight<-if(executed) c(list(unit_gate=x$census$unit_gate,endpoint_gate=x$census$endpoint_gate,entry_gate="PASS",stop_reason=NA_character_,integrity=x$integrity),x$census[c("capacity","inferential_support")]) else c(list(unit_gate=x$unit_gate,endpoint_gate=x$endpoint_gate,entry_gate=x$entry_gate,stop_reason=x$stop_reason,integrity=x$integrity),if(is.null(x$census)) .not_assessed(spec) else x$census[c("capacity","inferential_support")])
 preflight$feature_coverage<-mean(spec$representation_spec$feature_mapping$resolved)
 declarations<-c(list(claim=spec$claim,representation=.plain(spec$representation_spec),unit_spec=.plain(spec$unit_spec),reference_spec=.plain(spec$reference_spec),alternative_spec=.plain(spec$alternative_spec),inferential_rule=.plain(spec$inferential_rule)),
 .select(.plain(spec),c("representation_hash","representation_rule_hash","unit_spec_hash","reference_spec_hash","alternative_spec_hash","inferential_rule_hash","endpoint_compatibility_declared","metric_version","metric_hash","metric_direction","metric_fn_depends_on","severity_grid","decision_map_hash","decision_map","denominator_contract","denominator_contract_hash","notes","thresholds","n_worlds","pairing","replacement_policy")))
 rec<-list(identity=list(record_id=.uuid(),schema_version="1.0.2",subject_id=spec$subject$subject_id,subject_label=spec$subject$subject_label,subject_provenance=spec$subject$subject_provenance,
 claim_id=spec$claim$claim_id,claim_family=spec$claim$claim_family,claim_class=spec$claim_class,claim_contract_hash=spec$claim_contract_hash,context_id=spec$context$context_id,
 inferential_design_id=substr(spec$unit_spec_hash,1,12),audit_member_key=spec$audit_member_key),
 declarations=declarations,preflight=.plain(preflight),execution_evidence=execution,
 qualification=.plain(result)[c("terminal_state","flags","capability_descriptor","rule_id","because","precedence_rule_fired")],
 interpretation_license=.license(spec,result,execution,unit),
 provenance=list(lifecycle=spec$lifecycle,spec_hash=spec$spec_hash,parent_spec_hash=spec$parent_spec_hash,amendment_ledger=spec$amendment_ledger,
 source_locators=spec$source_locators,source_sha256=spec$source_sha256,pkg_name="evaluably",pkg_version=as.character(utils::packageVersion("evaluably")),r_version=as.character(getRversion()),platform=R.version$platform,
 run_timestamp=if(executed) x$run_timestamp else x$checked_at,frozen_at=spec$frozen_at,runtime_data_fingerprint=x$runtime_data_fingerprint),relationships=relationships,rendered_prose=list())
 rec$rendered_prose<-.render_prose(rec,"1.0.0");structure(.record_hashes(rec),class="evidence_record")
}
#' Emit a Claim-Scoped Evidence Record
#'
#' Qualifies a run or blocked check and assembles structured interpretation constraints and dual hashes. Relationships never transfer evidence. Raw patient data are not included.
#' @param x Audit object or evidence JSON path where supported.
#' @param ... Additional arguments forwarded to the documented next phase.
#' @param relationships List of record link identifiers/hashes, never evidence content.
#' @param render Whether to write when a path is supplied.
#' @param path Optional output directory.
#' @param formats Requested output formats; JSON is always written.
#' @return Immutable evidence_record; optionally writes JSON and a human-readable report.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' evidence_record(check_audit(freeze_audit(toy_spec(capacity = FALSE)$spec), t$data))
#' @export
evidence_record <- function(x,...,relationships=list(),render=TRUE,path=NULL,formats=c("json","html")) {
 .validate_relationships(relationships)
 if(!inherits(x,"audit_run")&&!(inherits(x,"audit_check")&&x$entry_gate=="BLOCKED")) .abort("Record requires an executed run or blocked check")
 result<-apply_decision_map(x);rec<-.record_assemble(x,result,relationships)
 if(render&&!is.null(path)) {render_audit(rec,path=path,formats=formats,...);return(invisible(rec))}
 rec
}
.replication_sentence <- function(record) {
 e<-record$execution_evidence;scope<-record$interpretation_license$licensed_scope
 if(!e$executed) return("No stress execution occurred. Preflight is not recovery evidence.")
 sprintf("%s synthetic worlds x %s inner resamples characterize conditional simulation behaviour under a frozen design. They do not constitute %s independent biological datasets. The declared scientific inferential unit is %s (n = %s), observed across %s independent settings; scientific replicates = %s.",e$outer_worlds,e$inner_resamples,e$outer_worlds,scope$scientific_unit,scope$n_scientific,e$dataset_settings,e$scientific_replicates)
}
.render_prose <- function(record,template_version) {
 l<-record$interpretation_license
 templates<-jsonlite::fromJSON(system.file("templates","license.json",package="evaluably"),simplifyVector=FALSE)
 paragraph<-if(l$disposition=="SUPPRESSED") NULL else templates[[l$disposition]]
 list(template_version=template_version,preamble=if(is.null(record$provenance$frozen_at)) templates$exploratory_preamble else gsub("{timestamp}",record$provenance$frozen_at,templates$preamble,fixed=TRUE),license_paragraph=paragraph,
 qualifier_lines=if(l$disposition=="SUPPRESSED") character() else unname(vapply(l$required_qualifiers,function(q) templates$qualifiers[[q]],character(1))),
 prohibition_lines=if(l$disposition=="SUPPRESSED") character() else paste("Not licensed:",l$prohibited_extensions),replication_sentence=.replication_sentence(record))
}
#' Render a Verified Evidence Record
#'
#' Renders versioned templates from structured licence fields. Suppression preserves evidence receipts and the replication sentence. Rendered wording affects file identity only.
#' @param x Audit object or evidence JSON path where supported.
#' @param path Optional output directory.
#' @param formats Requested output formats; JSON is always written.
#' @param template_version Version label of the rendered template.
#' @return Updated evidence_record, invisible when written to path.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' r <- evidence_record(run_audit(check_audit(s, t$data), t$data))
#' render_audit(r, path = tempfile())
#' @export
render_audit <- function(x,path=NULL,formats=c("json","html"),template_version="1.0.0") {
 if(inherits(x,c("audit_run","audit_check"))) x<-evidence_record(x,render=FALSE)
 .ensure_class(x,"evidence_record");.require_verified(x)
 y<-.plain(x);y$rendered_prose<-.render_prose(y,template_version);y<-structure(.record_hashes(y),class="evidence_record")
 if(any(!formats%in%c("json","html","md"))) .abort("Unknown output format")
 if(is.null(path)) return(y)
 dir.create(path,recursive=TRUE,showWarnings=FALSE)
 writeLines(.canonical_json(.plain(y)),file.path(path,"audit_record.json"),useBytes=TRUE)
 sections<-c("Identity","Frozen declarations","Scientific unit","Reference","Null","Alternative and integrity","Constructibility","Raw stress response","Discrimination","Saturation","Qualification","Interpretation licence","No-rescue ledger","Provenance","Limitations")
 values<-list(y$identity,y$declarations,y$declarations$unit_spec,y$declarations$reference_spec,y$execution_evidence$null_dispersion,y$preflight$integrity,y$preflight$capacity,y$execution_evidence$raw_ladders,y$execution_evidence$discrimination,y$execution_evidence$saturation,y$qualification,y$interpretation_license,y$provenance$amendment_ledger,y$provenance,
 list(noninheritance="Relationships never transfer qualification.",runtime_fingerprint="Runtime fingerprint; establishes that preflight and execution used the same in-session object. It is not a canonical dataset identity. Archival dataset provenance is recorded in source_locators and source_sha256."))
 md<-c(paste("# Audit",y$identity$record_id),paste("Lifecycle:",y$provenance$lifecycle),paste("Interpretation:",y$interpretation_license$disposition),y$rendered_prose$replication_sentence)
 for(i in seq_along(sections)) md<-c(md,"",paste("##",i,sections[i]),"",if(i==12L) c(y$rendered_prose$preamble,y$rendered_prose$license_paragraph,y$rendered_prose$qualifier_lines,y$rendered_prose$prohibition_lines,paste("Suppression reasons:",paste(y$interpretation_license$suppression$reasons,collapse=", "))) else c("```json",.canonical_json(values[[i]]),"```"))
 writeLines(md,file.path(path,"audit_record.md"),useBytes=TRUE)
 if("html"%in%formats) {
  escape<-function(t) gsub(">","&gt;",gsub("<","&lt;",gsub("&","&amp;",t,fixed=TRUE),fixed=TRUE),fixed=TRUE)
  writeLines(c("<!doctype html><html lang='en'><meta charset='utf-8'><title>Claim-scoped audit</title><body><pre>",escape(md),"</pre></body></html>"),file.path(path,"audit_record.html"),useBytes=TRUE)
 }
 invisible(y)
}
