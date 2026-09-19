.run_payload <- function(x) .plain(x)[setdiff(names(x),c("run_hash","spec","run_timestamp","runtime_data_fingerprint"))]
.run_flags <- function(spec,unit,integrity,census) {
 f<-unique(c(spec$flags,spec$unit_spec$flags,integrity$flags,census$flags,"DOMAIN_BOUNDED"))
 eq<-spec$representation_spec$equivalence_status
 if(eq=="NOT_EQUIVALENT") f<-c(f,"REPRESENTATION_NOT_EQUIVALENT")
 if(eq=="UNKNOWN") f<-c(f,"REPRESENTATION_EQUIVALENCE_UNKNOWN")
 if(spec$reference_spec$type=="NO_VALID_REFERENCE") f<-c(f,"NO_REFERENCE")
 if(!is.null(spec$unit_spec$min_scientific_units)&&unit$n_scientific<spec$unit_spec$min_scientific_units) f<-c(f,"LOW_SCIENTIFIC_N")
 if(unit$n_units_per_block[["min"]]>0&&unit$n_units_per_block[["max"]]/unit$n_units_per_block[["min"]]>10) f<-c(f,"UNBALANCED_BLOCKS")
 if(spec$replacement_policy!="none") f<-c(f,"REPLACEMENT_USED")
 unique(f)
}
#' Execute the Frozen Stress Ladder
#'
#' Executes user callbacks at exactly the declared severity grid. Raw outputs precede optional conditional discrimination. Scientific-unit slopes require metric components named by the declared scientific IDs. Worlds do not become biological replicates.
#' @param spec audit_spec object.
#' @param data Runtime input with named accessible/protected objects and realized id_map.
#' @param census Constructibility receipt bound to this spec and severity grid.
#' @param integrity Integrity receipt; cannot replace a conflicting executed receipt.
#' @param seed_context Optional reproducible seed namespace.
#' @param discrimination Whether to compute secondary conditional discrimination; default FALSE.
#' @return audit_run with raw evidence and no verdict.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' stress_response(s, t$data, constructibility_census(s, t$data))
#' @export
stress_response <- function(spec,data,census,integrity=NULL,seed_context=NULL,discrimination=FALSE) {
 .ensure_class(spec,"audit_spec");.ensure_class(census,"constructibility_census");.require_verified(spec)
 if(!identical(census$spec_hash,spec$spec_hash)) .abort("Census/spec hash mismatch","evaluably_error_freeze_verification")
 if(!identical(census$severity_grid,spec$severity_grid)) .abort("Severity grid differs from census","evaluably_error_severity_grid_mismatch")
 if(census$entry_gate!="PASS") .abort("Entry gate blocked","evaluably_error_entry_gate_blocked")
 if(is.null(integrity)) integrity<-census$integrity
 if(is.null(integrity)||integrity$outcome=="INVALID_TRUTH_LEAKAGE") .abort("Invalid integrity receipt","evaluably_error_truth_leakage")
 unit<-.unit_gate(spec,data);if(unit$gate!="LEGAL") .abort("No legal unit","evaluably_error_entry_gate_blocked")
 ladders<-list();ledger<-list();shape<-NULL;detections<-list()
 for(q in spec$severity_grid) for(w in seq_len(spec$n_worlds)) {
  seed<-.derive_seed(spec$spec_hash,spec$context$context_id,spec$alternative_spec$name,q,w,seed_context)
  out<-.attempt_world(spec,data,q,seed);attempt<-1L
  ledger[[length(ledger)+1L]]<-list(severity=q,world_index=w,attempt=attempt,constructed=out$constructed,valid=out$valid,failure_class=out$failure_class,detail=out$detail)
  while(!out$valid&&spec$replacement_policy!="none"&&attempt<=spec$max_replacements) {
   if(spec$replacement_policy=="declared"&&!isTRUE(.call(attr(spec,"callbacks")$replacement_fn,list(failure=out,attempt=attempt,spec=spec)))) break
   attempt<-attempt+1L;seed<-.derive_seed(spec$spec_hash,spec$context$context_id,spec$alternative_spec$name,q,paste(w,attempt,sep=":"),seed_context)
   out<-.attempt_world(spec,data,q,seed)
   ledger[[length(ledger)+1L]]<-list(severity=q,world_index=w,attempt=attempt,constructed=out$constructed,valid=out$valid,failure_class=out$failure_class,detail=out$detail)
  }
  if(out$failure_class%in%c("TRUTH_ACCESS_VIOLATION","INVALID_INPUT")) .abort("Execution truth/input violation","evaluably_error_truth_leakage")
  if(!out$valid) next
  v<-out$metric
  current<-list(length=length(v),names=names(v))
  if(is.null(shape)) shape<-current else if(!identical(shape,current)) .abort("Metric return shape changed across worlds")
  names_v<-names(v);if(is.null(names_v)) names_v<-paste0("component_",seq_along(v))
  ladders[[length(ladders)+1L]]<-data.frame(severity=q,world_index=w,component=names_v,value=as.double(v),stringsAsFactors=FALSE)
  sf<-attr(spec,"callbacks")$stable_flag_rule
  if(!is.null(sf)) {
   detected<-.call(sf,list(values=v,metric=v,severity=q,world_index=w,spec=spec))
   if(!is.logical(detected)||length(detected)!=1L||is.na(detected)) .abort("stable_flag_rule must return one nonmissing logical")
   detections[[length(detections)+1L]]<-data.frame(severity=q,world_index=w,detected=detected)
  }
 }
 raw<-if(length(ladders)) do.call(rbind,ladders) else data.frame(severity=numeric(),world_index=integer(),component=character(),value=numeric())
 summaries<-list()
 for(q in spec$severity_grid) for(component in unique(raw$component)) {
  v<-raw$value[raw$severity==q&raw$component==component]
  summaries[[length(summaries)+1L]]<-data.frame(severity=q,component=component,n=length(v),median=if(length(v)) stats::median(v) else NA_real_,
  IQR=if(length(v)) stats::IQR(v) else NA_real_,min=if(length(v)) min(v) else NA_real_,max=if(length(v)) max(v) else NA_real_,sd=if(length(v)>1L) stats::sd(v) else NA_real_)
 }
 summary<-if(length(summaries)) do.call(rbind,summaries) else data.frame()
 # Only explicitly named scientific IDs produce scientific-unit slopes.
 slopes<-list()
 for(id in intersect(unit$scientific_ids,unique(raw$component))) {
  s<-summary[summary$component==id,];ok<-is.finite(s$median)
  slope<-if(sum(ok)>=2L) unname(stats::coef(stats::lm(median~severity,data=s[ok,]))[[2]]) else NA_real_
  slopes[[length(slopes)+1L]]<-data.frame(scientific_unit=id,slope=slope,paired_difference=if(spec$pairing=="paired"&&sum(ok)>=2) tail(s$median[ok],1)-head(s$median[ok],1) else NA_real_)
 }
 ps<-if(length(slopes)) do.call(rbind,slopes) else data.frame(scientific_unit=character(),slope=numeric(),paired_difference=numeric())
 direction<-if(spec$metric_direction%in%c("greater","higher_better")) 1 else -1
 consistency<-if(nrow(ps)&&any(is.finite(ps$slope))) mean(direction*ps$slope[is.finite(ps$slope)]>0) else NA_real_
 det<-if(length(detections)) do.call(rbind,detections) else data.frame(severity=numeric(),world_index=integer(),detected=logical())
 recovery<-if(nrow(det)) list(detections=sum(det$detected[det$severity==max(spec$severity_grid)]),worlds=spec$n_worlds,capability_result_not_entry_gate=TRUE) else NULL
 reference_values<-NULL
 rf<-attr(spec$reference_spec,"callbacks")$fn
 if(!is.null(rf)) reference_values<-.call(rf,list(x=raw$value,values=raw$value,data=data,spec=spec))
 if(!is.null(reference_values)&&(!is.numeric(reference_values)||any(!is.finite(reference_values))||!length(reference_values)%in%c(1L,nrow(raw)))) .abort("Reference must return a finite scalar or raw-value-length vector")
 null<-raw$value[raw$severity==0];null_sd<-if(length(null)>1L) stats::sd(null) else NA_real_
 flags<-.run_flags(spec,unit,integrity,census)
 degenerate<-length(null)>1L&&((!is.na(null_sd)&&null_sd==0)||(!is.null(spec$reference_spec$identity_tolerance)&&null_sd<spec$reference_spec$identity_tolerance))
 if(degenerate) {
  flags<-c(flags,"DEGENERATE_NULL")
  .inform("The observed null is degenerate", "evaluably_message_degenerate_null")
 }
 disc<-NULL
 if(discrimination) {
  if(degenerate) disc<-list(status="DISCRIMINATION_UNDEFINED_DEGENERATE_NULL")
  else if(!length(null)) disc<-list(status="NO_NULL_RUNG")
  else disc<-list(status="COMPUTED",auroc_conditional=lapply(setdiff(spec$severity_grid,0),function(q) {
   v<-raw$value[raw$severity==q];if(!length(v)) return(NA_real_)
   mean(outer(v,null,function(a,b) as.double(direction*(a-b)>0)+0.5*as.double(a==b)))
  }))
 }
 x<-list(spec=spec,spec_hash=spec$spec_hash,severity_grid=spec$severity_grid,census=census,integrity=integrity,unit_details=unit,
 raw_ladders=raw,raw_by_severity=summary,per_unit_slopes=ps,direction_consistency_conditional=consistency,
 null_dispersion=list(sd=null_sd,IQR=if(length(null)) stats::IQR(null) else NA_real_),reference_values=reference_values,
 effect_relative_to_null=if(is.finite(null_sd)&&null_sd>0) (raw$value-mean(null))/null_sd else rep(NA_real_,nrow(raw)),
 recovery=recovery,detections=det,calibration=spec$calibration,frozen_failure_mode=spec$frozen_failure_mode,discrimination=disc,
 outer_worlds=spec$n_worlds,inner_resamples=spec$inner_resamples,dataset_settings=spec$dataset_settings,
 scientific_replicates=if(is.null(spec$scientific_replicates)) unit$n_scientific else spec$scientific_replicates,
 ledger=ledger,flags=unique(flags),run_timestamp=.now(),runtime_data_fingerprint=.runtime_fingerprint(data))
 x$saturation<-audit_saturation(structure(x,class="audit_run"));x$flags<-unique(c(x$flags,x$saturation$flags));x$run_hash<-.hash(.run_payload(x))
 x<-structure(x,class="audit_run")
 receipt<-attr(spec,"lineage_receipt");receipt$has_run<-TRUE;receipt$history<-c(receipt$history,list(list(spec_hash=spec$spec_hash,run_hash=x$run_hash,timestamp=x$run_timestamp)))
 x
}
#' Describe Saturation Without Assigning a Verdict
#'
#' Detects exact adjacent ties without thresholds. Plateau and ceiling detectors require author-declared tolerances. Saturation is a flag, never a terminal state.
#' @param run audit_run, or blocked audit_check for qualification.
#' @return Plain saturation descriptor with regions and flags.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' audit_saturation(run_audit(check_audit(s, t$data), t$data))
#' @export
audit_saturation <- function(run) {
 .ensure_class(run,"audit_run");spec<-run$spec;s<-run$raw_by_severity;regions<-list();ties<-0L;floor<-NA
 if(nrow(s)) for(component in unique(s$component)) {
  z<-s[s$component==component,];d<-diff(z$median)
  exact<-is.finite(d)&d==0;ties<-ties+sum(exact)
  for(i in which(exact)) regions[[length(regions)+1L]]<-list(component=component,from_index=i,to_index=i+1L,from_severity=z$severity[i],to_severity=z$severity[i+1L],max_abs_delta=0,detector="EXACT_TIE")
  if(!is.null(spec$plateau_tol)&&!is.null(spec$plateau_min_rungs)) {
   good<-is.finite(d)&abs(d)<spec$plateau_tol;r<-rle(good);ends<-cumsum(r$lengths);starts<-ends-r$lengths+1L
   for(i in which(r$values&r$lengths+1>=spec$plateau_min_rungs)) regions[[length(regions)+1L]]<-list(component=component,from_index=starts[i],to_index=ends[i]+1L,from_severity=z$severity[starts[i]],to_severity=z$severity[ends[i]+1L],max_abs_delta=max(abs(d[starts[i]:ends[i]])),detector="PLATEAU")
  }
  if(!is.null(spec$ceiling_tol)&&!is.null(spec$theoretical_max)) for(i in which(is.finite(z$median)&abs(z$median-spec$theoretical_max)<=spec$ceiling_tol)) regions[[length(regions)+1L]]<-list(component=component,from_index=i,to_index=i,from_severity=z$severity[i],to_severity=z$severity[i],max_abs_delta=abs(z$median[i]-spec$theoretical_max),detector="CEILING")
  pos<-z$median[z$severity>0];if(length(pos)>=2L&&all(is.finite(pos))&&length(unique(pos))==1L) floor<-FALSE
 }
 det<-run$detections
 if(nrow(det)&&!is.null(spec$detection_target)) {
  q<-min(det$severity[det$severity>0]);p<-sum(det$detected[det$severity==q])/spec$n_worlds
  if(p>=spec$detection_target) floor<-FALSE else if(any(det$detected[det$severity>q])) floor<-TRUE
 }
 flags<-if(length(regions)) "SATURATED" else character()
 if(identical(floor,FALSE)) flags<-c(flags,"DETECTION_FLOOR_NOT_BRACKETED")
 list(exact_ties=ties,plateau_regions=regions,plateau_status=if(is.null(spec$plateau_tol)||is.null(spec$plateau_min_rungs)||length(spec$severity_grid)<3L) "UNRESOLVED" else "ASSESSED",
 ceiling_status=if(is.null(spec$ceiling_tol)||is.null(spec$theoretical_max)) "UNRESOLVED" else "ASSESSED",detection_floor_bracketed=floor,flags=flags)
}
#' Execute a Passing Preflight Receipt
#'
#' Checks exact runtime serialization fingerprint equality before calling stress_response. A blocked check cannot execute. The runtime fingerprint is not an archival dataset identity.
#' @param check Passing audit_check receipt.
#' @param data Runtime input with named accessible/protected objects and realized id_map.
#' @param ... Additional arguments forwarded to the documented next phase.
#' @param seed_context Optional reproducible seed namespace.
#' @return audit_run containing evidence, no qualification.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' run_audit(check_audit(s, t$data), t$data)
#' @export
run_audit <- function(check,data,...,seed_context=NULL) {
 .ensure_class(check,"audit_check");.require_verified(check)
 if(check$entry_gate!="PASS") .abort("Blocked checks cannot run","evaluably_error_entry_gate_blocked")
 if(!identical(check$runtime_data_fingerprint,.runtime_fingerprint(data))) .abort("CHECK/RUN data fingerprint mismatch","evaluably_error_data_fingerprint_mismatch")
 stress_response(check$spec,data,check$census,integrity=check$integrity,seed_context=seed_context,...)
}
