redeclare <- function(t,changes=list()) {
 s<-t$spec;args<-s[intersect(names(s),names(formals(audit_spec)))]
 args<-c(args,attr(s,"callbacks"))
 args<-args[!duplicated(names(args),fromLast=TRUE)]
 ds<-attr(s,"callback_declarations");for(n in names(ds)) args[paste0(n,"_depends_on")]<-ds[n]
 for(n in names(changes)) args[n]<-changes[n]
 do.call(audit_spec,args)
}
test_that("optional scientific declarations are explicit and checked", {
 t<-toy_spec();s<-redeclare(t,list(scientific_replicates=4L,detection_target=.8,pairing="paired",plateau_tol=.01,plateau_min_rungs=2L,ceiling_tol=.01,theoretical_max=4,
 calibration=list(value=.01,criterion="declared",outcome="PASS",calibration_scope="DESIGN_LEVEL"),frozen_failure_mode="SHIFT_LIMITED",source_sha256=strrep("a",64)))
 r<-run_audit(check_audit(freeze_audit(s),t$data),t$data,discrimination=TRUE)
 expect_identical(r$discrimination$status,"COMPUTED");expect_identical(r$saturation$detection_floor_bracketed,FALSE)
 expect_true(all(is.finite(r$per_unit_slopes$paired_difference)))
 expect_true("CALIBRATION_SCOPE_DESIGN_LEVEL"%in%evidence_record(r)$interpretation_license$required_qualifiers)
 for(change in list(list(severity_grid=c(1,0)),list(null_generator=NULL),list(scientific_replicates=0),list(detection_target=2),list(replacement_policy="declared"),list(thresholds=1),list(flags="BAD"),list(calibration=list()),list(plateau_tol=-1),list(theoretical_max=Inf),list(source_sha256="bad"))) expect_error(redeclare(t,change))
})
test_that("unit callback mapping and aggregation preserve declared scientific units", {
 t<-toy_spec();u<-t$spec$unit_spec
 x<-unit_spec(u$scientific,u$computational,u$resampling,u$blocking,u$nesting,function(data) data$id_map,aggregation_fn=function(x) x,min_scientific_units=5L)
 s<-freeze_audit(redeclare(t,list(unit_spec=x)));c<-check_audit(s,t$data)
 expect_identical(c$entry_gate,"PASS");expect_true("LOW_SCIENTIFIC_N"%in%run_audit(c,t$data)$flags)
 expect_warning(unit_spec(u$scientific,u$computational,u$resampling,u$blocking,u$nesting,u$id_map,on_violation="declared"),class="evaluably_warning_unit_mismatch_declared")
 bad<-u$id_map;bad$patient[2]<-bad$patient[3];expect_error(unit_spec(u$scientific,u$computational,u$resampling,u$blocking,u$nesting,bad))
 expect_error(unit_spec(u$scientific,u$computational,u$resampling,u$blocking,u$nesting,1))
 expect_error(unit_spec(u$scientific,u$computational,u$resampling,u$blocking,u$nesting,u$id_map,world_is_scientific_unit=TRUE))
})
test_that("reference routes carry explicit exactness and external checks", {
 expect_error(reference_spec("EXACT_IDENTITY","EXACT",function(x) x,"toy",character(),"1"))
 expect_error(reference_spec("ANALYTIC_EXPECTATION","APPROXIMATE",function(x) x,"toy",character(),"1",identity_tolerance=.1))
 expect_error(reference_spec("EXTERNAL_PUBLISHED_REFERENCE","EMPIRICAL",function(x) x,"toy",character(),"1"))
 ref<-reference_spec("EXTERNAL_PUBLISHED_REFERENCE","EMPIRICAL",function(x) x,"toy",character(),"1",reproduction_check=list(outcome="PASS"),doi="declared-example")
 expect_s3_class(ref,"reference_spec")
 ref<-reference_spec("EXACT_IDENTITY","EXACT",function(x) x,"toy",character(),"1",identity_tolerance=0)
 expect_s3_class(ref,"reference_spec")
 t<-toy_spec();ref<-reference_spec("NO_VALID_REFERENCE","APPROXIMATE",NULL,"No reference",character(),"1")
 s<-freeze_audit(redeclare(t,list(reference_spec=ref)));run<-run_audit(check_audit(s,t$data),t$data)
 expect_true("NO_REFERENCE"%in%run$flags)
})
test_that("constraints and generator refusals remain in the census", {
 t<-toy_spec();a<-alternative_spec("bounded",function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x+severity),"x","truth",character(),constraints=list(positive=function(world) all(world>=0)))
 s<-freeze_audit(redeclare(t,list(alternative_spec=a)));c<-constructibility_census(s,t$data)
 expect_true(all(vapply(c$cells,function(x) x$valid==3,logical(1))))
 a<-alternative_spec("refusal",function(accessible,severity,seed,constraints) list(status="FAILED",failure_class="RESOURCE_LIMIT",failure_detail="synthetic bound"),"x","truth",character())
 s<-freeze_audit(redeclare(t,list(alternative_spec=a)));c<-constructibility_census(s,t$data)
 expect_identical(c$capacity$capacity_gate,"ZERO_CAPACITY")
 expect_equal(c$cells[[2]]$failure_class_histogram$RESOURCE_LIMIT,3)
 a<-alternative_spec("invalid metric",function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=NA_real_),"x","truth",character())
 s<-freeze_audit(redeclare(t,list(alternative_spec=a)));expect_equal(constructibility_census(s,t$data)$cells[[2]]$failure_class_histogram$GENERATOR_ERROR,3)
 expect_error(alternative_spec("x",function() 1,"x","x",character()))
 expect_error(alternative_spec("x",function() 1,"x","truth",character(),claims_capacity_check=TRUE))
 expect_error(alternative_spec("x",function() 1,"x","truth",character(),constraints=list(1)))
})
test_that("decision predicates reject undeclared input and preserve first priority", {
 rule<-list(id="first",priority=1L,when="alpha_reachable",then="SUPPORTED_WITHIN_DOMAIN",because="declared")
 expect_s3_class(decision_map(list(rule)),"decision_map")
 rule$when<-expression(alpha_reachable);expect_s3_class(decision_map(list(rule)),"decision_map")
 rule$when<-"unknown_science > 0";expect_error(decision_map(list(rule)),class="evaluably_error_undeclared_decision_rule")
 rule$when<-"TRUE";expect_error(decision_map(list(rule,rule)))
 r2<-rule;r2$id<-"second";r2$priority<-2L
 expect_warning(decision_map(list(rule,r2)),class="evaluably_warning_unreachable_rule")
 expect_error(decision_map(list(list())))
 expect_error(.rule_expression(1))
})
test_that("integrity and lifecycle negative paths fail closed", {
 t<-toy_spec();expect_error(check_alternative_integrity(t$spec,t$data,strict=TRUE))
 expect_error(check_alternative_integrity(t$spec,t$data[names(t$data)!="truth"]))
 expect_error(amend_audit(t$spec,"NON_SCIENTIFIC","wrong class","a",list(n_worlds=4)))
 expect_error(amend_audit(t$spec,"SCIENTIFIC","bad key","a",list(new_unknown=1)))
 expect_error(amend_audit(t$spec,"SCIENTIFIC","governance","a",list(lifecycle="FROZEN_CONFIRMATORY")))
 s<-amend_audit(t$spec,"NON_SCIENTIFIC","prose","a",list(notes="prose"));expect_true(verify_audit(s))
 s<-freeze_audit(s,decision_map());expect_true(verify_audit(s))
 a<-alternative_spec("no firewall",function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x),"x",character(),character())
 s<-redeclare(t,list(alternative_spec=a));expect_identical(check_alternative_integrity(s,t$data)$outcome,"UNVERIFIABLE")
 a<-alternative_spec("broken",function(accessible,severity,seed,constraints) stop("synthetic"),"x","truth",character())
 s<-redeclare(t,list(alternative_spec=a));expect_warning(check_alternative_integrity(s,t$data),class="evaluably_warning_nondeterministic_generator")
})
test_that("named scientific thresholds are checked at freeze and applied", {
 t<-toy_spec();dm<-decision_map(list(list(id="recovery",priority=1L,when="recovery_fraction >= thresholds$target",then="SUPPORTED_WITHIN_DOMAIN",because="Author criterion")))
 s<-redeclare(t,list(decision_map=dm));expect_error(freeze_audit(s),class="evaluably_error_undeclared_decision_rule")
 s<-freeze_audit(redeclare(t,list(decision_map=dm,thresholds=list(target=.8))))
 r<-run_audit(check_audit(s,t$data),t$data)
 expect_identical(as.character(apply_decision_map(r)$terminal_state),"SUPPORTED_WITHIN_DOMAIN")
 expect_error(.rule_expression("TRUE; FALSE"))
})
test_that("inferential support never infers probability from cardinality", {
 t<-toy_spec()
 rules<-list(
 inferential_rule(.05,"ASYMPTOTIC","TWO_SIDED","DECLARED_EXTERNAL","ASYMPTOTIC","NOT_APPLICABLE",justification="continuous"),
 inferential_rule(.05,"DECLARED_EXTERNAL","TWO_SIDED","DECLARED_EXTERNAL","APPROXIMATE","DECLARED_EXTERNAL",external_support=list(min_attainable_p=.02,support_cardinality=1,source="declared",source_hash="NO_MACHINE_READABLE_ARTEFACT",rule_version="1"),justification="external support"),
 inferential_rule(.05,"PERMUTATION_EXACT","ONE_SIDED_GREATER","INCLUSIVE_OBSERVED","EXACT","ANALYTIC_BOUND",support_fn=function(spec,data) list(min_attainable_p=.01)))
 for(rule in rules) {
  s<-freeze_audit(redeclare(t,list(inferential_rule=rule)));c<-constructibility_census(s,t$data)
  expect_identical(c$inferential_support$support_gate,"REACHABLE")
 }
 expect_true("HUMAN_REVIEW_REQUIRED"%in%constructibility_census(freeze_audit(redeclare(t,list(inferential_rule=rules[[2]]))),t$data)$flags)
 rule<-inferential_rule(.05,"PERMUTATION_EXACT","ONE_SIDED_GREATER","INCLUSIVE_OBSERVED","EXACT","UNDETERMINED")
 expect_error(.inferential_support(redeclare(t,list(inferential_rule=rule)),t$data),class="evaluably_error_reachability_undetermined")
 for(fn in list(function(spec,data) 1,function(spec,data) list(min_attainable_p=.01))) {
  rule<-inferential_rule(.05,"PERMUTATION_EXACT","ONE_SIDED_GREATER","INCLUSIVE_OBSERVED","EXACT","EXACT_ENUMERATION",support_fn=fn)
  expect_error(.inferential_support(redeclare(t,list(inferential_rule=rule)),t$data))
 }
})
test_that("representation equivalence uncertainty remains distinct", {
 t<-toy_spec();base<-t$spec$representation_spec
 args<-base[intersect(names(base),names(formals(representation_spec)))];args$derived_from_original<-TRUE;args$original_object_ref<-"Original model"
 for(eq in c("UNKNOWN","NOT_EQUIVALENT","EQUIVALENT")) {
  args$equivalence_status<-eq;args$equivalence_basis<-if(eq=="EQUIVALENT") "Explicit checked identity" else NULL
  rep<-do.call(representation_spec,args);s<-freeze_audit(redeclare(t,list(representation_spec=rep)))
  r<-evidence_record(run_audit(check_audit(s,t$data),t$data),render=FALSE)
  expect_identical("ORIGINAL_MODEL_EQUIVALENCE"%in%r$interpretation_license$prohibited_extensions,eq!="EQUIVALENT")
 }
 args$equivalence_status<-"NOT_APPLICABLE";expect_error(do.call(representation_spec,args))
 args$derived_from_original<-FALSE;args$original_object_ref<-NULL;args$object_type<-"SIGNED_MEMBERSHIP";args$signs<-c(A=1L,B=-1L)
 expect_s3_class(do.call(representation_spec,args),"representation_spec")
 args$signs<-c(A=1,B=-1);expect_error(do.call(representation_spec,args))
})
test_that("saturation is a descriptor and missing null never implies discrimination", {
 t<-toy_spec();s<-freeze_audit(redeclare(t,list(metric_fn=function(world) 1,plateau_tol=.1,plateau_min_rungs=2L,ceiling_tol=.1,theoretical_max=1)))
 run<-run_audit(check_audit(s,t$data),t$data,discrimination=TRUE)
 expect_identical(run$discrimination$status,"DISCRIMINATION_UNDEFINED_DEGENERATE_NULL")
 expect_true(all(c("SATURATED","DETECTION_FLOOR_NOT_BRACKETED","DEGENERATE_NULL")%in%run$flags))
 expect_true(any(vapply(run$saturation$plateau_regions,function(x) x$detector=="PLATEAU",logical(1))))
 s<-freeze_audit(redeclare(t,list(severity_grid=c(.5,1),null_generator=NULL,stable_flag_rule=NULL)))
 run<-run_audit(check_audit(s,t$data),t$data,discrimination=TRUE);expect_identical(run$discrimination$status,"NO_NULL_RUNG");expect_null(run$recovery)
})
test_that("direct phase calls reject mismatched or blocked receipts", {
 t<-toy_spec();s<-freeze_audit(t$spec);c<-constructibility_census(s,t$data)
 wrong<-.replace(c,list(spec_hash="wrong"));expect_error(stress_response(s,t$data,wrong),class="evaluably_error_freeze_verification")
 wrong<-.replace(c,list(severity_grid=2));expect_error(stress_response(s,t$data,wrong),class="evaluably_error_severity_grid_mismatch")
 wrong<-.replace(c,list(entry_gate="BLOCKED"));expect_error(stress_response(s,t$data,wrong),class="evaluably_error_entry_gate_blocked")
 expect_error(stress_response(s,t$data,c,integrity=list(outcome="INVALID_TRUTH_LEAKAGE")),class="evaluably_error_truth_leakage")
 bad<-t$data;bad$id_map<-bad$id_map[1:2,];expect_error(stress_response(s,bad,c),class="evaluably_error_entry_gate_blocked")
 expect_identical(constructibility_census(s,bad)$capacity$capacity_gate,"NOT_ASSESSED")
 u<-toy_spec(endpoint="INCOMPATIBLE");expect_identical(constructibility_census(freeze_audit(u$spec),u$data)$capacity$capacity_gate,"NOT_ASSESSED")
 u<-toy_spec(leak=TRUE);expect_identical(constructibility_census(freeze_audit(u$spec),u$data)$capacity$capacity_gate,"NOT_ASSESSED")
 s<-freeze_audit(redeclare(t,list(min_constructibility=1)));expect_true("LOW_CONSTRUCTIBILITY"%in%constructibility_census(s,t$data)$flags)
})
test_that("relationships contain only links and cannot change evidence", {
 t<-toy_spec();run<-run_audit(check_audit(freeze_audit(t$spec),t$data),t$data);r<-evidence_record(run,render=FALSE)
 link<-list(record_id=r$identity$record_id,relation="SAME_SUBJECT_DIFFERENT_CONTEXT",representation_hash=r$declarations$representation_hash,record_scientific_hash=r$provenance$record_scientific_hash,note="Cross-reference only")
 linked<-evidence_record(run,relationships=list(link));expect_identical(r$provenance$record_scientific_hash,linked$provenance$record_scientific_hash)
 expect_error(.validate_relationships(1));expect_error(.validate_relationships(list(list())))
 link$representation_hash<-"bad";expect_error(.validate_relationships(list(link)),class="evaluably_error_unsupported_representation_relation")
 p<-tempfile();expect_invisible(evidence_record(run,path=p));expect_true(file.exists(file.path(p,"audit_record.html")))
 expect_s3_class(render_audit(run),"evidence_record")
 expect_error(.canonical_json(new.env()));expect_error(.canonical_json(setNames(list(1,2),c("same","same"))))
 expect_identical(.canonical_json(factor("a")),.canonical_json("a"))
})
test_that("callback dependency validation rejects malformed scientific identities", {
 empty<-list(packages=character(),constants=list(),functions=list())
 for(d in list(list(),list(packages=1,constants=list(),functions=list()),list(packages=c("x::1","x::2"),constants=list(),functions=list()),list(packages=character(),constants=1,functions=list()),list(packages=character(),constants=list(x=1),functions=list(x=list())),list(packages=character(),constants=list(x=as.Date("2020-01-01")),functions=list()),list(packages=character(),constants=list(x=1:1001),functions=list()),list(packages=character(),constants=list(),functions=list(x="hash")))) expect_error(.validate_dependencies(d))
 expect_error(.resolve_callback(sum));expect_null(.binding("absent",emptyenv()))
 expect_identical(.binding_package(baseenv()),"base")
 expect_error(.resolve_callback(function() unbound_dependency()),class="evaluably_error_undeclared_callback_dependency")
 expect_error(.resolve_callback(function() stats::not_an_export()),class="evaluably_error_callback_dependency_value_mismatch")
 f<-function(x=stats::median(1:3)) x;expect_type(.callback_digest(f),"character")
 helper<-function(x) x+1;d<-empty;d$functions$helper<-list(fn=helper,depends_on=NULL)
 expect_warning(.resolve_callback(function(x) x,d),class="evaluably_warning_unused_callback_dependency")
 x<-list(a=1,b=list(2));d<-empty;d$constants<-list(x=x)
 expect_type(.callback_digest(function() x,d),"character")
})
test_that("protected binding traversal and probe cloning never mutate closures", {
 t<-toy_spec();truth<-t$data$truth;helper<-function() truth
 f<-function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x+helper())
 expect_match(.protected_bindings(f,list(truth=truth)),"truth")
 clone<-.probe_clone(f,list(truth=truth),1)
 expect_false(identical(clone(list(x=1),1,1,list()),f(list(x=1),1,1,list())))
 expect_identical(truth,t$data$truth)
 for(v in list(TRUE,"a",list(1,2))) {
  f<-local({x<-v;function() x});g<-.probe_clone(f,list(truth=v),2)
  expect_false(identical(f(),g()))
 }
 expect_error(.accessible(t$spec,list()));expect_error(.call(NULL,list()))
 expect_error(.unit_data(t$spec,list()))
})
test_that("qualification rejects malformed results and exploratory evidence is suppressed", {
 expect_error(apply_decision_map(list()))
 t<-toy_spec();s<-redeclare(t,list(decision_map=decision_map(list(list(id="bad",priority=1L,when="worlds",then="INVALID",because="Malformed predicate return")))))
 s<-freeze_audit(s);run<-run_audit(check_audit(s,t$data),t$data);expect_error(apply_decision_map(run))
 t<-toy_spec("SUPPORTED_WITHIN_DOMAIN");run<-stress_response(t$spec,t$data,constructibility_census(t$spec,t$data))
 expect_identical(evidence_record(run)$interpretation_license$disposition,"SUPPRESSED")
})
test_that("construction records failures without replacing their reason", {
 t<-toy_spec()
 make<-function(fn,cs=list(),cap=NULL) alternative_spec("failure cases",fn,"x","truth",character(),constraints=cs,capacity_fn=cap)
 for(fn in list(function(accessible,severity,seed,constraints) 1,function(accessible,severity,seed,constraints) list(status="FAILED",failure_class="bad"))) {
  s<-redeclare(t,list(alternative_spec=make(fn)));expect_identical(.attempt_world(s,t$data,1,1)$failure_class,"GENERATOR_ERROR")
 }
 a<-make(function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x),list(no=function(world) FALSE))
 s<-redeclare(t,list(alternative_spec=a));expect_identical(.attempt_world(s,t$data,1,1)$failure_class,"CONSTRAINT_FAILURE")
 a<-make(function(accessible,severity,seed,constraints) list(status="FAILED",failure_class="INVALID_INPUT"))
 s<-redeclare(t,list(alternative_spec=a));expect_error(.cell_capacity(s,t$data,1,2,.05),class="evaluably_error_truth_leakage")
 a<-make(function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x),cap=function(accessible,severity,constraints) FALSE)
 s<-redeclare(t,list(alternative_spec=a));expect_error(.cell_capacity(s,t$data,1,2,.05))
 u<-t$spec$unit_spec
 u<-unit_spec(u$scientific,u$computational,u$resampling,u$blocking,u$nesting,u$id_map,aggregation_fn=function(x) "bad")
 expect_identical(.attempt_world(redeclare(t,list(unit_spec=u)),t$data,1,1)$failure_class,"GENERATOR_ERROR")
})
