capture_messages <- function(expr) {
 messages<-list()
 value<-withCallingHandlers(expr,message=function(e) {messages[[length(messages)+1L]]<<-e;invokeRestart("muffleMessage")})
 list(value=value,messages=messages)
}
test_that("ISS-01 acceptable coverage emits the registered message, not a warning", {
 t<-toy_spec();args<-t$spec$representation_spec[intersect(names(t$spec$representation_spec),names(formals(representation_spec)))]
 args$feature_mapping$resolved[2]<-FALSE;args$feature_mapping$resolved_feature[2]<-NA_character_
 args$min_feature_coverage<-.5
 got<-capture_messages(do.call(representation_spec,args))
 expect_s3_class(got$value,"representation_spec");expect_length(got$messages,1L)
 expect_s3_class(got$messages[[1]],"evaluably_message_feature_coverage")
 expect_s3_class(got$messages[[1]],"evaluably_condition")
 expect_false(inherits(got$messages[[1]],"warning"))
 args$min_feature_coverage<-NULL
 expect_s3_class(capture_messages(do.call(representation_spec,args))$messages[[1]],"evaluably_message_feature_coverage")
 args$min_feature_coverage<-.75
 seen<-list()
 expect_error(withCallingHandlers(do.call(representation_spec,args),message=function(e) {seen[[length(seen)+1L]]<<-e;invokeRestart("muffleMessage")}),class="evaluably_error_feature_coverage")
 expect_length(seen,0L)
})
test_that("ISS-01 degenerate null informs and execution and record emission continue", {
 t<-toy_spec();t$data$x[]<-1
 s<-freeze_audit(t$spec);got<-capture_messages(run_audit(check_audit(s,t$data),t$data))
 expect_s3_class(got$value,"audit_run");expect_length(got$messages,1L)
 expect_s3_class(got$messages[[1]],"evaluably_message_degenerate_null")
 expect_s3_class(got$messages[[1]],"evaluably_condition")
 r<-evidence_record(got$value,render=FALSE)
 expect_true("DEGENERATE_NULL"%in%r$qualification$flags)
 expect_true(r$execution_evidence$executed);expect_true(verify_audit(r))
})
test_that("ISS-01 every registered condition has an actual signalling call", {
 found<-character()
 collect<-function(e) {
  if(is.character(e)) return(intersect(e,.condition_classes))
  if(is.call(e)||is.pairlist(e)||is.expression(e)||is.list(e)) return(unlist(lapply(as.list(e),collect),use.names=FALSE))
  character()
 }
 walk<-function(e) {
  if(!is.call(e)&&!is.expression(e)&&!is.pairlist(e)) return(invisible(NULL))
  if(is.call(e)) {
   callee<-paste(deparse(e[[1L]]),collapse="")
   if(callee%in%c(".abort",".warn",".inform","rlang::abort","rlang::warn","rlang::inform")) found<<-c(found,collect(as.list(e)[-1L]))
  }
  for(i in seq_along(e)) if(!identical(e[[i]],quote(expr=))) walk(e[[i]])
 }
 ns<-asNamespace("MeasureQual")
 for(n in ls(ns,all.names=TRUE)) {
  f<-get(n,ns)
  if(is.function(f)&&!is.primitive(f)) walk(body(f))
 }
 # The generic invalid-spec condition is the real default of the error helper.
 found<-c(found,collect(formals(.abort)$class))
 expect_setequal(unique(found),.condition_classes)
 expect_length(.condition_classes,29L)
})
test_that("ISS-02 dangerous census and run mutations fail", {
 t<-toy_spec(capacity=FALSE);c<-constructibility_census(freeze_audit(t$spec),t$data)
 expect_identical(c$entry_gate,"BLOCKED")
 expect_error(c$entry_gate<-"PASS",class="evaluably_error_invalid_spec")
 expect_identical(c$entry_gate,"BLOCKED")
 t<-toy_spec();r<-run_audit(check_audit(freeze_audit(t$spec),t$data),t$data)
 modified<-r$raw_by_severity;modified$median[]<-999
 expect_error(r$raw_by_severity<-modified,class="evaluably_error_invalid_spec")
 expect_false(any(r$raw_by_severity$median==999));expect_true(verify_audit(r))
})
test_that("ISS-03 character states retain all pre-factor-removal hashes", {
 for(state in .terminal_states) {
  t<-toy_spec(state);run<-run_audit(check_audit(freeze_audit(t$spec),t$data),t$data)
  result<-apply_decision_map(run);r<-evidence_record(run,render=FALSE)
  expect_type(result$terminal_state,"character");expect_length(result$terminal_state,1L)
  expect_null(attributes(result$terminal_state))
  # Emulate old storage only in a detached test payload; not a public mutation path.
  legacy<-unclass(result);legacy$terminal_state<-factor(state,levels=.terminal_states,ordered=FALSE)
  expect_identical(.hash(.plain(legacy)[setdiff(names(legacy),c("spec","result_hash"))]),result$result_hash)
  old_record<-.plain(r);old_record$qualification$terminal_state<-legacy$terminal_state
  hashes<-.record_hashes(old_record)$provenance
  expect_identical(hashes$record_scientific_hash,r$provenance$record_scientific_hash)
  expect_identical(hashes$record_file_hash,r$provenance$record_file_hash)
 }
 expect_false(any(grepl("rank|leaderboard|numeric",getNamespaceExports("MeasureQual"))))
})
