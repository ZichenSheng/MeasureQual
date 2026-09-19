test_that("complete unresolved workflow emits independently verifiable record", {
 t<-toy_spec();s<-freeze_audit(t$spec);expect_true(verify_audit(s))
 chk<-check_audit(s,t$data);expect_identical(chk$entry_gate,"PASS")
 run<-run_audit(chk,t$data);expect_s3_class(run,"audit_run");expect_null(run$terminal_state)
 rec<-evidence_record(run,render=FALSE);expect_true(verify_audit(rec));expect_identical(rec$qualification$terminal_state,"UNRESOLVED")
 expect_identical(rec$interpretation_license$disposition,"NOT_LICENSED")
 p<-tempfile();render_audit(rec,path=p);expect_true(verify_audit(file.path(p,"audit_record.json")))
})
test_that("all blocking routes preserve fixed gate order", {
 for(kind in c("unit","endpoint","unknown","truth","capacity","alpha")) {
  args<-switch(kind,unit=list(),endpoint=list(endpoint="INCOMPATIBLE"),unknown=list(endpoint="UNDETERMINED"),truth=list(leak=TRUE),capacity=list(capacity=FALSE),alpha=list(min_p=.0625))
  t<-do.call(toy_spec,args);s<-freeze_audit(t$spec)
  if(kind=="unit") t$data$id_map<-t$data$id_map[1:2,]
  chk<-check_audit(s,t$data);expect_identical(chk$entry_gate,"BLOCKED")
  expect_error(run_audit(chk,t$data),class="evaluably_error_entry_gate_blocked")
  rec<-evidence_record(chk,render=FALSE);expect_true(verify_audit(rec));expect_identical(rec$interpretation_license$disposition,"SUPPRESSED")
  gates<-c(chk$unit_gate,chk$endpoint_gate,chk$integrity$outcome,chk$capacity_gate,chk$support_gate)
  i<-switch(kind,unit=1L,endpoint=2L,unknown=2L,truth=3L,capacity=4L,alpha=5L)
  if(i<5L) expect_true(all(gates[(i+1L):5L]=="NOT_ASSESSED"))
 }
})
test_that("runtime handoff and no-rescue lifecycle are enforced", {
 t<-toy_spec("SUPPORTED_WITHIN_DOMAIN");s<-freeze_audit(t$spec);chk<-check_audit(s,t$data)
 altered<-t$data;altered$x[1]<-99
 expect_error(run_audit(chk,altered),class="evaluably_error_data_fingerprint_mismatch")
 run<-run_audit(chk,t$data)
 expect_warning(a<-amend_audit(s,"SCIENTIFIC","new endpoint","author",list(endpoint_compatibility_declared="UNDETERMINED")),class="evaluably_warning_post_hoc_amendment")
 expect_identical(a$lifecycle,"EXPLORATORY_POST_HOC");expect_error(freeze_audit(a),class="evaluably_error_not_frozen")
 expect_true(verify_audit(s));expect_true(verify_audit(a))
})
