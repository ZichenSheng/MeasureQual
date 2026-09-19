test_that("all thirteen real object classes have descriptive methods", {
 t<-toy_spec();s<-freeze_audit(t$spec);c<-check_audit(s,t$data);run<-run_audit(c,t$data)
 result<-apply_decision_map(run);r<-evidence_record(run,render=FALSE)
 objects<-c(unname(s[c("representation_spec","unit_spec","reference_spec","alternative_spec","inferential_rule","decision_map")]),list(s,c,c$census,run,result,r,audit_collection(list(r),"test display")))
 expect_setequal(vapply(objects,function(x) class(x)[1],character(1)),.s3_classes)
 for(x in objects) {
  expect_type(format(x),"character");expect_output(print(x));expect_type(summary(x),"list")
  {
   expect_error(x$injected<-TRUE,class="evaluably_error_invalid_spec")
   expect_error(x[[1]]<-NULL,class="evaluably_error_invalid_spec")
   expect_error(x[1]<-list(NULL),class="evaluably_error_invalid_spec")
  }
 }
})
test_that("every terminal state and scope restriction is represented without ranking", {
 for(state in .terminal_states) {
  t<-toy_spec(state);s<-freeze_audit(t$spec);run<-run_audit(check_audit(s,t$data),t$data)
  result<-apply_decision_map(run);r<-evidence_record(run,render=FALSE)
  expect_identical(as.character(result$terminal_state),state)
  expect_false(is.ordered(result$terminal_state));expect_true(verify_audit(result));expect_true(verify_audit(r))
  expect_type(result$terminal_state,"character");expect_null(attr(result$terminal_state,"levels"))
 }
})
test_that("tampering and stale receipts cannot change the scientific contract", {
 t<-toy_spec();s<-freeze_audit(t$spec)
 for(field in c("representation_hash","representation_rule_hash","unit_spec_hash","metric_hash","audit_member_key","spec_hash")) {
  z<-unclass(s);z[[field]]<-paste(rep("0",64),collapse="");class(z)<-"audit_spec"
  expect_false(verify_audit(z));expect_error(check_audit(z,t$data),class="evaluably_error_freeze_verification")
 }
 c<-check_audit(s,t$data);run<-run_audit(c,t$data)
 expect_error(apply_decision_map(run,decision_map(list(list(id="x",priority=1,when="TRUE",then="INVALID",because="test")))) ,class="evaluably_error_decision_map_hash_mismatch")
 expect_error(apply_decision_map(run,census=list()),class="evaluably_error_freeze_verification")
 expect_error(apply_decision_map(run,integrity=list()),class="evaluably_error_freeze_verification")
 expect_error(apply_decision_map(c))
 expect_error(evidence_record(run,relationships=list(list(inherits_evidence_from="other"))))
 expect_error(.runtime_fingerprint(list(callback=function() 1)),class="evaluably_error_unserializable_data")
})
test_that("records render in all formats without altering scientific identity", {
 t<-toy_spec();s<-freeze_audit(t$spec);r<-evidence_record(run_audit(check_audit(s,t$data),t$data),render=FALSE)
 p<-tempfile();out<-render_audit(r,path=p)
 expect_true(file.exists(file.path(p,"audit_record.json")))
 expect_true(verify_audit(file.path(p,"audit_record.json")))
 expect_true(verify_audit(r));expect_error(render_audit(r,format="bad"))
 expect_equal(.cp_lower(100,100,.05),.9704869503929601,tolerance=1e-14)
 expect_identical(.cp_lower(0,100,.05),0)
})
