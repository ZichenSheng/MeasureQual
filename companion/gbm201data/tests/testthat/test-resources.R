test_that("frozen exports and resource keys are exact", {
 expect_setequal(getNamespaceExports("gbm201data"),c("gbm_signature","gbm_membership","gbm_representation","gbm_evaluability","gbm_claims","gbm_provenance","gbm_vocab"))
 m<-.table("PRIMARY201_MANIFEST");g<-.table("GENE_MEMBERSHIP_LONG")
 expect_equal(nrow(m),201);expect_false(anyDuplicated(m$model_instance_id)>0)
 expect_true(all(g$model_instance_id%in%m$model_instance_id));expect_false(anyDuplicated(g[c("model_instance_id","gene_symbol")])>0)
 r<-.table("REPRESENTATION_REGISTRY");expect_true(all(r$model_instance_id%in%m$model_instance_id));expect_false(anyDuplicated(r$representation_id)>0)
 e<-gbm_evaluability();expect_true(all(e$model_instance_id%in%m$model_instance_id));expect_true(all(e$claim_id%in%gbm_claims()$claim_id))
 expect_false(anyDuplicated(e[c("model_instance_id","claim_id","design_id")])>0)
 expect_identical(e$evaluable[1],FALSE);expect_equal(sum(e$evaluable),7)
 expect_true(all(e$denominator==8));expect_identical(e$stop_reason[1],"ALPHA_UNREACHABLE")
 expect_equal(nrow(gbm_membership("PF4C-B4-007")),5)
})
test_that("resource hash manifest verifies every shipped TSV", {
 h<-.table("HASHES")
 expect_false(anyDuplicated(h$file_path)>0)
 for(i in seq_len(nrow(h))) {
  p<-system.file("extdata",h$file_path[i],package="gbm201data")
  expect_true(file.exists(p));expect_equal(unname(file.info(p)$size),h$byte_size[i])
  expect_identical(digest::digest(file=p,algo="sha256"),h$sha256[i])
 }
 expect_equal(length(list.files(system.file("extdata",package="gbm201data"),pattern="tsv$")),9)
})
test_that("cascade preserves frozen partitions and NA is not imputed", {
 c<-gbm_provenance()$cascade
 expect_equal(c$from_n[c$step_id=="primary_operator"],232);expect_equal(c$to_n[c$step_id=="primary_operator"],201)
 expect_equal(sum(c$to_n[c$step_id%in%c("primary_pre","primary_post")]),201)
 expect_equal(nrow(gbm_provenance()$exclusions),70)
 expect_true(all(is.na(.table("PRIMARY201_MANIFEST")$claim_family)))
 expect_true(all(is.na(.table("REPRESENTATION_REGISTRY")$representation_hash)))
 expect_error(gbm_vocab("../PRIMARY201_MANIFEST"))
 expect_false("evaluably"%in%unlist(utils::packageDescription("gbm201data")[c("Depends","Imports","Suggests")]))
})
