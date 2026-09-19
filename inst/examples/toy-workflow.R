toy_spec <- function(state=NULL,endpoint="SUPPORTED",min_p=.01,capacity=TRUE,subject="S1",truth_dependence=FALSE,leak=FALSE) {
 ids<-data.frame(observation=paste0("o",1:4),patient=paste0("p",1:4),block=c("b1","b1","b2","b2"),cohort="c")
 rep<-representation_spec("r","UNWEIGHTED_MEMBERSHIP",c("A","B"),"MEAN","NONE","NONE","DROP",data.frame(declared_feature=c("A","B"),resolved_feature=c("A","B"),mapping_rule_id="identity",resolved=TRUE),derived_from_original=FALSE,equivalence_status="NOT_APPLICABLE")
 units<-unit_spec("patient","observation","patient","block",data.frame(child=c("observation","patient","block"),parent=c("patient","block","cohort")),ids)
 ref<-reference_spec("ANALYTIC_EXPECTATION","APPROXIMATE",function(x) 0,"synthetic",character(),"1")
 fn<-function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x+severity)
 d<-NULL;truth<-c(11,13,17,19)
 if(leak) {fn<-function(accessible,severity,seed,constraints) list(status="CONSTRUCTED",world=accessible$x+severity+truth);d<-list(packages=character(),constants=list(truth=truth),functions=list())}
 cap<-local({yes<-capacity;function(accessible,severity,constraints) list(has_capacity=yes,capacity_basis="ANALYTIC_PRE_ATTEMPT",detail=list())})
 alt<-alternative_spec("shift",fn,"x","truth",character(),truth_dependence_declared=truth_dependence,truth_dependence_rationale=if(truth_dependence) "Declared planting dependence" else NULL,
 capacity_fn=cap,claims_capacity_check=TRUE,generator_fn_depends_on=d,capacity_fn_depends_on=list(packages=character(),constants=list(yes=capacity),functions=list()))
 support<-local({p<-min_p;function(spec,data) list(min_attainable_p=p,support_cardinality=100L)})
 inf<-inferential_rule(.05,"PERMUTATION_EXACT","ONE_SIDED_GREATER","INCLUSIVE_OBSERVED","EXACT","EXACT_ENUMERATION",support_fn=support,support_fn_depends_on=list(packages=character(),constants=list(p=min_p),functions=list()))
 rules<-if(is.null(state)) list() else list(list(id="R1",priority=1L,when="TRUE",then=state,because="Synthetic author-declared rule"))
 spec<-audit_spec(list(subject_id=subject,subject_label=subject,subject_provenance="synthetic"),list(claim_id="C1",claim_family="recovery",text="Declared synthetic claim",estimand="response",scope="toy context"),"MEASUREMENT_CAPABILITY",list(context_id="X",description="Synthetic context"),endpoint,
 metric_fn=function(world) world,metric_direction="greater",severity_grid=c(0,.5,1),representation_spec=rep,unit_spec=units,reference_spec=ref,alternative_spec=alt,inferential_rule=inf,decision_map=decision_map(rules),n_worlds=3L,
 null_generator=function(accessible,seed) accessible$x,stable_flag_rule=function(values,severity,spec) severity>0,
 min_constructibility=.1)
 list(spec=spec,data=list(id_map=ids,x=setNames(c(0,1,2,3),ids$patient),truth=truth))
}
