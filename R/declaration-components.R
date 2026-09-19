#' Declare Scientific and Computational Units
#'
#' Declares the four unit roles and a child-to-parent nesting DAG. The author chooses the scientific unit. Malformed declarations, cyclic nesting and resampling finer than the scientific unit fail before an audit exists.
#' @param scientific Author-chosen scientific unit node.
#' @param computational Computational unit node.
#' @param resampling Resampling unit node; may not be finer than scientific.
#' @param blocking Blocking node.
#' @param nesting Data frame with child and parent columns.
#' @param id_map Declared unit mapping data frame or callback; runtime data must realize its IDs.
#' @param aggregation_fn Optional callback mapping metric values to scientific units.
#' @param min_scientific_units Explicit min scientific units declaration; no scientific value is inferred by the engine.
#' @param world_is_scientific_unit Explicit world is scientific unit declaration; no scientific value is inferred by the engine.
#' @param world_unit_justification Explicit world unit justification declaration; no scientific value is inferred by the engine.
#' @param on_violation Visible unit-policy declaration; non-error policy suppresses interpretation.
#' @param aggregation_fn_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param id_map_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @return Immutable unit_spec with its scientific hash.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' t$spec$unit_spec
#' @export
unit_spec <- function(scientific, computational, resampling, blocking, nesting, id_map,
 aggregation_fn=NULL, min_scientific_units=NULL, world_is_scientific_unit=FALSE,
 world_unit_justification=NULL, on_violation="error", aggregation_fn_depends_on=NULL,
 id_map_depends_on=NULL) {
 bad <- function(m) .abort(m,"evaluably_error_illegal_unit")
 roles <- c(scientific,computational,resampling,blocking)
 if(length(roles)!=4L||anyNA(roles)||any(!nzchar(roles))) bad("Four nonempty unit roles are required")
 if(!is.data.frame(nesting)||!all(c("child","parent")%in%names(nesting))||!nrow(nesting)) bad("nesting requires child -> parent edges")
 if(anyNA(nesting)||any(!nzchar(as.character(unlist(nesting))))) bad("Malformed nesting identifier")
 nodes <- unique(c(nesting$child,nesting$parent));roots<-setdiff(nodes,nesting$child)
 if(length(roots)!=1L||!all(roles%in%nodes)) bad("Nesting requires a unique root and all four roles")
 ancestors <- function(n,path=character()) {
  if(n%in%path) bad("Cyclic unit nesting")
  parents<-nesting$parent[nesting$child==n]
  unique(c(parents,unlist(lapply(parents,ancestors,path=c(path,n)))))
 }
 for(n in nodes) ancestors(n)
 if(scientific%in%ancestors(resampling)) bad("Resampling is finer than the scientific unit")
 if(!is.data.frame(id_map)&&!is.function(id_map)) bad("id_map must be a data frame or declared callback")
 if(is.data.frame(id_map)) .validate_id_map(id_map,nesting,nodes,bad)
 if(!is.logical(world_is_scientific_unit)||length(world_is_scientific_unit)!=1L||is.na(world_is_scientific_unit)) bad("Invalid world unit declaration")
 if(world_is_scientific_unit) .require_text(world_unit_justification,"world_unit_justification")
 if(grepl("world",scientific,ignore.case=TRUE)&&!world_is_scientific_unit) bad("World scientific unit requires explicit justification")
 if(!is.null(min_scientific_units)) min_scientific_units<-.int(min_scientific_units,"min_scientific_units")
 .require_text(on_violation,"on_violation")
 flags<-character()
 if(on_violation!="error") {flags<-c(flags,"UNIT_MISMATCH_DECLARED");.warn("Unit mismatch declaration suppresses interpretation","evaluably_warning_unit_mismatch_declared")}
 if(world_is_scientific_unit) flags<-c(flags,"WORLD_AS_SCIENTIFIC_UNIT")
 if(!blocking%in%c(scientific,ancestors(scientific))) flags<-c(flags,"CROSSED_BLOCKING")
 ac<-.cb(aggregation_fn,aggregation_fn_depends_on,"aggregation_fn")
 ic<-if(is.function(id_map)) .cb(id_map,id_map_depends_on,"id_map") else NULL
 x<-list(scientific=scientific,computational=computational,resampling=resampling,blocking=blocking,
  nesting=nesting,id_map=if(is.function(id_map)) NULL else id_map,id_map_hash=ic$digest,id_map_depends_on=ic$manifest,
  aggregation_fn_hash=ac$digest,aggregation_fn_depends_on=ac$manifest,min_scientific_units=min_scientific_units,
  world_is_scientific_unit=world_is_scientific_unit,world_unit_justification=world_unit_justification,on_violation=on_violation,flags=flags)
 x$unit_spec_hash<-.hash(x)
 .attach_callbacks(structure(x,class="unit_spec"),list(aggregation_fn=aggregation_fn,id_map=if(is.function(id_map)) id_map else NULL),list(aggregation_fn=aggregation_fn_depends_on,id_map=id_map_depends_on))
}
.validate_id_map <- function(m,nesting,nodes,bad) {
 if(!is.data.frame(m)||!all(nodes%in%names(m))) bad("Incomplete id_map")
 for(n in nodes) if(anyNA(m[[n]])||any(!nzchar(as.character(m[[n]])))) bad("id_map has missing identifiers")
 for(i in seq_len(nrow(nesting))) {
  groups<-split(as.character(m[[nesting$parent[i]]]),as.character(m[[nesting$child[i]]]))
  if(any(vapply(groups,function(v) length(unique(v))>1L,logical(1)))) bad("id_map contradicts declared nesting")
 }
 invisible(TRUE)
}
#' Declare an Explicit Reference
#'
#' Declares a user-supplied reference; the package never derives one. Exact references require an explicit tolerance. External references require a reproduction-check declaration.
#' @param type Frozen reference type vocabulary member.
#' @param exactness Explicit reference or inferential exactness.
#' @param fn User callback; declare all scientific dependencies.
#' @param source Reference provenance prose.
#' @param nuisance_preserved Explicit nuisance properties retained by the reference.
#' @param version Author-supplied version string.
#' @param identity_tolerance Explicit identity tolerance declaration; no scientific value is inferred by the engine.
#' @param reproduction_check Explicit reproduction check declaration; no scientific value is inferred by the engine.
#' @param fn_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param doi Explicit doi declaration; no scientific value is inferred by the engine.
#' @return Immutable reference_spec with callback manifest and hash.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' t$spec$reference_spec
#' @export
reference_spec <- function(type, exactness, fn, source, nuisance_preserved, version,
 identity_tolerance=NULL, reproduction_check=NULL, fn_depends_on=NULL, doi=NULL) {
 .choice(type,c("EXACT_IDENTITY","ANALYTIC_EXPECTATION","EMPIRICAL_MATCHED_NULL","SIMULATION_REFERENCE","EXTERNAL_PUBLISHED_REFERENCE","NO_VALID_REFERENCE"),"type")
 .choice(exactness,c("EXACT","APPROXIMATE","EMPIRICAL"),"exactness")
 .require_text(source,"source");.require_text(version,"version");.strings(nuisance_preserved,"nuisance_preserved")
 if(type!="NO_VALID_REFERENCE"&&!is.function(fn)) .abort("This reference requires fn")
 if(type=="NO_VALID_REFERENCE"&&!is.null(fn)) .abort("NO_VALID_REFERENCE cannot carry a reference function")
 if(exactness=="EXACT") {
  if(is.null(fn)||is.null(identity_tolerance)) .abort("EXACT reference requires fn and identity_tolerance")
  .scalar_number(identity_tolerance,0,Inf,"identity_tolerance")
 } else if(!is.null(identity_tolerance)) .abort("identity_tolerance is required only for EXACT references")
 if(type=="EXTERNAL_PUBLISHED_REFERENCE") {
  if(is.null(reproduction_check)) .abort("External reference requires reproduction_check")
  .choice(if(is.list(reproduction_check)) reproduction_check$outcome else reproduction_check,c("PASS","FAIL","NOT_ATTEMPTED"),"reproduction_check")
 }
 if(!is.null(doi)) .require_text(doi,"doi")
 cb<-.cb(fn,fn_depends_on,"fn")
 x<-list(type=type,exactness=exactness,source=source,doi=doi,nuisance_preserved=nuisance_preserved,
 version=version,identity_tolerance=identity_tolerance,reproduction_check=reproduction_check,fn_hash=cb$digest,fn_depends_on=cb$manifest)
 x$reference_spec_hash<-.hash(x[setdiff(names(x),"source")])
 .attach_callbacks(structure(x,class="reference_spec"),list(fn=fn),list(fn=fn_depends_on))
}
#' Declare a Positive Alternative and Truth Partition
#'
#' Declares generator-accessible and protected objects and the generator callback. The author decides whether this partition and positive alternative are scientifically appropriate. Every callback dependency must be explicit.
#' @param name Explicit alternative identifier.
#' @param fn User callback; declare all scientific dependencies.
#' @param generator_accessible Names of data objects permitted as generator arguments.
#' @param protected_evaluation_truth Names of evaluator truth objects.
#' @param protected_assignment Names of protected assignment objects.
#' @param truth_dependence_declared Explicit truth dependence declared declaration; no scientific value is inferred by the engine.
#' @param truth_dependence_rationale Explicit truth dependence rationale declaration; no scientific value is inferred by the engine.
#' @param constraints Named world-validity predicate callbacks.
#' @param capacity_fn Explicit capacity fn declaration; no scientific value is inferred by the engine.
#' @param claims_capacity_check Explicit claims capacity check declaration; no scientific value is inferred by the engine.
#' @param generator_fn_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param capacity_fn_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @param constraints_depends_on Explicit nested callback dependency declaration: packages, constants and functions. Helpers use list(fn, depends_on); raw helper hashes are forbidden.
#' @return Immutable alternative_spec with generator and capacity identities.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' library(evaluably)
#' source(system.file("examples", "toy-workflow.R", package = "evaluably"))
#' t <- toy_spec()
#' s <- freeze_audit(t$spec)
#' t$spec$alternative_spec
#' @export
alternative_spec <- function(name, fn, generator_accessible, protected_evaluation_truth, protected_assignment,
 truth_dependence_declared=FALSE, truth_dependence_rationale=NULL, constraints=list(),
 capacity_fn=NULL, claims_capacity_check=FALSE, generator_fn_depends_on=NULL, capacity_fn_depends_on=NULL,
 constraints_depends_on=list()) {
 .require_text(name,"name");if(!is.function(fn)) .abort("Alternative requires generator fn")
 for(n in c("generator_accessible","protected_evaluation_truth","protected_assignment")) .strings(get(n),n)
 if(length(intersect(generator_accessible,c(protected_evaluation_truth,protected_assignment)))) .abort("Accessible/protected partition overlaps")
 if(!is.logical(truth_dependence_declared)||length(truth_dependence_declared)!=1L||is.na(truth_dependence_declared)) .abort("Invalid truth dependence declaration")
 if(truth_dependence_declared) .require_text(truth_dependence_rationale,"truth_dependence_rationale")
 if(!is.logical(claims_capacity_check)||length(claims_capacity_check)!=1L||is.na(claims_capacity_check)) .abort("Invalid claims_capacity_check")
 if(claims_capacity_check&&is.null(capacity_fn)) .abort("Claimed capacity checking requires capacity_fn")
 if(!is.list(constraints)||!.named(constraints)||!all(vapply(constraints,is.function,logical(1)))) .abort("constraints must be named predicate callbacks")
 cb<-.cb(fn,generator_fn_depends_on,"generator_fn");cp<-.cb(capacity_fn,capacity_fn_depends_on,"capacity_fn")
 cm<-lapply(names(constraints),function(n) .resolve_callback(constraints[[n]],constraints_depends_on[[n]],paste0("constraint_",n)))
 names(cm)<-names(constraints)
 x<-list(name=name,generator_accessible=generator_accessible,protected_evaluation_truth=protected_evaluation_truth,
  protected_assignment=protected_assignment,truth_dependence_declared=truth_dependence_declared,
  truth_dependence_rationale=truth_dependence_rationale,constraints=lapply(cm,`[[`,"manifest"),
  generator_fn_hash=cb$digest,generator_fn_depends_on=cb$manifest,capacity_fn_hash=cp$digest,capacity_fn_depends_on=cp$manifest,
  claims_capacity_check=claims_capacity_check)
 x$alternative_spec_hash<-.hash(x)
 x<-.attach_callbacks(structure(x,class="alternative_spec"),list(generator_fn=fn,capacity_fn=capacity_fn),list(generator_fn=generator_fn_depends_on,capacity_fn=capacity_fn_depends_on))
 attr(x,"constraint_callbacks")<-constraints;attr(x,"constraint_declarations")<-constraints_depends_on;x
}
.rule_symbols <- c(.flags,"alpha_reachable","support_gate","capacity_gate","unit_gate","endpoint_gate","equivalence_status","claim_class","direction_consistency_conditional","direction_consistency","recovery_fraction","detections","worlds","null_sd","stable_flag","detection_floor_bracketed","frozen_failure_mode","calibration_value","calibration_outcome","thresholds")
.rule_expression <- function(x) {
 if(is.character(x)&&length(x)==1L) {
  parsed<-parse(text=x,keep.source=FALSE)
  if(length(parsed)!=1L) .abort("Decision predicate must contain one expression")
  x<-parsed[[1L]]
 }
 if(is.expression(x)&&length(x)==1L) x<-x[[1L]]
 if(!is.call(x)&&!is.logical(x)&&!is.symbol(x)) .abort("Invalid decision predicate")
 x
}
#' Declare Ordered Decision Predicates
#'
#' Rules are evaluated by unique integer priority. Zero default rules are shipped. An empty map is valid. Undefined symbols and duplicate priorities fail closed.
#' @param rules List of id, priority, when, then and because declarations; empty is allowed.
#' @param version Author-supplied version string.
#' @return Immutable decision_map carrying its rule hash.
#' @section Errors:
#' Malformed contracts raise structured evaluably conditions. Verification and
#' phase mismatches fail closed; no decision rule rescues a blocked gate.
#' @examples
#' decision_map()
#' @export
decision_map <- function(rules=list(), version="1.0.0") {
 .require_text(version,"version");if(!is.list(rules)) .abort("rules must be a list")
 normalized<-lapply(rules,function(r) {
  if(!is.list(r)||!all(c("id","priority","when","then","because")%in%names(r))) .abort("Incomplete decision rule")
  .require_text(r$id,"id");.require_text(r$because,"because");r$priority<-.int(r$priority,"priority",0)
  .choice(r$then,.terminal_states,"then");e<-.rule_expression(r$when)
  unknown<-setdiff(.predicate_variables(e),.rule_symbols)
  if(length(unknown)) .abort(paste("Undefined decision symbols:",paste(unknown,collapse=", ")),"evaluably_error_undeclared_decision_rule")
  r$when<-paste(deparse(e,control=c("keepInteger","keepNA")),collapse="\n")
  r[c("id","priority","when","then","because")]
 })
 if(length(normalized)) {
  if(anyDuplicated(vapply(normalized,`[[`,integer(1),"priority"))||anyDuplicated(vapply(normalized,`[[`,character(1),"id"))) .abort("Duplicate decision priority or id")
  normalized<-normalized[order(vapply(normalized,`[[`,integer(1),"priority"))]
  previous<-character();always<-FALSE
  for(r in normalized) {
   if(always||r$when%in%previous||r$when=="FALSE") .warn(paste("Unreachable decision rule",r$id),"evaluably_warning_unreachable_rule")
   if(r$when=="TRUE") always<-TRUE
   previous<-c(previous,r$when)
  }
 }
 x<-list(rules=normalized,version=version);x$decision_map_hash<-.hash(x);structure(x,class="decision_map")
}

.predicate_variables <- function(e) {
 if(is.symbol(e)) return(as.character(e))
 if(!is.call(e)) return(character())
 if(identical(e[[1L]],as.name("$"))) return(.predicate_variables(e[[2L]]))
 unique(unlist(lapply(as.list(e)[-1L],.predicate_variables)))
}
