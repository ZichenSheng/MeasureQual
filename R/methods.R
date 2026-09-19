# Ordinary R mutation can be forced by removing classes; verification is authoritative.
.format_object <- function(x) {
 if(inherits(x,"evidence_record")) return(paste("evidence_record",x$identity$record_id,x$identity$claim_id,x$identity$context_id,x$qualification$terminal_state,x$interpretation_license$disposition))
 if(inherits(x,"audit_check")) return(paste("audit_check:",x$entry_gate,"at",x$blocking_gate,"reason",x$stop_reason))
 if(inherits(x,"audit_result")) return(paste("audit_result:",as.character(x$terminal_state),"claim",x$spec$claim$claim_id,"context",x$spec$context$context_id))
 if(inherits(x,"audit_collection")) return(paste("audit_collection:",length(x$records),"records; NOT_A_SIGNATURE_SUMMARY; aggregation_allowed =",x$aggregation_allowed))
 if(inherits(x,"representation_spec")) return(paste("representation_spec",x$representation_id,x$object_type,"equivalence",x$equivalence_status))
 paste(class(x)[1L],if(!is.null(x$lifecycle)) x$lifecycle else "",if(!is.null(x$spec_hash)) x$spec_hash else "")
}
.immutable <- function(...) .abort("Immutable object; use declaration/lifecycle functions")

#' @export
format.representation_spec <- function(x,...) .format_object(x)

#' @export
print.representation_spec <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.representation_spec <- function(object,...) .plain(object)

#' @export
format.unit_spec <- function(x,...) .format_object(x)

#' @export
print.unit_spec <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.unit_spec <- function(object,...) .plain(object)

#' @export
format.reference_spec <- function(x,...) .format_object(x)

#' @export
print.reference_spec <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.reference_spec <- function(object,...) .plain(object)

#' @export
format.alternative_spec <- function(x,...) .format_object(x)

#' @export
print.alternative_spec <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.alternative_spec <- function(object,...) .plain(object)

#' @export
format.inferential_rule <- function(x,...) .format_object(x)

#' @export
print.inferential_rule <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.inferential_rule <- function(object,...) .plain(object)

#' @export
format.decision_map <- function(x,...) .format_object(x)

#' @export
print.decision_map <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.decision_map <- function(object,...) .plain(object)

#' @export
format.audit_spec <- function(x,...) .format_object(x)

#' @export
print.audit_spec <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.audit_spec <- function(object,...) .plain(object)

#' @export
format.audit_check <- function(x,...) .format_object(x)

#' @export
print.audit_check <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.audit_check <- function(object,...) .plain(object)

#' @export
format.constructibility_census <- function(x,...) .format_object(x)

#' @export
print.constructibility_census <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.constructibility_census <- function(object,...) .plain(object)

#' @export
format.audit_run <- function(x,...) .format_object(x)

#' @export
print.audit_run <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.audit_run <- function(object,...) .plain(object)

#' @export
format.audit_result <- function(x,...) .format_object(x)

#' @export
print.audit_result <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.audit_result <- function(object,...) .plain(object)

#' @export
format.evidence_record <- function(x,...) .format_object(x)

#' @export
print.evidence_record <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.evidence_record <- function(object,...) .plain(object)

#' @export
format.audit_collection <- function(x,...) .format_object(x)

#' @export
print.audit_collection <- function(x,...) {cat(format(x),"\n");invisible(x)}

#' @export
summary.audit_collection <- function(object,...) .plain(object)

#' @export
`$<-.unit_spec` <- function(x,...,value) .immutable()

#' @export
`[<-.unit_spec` <- function(x,...,value) .immutable()

#' @export
`[[<-.unit_spec` <- function(x,...,value) .immutable()

#' @export
`$<-.reference_spec` <- function(x,...,value) .immutable()

#' @export
`[<-.reference_spec` <- function(x,...,value) .immutable()

#' @export
`[[<-.reference_spec` <- function(x,...,value) .immutable()

#' @export
`$<-.alternative_spec` <- function(x,...,value) .immutable()

#' @export
`[<-.alternative_spec` <- function(x,...,value) .immutable()

#' @export
`[[<-.alternative_spec` <- function(x,...,value) .immutable()

#' @export
`$<-.decision_map` <- function(x,...,value) .immutable()

#' @export
`[<-.decision_map` <- function(x,...,value) .immutable()

#' @export
`[[<-.decision_map` <- function(x,...,value) .immutable()

#' @export
`$<-.audit_spec` <- function(x,...,value) .immutable()

#' @export
`[<-.audit_spec` <- function(x,...,value) .immutable()

#' @export
`[[<-.audit_spec` <- function(x,...,value) .immutable()

#' @export
`$<-.audit_check` <- function(x,...,value) .immutable()

#' @export
`[<-.audit_check` <- function(x,...,value) .immutable()

#' @export
`[[<-.audit_check` <- function(x,...,value) .immutable()

#' @export
`$<-.audit_result` <- function(x,...,value) .immutable()

#' @export
`[<-.audit_result` <- function(x,...,value) .immutable()

#' @export
`[[<-.audit_result` <- function(x,...,value) .immutable()

#' @export
`$<-.evidence_record` <- function(x,...,value) .immutable()

#' @export
`[<-.evidence_record` <- function(x,...,value) .immutable()

#' @export
`[[<-.evidence_record` <- function(x,...,value) .immutable()

#' @export
`$<-.audit_collection` <- function(x,...,value) .immutable()

#' @export
`[<-.audit_collection` <- function(x,...,value) .immutable()

#' @export
`[[<-.audit_collection` <- function(x,...,value) .immutable()

#' @export
`$<-.constructibility_census` <- function(x,...,value) .immutable()

#' @export
`[<-.constructibility_census` <- function(x,...,value) .immutable()

#' @export
`[[<-.constructibility_census` <- function(x,...,value) .immutable()

#' @export
`$<-.audit_run` <- function(x,...,value) .immutable()

#' @export
`[<-.audit_run` <- function(x,...,value) .immutable()

#' @export
`[[<-.audit_run` <- function(x,...,value) .immutable()
