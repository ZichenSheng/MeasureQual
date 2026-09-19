.table <- function(name) {
 path<-system.file("extdata",paste0(name,".tsv"),package="gbm201data")
 if(!nzchar(path)) stop("Unknown resource table: ",name,call.=FALSE)
 utils::read.delim(path,check.names=FALSE,stringsAsFactors=FALSE,na.strings="NA",quote="",comment.char="")
}
.filter <- function(x,column,value) {
 if(is.null(value)) return(x)
 if(!is.character(value)||anyNA(value)) stop("Identifiers must be nonmissing character strings",call.=FALSE)
 x[!is.na(x[[column]])&x[[column]]%in%value,,drop=FALSE]
}
#' Retrieve Frozen Model Membership and Provenance
#'
#' Returns manifest rows from the frozen Primary201 population; this is not a validation verdict.
#' @param id Model-instance identifiers.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_signature("PF4C-B4-007")
#' @export
gbm_signature <- function(id) .filter(.table("PRIMARY201_MANIFEST"),"model_instance_id",id)
#' Retrieve Canonical Frozen Gene Membership
#'
#' Returns only frozen membership mapped by the recorded crosswalk, with the declared membership hash.
#' @param id Model-instance identifiers.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_membership("PF4C-B4-007")
#' @export
gbm_membership <- function(id) {
 x<-.filter(.table("GENE_MEMBERSHIP_LONG"),"model_instance_id",id)
 m<-gbm_signature(id)
 x$gene_membership_hash<-m$gene_membership_hash[match(x$model_instance_id,m$model_instance_id)]
 x
}
#' Retrieve Explicit Representation Declarations
#'
#' Only identities recorded in the frozen registry have rows. Absent rows do not imply equivalence or non-equivalence.
#' @param id Model-instance identifiers.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_representation("PF4C-B4-007")
#' @export
gbm_representation <- function(id) .filter(.table("REPRESENTATION_REGISTRY"),"model_instance_id",id)
#' Retrieve Claim and Design Scoped Evaluability
#'
#' Explicit non-evaluable rows are returned first. Missing source values are never converted to false. Coverage is limited to recorded claim/design combinations.
#' @param id Optional model-instance identifiers.
#' @param claim_id Optional claim identifiers.
#' @param design_id Optional design identifiers.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_evaluability()
#' @export
gbm_evaluability <- function(id=NULL,claim_id=NULL,design_id=NULL) {
 x<-.filter(.filter(.filter(.table("EVALUABILITY_MATRIX"),"model_instance_id",id),"claim_id",claim_id),"design_id",design_id)
 # Only explicit FALSE rows lead. Unknown source values are never false.
 x[c(which(!is.na(x$evaluable)&!x$evaluable),which(!is.na(x$evaluable)&x$evaluable),which(is.na(x$evaluable))),,drop=FALSE]
}
#' Resolve Claim Identifiers
#'
#' Returns frozen claim descriptions used by the evaluability table. It does not create new claims or transfer qualifications.
#' @param claim_id Optional claim identifiers.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_claims()
#' @export
gbm_claims <- function(claim_id=NULL) .filter(.table("CLAIM_REGISTRY"),"claim_id",claim_id)
#' Retrieve Cascade and Amendment Provenance
#'
#' Returns cascade, exclusion, amendment and file-hash tables. Exclusion model IDs refer to the upstream 271-instance universe, which intentionally includes models outside Primary201.
#' @param id Optional model-instance identifiers for exclusions.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_provenance("PF4C-B4-007")
#' @export
gbm_provenance <- function(id=NULL) list(cascade=.table("DERIVATION_CASCADE"),exclusions=.filter(.table("EXCLUSION_LEDGER"),"model_instance_id",id),amendments=.table("AMENDMENT_REGISTRY"),hashes=.table("HASHES"))
#' Retrieve a Frozen Controlled Vocabulary
#'
#' Returns a vocabulary table without imposing an ordering or numeric state encoding.
#' @param name Vocabulary name: terminal_state, software_flag or stop_reason.
#' @return A plain data frame, or a named list of provenance tables.
#' @section Errors:
#' Invalid identifier types and unknown vocabulary names error. Unrecorded
#' source values remain NA; the literal NOT_APPLICABLE is distinct.
#' @examples
#' gbm_vocab("terminal_state")
#' @export
gbm_vocab <- function(name) {
 if(!is.character(name)||length(name)!=1L||is.na(name)||!grepl("^[a-z_]+$",name)) stop("Invalid vocabulary name",call.=FALSE)
 .table(paste0("vocab/",name))
}
