#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the hash of a raw vector or string
#' 
#' This performs a hash of the raw bytes - not of the serialized representation.
#' 
#' @param vec raw vector or single character string
#' @param algo Select the specific xxhash algorithm. Default: 'xxh128'.
#'        (the latest algorithm in the xxhash family)
#'        Valid values: 'xxh32', 'xxh64', 'xxh128', 'xxh3'
#' @param as_raw Return the hash as a raw vector of bytes instead of string?
#'        Default: FALSE.  If TRUE, then the raw bytes are returned in big-endian
#'        order - which is what \code{xxHash} considers the \emph{canonical} form.
#' 
#' @return String representation of hash. If \code{as_raw = TRUE} then a 
#'         raw vector is returned instead.
#'         
#' @examples
#' vec <- "hello"
#' xxhash_raw(vec)
#' vec <- as.raw(c(0x01, 0x02, 0x99))
#' xxhash_raw(vec)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xxhash_raw <- function(vec, algo = "xxh128", as_raw = FALSE) {
  .Call(xxhash_raw_, vec, algo, as_raw) # Only really used as reference for the `dv_xxh_*` bulk hashes
}
