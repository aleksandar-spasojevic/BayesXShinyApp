
#' @export
data_.default <- function(obj, ...){
  return( get_(obj, "Data")[variables(obj)] )
}


#' @export
ranges <- function(obj, ...) UseMethod("ranges")

#' @export
ranges.default <- function(obj, ...){
  return( get_(obj, "Ranges")[variables(obj)] )
}


#' @export
sequences <- function(obj, ...) UseMethod("sequences")

#' @export
sequences.default <- function(obj, ...){
  return( get_(obj, "Sequences")[variables(obj)] )
}
