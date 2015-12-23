
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


.get_ranges <- function(Data){
  lapply(Data, function(var) {
    if( is.numeric(var) && !is.integer(var) ){
      r <- range(var)
      return( c(floor(r[1]), ceiling(r[2])) )
    } else {
      # 'factor' or 'character'
      return( unique(var) )
    }
  })
}


.range_to_sequence <- function(Ranges, length.out = 5) {
  lapply(Ranges, function(var) {
    if( is.numeric(var) && !is.integer(var) ){
      seq(floor(var[1]), ceiling(var[2]), length.out = length.out)
    } else {
      # 'factor' or 'character'
      return( var )
    }
  })
}

