#' Utility function to get rle(Compute the lengths and values of runs of equal values in a vector – or the reverse operation) as a named vector
#'
#' @param v import Actigraphy Raw data
#' @return The lengths and values of runs of equal values in a vector.
#' @export


vec_rle <- function(v){
  temp <- rle(v)
  out <- temp$values
  names(out) <- temp$lengths
  return(out)
}
