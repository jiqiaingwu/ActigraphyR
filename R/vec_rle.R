#' Utility function to get rle as a named vector
#'
#' @param v import Actigraphy Raw data
#' @return The lengths and values of runs of equal values in a vector.
#' @export


vec_rle <- function(v){
  temp <- rle(v)  #Compute the lengths and values of runs of equal values in a vector
  out <- temp$values #Vector value
  names(out) <- temp$lengths #put length as value's name
  return(out)
}
