#' Split a large table into smaller tables if there are blank columns or rows (if you still see entire rows or columns missing. Please increase complexity)
#'
#' @param df Actigraphy Raw data
#' @return The lengths and values of runs of equal values in a vector.
#' @export


split_df <- function(df,showWarnig = TRUE,complexity = 1){
  if(showWarnig){
    warning("Please don't use first row as column names.")
  }

  out <- split_direction(df,"col")

  for(i in 1 :complexity){
    out <- out %>%
      map(~split_direction(.x,"row")) %>%
      flatten() %>%
      map(~split_direction(.x,"col")) %>%
      flatten()
  }
  return(out)

}
