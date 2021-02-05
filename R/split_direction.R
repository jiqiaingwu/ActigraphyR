#' Split a large table in one direction if there are blank columns or rows
#'
#' @param df Actigraphy Raw data
#' @return The lengths and values of runs of equal values in a vector.
#' @export


split_direction <- function(df,direction = "col"){
  if(direction == "col"){
    col_has_data <- unname(map_lgl(df,~!all(is.na(.x)))) ## map_lgl() returns a logical vector
    df_mapping <- make_df_index(col_has_data)
    out <- map(df_mapping,~df[,.x])
  } else if(direction == "row"){
    row_has_data <- df %>%
      mutate_all(~!is.na(.x)) %>%
      as.matrix() %>%
      apply(1,any)
    df_mapping <- make_df_index(row_has_data)
    out <- map(df_mapping,~df[.x,])
  }
  return(out)
}
