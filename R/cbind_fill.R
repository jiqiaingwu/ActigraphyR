#' Combine Arbitrary Data Types, Filling In Missing Rows.
#'
#' @param ... any number of R data objects.
#' @return Combined data.
#' @export

cbind.fill <- function(...){
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}
