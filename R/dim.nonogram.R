#' dim.nonogram
#'
#' @description Returns the dimensions of a nonogram object.
#' @param nonogram 
#'
#' @return
#' @export
#'
#' @examples
dim.nonogram <- function(nonogram) {
  c(nonogram$nrows, nonogram$ncolumns)
}
