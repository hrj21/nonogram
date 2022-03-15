#' as.data.frame.nonogram
#'
#' @description Extracts the solution of a solved nonogram into a tidy data.frame.
#' @param nonogram 
#' @param with_solution 
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.nonogram <- function(nonogram, with_solution = TRUE) {
  if(!nonogram$solved & with_solution) stop("This nonogram has not been solved.")
  
  if(with_solution) {
    data.frame(
      value = factor(unlist(nonogram$column_solution)),
      column = factor(rep(1:nonogram$ncolumns, each = nonogram$nrows)),
      row = factor(rep(1:nonogram$nrows, nonogram$ncolumns))
    )
  } else {
    data.frame(
      value = factor(0),
      column = factor(rep(1:nonogram$ncolumns, each = nonogram$nrows)),
      row = factor(rep(1:nonogram$nrows, nonogram$ncolumns))
    )
  }
}