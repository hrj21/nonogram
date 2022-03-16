#' as.data.frame.nonogram
#'
#' @description Extracts the solution of a solved nonogram into a tidy data.frame.
#'     Used primarily for tidying the data prior to plotting.
#' @param nonogram An object of class nonogram.
#' @param with_solution `TRUE` (default) or `FALSE`. Whether the data.frame should 
#'     contain the nonogram solution.
#'
#' @return A data.frame with 3 columns:
#' \itemize{
#'  \item{`value`: } {If `with_solution` = `TRUE`, the solution for each position. All 0 otherwise.}
#'  \item{`column`: }{The column index where 1 = the leftmost column.}
#'  \item{`row`: }   {The row index where 1 = the top row.}
#' }
#' 
#' @details If `with_solution` = `FALSE`, the function will return a data.frame
#'     with the appropriate column and row indices, but the `value` column will
#'     contain only 0s. This is only really useful when plotting a nonogram without
#'     its solution. If `with_solution` = `TRUE` and the nonogram does not have a 
#'     solution, the function will throw an error.
#'     
#' @export
#'
#' @examples
#' f <- examples[[1]] |> solve(verbose = FALSE)
#' as.data.frame(f)
#' as.data.frame(f, with_solution = FALSE)
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